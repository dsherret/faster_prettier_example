/*@internal*/
namespace ts.server {
  interface LogOptions {
    file?: string;
    detailLevel?: LogLevel;
    traceToConsole?: boolean;
    logToFile?: boolean;
  }

  interface NodeChildProcess {
    send(message: any, sendHandle?: any): void;
    on(message: "message" | "exit", f: (m: any) => void): void;
    kill(): void;
    pid: number;
  }

  interface ReadLineOptions {
    input: NodeJS.ReadableStream;
    output?: NodeJS.WritableStream;
    terminal?: boolean;
    historySize?: number;
  }

  interface NodeSocket {
    write(data: string, encoding: string): boolean;
  }

  function parseLoggingEnvironmentString(
    logEnvStr: string | undefined,
  ): LogOptions {
    if (!logEnvStr) {
      return {};
    }
    const logEnv: LogOptions = { logToFile: true };
    const args = logEnvStr.split(" ");
    const len = args.length - 1;
    for (let i = 0; i < len; i += 2) {
      const option = args[i];
      const { value, extraPartCounter } = getEntireValue(i + 1);
      i += extraPartCounter;
      if (option && value) {
        switch (option) {
          case "-file":
            logEnv.file = value;
            break;
          case "-level":
            const level = getLogLevel(value);
            logEnv.detailLevel = level !== undefined ? level : LogLevel.normal;
            break;
          case "-traceToConsole":
            logEnv.traceToConsole = value.toLowerCase() === "true";
            break;
          case "-logToFile":
            logEnv.logToFile = value.toLowerCase() === "true";
            break;
        }
      }
    }
    return logEnv;

    function getEntireValue(initialIndex: number) {
      let pathStart = args[initialIndex];
      let extraPartCounter = 0;
      if (
        pathStart.charCodeAt(0) === CharacterCodes.doubleQuote
        && pathStart.charCodeAt(pathStart.length - 1)
          !== CharacterCodes.doubleQuote
      ) {
        for (let i = initialIndex + 1; i < args.length; i++) {
          pathStart += " ";
          pathStart += args[i];
          extraPartCounter++;
          if (
            pathStart.charCodeAt(pathStart.length - 1)
              === CharacterCodes.doubleQuote
          ) {
            break;
          }
        }
      }
      return { value: stripQuotes(pathStart), extraPartCounter };
    }
  }

  function parseServerMode(): LanguageServiceMode | string | undefined {
    const mode = findArgument("--serverMode");
    if (!mode) return undefined;

    switch (mode.toLowerCase()) {
      case "semantic":
        return LanguageServiceMode.Semantic;
      case "partialsemantic":
        return LanguageServiceMode.PartialSemantic;
      case "syntactic":
        return LanguageServiceMode.Syntactic;
      default:
        return mode;
    }
  }

  export function initializeNodeSystem(): StartInput {
    const sys = Debug.checkDefined(ts.sys) as ServerHost;
    const childProcess: {
      execFileSync(
        file: string,
        args: string[],
        options: { stdio: "ignore"; env: MapLike<string> },
      ): string | Buffer;
    } = require("child_process");

    interface Stats {
      isFile(): boolean;
      isDirectory(): boolean;
      isBlockDevice(): boolean;
      isCharacterDevice(): boolean;
      isSymbolicLink(): boolean;
      isFIFO(): boolean;
      isSocket(): boolean;
      dev: number;
      ino: number;
      mode: number;
      nlink: number;
      uid: number;
      gid: number;
      rdev: number;
      size: number;
      blksize: number;
      blocks: number;
      atime: Date;
      mtime: Date;
      ctime: Date;
      birthtime: Date;
    }

    const fs: {
      openSync(path: string, options: string): number;
      close(fd: number, callback: (err: NodeJS.ErrnoException) => void): void;
      writeSync(
        fd: number,
        buffer: Buffer,
        offset: number,
        length: number,
        position?: number,
      ): number;
      statSync(path: string): Stats;
      stat(
        path: string,
        callback?: (err: NodeJS.ErrnoException, stats: Stats) => any,
      ): void;
    } = require("fs");

    class Logger extends BaseLogger {
      private fd = -1;
      constructor(
        private readonly logFilename: string,
        private readonly traceToConsole: boolean,
        level: LogLevel,
      ) {
        super(level);
        if (this.logFilename) {
          try {
            this.fd = fs.openSync(this.logFilename, "w");
          } catch (_) {
            // swallow the error and keep logging disabled if file cannot be opened
          }
        }
      }

      close() {
        if (this.fd >= 0) {
          fs.close(this.fd, noop);
        }
      }

      getLogFileName() {
        return this.logFilename;
      }

      loggingEnabled() {
        return !!this.logFilename || this.traceToConsole;
      }

      protected canWrite() {
        return this.fd >= 0 || this.traceToConsole;
      }

      protected write(s: string, _type: Msg) {
        if (this.fd >= 0) {
          const buf = sys.bufferFrom!(s);
          // eslint-disable-next-line no-null/no-null
          fs.writeSync(
            this.fd,
            buf as globalThis.Buffer,
            0,
            buf.length,
            /*position*/ null!,
          ); // TODO: GH#18217
        }
        if (this.traceToConsole) {
          console.warn(s);
        }
      }
    }

    const nodeVersion = getNodeMajorVersion();
    // use watchGuard process on Windows when node version is 4 or later
    const useWatchGuard = process.platform === "win32" && nodeVersion! >= 4;
    const originalWatchDirectory: ServerHost["watchDirectory"] = sys.watchDirectory.bind(sys);
    const logger = createLogger();

    const pending: Buffer[] = [];
    let canWrite = true;

    if (useWatchGuard) {
      const currentDrive = extractWatchDirectoryCacheKey(
        sys.resolvePath(sys.getCurrentDirectory()),
        /*currentDriveKey*/ undefined,
      );
      const statusCache = new Map<string, boolean>();
      sys.watchDirectory = (path, callback, recursive, options) => {
        const cacheKey = extractWatchDirectoryCacheKey(path, currentDrive);
        let status = cacheKey && statusCache.get(cacheKey);
        if (status === undefined) {
          if (logger.hasLevel(LogLevel.verbose)) {
            logger.info(`${cacheKey} for path ${path} not found in cache...`);
          }
          try {
            const args = [combinePaths(__dirname, "watchGuard.js"), path];
            if (logger.hasLevel(LogLevel.verbose)) {
              logger.info(
                `Starting ${process.execPath} with args:${
                  stringifyIndented(
                    args,
                  )
                }`,
              );
            }
            childProcess.execFileSync(process.execPath, args, {
              stdio: "ignore",
              env: { ELECTRON_RUN_AS_NODE: "1" },
            });
            status = true;
            if (logger.hasLevel(LogLevel.verbose)) {
              logger.info(`WatchGuard for path ${path} returned: OK`);
            }
          } catch (e) {
            status = false;
            if (logger.hasLevel(LogLevel.verbose)) {
              logger.info(`WatchGuard for path ${path} returned: ${e.message}`);
            }
          }
          if (cacheKey) {
            statusCache.set(cacheKey, status);
          }
        } else if (logger.hasLevel(LogLevel.verbose)) {
          logger.info(
            `watchDirectory for ${path} uses cached drive information.`,
          );
        }
        if (status) {
          // this drive is safe to use - call real 'watchDirectory'
          return watchDirectorySwallowingException(
            path,
            callback,
            recursive,
            options,
          );
        } else {
          // this drive is unsafe - return no-op watcher
          return noopFileWatcher;
        }
      };
    } else {
      sys.watchDirectory = watchDirectorySwallowingException;
    }

    // Override sys.write because fs.writeSync is not reliable on Node 4
    sys.write = (s: string) => writeMessage(sys.bufferFrom!(s, "utf8") as globalThis.Buffer);
    // REVIEW: for now this implementation uses polling.
    // The advantage of polling is that it works reliably
    // on all os and with network mounted files.
    // For 90 referenced files, the average time to detect
    // changes is 2*msInterval (by default 5 seconds).
    // The overhead of this is .04 percent (1/2500) with
    // average pause of < 1 millisecond (and max
    // pause less than 1.5 milliseconds); question is
    // do we anticipate reference sets in the 100s and
    // do we care about waiting 10-20 seconds to detect
    // changes for large reference sets? If so, do we want
    // to increase the chunk size or decrease the interval
    // time dynamically to match the large reference set?
    sys.defaultWatchFileKind = () => WatchFileKind.FixedChunkSizePolling;

    /* eslint-disable no-restricted-globals */
    sys.setTimeout = setTimeout;
    sys.clearTimeout = clearTimeout;
    sys.setImmediate = setImmediate;
    sys.clearImmediate = clearImmediate;
    /* eslint-enable no-restricted-globals */

    if (typeof global !== "undefined" && global.gc) {
      sys.gc = () => global.gc?.();
    }

    sys.require = (initialDir: string, moduleName: string): RequireResult => {
      try {
        return {
          module: require(resolveJSModule(moduleName, initialDir, sys)),
          error: undefined,
        };
      } catch (error) {
        return { module: undefined, error };
      }
    };

    let cancellationToken: ServerCancellationToken;
    try {
      const factory = require("./cancellationToken");
      cancellationToken = factory(sys.args);
    } catch (e) {
      cancellationToken = nullCancellationToken;
    }

    const localeStr = findArgument("--locale");
    if (localeStr) {
      validateLocaleAndSetLanguage(localeStr, sys);
    }

    const modeOrUnknown = parseServerMode();
    let serverMode: LanguageServiceMode | undefined;
    let unknownServerMode: string | undefined;
    if (modeOrUnknown !== undefined) {
      if (typeof modeOrUnknown === "number") serverMode = modeOrUnknown;
      else unknownServerMode = modeOrUnknown;
    }
    return {
      args: process.argv,
      logger,
      cancellationToken,
      serverMode,
      unknownServerMode,
      startSession: startNodeSession,
    };

    // TSS_LOG "{ level: "normal | verbose | terse", file?: string}"
    function createLogger() {
      const cmdLineLogFileName = findArgument("--logFile");
      const cmdLineVerbosity = getLogLevel(findArgument("--logVerbosity"));
      const envLogOptions = parseLoggingEnvironmentString(process.env.TSS_LOG);

      const unsubstitutedLogFileName = cmdLineLogFileName
        ? stripQuotes(cmdLineLogFileName)
        : envLogOptions.logToFile
        ? envLogOptions.file || __dirname + "/.log" + process.pid.toString()
        : undefined;

      const substitutedLogFileName = unsubstitutedLogFileName
        ? unsubstitutedLogFileName.replace("PID", process.pid.toString())
        : undefined;

      const logVerbosity = cmdLineVerbosity || envLogOptions.detailLevel;
      return new Logger(
        substitutedLogFileName!,
        envLogOptions.traceToConsole!,
        logVerbosity!,
      ); // TODO: GH#18217
    }

    function writeMessage(buf: Buffer) {
      if (!canWrite) {
        pending.push(buf);
      } else {
        canWrite = false;
        process.stdout.write(buf, setCanWriteFlagAndWriteMessageIfNecessary);
      }
    }

    function setCanWriteFlagAndWriteMessageIfNecessary() {
      canWrite = true;
      if (pending.length) {
        writeMessage(pending.shift()!);
      }
    }

    function extractWatchDirectoryCacheKey(
      path: string,
      currentDriveKey: string | undefined,
    ) {
      path = normalizeSlashes(path);
      if (isUNCPath(path)) {
        // UNC path: extract server name
        // //server/location
        //         ^ <- from 0 to this position
        const firstSlash = path.indexOf(directorySeparator, 2);
        return firstSlash !== -1
          ? toFileNameLowerCase(path.substring(0, firstSlash))
          : path;
      }
      const rootLength = getRootLength(path);
      if (rootLength === 0) {
        // relative path - assume file is on the current drive
        return currentDriveKey;
      }
      if (
        path.charCodeAt(1) === CharacterCodes.colon
        && path.charCodeAt(2) === CharacterCodes.slash
      ) {
        // rooted path that starts with c:/... - extract drive letter
        return toFileNameLowerCase(path.charAt(0));
      }
      if (
        path.charCodeAt(0) === CharacterCodes.slash
        && path.charCodeAt(1) !== CharacterCodes.slash
      ) {
        // rooted path that starts with slash - /somename - use key for current drive
        return currentDriveKey;
      }
      // do not cache any other cases
      return undefined;
    }

    function isUNCPath(s: string): boolean {
      return (
        s.length > 2
        && s.charCodeAt(0) === CharacterCodes.slash
        && s.charCodeAt(1) === CharacterCodes.slash
      );
    }

    // This is the function that catches the exceptions when watching directory, and yet lets project service continue to function
    // Eg. on linux the number of watches are limited and one could easily exhaust watches and the exception ENOSPC is thrown when creating watcher at that point
    function watchDirectorySwallowingException(
      path: string,
      callback: DirectoryWatcherCallback,
      recursive?: boolean,
      options?: WatchOptions,
    ): FileWatcher {
      try {
        return originalWatchDirectory(path, callback, recursive, options);
      } catch (e) {
        logger.info(`Exception when creating directory watcher: ${e.message}`);
        return noopFileWatcher;
      }
    }
  }

  function parseEventPort(eventPortStr: string | undefined) {
    const eventPort = eventPortStr === undefined ? undefined : parseInt(eventPortStr);
    return eventPort !== undefined && !isNaN(eventPort) ? eventPort : undefined;
  }

  function startNodeSession(
    options: StartSessionOptions,
    logger: Logger,
    cancellationToken: ServerCancellationToken,
  ) {
    const childProcess: {
      fork(
        modulePath: string,
        args: string[],
        options?: { execArgv: string[]; env?: MapLike<string> },
      ): NodeChildProcess;
    } = require("child_process");

    const os: {
      homedir?(): string;
      tmpdir(): string;
    } = require("os");

    const net: {
      connect(options: { port: number }, onConnect?: () => void): NodeSocket;
    } = require("net");

    const readline: {
      createInterface(options: ReadLineOptions): NodeJS.EventEmitter;
    } = require("readline");

    const rl = readline.createInterface({
      input: process.stdin,
      output: process.stdout,
      terminal: false,
    });

    interface QueuedOperation {
      operationId: string;
      operation: () => void;
    }

    class NodeTypingsInstaller implements ITypingsInstaller {
      private installer!: NodeChildProcess;
      private projectService!: ProjectService;
      private activeRequestCount = 0;
      private requestQueue: QueuedOperation[] = [];
      private requestMap = new Map<string, QueuedOperation>(); // Maps operation ID to newest requestQueue entry with that ID
      /** We will lazily request the types registry on the first call to `isKnownTypesPackageName` and store it in `typesRegistryCache`. */
      private requestedRegistry = false;
      private typesRegistryCache: ESMap<string, MapLike<string>> | undefined;

      // This number is essentially arbitrary.  Processing more than one typings request
      // at a time makes sense, but having too many in the pipe results in a hang
      // (see https://github.com/nodejs/node/issues/7657).
      // It would be preferable to base our limit on the amount of space left in the
      // buffer, but we have yet to find a way to retrieve that value.
      private static readonly maxActiveRequestCount = 10;
      private static readonly requestDelayMillis = 100;
      private packageInstalledPromise:
        | {
          resolve(value: ApplyCodeActionCommandResult): void;
          reject(reason: unknown): void;
        }
        | undefined;

      constructor(
        private readonly telemetryEnabled: boolean,
        private readonly logger: Logger,
        private readonly host: ServerHost,
        readonly globalTypingsCacheLocation: string,
        readonly typingSafeListLocation: string,
        readonly typesMapLocation: string,
        private readonly npmLocation: string | undefined,
        private readonly validateDefaultNpmLocation: boolean,
        private event: Event,
      ) {}

      isKnownTypesPackageName(name: string): boolean {
        // We want to avoid looking this up in the registry as that is expensive. So first check that it's actually an NPM package.
        const validationResult = JsTyping.validatePackageName(name);
        if (validationResult !== JsTyping.NameValidationResult.Ok) {
          return false;
        }

        if (this.requestedRegistry) {
          return !!this.typesRegistryCache && this.typesRegistryCache.has(name);
        }

        this.requestedRegistry = true;
        this.send({ kind: "typesRegistry" });
        return false;
      }

      installPackage(
        options: InstallPackageOptionsWithProject,
      ): Promise<ApplyCodeActionCommandResult> {
        this.send<InstallPackageRequest>({
          kind: "installPackage",
          ...options,
        });
        Debug.assert(this.packageInstalledPromise === undefined);
        return new Promise<ApplyCodeActionCommandResult>((resolve, reject) => {
          this.packageInstalledPromise = { resolve, reject };
        });
      }

      attach(projectService: ProjectService) {
        this.projectService = projectService;
        if (this.logger.hasLevel(LogLevel.requestTime)) {
          this.logger.info("Binding...");
        }

        const args: string[] = [
          Arguments.GlobalCacheLocation,
          this.globalTypingsCacheLocation,
        ];
        if (this.telemetryEnabled) {
          args.push(Arguments.EnableTelemetry);
        }
        if (this.logger.loggingEnabled() && this.logger.getLogFileName()) {
          args.push(
            Arguments.LogFile,
            combinePaths(
              getDirectoryPath(normalizeSlashes(this.logger.getLogFileName()!)),
              `ti-${process.pid}.log`,
            ),
          );
        }
        if (this.typingSafeListLocation) {
          args.push(
            Arguments.TypingSafeListLocation,
            this.typingSafeListLocation,
          );
        }
        if (this.typesMapLocation) {
          args.push(Arguments.TypesMapLocation, this.typesMapLocation);
        }
        if (this.npmLocation) {
          args.push(Arguments.NpmLocation, this.npmLocation);
        }
        if (this.validateDefaultNpmLocation) {
          args.push(Arguments.ValidateDefaultNpmLocation);
        }

        const execArgv: string[] = [];
        for (const arg of process.execArgv) {
          const match = /^--((?:debug|inspect)(?:-brk)?)(?:=(\d+))?$/.exec(arg);
          if (match) {
            // if port is specified - use port + 1
            // otherwise pick a default port depending on if 'debug' or 'inspect' and use its value + 1
            const currentPort = match[2] !== undefined
              ? +match[2]
              : match[1].charAt(0) === "d"
              ? 5858
              : 9229;
            execArgv.push(`--${match[1]}=${currentPort + 1}`);
            break;
          }
        }

        this.installer = childProcess.fork(
          combinePaths(__dirname, "typingsInstaller.js"),
          args,
          { execArgv },
        );
        this.installer.on("message", (m) => this.handleMessage(m));

        // We have to schedule this event to the next tick
        // cause this fn will be called during
        // new IOSession => super(which is Session) => new ProjectService => NodeTypingsInstaller.attach
        // and if "event" is referencing "this" before super class is initialized, it will be a ReferenceError in ES6 class.
        this.host.setImmediate(() => this.event({ pid: this.installer.pid }, "typingsInstallerPid"));

        process.on("exit", () => {
          this.installer.kill();
        });
      }

      onProjectClosed(p: Project): void {
        this.send({ projectName: p.getProjectName(), kind: "closeProject" });
      }

      private send<T extends TypingInstallerRequestUnion>(rq: T): void {
        this.installer.send(rq);
      }

      enqueueInstallTypingsRequest(
        project: Project,
        typeAcquisition: TypeAcquisition,
        unresolvedImports: SortedReadonlyArray<string>,
      ): void {
        const request = createInstallTypingsRequest(
          project,
          typeAcquisition,
          unresolvedImports,
        );
        if (this.logger.hasLevel(LogLevel.verbose)) {
          if (this.logger.hasLevel(LogLevel.verbose)) {
            this.logger.info(
              `Scheduling throttled operation:${stringifyIndented(request)}`,
            );
          }
        }

        const operationId = project.getProjectName();
        const operation = () => {
          if (this.logger.hasLevel(LogLevel.verbose)) {
            this.logger.info(`Sending request:${stringifyIndented(request)}`);
          }
          this.send(request);
        };
        const queuedRequest: QueuedOperation = { operationId, operation };

        if (
          this.activeRequestCount < NodeTypingsInstaller.maxActiveRequestCount
        ) {
          this.scheduleRequest(queuedRequest);
        } else {
          if (this.logger.hasLevel(LogLevel.verbose)) {
            this.logger.info(`Deferring request for: ${operationId}`);
          }
          this.requestQueue.push(queuedRequest);
          this.requestMap.set(operationId, queuedRequest);
        }
      }

      private handleMessage(
        response:
          | TypesRegistryResponse
          | PackageInstalledResponse
          | SetTypings
          | InvalidateCachedTypings
          | BeginInstallTypes
          | EndInstallTypes
          | InitializationFailedResponse,
      ) {
        if (this.logger.hasLevel(LogLevel.verbose)) {
          this.logger.info(`Received response:${stringifyIndented(response)}`);
        }

        switch (response.kind) {
          case EventTypesRegistry:
            this.typesRegistryCache = new Map(
              getEntries(response.typesRegistry),
            );
            break;
          case ActionPackageInstalled: {
            const { success, message } = response;
            if (success) {
              this.packageInstalledPromise!.resolve({
                successMessage: message,
              });
            } else {
              this.packageInstalledPromise!.reject(message);
            }
            this.packageInstalledPromise = undefined;

            this.projectService.updateTypingsForProject(response);

            // The behavior is the same as for setTypings, so send the same event.
            this.event(response, "setTypings");
            break;
          }
          case EventInitializationFailed: {
            const body: protocol.TypesInstallerInitializationFailedEventBody = {
              message: response.message,
            };
            const eventName: protocol.TypesInstallerInitializationFailedEventName =
              "typesInstallerInitializationFailed";
            this.event(body, eventName);
            break;
          }
          case EventBeginInstallTypes: {
            const body: protocol.BeginInstallTypesEventBody = {
              eventId: response.eventId,
              packages: response.packagesToInstall,
            };
            const eventName: protocol.BeginInstallTypesEventName = "beginInstallTypes";
            this.event(body, eventName);
            break;
          }
          case EventEndInstallTypes: {
            if (this.telemetryEnabled) {
              const body: protocol.TypingsInstalledTelemetryEventBody = {
                telemetryEventName: "typingsInstalled",
                payload: {
                  installedPackages: response.packagesToInstall.join(","),
                  installSuccess: response.installSuccess,
                  typingsInstallerVersion: response.typingsInstallerVersion,
                },
              };
              const eventName: protocol.TelemetryEventName = "telemetry";
              this.event(body, eventName);
            }

            const body: protocol.EndInstallTypesEventBody = {
              eventId: response.eventId,
              packages: response.packagesToInstall,
              success: response.installSuccess,
            };
            const eventName: protocol.EndInstallTypesEventName = "endInstallTypes";
            this.event(body, eventName);
            break;
          }
          case ActionInvalidate: {
            this.projectService.updateTypingsForProject(response);
            break;
          }
          case ActionSet: {
            if (this.activeRequestCount > 0) {
              this.activeRequestCount--;
            } else {
              Debug.fail("Received too many responses");
            }

            while (this.requestQueue.length > 0) {
              const queuedRequest = this.requestQueue.shift()!;
              if (
                this.requestMap.get(queuedRequest.operationId) === queuedRequest
              ) {
                this.requestMap.delete(queuedRequest.operationId);
                this.scheduleRequest(queuedRequest);
                break;
              }

              if (this.logger.hasLevel(LogLevel.verbose)) {
                this.logger.info(
                  `Skipping defunct request for: ${queuedRequest.operationId}`,
                );
              }
            }

            this.projectService.updateTypingsForProject(response);

            this.event(response, "setTypings");

            break;
          }
          default:
            assertType<never>(response);
        }
      }

      private scheduleRequest(request: QueuedOperation) {
        if (this.logger.hasLevel(LogLevel.verbose)) {
          this.logger.info(`Scheduling request for: ${request.operationId}`);
        }
        this.activeRequestCount++;
        this.host.setTimeout(
          request.operation,
          NodeTypingsInstaller.requestDelayMillis,
        );
      }
    }

    class IOSession extends Session {
      private eventPort: number | undefined;
      private eventSocket: NodeSocket | undefined;
      private socketEventQueue: { body: any; eventName: string }[] | undefined;
      /** No longer needed if syntax target is es6 or above. Any access to "this" before initialized will be a runtime error. */
      private constructed: boolean | undefined;

      constructor() {
        const event = (body: object, eventName: string) => {
          this.event(body, eventName);
        };

        const host = sys as ServerHost;

        const typingsInstaller = disableAutomaticTypingAcquisition
          ? undefined
          : new NodeTypingsInstaller(
            telemetryEnabled,
            logger,
            host,
            getGlobalTypingsCacheLocation(),
            typingSafeListLocation,
            typesMapLocation,
            npmLocation,
            validateDefaultNpmLocation,
            event,
          );

        super({
          host,
          cancellationToken,
          ...options,
          typingsInstaller: typingsInstaller || nullTypingsInstaller,
          byteLength: Buffer.byteLength,
          hrtime: process.hrtime,
          logger,
          canUseEvents: true,
          typesMapLocation,
        });

        this.eventPort = eventPort;
        if (this.canUseEvents && this.eventPort) {
          const s = net.connect({ port: this.eventPort }, () => {
            this.eventSocket = s;
            if (this.socketEventQueue) {
              // flush queue.
              for (const event of this.socketEventQueue) {
                this.writeToEventSocket(event.body, event.eventName);
              }
              this.socketEventQueue = undefined;
            }
          });
        }

        this.constructed = true;
      }

      event<T extends object>(body: T, eventName: string): void {
        Debug.assert(
          !!this.constructed,
          "Should only call `IOSession.prototype.event` on an initialized IOSession",
        );

        if (this.canUseEvents && this.eventPort) {
          if (!this.eventSocket) {
            if (this.logger.hasLevel(LogLevel.verbose)) {
              this.logger.info(
                `eventPort: event "${eventName}" queued, but socket not yet initialized`,
              );
            }
            (this.socketEventQueue || (this.socketEventQueue = [])).push({
              body,
              eventName,
            });
            return;
          } else {
            Debug.assert(this.socketEventQueue === undefined);
            this.writeToEventSocket(body, eventName);
          }
        } else {
          super.event(body, eventName);
        }
      }

      private writeToEventSocket(body: object, eventName: string): void {
        this.eventSocket!.write(
          formatMessage(
            toEvent(eventName, body),
            this.logger,
            this.byteLength,
            this.host.newLine,
          ),
          "utf8",
        );
      }

      exit() {
        this.logger.info("Exiting...");
        this.projectService.closeLog();
        tracing?.stopTracing();
        process.exit(0);
      }

      listen() {
        rl.on("line", (input: string) => {
          const message = input.trim();
          this.onMessage(message);
        });

        rl.on("close", () => {
          this.exit();
        });
      }
    }

    class IpcIOSession extends IOSession {
      protected writeMessage(msg: protocol.Message): void {
        const verboseLogging = logger.hasLevel(LogLevel.verbose);
        if (verboseLogging) {
          const json = JSON.stringify(msg);
          logger.info(`${msg.type}:${indent(json)}`);
        }

        process.send!(msg);
      }

      protected parseMessage(message: any): protocol.Request {
        return message as protocol.Request;
      }

      protected toStringMessage(message: any) {
        return JSON.stringify(message, undefined, 2);
      }

      public listen() {
        process.on("message", (e: any) => {
          this.onMessage(e);
        });
      }
    }

    const eventPort: number | undefined = parseEventPort(
      findArgument("--eventPort"),
    );
    const typingSafeListLocation = findArgument(
      Arguments.TypingSafeListLocation,
    )!; // TODO: GH#18217
    const typesMapLocation = findArgument(Arguments.TypesMapLocation)
      || combinePaths(
        getDirectoryPath(sys.getExecutingFilePath()),
        "typesMap.json",
      );
    const npmLocation = findArgument(Arguments.NpmLocation);
    const validateDefaultNpmLocation = hasArgument(
      Arguments.ValidateDefaultNpmLocation,
    );
    const disableAutomaticTypingAcquisition = hasArgument(
      "--disableAutomaticTypingAcquisition",
    );
    const useNodeIpc = hasArgument("--useNodeIpc");
    const telemetryEnabled = hasArgument(Arguments.EnableTelemetry);
    const commandLineTraceDir = findArgument("--traceDirectory");
    const traceDir = commandLineTraceDir
      ? stripQuotes(commandLineTraceDir)
      : process.env.TSS_TRACE;
    if (traceDir) {
      startTracing("server", traceDir);
    }

    const ioSession = useNodeIpc ? new IpcIOSession() : new IOSession();
    process.on("uncaughtException", (err) => {
      ioSession.logError(err, "unknown");
    });
    // See https://github.com/Microsoft/TypeScript/issues/11348
    (process as any).noAsar = true;
    // Start listening
    ioSession.listen();

    function getGlobalTypingsCacheLocation() {
      switch (process.platform) {
        case "win32": {
          const basePath = process.env.LOCALAPPDATA
            || process.env.APPDATA
            || (os.homedir && os.homedir())
            || process.env.USERPROFILE
            || (process.env.HOMEDRIVE
              && process.env.HOMEPATH
              && normalizeSlashes(process.env.HOMEDRIVE + process.env.HOMEPATH))
            || os.tmpdir();
          return combinePaths(
            combinePaths(normalizeSlashes(basePath), "Microsoft/TypeScript"),
            versionMajorMinor,
          );
        }
        case "openbsd":
        case "freebsd":
        case "netbsd":
        case "darwin":
        case "linux":
        case "android": {
          const cacheLocation = getNonWindowsCacheLocation(
            process.platform === "darwin",
          );
          return combinePaths(
            combinePaths(cacheLocation, "typescript"),
            versionMajorMinor,
          );
        }
        default:
          return Debug.fail(`unsupported platform '${process.platform}'`);
      }
    }

    function getNonWindowsCacheLocation(platformIsDarwin: boolean) {
      if (process.env.XDG_CACHE_HOME) {
        return process.env.XDG_CACHE_HOME;
      }
      const usersDir = platformIsDarwin ? "Users" : "home";
      const homePath = (os.homedir && os.homedir())
        || process.env.HOME
        || ((process.env.LOGNAME || process.env.USER)
          && `/${usersDir}/${process.env.LOGNAME || process.env.USER}`)
        || os.tmpdir();
      const cacheFolder = platformIsDarwin ? "Library/Caches" : ".cache";
      return combinePaths(normalizeSlashes(homePath), cacheFolder);
    }
  }
}
