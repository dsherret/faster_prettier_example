namespace ts {
  export interface ReadBuildProgramHost {
    useCaseSensitiveFileNames(): boolean;
    getCurrentDirectory(): string;
    readFile(fileName: string): string | undefined;
  }
  export function readBuilderProgram(
    compilerOptions: CompilerOptions,
    host: ReadBuildProgramHost,
  ) {
    if (outFile(compilerOptions)) return undefined;
    const buildInfoPath = getTsBuildInfoEmitOutputFilePath(compilerOptions);
    if (!buildInfoPath) return undefined;
    const content = host.readFile(buildInfoPath);
    if (!content) return undefined;
    const buildInfo = getBuildInfo(content);
    if (buildInfo.version !== version) return undefined;
    if (!buildInfo.program) return undefined;
    return createBuildProgramUsingProgramBuildInfo(
      buildInfo.program,
      buildInfoPath,
      host,
    );
  }

  export function createIncrementalCompilerHost(
    options: CompilerOptions,
    system = sys,
  ): CompilerHost {
    const host = createCompilerHostWorker(
      options,
      /*setParentNodes*/ undefined,
      system,
    );
    host.createHash = maybeBind(system, system.createHash);
    host.disableUseFileVersionAsSignature = system.disableUseFileVersionAsSignature;
    setGetSourceFileAsHashVersioned(host, system);
    changeCompilerHostLikeToUseCache(
      host,
      (fileName) => toPath(fileName, host.getCurrentDirectory(), host.getCanonicalFileName),
    );
    return host;
  }

  export interface IncrementalProgramOptions<T extends BuilderProgram> {
    rootNames: readonly string[];
    options: CompilerOptions;
    configFileParsingDiagnostics?: readonly Diagnostic[];
    projectReferences?: readonly ProjectReference[];
    host?: CompilerHost;
    createProgram?: CreateProgram<T>;
  }

  export function createIncrementalProgram<
    T extends BuilderProgram = EmitAndSemanticDiagnosticsBuilderProgram,
  >({
    rootNames,
    options,
    configFileParsingDiagnostics,
    projectReferences,
    host,
    createProgram,
  }: IncrementalProgramOptions<T>): T {
    host = host || createIncrementalCompilerHost(options);
    createProgram = createProgram
      || (createEmitAndSemanticDiagnosticsBuilderProgram as any as CreateProgram<T>);
    const oldProgram = readBuilderProgram(options, host) as any as T;
    return createProgram(
      rootNames,
      options,
      host,
      oldProgram,
      configFileParsingDiagnostics,
      projectReferences,
    );
  }

  export type WatchStatusReporter = (
    diagnostic: Diagnostic,
    newLine: string,
    options: CompilerOptions,
    errorCount?: number,
  ) => void;
  /** Create the program with rootNames and options, if they are undefined, oldProgram and new configFile diagnostics create new program */
  export type CreateProgram<T extends BuilderProgram> = (
    rootNames: readonly string[] | undefined,
    options: CompilerOptions | undefined,
    host?: CompilerHost,
    oldProgram?: T,
    configFileParsingDiagnostics?: readonly Diagnostic[],
    projectReferences?: readonly ProjectReference[] | undefined,
  ) => T;

  /** Host that has watch functionality used in --watch mode */
  export interface WatchHost {
    /** If provided, called with Diagnostic message that informs about change in watch status */
    onWatchStatusChange?(
      diagnostic: Diagnostic,
      newLine: string,
      options: CompilerOptions,
      errorCount?: number,
    ): void;

    /** Used to watch changes in source files, missing files needed to update the program or config file */
    watchFile(
      path: string,
      callback: FileWatcherCallback,
      pollingInterval?: number,
      options?: CompilerOptions,
    ): FileWatcher;
    /** Used to watch resolved module's failed lookup locations, config file specs, type roots where auto type reference directives are added */
    watchDirectory(
      path: string,
      callback: DirectoryWatcherCallback,
      recursive?: boolean,
      options?: CompilerOptions,
    ): FileWatcher;
    /** If provided, will be used to set delayed compilation, so that multiple changes in short span are compiled together */
    setTimeout?(
      callback: (...args: any[]) => void,
      ms: number,
      ...args: any[]
    ): any;
    /** If provided, will be used to reset existing delayed compilation */
    clearTimeout?(timeoutId: any): void;
  }
  export interface ProgramHost<T extends BuilderProgram> {
    /**
     * Used to create the program when need for program creation or recreation detected
     */
    createProgram: CreateProgram<T>;

    // Sub set of compiler host methods to read and generate new program
    useCaseSensitiveFileNames(): boolean;
    getNewLine(): string;
    getCurrentDirectory(): string;
    getDefaultLibFileName(options: CompilerOptions): string;
    getDefaultLibLocation?(): string;
    createHash?(data: string): string;

    /**
     * Use to check file presence for source files and
     * if resolveModuleNames is not provided (complier is in charge of module resolution) then module files as well
     */
    fileExists(path: string): boolean;
    /**
     * Use to read file text for source files and
     * if resolveModuleNames is not provided (complier is in charge of module resolution) then module files as well
     */
    readFile(path: string, encoding?: string): string | undefined;

    /** If provided, used for module resolution as well as to handle directory structure */
    directoryExists?(path: string): boolean;
    /** If provided, used in resolutions as well as handling directory structure */
    getDirectories?(path: string): string[];
    /** If provided, used to cache and handle directory structure modifications */
    readDirectory?(
      path: string,
      extensions?: readonly string[],
      exclude?: readonly string[],
      include?: readonly string[],
      depth?: number,
    ): string[];

    /** Symbol links resolution */
    realpath?(path: string): string;
    /** If provided would be used to write log about compilation */
    trace?(s: string): void;
    /** If provided is used to get the environment variable */
    getEnvironmentVariable?(name: string): string | undefined;

    /** If provided, used to resolve the module names, otherwise typescript's default module resolution */
    resolveModuleNames?(
      moduleNames: string[],
      containingFile: string,
      reusedNames: string[] | undefined,
      redirectedReference: ResolvedProjectReference | undefined,
      options: CompilerOptions,
      containingSourceFile?: SourceFile,
    ): (ResolvedModule | undefined)[];
    /** If provided, used to resolve type reference directives, otherwise typescript's default resolution */
    resolveTypeReferenceDirectives?(
      typeReferenceDirectiveNames: string[] | readonly FileReference[],
      containingFile: string,
      redirectedReference: ResolvedProjectReference | undefined,
      options: CompilerOptions,
      containingFileMode?: SourceFile["impliedNodeFormat"] | undefined,
    ): (ResolvedTypeReferenceDirective | undefined)[];
  }
  /** Internal interface used to wire emit through same host */

  /*@internal*/
  export interface ProgramHost<T extends BuilderProgram> {
    // TODO: GH#18217 Optional methods are frequently asserted
    createDirectory?(path: string): void;
    writeFile?(path: string, data: string, writeByteOrderMark?: boolean): void;
    // For testing
    disableUseFileVersionAsSignature?: boolean;
  }

  export interface WatchCompilerHost<T extends BuilderProgram> extends ProgramHost<T>, WatchHost {
    /** Instead of using output d.ts file from project reference, use its source file */
    useSourceOfProjectReferenceRedirect?(): boolean;

    /** If provided, use this method to get parsed command lines for referenced projects */
    getParsedCommandLine?(fileName: string): ParsedCommandLine | undefined;

    /** If provided, callback to invoke after every new program creation */
    afterProgramCreate?(program: T): void;
  }

  /**
   * Host to create watch with root files and options
   */
  export interface WatchCompilerHostOfFilesAndCompilerOptions<
    T extends BuilderProgram,
  > extends WatchCompilerHost<T> {
    /** root files to use to generate program */
    rootFiles: string[];

    /** Compiler options */
    options: CompilerOptions;

    watchOptions?: WatchOptions;

    /** Project References */
    projectReferences?: readonly ProjectReference[];
  }

  /**
   * Host to create watch with config file
   */
  export interface WatchCompilerHostOfConfigFile<T extends BuilderProgram>
    extends WatchCompilerHost<T>, ConfigFileDiagnosticsReporter
  {
    /** Name of the config file to compile */
    configFileName: string;

    /** Options to extend */
    optionsToExtend?: CompilerOptions;

    watchOptionsToExtend?: WatchOptions;

    extraFileExtensions?: readonly FileExtensionInfo[];

    /**
     * Used to generate source file names from the config file and its include, exclude, files rules
     * and also to cache the directory stucture
     */
    readDirectory(
      path: string,
      extensions?: readonly string[],
      exclude?: readonly string[],
      include?: readonly string[],
      depth?: number,
    ): string[];
  }

  /**
   * Host to create watch with config file that is already parsed (from tsc)
   */
  /*@internal*/
  export interface WatchCompilerHostOfConfigFile<T extends BuilderProgram> extends WatchCompilerHost<T> {
    configFileParsingResult?: ParsedCommandLine;
    extendedConfigCache?: Map<ExtendedConfigCacheEntry>;
  }

  export interface Watch<T> {
    /** Synchronize with host and get updated program */
    getProgram(): T;
    /** Gets the existing program without synchronizing with changes on host */
    /*@internal*/
    getCurrentProgram(): T;
    /** Closes the watch */
    close(): void;
  }

  /**
   * Creates the watch what generates program using the config file
   */
  export interface WatchOfConfigFile<T> extends Watch<T> {}

  /**
   * Creates the watch that generates program using the root files and compiler options
   */
  export interface WatchOfFilesAndCompilerOptions<T> extends Watch<T> {
    /** Updates the root files in the program, only if this is not config file compilation */
    updateRootFileNames(fileNames: string[]): void;
  }

  /**
   * Create the watch compiler host for either configFile or fileNames and its options
   */
  export function createWatchCompilerHost<T extends BuilderProgram>(
    configFileName: string,
    optionsToExtend: CompilerOptions | undefined,
    system: System,
    createProgram?: CreateProgram<T>,
    reportDiagnostic?: DiagnosticReporter,
    reportWatchStatus?: WatchStatusReporter,
    watchOptionsToExtend?: WatchOptions,
    extraFileExtensions?: readonly FileExtensionInfo[],
  ): WatchCompilerHostOfConfigFile<T>;
  export function createWatchCompilerHost<T extends BuilderProgram>(
    rootFiles: string[],
    options: CompilerOptions,
    system: System,
    createProgram?: CreateProgram<T>,
    reportDiagnostic?: DiagnosticReporter,
    reportWatchStatus?: WatchStatusReporter,
    projectReferences?: readonly ProjectReference[],
    watchOptions?: WatchOptions,
  ): WatchCompilerHostOfFilesAndCompilerOptions<T>;
  export function createWatchCompilerHost<T extends BuilderProgram>(
    rootFilesOrConfigFileName: string | string[],
    options: CompilerOptions | undefined,
    system: System,
    createProgram?: CreateProgram<T>,
    reportDiagnostic?: DiagnosticReporter,
    reportWatchStatus?: WatchStatusReporter,
    projectReferencesOrWatchOptionsToExtend?:
      | readonly ProjectReference[]
      | WatchOptions,
    watchOptionsOrExtraFileExtensions?:
      | WatchOptions
      | readonly FileExtensionInfo[],
  ):
    | WatchCompilerHostOfFilesAndCompilerOptions<T>
    | WatchCompilerHostOfConfigFile<T>
  {
    if (isArray(rootFilesOrConfigFileName)) {
      return createWatchCompilerHostOfFilesAndCompilerOptions({
        rootFiles: rootFilesOrConfigFileName,
        options: options!,
        watchOptions: watchOptionsOrExtraFileExtensions as WatchOptions,
        projectReferences: projectReferencesOrWatchOptionsToExtend as readonly ProjectReference[],
        system,
        createProgram,
        reportDiagnostic,
        reportWatchStatus,
      });
    } else {
      return createWatchCompilerHostOfConfigFile({
        configFileName: rootFilesOrConfigFileName,
        optionsToExtend: options,
        watchOptionsToExtend: projectReferencesOrWatchOptionsToExtend as WatchOptions,
        extraFileExtensions: watchOptionsOrExtraFileExtensions as readonly FileExtensionInfo[],
        system,
        createProgram,
        reportDiagnostic,
        reportWatchStatus,
      });
    }
  }

  interface ParsedConfig {
    /** ParsedCommandLine for the config file if present */
    parsedCommandLine: ParsedCommandLine | undefined;
    /** File watcher of the config file */
    watcher?: FileWatcher;
    /** Wild card directories watched from this config file */
    watchedDirectories?: Map<WildcardDirectoryWatcher>;
    /** Reload to be done for this config file */
    reloadLevel?:
      | ConfigFileProgramReloadLevel.Partial
      | ConfigFileProgramReloadLevel.Full;
  }

  /**
   * Creates the watch from the host for root files and compiler options
   */
  export function createWatchProgram<T extends BuilderProgram>(
    host: WatchCompilerHostOfFilesAndCompilerOptions<T>,
  ): WatchOfFilesAndCompilerOptions<T>;
  /**
   * Creates the watch from the host for config file
   */
  export function createWatchProgram<T extends BuilderProgram>(
    host: WatchCompilerHostOfConfigFile<T>,
  ): WatchOfConfigFile<T>;
  export function createWatchProgram<T extends BuilderProgram>(
    host:
      & WatchCompilerHostOfFilesAndCompilerOptions<T>
      & WatchCompilerHostOfConfigFile<T>,
  ): WatchOfFilesAndCompilerOptions<T> | WatchOfConfigFile<T> {
    interface FilePresentOnHost {
      version: string;
      sourceFile: SourceFile;
      fileWatcher: FileWatcher;
    }
    type FileMissingOnHost = false;
    interface FilePresenceUnknownOnHost {
      version: false;
      fileWatcher?: FileWatcher;
    }
    type FileMayBePresentOnHost = FilePresentOnHost | FilePresenceUnknownOnHost;
    type HostFileInfo =
      | FilePresentOnHost
      | FileMissingOnHost
      | FilePresenceUnknownOnHost;

    let builderProgram: T;
    let reloadLevel: ConfigFileProgramReloadLevel; // level to indicate if the program needs to be reloaded from config file/just filenames etc
    let missingFilesMap: ESMap<Path, FileWatcher>; // Map of file watchers for the missing files
    let packageJsonMap: ESMap<Path, FileWatcher>; // map of watchers for package json files used in module resolution
    let watchedWildcardDirectories: ESMap<string, WildcardDirectoryWatcher>; // map of watchers for the wild card directories in the config file
    let timerToUpdateProgram: any; // timer callback to recompile the program
    let timerToInvalidateFailedLookupResolutions: any; // timer callback to invalidate resolutions for changes in failed lookup locations
    let parsedConfigs: ESMap<Path, ParsedConfig> | undefined; // Parsed commandline and watching cached for referenced projects
    let sharedExtendedConfigFileWatchers: ESMap<
      Path,
      SharedExtendedConfigFileWatcher<Path>
    >; // Map of file watchers for extended files, shared between different referenced projects
    let extendedConfigCache = host.extendedConfigCache; // Cache for extended config evaluation
    let changesAffectResolution = false; // Flag for indicating non-config changes affect module resolution
    let reportFileChangeDetectedOnCreateProgram = false; // True if synchronizeProgram should report "File change detected..." when a new program is created

    const sourceFilesCache = new Map<string, HostFileInfo>(); // Cache that stores the source file and version info
    let missingFilePathsRequestedForRelease: Path[] | undefined; // These paths are held temporarily so that we can remove the entry from source file cache if the file is not tracked by missing files
    let hasChangedCompilerOptions = false; // True if the compiler options have changed between compilations

    const useCaseSensitiveFileNames = host.useCaseSensitiveFileNames();
    const currentDirectory = host.getCurrentDirectory();
    const {
      configFileName,
      optionsToExtend: optionsToExtendForConfigFile = {},
      watchOptionsToExtend,
      extraFileExtensions,
      createProgram,
    } = host;
    let {
      rootFiles: rootFileNames,
      options: compilerOptions,
      watchOptions,
      projectReferences,
    } = host;
    let wildcardDirectories: MapLike<WatchDirectoryFlags> | undefined;
    let configFileParsingDiagnostics: Diagnostic[] | undefined;
    let canConfigFileJsonReportNoInputFiles = false;
    let hasChangedConfigFileParsingErrors = false;

    const cachedDirectoryStructureHost = configFileName === undefined
      ? undefined
      : createCachedDirectoryStructureHost(
        host,
        currentDirectory,
        useCaseSensitiveFileNames,
      );
    const directoryStructureHost: DirectoryStructureHost = cachedDirectoryStructureHost || host;
    const parseConfigFileHost = parseConfigHostFromCompilerHostLike(
      host,
      directoryStructureHost,
    );

    // From tsc we want to get already parsed result and hence check for rootFileNames
    let newLine = updateNewLine();
    if (configFileName && host.configFileParsingResult) {
      setConfigFileParsingResult(host.configFileParsingResult);
      newLine = updateNewLine();
    }
    reportWatchDiagnostic(Diagnostics.Starting_compilation_in_watch_mode);
    if (configFileName && !host.configFileParsingResult) {
      newLine = getNewLineCharacter(optionsToExtendForConfigFile, () => host.getNewLine());
      Debug.assert(!rootFileNames);
      parseConfigFile();
      newLine = updateNewLine();
    }

    const { watchFile, watchDirectory, writeLog } = createWatchFactory(
      host,
      compilerOptions,
    );
    const getCanonicalFileName = createGetCanonicalFileName(
      useCaseSensitiveFileNames,
    );

    writeLog(
      `Current directory: ${currentDirectory} CaseSensitiveFileNames: ${useCaseSensitiveFileNames}`,
    );
    let configFileWatcher: FileWatcher | undefined;
    if (configFileName) {
      configFileWatcher = watchFile(
        configFileName,
        scheduleProgramReload,
        PollingInterval.High,
        watchOptions,
        WatchType.ConfigFile,
      );
    }

    const compilerHost = createCompilerHostFromProgramHost(
      host,
      () => compilerOptions,
      directoryStructureHost,
    ) as CompilerHost & ResolutionCacheHost;
    setGetSourceFileAsHashVersioned(compilerHost, host);
    // Members for CompilerHost
    const getNewSourceFile = compilerHost.getSourceFile;
    compilerHost.getSourceFile = (fileName, ...args) =>
      getVersionedSourceFileByPath(fileName, toPath(fileName), ...args);
    compilerHost.getSourceFileByPath = getVersionedSourceFileByPath;
    compilerHost.getNewLine = () => newLine;
    compilerHost.fileExists = fileExists;
    compilerHost.onReleaseOldSourceFile = onReleaseOldSourceFile;
    compilerHost.onReleaseParsedCommandLine = onReleaseParsedCommandLine;
    // Members for ResolutionCacheHost
    compilerHost.toPath = toPath;
    compilerHost.getCompilationSettings = () => compilerOptions;
    compilerHost.useSourceOfProjectReferenceRedirect = maybeBind(
      host,
      host.useSourceOfProjectReferenceRedirect,
    );
    compilerHost.watchDirectoryOfFailedLookupLocation = (dir, cb, flags) =>
      watchDirectory(
        dir,
        cb,
        flags,
        watchOptions,
        WatchType.FailedLookupLocations,
      );
    compilerHost.watchTypeRootsDirectory = (dir, cb, flags) =>
      watchDirectory(dir, cb, flags, watchOptions, WatchType.TypeRoots);
    compilerHost.getCachedDirectoryStructureHost = () => cachedDirectoryStructureHost;
    compilerHost.scheduleInvalidateResolutionsOfFailedLookupLocations =
      scheduleInvalidateResolutionsOfFailedLookupLocations;
    compilerHost.onInvalidatedResolution = scheduleProgramUpdate;
    compilerHost.onChangedAutomaticTypeDirectiveNames = scheduleProgramUpdate;
    compilerHost.fileIsOpen = returnFalse;
    compilerHost.getCurrentProgram = getCurrentProgram;
    compilerHost.writeLog = writeLog;
    compilerHost.getParsedCommandLine = getParsedCommandLine;

    // Cache for the module resolution
    const resolutionCache = createResolutionCache(
      compilerHost,
      configFileName
        ? getDirectoryPath(
          getNormalizedAbsolutePath(configFileName, currentDirectory),
        )
        : currentDirectory,
      /*logChangesWhenResolvingModule*/ false,
    );
    // Resolve module using host module resolution strategy if provided otherwise use resolution cache to resolve module names
    compilerHost.resolveModuleNames = host.resolveModuleNames
      ? (...args) => host.resolveModuleNames!(...args)
      : (
        moduleNames,
        containingFile,
        reusedNames,
        redirectedReference,
        _options,
        sourceFile,
      ) =>
        resolutionCache.resolveModuleNames(
          moduleNames,
          containingFile,
          reusedNames,
          redirectedReference,
          sourceFile,
        );
    compilerHost.resolveTypeReferenceDirectives = host.resolveTypeReferenceDirectives
      ? (...args) => host.resolveTypeReferenceDirectives!(...args)
      : (
        typeDirectiveNames,
        containingFile,
        redirectedReference,
        _options,
        containingFileMode,
      ) =>
        resolutionCache.resolveTypeReferenceDirectives(
          typeDirectiveNames,
          containingFile,
          redirectedReference,
          containingFileMode,
        );
    const userProvidedResolution = !!host.resolveModuleNames || !!host.resolveTypeReferenceDirectives;

    builderProgram = readBuilderProgram(
      compilerOptions,
      compilerHost,
    ) as any as T;
    synchronizeProgram();

    // Update the wild card directory watch
    watchConfigFileWildCardDirectories();

    // Update extended config file watch
    if (configFileName) {
      updateExtendedConfigFilesWatches(
        toPath(configFileName),
        compilerOptions,
        watchOptions,
        WatchType.ExtendedConfigFile,
      );
    }

    return configFileName
      ? {
        getCurrentProgram: getCurrentBuilderProgram,
        getProgram: updateProgram,
        close,
      }
      : {
        getCurrentProgram: getCurrentBuilderProgram,
        getProgram: updateProgram,
        updateRootFileNames,
        close,
      };

    function close() {
      clearInvalidateResolutionsOfFailedLookupLocations();
      resolutionCache.clear();
      clearMap(sourceFilesCache, (value) => {
        if (value && value.fileWatcher) {
          value.fileWatcher.close();
          value.fileWatcher = undefined;
        }
      });
      if (configFileWatcher) {
        configFileWatcher.close();
        configFileWatcher = undefined;
      }
      extendedConfigCache?.clear();
      extendedConfigCache = undefined;
      if (sharedExtendedConfigFileWatchers) {
        clearMap(sharedExtendedConfigFileWatchers, closeFileWatcherOf);
        sharedExtendedConfigFileWatchers = undefined!;
      }
      if (watchedWildcardDirectories) {
        clearMap(watchedWildcardDirectories, closeFileWatcherOf);
        watchedWildcardDirectories = undefined!;
      }
      if (missingFilesMap) {
        clearMap(missingFilesMap, closeFileWatcher);
        missingFilesMap = undefined!;
      }
      if (parsedConfigs) {
        clearMap(parsedConfigs, (config) => {
          config.watcher?.close();
          config.watcher = undefined;
          if (config.watchedDirectories) {
            clearMap(config.watchedDirectories, closeFileWatcherOf);
          }
          config.watchedDirectories = undefined;
        });
        parsedConfigs = undefined;
      }
      if (packageJsonMap) {
        clearMap(packageJsonMap, closeFileWatcher);
        packageJsonMap = undefined!;
      }
    }

    function getCurrentBuilderProgram() {
      return builderProgram;
    }

    function getCurrentProgram() {
      return builderProgram && builderProgram.getProgramOrUndefined();
    }

    function synchronizeProgram() {
      writeLog(`Synchronizing program`);
      clearInvalidateResolutionsOfFailedLookupLocations();

      const program = getCurrentBuilderProgram();
      if (hasChangedCompilerOptions) {
        newLine = updateNewLine();
        if (
          program
          && (changesAffectResolution
            || changesAffectModuleResolution(
              program.getCompilerOptions(),
              compilerOptions,
            ))
        ) {
          resolutionCache.clear();
        }
      }

      // All resolutions are invalid if user provided resolutions
      const hasInvalidatedResolution = resolutionCache.createHasInvalidatedResolution(
        userProvidedResolution || changesAffectResolution,
      );
      if (
        isProgramUptoDate(
          getCurrentProgram(),
          rootFileNames,
          compilerOptions,
          getSourceVersion,
          fileExists,
          hasInvalidatedResolution,
          hasChangedAutomaticTypeDirectiveNames,
          getParsedCommandLine,
          projectReferences,
        )
      ) {
        if (hasChangedConfigFileParsingErrors) {
          if (reportFileChangeDetectedOnCreateProgram) {
            reportWatchDiagnostic(
              Diagnostics.File_change_detected_Starting_incremental_compilation,
            );
          }
          builderProgram = createProgram(
            /*rootNames*/ undefined,
            /*options*/ undefined,
            compilerHost,
            builderProgram,
            configFileParsingDiagnostics,
            projectReferences,
          );
          hasChangedConfigFileParsingErrors = false;
        }
      } else {
        if (reportFileChangeDetectedOnCreateProgram) {
          reportWatchDiagnostic(
            Diagnostics.File_change_detected_Starting_incremental_compilation,
          );
        }
        createNewProgram(hasInvalidatedResolution);
      }

      changesAffectResolution = false; // reset for next sync
      reportFileChangeDetectedOnCreateProgram = false;

      if (host.afterProgramCreate && program !== builderProgram) {
        host.afterProgramCreate(builderProgram);
      }

      return builderProgram;
    }

    function createNewProgram(
      hasInvalidatedResolution: HasInvalidatedResolution,
    ) {
      // Compile the program
      writeLog("CreatingProgramWith::");
      writeLog(`  roots: ${JSON.stringify(rootFileNames)}`);
      writeLog(`  options: ${JSON.stringify(compilerOptions)}`);
      if (projectReferences) {
        writeLog(`  projectReferences: ${JSON.stringify(projectReferences)}`);
      }

      const needsUpdateInTypeRootWatch = hasChangedCompilerOptions || !getCurrentProgram();
      hasChangedCompilerOptions = false;
      hasChangedConfigFileParsingErrors = false;
      resolutionCache.startCachingPerDirectoryResolution();
      compilerHost.hasInvalidatedResolution = hasInvalidatedResolution;
      compilerHost.hasChangedAutomaticTypeDirectiveNames = hasChangedAutomaticTypeDirectiveNames;
      builderProgram = createProgram(
        rootFileNames,
        compilerOptions,
        compilerHost,
        builderProgram,
        configFileParsingDiagnostics,
        projectReferences,
      );
      // map package json cache entries to their realpaths so we don't try to watch across symlinks
      const packageCacheEntries = map(
        resolutionCache
          .getModuleResolutionCache()
          .getPackageJsonInfoCache()
          .entries(),
        ([path, data]) =>
          [
            compilerHost.realpath ? toPath(compilerHost.realpath(path)) : path,
            data,
          ] as const,
      );
      resolutionCache.finishCachingPerDirectoryResolution();

      // Update watches
      updateMissingFilePathsWatch(
        builderProgram.getProgram(),
        missingFilesMap || (missingFilesMap = new Map()),
        watchMissingFilePath,
      );
      updatePackageJsonWatch(
        packageCacheEntries,
        packageJsonMap || (packageJsonMap = new Map()),
        watchPackageJsonLookupPath,
      );
      if (needsUpdateInTypeRootWatch) {
        resolutionCache.updateTypeRootsWatch();
      }

      if (missingFilePathsRequestedForRelease) {
        // These are the paths that program creater told us as not in use any more but were missing on the disk.
        // We didnt remove the entry for them from sourceFiles cache so that we dont have to do File IO,
        // if there is already watcher for it (for missing files)
        // At this point our watches were updated, hence now we know that these paths are not tracked and need to be removed
        // so that at later time we have correct result of their presence
        for (const missingFilePath of missingFilePathsRequestedForRelease) {
          if (!missingFilesMap.has(missingFilePath)) {
            sourceFilesCache.delete(missingFilePath);
          }
        }
        missingFilePathsRequestedForRelease = undefined;
      }
    }

    function updateRootFileNames(files: string[]) {
      Debug.assert(
        !configFileName,
        "Cannot update root file names with config file watch mode",
      );
      rootFileNames = files;
      scheduleProgramUpdate();
    }

    function updateNewLine() {
      return getNewLineCharacter(
        compilerOptions || optionsToExtendForConfigFile,
        () => host.getNewLine(),
      );
    }

    function toPath(fileName: string) {
      return ts.toPath(fileName, currentDirectory, getCanonicalFileName);
    }

    function isFileMissingOnHost(
      hostSourceFile: HostFileInfo | undefined,
    ): hostSourceFile is FileMissingOnHost {
      return typeof hostSourceFile === "boolean";
    }

    function isFilePresenceUnknownOnHost(
      hostSourceFile: FileMayBePresentOnHost,
    ): hostSourceFile is FilePresenceUnknownOnHost {
      return (
        typeof (hostSourceFile as FilePresenceUnknownOnHost).version
          === "boolean"
      );
    }

    function fileExists(fileName: string) {
      const path = toPath(fileName);
      // If file is missing on host from cache, we can definitely say file doesnt exist
      // otherwise we need to ensure from the disk
      if (isFileMissingOnHost(sourceFilesCache.get(path))) {
        return false;
      }

      return directoryStructureHost.fileExists(fileName);
    }

    function getVersionedSourceFileByPath(
      fileName: string,
      path: Path,
      languageVersionOrOptions: ScriptTarget | CreateSourceFileOptions,
      onError?: (message: string) => void,
      shouldCreateNewSourceFile?: boolean,
    ): SourceFile | undefined {
      const hostSourceFile = sourceFilesCache.get(path);
      // No source file on the host
      if (isFileMissingOnHost(hostSourceFile)) {
        return undefined;
      }

      // Create new source file if requested or the versions dont match
      if (
        hostSourceFile === undefined
        || shouldCreateNewSourceFile
        || isFilePresenceUnknownOnHost(hostSourceFile)
      ) {
        const sourceFile = getNewSourceFile(
          fileName,
          languageVersionOrOptions,
          onError,
        );
        if (hostSourceFile) {
          if (sourceFile) {
            // Set the source file and create file watcher now that file was present on the disk
            (hostSourceFile as FilePresentOnHost).sourceFile = sourceFile;
            hostSourceFile.version = sourceFile.version;
            if (!hostSourceFile.fileWatcher) {
              hostSourceFile.fileWatcher = watchFilePath(
                path,
                fileName,
                onSourceFileChange,
                PollingInterval.Low,
                watchOptions,
                WatchType.SourceFile,
              );
            }
          } else {
            // There is no source file on host any more, close the watch, missing file paths will track it
            if (hostSourceFile.fileWatcher) {
              hostSourceFile.fileWatcher.close();
            }
            sourceFilesCache.set(path, false);
          }
        } else {
          if (sourceFile) {
            const fileWatcher = watchFilePath(
              path,
              fileName,
              onSourceFileChange,
              PollingInterval.Low,
              watchOptions,
              WatchType.SourceFile,
            );
            sourceFilesCache.set(path, {
              sourceFile,
              version: sourceFile.version,
              fileWatcher,
            });
          } else {
            sourceFilesCache.set(path, false);
          }
        }
        if (sourceFile) {
          sourceFile.impliedNodeFormat = getImpliedNodeFormatForFile(
            path,
            resolutionCache
              .getModuleResolutionCache()
              .getPackageJsonInfoCache(),
            compilerHost,
            compilerHost.getCompilationSettings(),
          );
        }
        return sourceFile;
      }
      return hostSourceFile.sourceFile;
    }

    function nextSourceFileVersion(path: Path) {
      const hostSourceFile = sourceFilesCache.get(path);
      if (hostSourceFile !== undefined) {
        if (isFileMissingOnHost(hostSourceFile)) {
          // The next version, lets set it as presence unknown file
          sourceFilesCache.set(path, { version: false });
        } else {
          (hostSourceFile as FilePresenceUnknownOnHost).version = false;
        }
      }
    }

    function getSourceVersion(path: Path): string | undefined {
      const hostSourceFile = sourceFilesCache.get(path);
      return !hostSourceFile || !hostSourceFile.version
        ? undefined
        : hostSourceFile.version;
    }

    function onReleaseOldSourceFile(
      oldSourceFile: SourceFile,
      _oldOptions: CompilerOptions,
      hasSourceFileByPath: boolean,
    ) {
      const hostSourceFileInfo = sourceFilesCache.get(
        oldSourceFile.resolvedPath,
      );
      // If this is the source file thats in the cache and new program doesnt need it,
      // remove the cached entry.
      // Note we arent deleting entry if file became missing in new program or
      // there was version update and new source file was created.
      if (hostSourceFileInfo !== undefined) {
        // record the missing file paths so they can be removed later if watchers arent tracking them
        if (isFileMissingOnHost(hostSourceFileInfo)) {
          (
            missingFilePathsRequestedForRelease
            || (missingFilePathsRequestedForRelease = [])
          ).push(oldSourceFile.path);
        } else if (
          (hostSourceFileInfo as FilePresentOnHost).sourceFile === oldSourceFile
        ) {
          if (hostSourceFileInfo.fileWatcher) {
            hostSourceFileInfo.fileWatcher.close();
          }
          sourceFilesCache.delete(oldSourceFile.resolvedPath);
          if (!hasSourceFileByPath) {
            resolutionCache.removeResolutionsOfFile(oldSourceFile.path);
          }
        }
      }
    }

    function reportWatchDiagnostic(message: DiagnosticMessage) {
      if (host.onWatchStatusChange) {
        host.onWatchStatusChange(
          createCompilerDiagnostic(message),
          newLine,
          compilerOptions || optionsToExtendForConfigFile,
        );
      }
    }

    function hasChangedAutomaticTypeDirectiveNames() {
      return resolutionCache.hasChangedAutomaticTypeDirectiveNames();
    }

    function clearInvalidateResolutionsOfFailedLookupLocations() {
      if (!timerToInvalidateFailedLookupResolutions) return false;
      host.clearTimeout!(timerToInvalidateFailedLookupResolutions);
      timerToInvalidateFailedLookupResolutions = undefined;
      return true;
    }

    function scheduleInvalidateResolutionsOfFailedLookupLocations() {
      if (!host.setTimeout || !host.clearTimeout) {
        return resolutionCache.invalidateResolutionsOfFailedLookupLocations();
      }
      const pending = clearInvalidateResolutionsOfFailedLookupLocations();
      writeLog(
        `Scheduling invalidateFailedLookup${pending ? ", Cancelled earlier one" : ""}`,
      );
      timerToInvalidateFailedLookupResolutions = host.setTimeout(
        invalidateResolutionsOfFailedLookup,
        250,
      );
    }

    function invalidateResolutionsOfFailedLookup() {
      timerToInvalidateFailedLookupResolutions = undefined;
      if (resolutionCache.invalidateResolutionsOfFailedLookupLocations()) {
        scheduleProgramUpdate();
      }
    }

    // Upon detecting a file change, wait for 250ms and then perform a recompilation. This gives batch
    // operations (such as saving all modified files in an editor) a chance to complete before we kick
    // off a new compilation.
    function scheduleProgramUpdate() {
      if (!host.setTimeout || !host.clearTimeout) {
        return;
      }

      if (timerToUpdateProgram) {
        host.clearTimeout(timerToUpdateProgram);
      }
      writeLog("Scheduling update");
      timerToUpdateProgram = host.setTimeout(updateProgramWithWatchStatus, 250);
    }

    function scheduleProgramReload() {
      Debug.assert(!!configFileName);
      reloadLevel = ConfigFileProgramReloadLevel.Full;
      scheduleProgramUpdate();
    }

    function updateProgramWithWatchStatus() {
      timerToUpdateProgram = undefined;
      reportFileChangeDetectedOnCreateProgram = true;
      updateProgram();
    }

    function updateProgram() {
      switch (reloadLevel) {
        case ConfigFileProgramReloadLevel.Partial:
          perfLogger.logStartUpdateProgram("PartialConfigReload");
          reloadFileNamesFromConfigFile();
          break;
        case ConfigFileProgramReloadLevel.Full:
          perfLogger.logStartUpdateProgram("FullConfigReload");
          reloadConfigFile();
          break;
        default:
          perfLogger.logStartUpdateProgram("SynchronizeProgram");
          synchronizeProgram();
          break;
      }
      perfLogger.logStopUpdateProgram("Done");
      return getCurrentBuilderProgram();
    }

    function reloadFileNamesFromConfigFile() {
      writeLog("Reloading new file names and options");
      rootFileNames = getFileNamesFromConfigSpecs(
        compilerOptions.configFile!.configFileSpecs!,
        getNormalizedAbsolutePath(
          getDirectoryPath(configFileName),
          currentDirectory,
        ),
        compilerOptions,
        parseConfigFileHost,
        extraFileExtensions,
      );
      if (
        updateErrorForNoInputFiles(
          rootFileNames,
          getNormalizedAbsolutePath(configFileName, currentDirectory),
          compilerOptions.configFile!.configFileSpecs!,
          configFileParsingDiagnostics!,
          canConfigFileJsonReportNoInputFiles,
        )
      ) {
        hasChangedConfigFileParsingErrors = true;
      }

      // Update the program
      synchronizeProgram();
    }

    function reloadConfigFile() {
      writeLog(`Reloading config file: ${configFileName}`);
      reloadLevel = ConfigFileProgramReloadLevel.None;

      if (cachedDirectoryStructureHost) {
        cachedDirectoryStructureHost.clearCache();
      }
      parseConfigFile();
      hasChangedCompilerOptions = true;
      synchronizeProgram();

      // Update the wild card directory watch
      watchConfigFileWildCardDirectories();

      // Update extended config file watch
      updateExtendedConfigFilesWatches(
        toPath(configFileName),
        compilerOptions,
        watchOptions,
        WatchType.ExtendedConfigFile,
      );
    }

    function parseConfigFile() {
      setConfigFileParsingResult(
        getParsedCommandLineOfConfigFile(
          configFileName,
          optionsToExtendForConfigFile,
          parseConfigFileHost,
          extendedConfigCache ||= new Map(),
          watchOptionsToExtend,
          extraFileExtensions,
        )!,
      ); // TODO: GH#18217
    }

    function setConfigFileParsingResult(
      configFileParseResult: ParsedCommandLine,
    ) {
      rootFileNames = configFileParseResult.fileNames;
      compilerOptions = configFileParseResult.options;
      watchOptions = configFileParseResult.watchOptions;
      projectReferences = configFileParseResult.projectReferences;
      wildcardDirectories = configFileParseResult.wildcardDirectories;
      configFileParsingDiagnostics = getConfigFileParsingDiagnostics(
        configFileParseResult,
      ).slice();
      canConfigFileJsonReportNoInputFiles = canJsonReportNoInputFiles(
        configFileParseResult.raw,
      );
      hasChangedConfigFileParsingErrors = true;
    }

    function getParsedCommandLine(
      configFileName: string,
    ): ParsedCommandLine | undefined {
      const configPath = toPath(configFileName);
      let config = parsedConfigs?.get(configPath);
      if (config) {
        if (!config.reloadLevel) return config.parsedCommandLine;
        // With host implementing getParsedCommandLine we cant just update file names
        if (
          config.parsedCommandLine
          && config.reloadLevel === ConfigFileProgramReloadLevel.Partial
          && !host.getParsedCommandLine
        ) {
          writeLog("Reloading new file names and options");
          const fileNames = getFileNamesFromConfigSpecs(
            config.parsedCommandLine.options.configFile!.configFileSpecs!,
            getNormalizedAbsolutePath(
              getDirectoryPath(configFileName),
              currentDirectory,
            ),
            compilerOptions,
            parseConfigFileHost,
          );
          config.parsedCommandLine = { ...config.parsedCommandLine, fileNames };
          config.reloadLevel = undefined;
          return config.parsedCommandLine;
        }
      }

      writeLog(`Loading config file: ${configFileName}`);
      const parsedCommandLine = host.getParsedCommandLine
        ? host.getParsedCommandLine(configFileName)
        : getParsedCommandLineFromConfigFileHost(configFileName);
      if (config) {
        config.parsedCommandLine = parsedCommandLine;
        config.reloadLevel = undefined;
      } else {
        (parsedConfigs ||= new Map()).set(
          configPath,
          config = { parsedCommandLine },
        );
      }
      watchReferencedProject(configFileName, configPath, config);
      return parsedCommandLine;
    }

    function getParsedCommandLineFromConfigFileHost(configFileName: string) {
      // Ignore the file absent errors
      const onUnRecoverableConfigFileDiagnostic = parseConfigFileHost.onUnRecoverableConfigFileDiagnostic;
      parseConfigFileHost.onUnRecoverableConfigFileDiagnostic = noop;
      const parsedCommandLine = getParsedCommandLineOfConfigFile(
        configFileName,
        /*optionsToExtend*/ undefined,
        parseConfigFileHost,
        extendedConfigCache ||= new Map(),
        watchOptionsToExtend,
      );
      parseConfigFileHost.onUnRecoverableConfigFileDiagnostic = onUnRecoverableConfigFileDiagnostic;
      return parsedCommandLine;
    }

    function onReleaseParsedCommandLine(fileName: string) {
      const path = toPath(fileName);
      const config = parsedConfigs?.get(path);
      if (!config) return;

      parsedConfigs!.delete(path);
      if (config.watchedDirectories) {
        clearMap(config.watchedDirectories, closeFileWatcherOf);
      }
      config.watcher?.close();
      clearSharedExtendedConfigFileWatcher(
        path,
        sharedExtendedConfigFileWatchers,
      );
    }

    function watchFilePath(
      path: Path,
      file: string,
      callback: (
        fileName: string,
        eventKind: FileWatcherEventKind,
        filePath: Path,
      ) => void,
      pollingInterval: PollingInterval,
      options: WatchOptions | undefined,
      watchType: WatchType,
    ): FileWatcher {
      return watchFile(
        file,
        (fileName, eventKind) => callback(fileName, eventKind, path),
        pollingInterval,
        options,
        watchType,
      );
    }

    function onSourceFileChange(
      fileName: string,
      eventKind: FileWatcherEventKind,
      path: Path,
    ) {
      updateCachedSystemWithFile(fileName, path, eventKind);

      // Update the source file cache
      if (
        eventKind === FileWatcherEventKind.Deleted
        && sourceFilesCache.has(path)
      ) {
        resolutionCache.invalidateResolutionOfFile(path);
      }
      nextSourceFileVersion(path);

      // Update the program
      scheduleProgramUpdate();
    }

    function updateCachedSystemWithFile(
      fileName: string,
      path: Path,
      eventKind: FileWatcherEventKind,
    ) {
      if (cachedDirectoryStructureHost) {
        cachedDirectoryStructureHost.addOrDeleteFile(fileName, path, eventKind);
      }
    }

    function watchMissingFilePath(missingFilePath: Path) {
      // If watching missing referenced config file, we are already watching it so no need for separate watcher
      return parsedConfigs?.has(missingFilePath)
        ? noopFileWatcher
        : watchFilePath(
          missingFilePath,
          missingFilePath,
          onMissingFileChange,
          PollingInterval.Medium,
          watchOptions,
          WatchType.MissingFile,
        );
    }

    function watchPackageJsonLookupPath(packageJsonPath: Path) {
      // If the package.json is pulled into the compilation itself (eg, via json imports), don't add a second watcher here
      return sourceFilesCache.has(packageJsonPath)
        ? noopFileWatcher
        : watchFilePath(
          packageJsonPath,
          packageJsonPath,
          onPackageJsonChange,
          PollingInterval.High,
          watchOptions,
          WatchType.PackageJson,
        );
    }

    function onPackageJsonChange(
      fileName: string,
      eventKind: FileWatcherEventKind,
      path: Path,
    ) {
      updateCachedSystemWithFile(fileName, path, eventKind);

      // package.json changes invalidate module resolution and can change the set of loaded files
      // so if we witness a change to one, we have to do a full reload
      reloadLevel = ConfigFileProgramReloadLevel.Full;
      changesAffectResolution = true;
      // Update the program
      scheduleProgramUpdate();
    }

    function onMissingFileChange(
      fileName: string,
      eventKind: FileWatcherEventKind,
      missingFilePath: Path,
    ) {
      updateCachedSystemWithFile(fileName, missingFilePath, eventKind);

      if (
        eventKind === FileWatcherEventKind.Created
        && missingFilesMap.has(missingFilePath)
      ) {
        missingFilesMap.get(missingFilePath)!.close();
        missingFilesMap.delete(missingFilePath);

        // Delete the entry in the source files cache so that new source file is created
        nextSourceFileVersion(missingFilePath);

        // When a missing file is created, we should update the graph.
        scheduleProgramUpdate();
      }
    }

    function watchConfigFileWildCardDirectories() {
      if (wildcardDirectories) {
        updateWatchingWildcardDirectories(
          watchedWildcardDirectories
            || (watchedWildcardDirectories = new Map()),
          new Map(getEntries(wildcardDirectories)),
          watchWildcardDirectory,
        );
      } else if (watchedWildcardDirectories) {
        clearMap(watchedWildcardDirectories, closeFileWatcherOf);
      }
    }

    function watchWildcardDirectory(
      directory: string,
      flags: WatchDirectoryFlags,
    ) {
      return watchDirectory(
        directory,
        (fileOrDirectory) => {
          Debug.assert(!!configFileName);

          const fileOrDirectoryPath = toPath(fileOrDirectory);

          // Since the file existence changed, update the sourceFiles cache
          if (cachedDirectoryStructureHost) {
            cachedDirectoryStructureHost.addOrDeleteFileOrDirectory(
              fileOrDirectory,
              fileOrDirectoryPath,
            );
          }
          nextSourceFileVersion(fileOrDirectoryPath);

          if (
            isIgnoredFileFromWildCardWatching({
              watchedDirPath: toPath(directory),
              fileOrDirectory,
              fileOrDirectoryPath,
              configFileName,
              extraFileExtensions,
              options: compilerOptions,
              program: getCurrentBuilderProgram() || rootFileNames,
              currentDirectory,
              useCaseSensitiveFileNames,
              writeLog,
              toPath,
            })
          ) {
            return;
          }

          // Reload is pending, do the reload
          if (reloadLevel !== ConfigFileProgramReloadLevel.Full) {
            reloadLevel = ConfigFileProgramReloadLevel.Partial;

            // Schedule Update the program
            scheduleProgramUpdate();
          }
        },
        flags,
        watchOptions,
        WatchType.WildcardDirectory,
      );
    }

    function updateExtendedConfigFilesWatches(
      forProjectPath: Path,
      options: CompilerOptions | undefined,
      watchOptions: WatchOptions | undefined,
      watchType:
        | WatchTypeRegistry["ExtendedConfigFile"]
        | WatchTypeRegistry["ExtendedConfigOfReferencedProject"],
    ) {
      updateSharedExtendedConfigFileWatcher(
        forProjectPath,
        options,
        sharedExtendedConfigFileWatchers ||= new Map(),
        (extendedConfigFileName, extendedConfigFilePath) =>
          watchFile(
            extendedConfigFileName,
            (_fileName, eventKind) => {
              updateCachedSystemWithFile(
                extendedConfigFileName,
                extendedConfigFilePath,
                eventKind,
              );
              // Update extended config cache
              if (extendedConfigCache) {
                cleanExtendedConfigCache(
                  extendedConfigCache,
                  extendedConfigFilePath,
                  toPath,
                );
              }
              // Update projects
              const projects = sharedExtendedConfigFileWatchers.get(
                extendedConfigFilePath,
              )?.projects;
              // If there are no referenced projects this extended config file watcher depend on ignore
              if (!projects?.size) return;
              projects.forEach((projectPath) => {
                if (toPath(configFileName) === projectPath) {
                  // If this is the config file of the project, reload completely
                  reloadLevel = ConfigFileProgramReloadLevel.Full;
                } else {
                  // Reload config for the referenced projects and remove the resolutions from referenced projects since the config file changed
                  const config = parsedConfigs?.get(projectPath);
                  if (config) {
                    config.reloadLevel = ConfigFileProgramReloadLevel.Full;
                  }
                  resolutionCache.removeResolutionsFromProjectReferenceRedirects(
                    projectPath,
                  );
                }
                scheduleProgramUpdate();
              });
            },
            PollingInterval.High,
            watchOptions,
            watchType,
          ),
        toPath,
      );
    }

    function watchReferencedProject(
      configFileName: string,
      configPath: Path,
      commandLine: ParsedConfig,
    ) {
      // Watch file
      commandLine.watcher ||= watchFile(
        configFileName,
        (_fileName, eventKind) => {
          updateCachedSystemWithFile(configFileName, configPath, eventKind);
          const config = parsedConfigs?.get(configPath);
          if (config) config.reloadLevel = ConfigFileProgramReloadLevel.Full;
          resolutionCache.removeResolutionsFromProjectReferenceRedirects(
            configPath,
          );
          scheduleProgramUpdate();
        },
        PollingInterval.High,
        commandLine.parsedCommandLine?.watchOptions || watchOptions,
        WatchType.ConfigFileOfReferencedProject,
      );
      // Watch Wild card
      if (commandLine.parsedCommandLine?.wildcardDirectories) {
        updateWatchingWildcardDirectories(
          commandLine.watchedDirectories ||= new Map(),
          new Map(
            getEntries(commandLine.parsedCommandLine?.wildcardDirectories),
          ),
          (directory, flags) =>
            watchDirectory(
              directory,
              (fileOrDirectory) => {
                const fileOrDirectoryPath = toPath(fileOrDirectory);
                // Since the file existence changed, update the sourceFiles cache
                if (cachedDirectoryStructureHost) {
                  cachedDirectoryStructureHost.addOrDeleteFileOrDirectory(
                    fileOrDirectory,
                    fileOrDirectoryPath,
                  );
                }
                nextSourceFileVersion(fileOrDirectoryPath);

                const config = parsedConfigs?.get(configPath);
                if (!config?.parsedCommandLine) return;
                if (
                  isIgnoredFileFromWildCardWatching({
                    watchedDirPath: toPath(directory),
                    fileOrDirectory,
                    fileOrDirectoryPath,
                    configFileName,
                    options: config.parsedCommandLine.options,
                    program: config.parsedCommandLine.fileNames,
                    currentDirectory,
                    useCaseSensitiveFileNames,
                    writeLog,
                    toPath,
                  })
                ) {
                  return;
                }

                // Reload is pending, do the reload
                if (config.reloadLevel !== ConfigFileProgramReloadLevel.Full) {
                  config.reloadLevel = ConfigFileProgramReloadLevel.Partial;

                  // Schedule Update the program
                  scheduleProgramUpdate();
                }
              },
              flags,
              commandLine.parsedCommandLine?.watchOptions || watchOptions,
              WatchType.WildcardDirectoryOfReferencedProject,
            ),
        );
      } else if (commandLine.watchedDirectories) {
        clearMap(commandLine.watchedDirectories, closeFileWatcherOf);
        commandLine.watchedDirectories = undefined;
      }
      // Watch extended config files
      updateExtendedConfigFilesWatches(
        configPath,
        commandLine.parsedCommandLine?.options,
        commandLine.parsedCommandLine?.watchOptions || watchOptions,
        WatchType.ExtendedConfigOfReferencedProject,
      );
    }
  }
}
