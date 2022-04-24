namespace ts.server {
  export enum ProjectKind {
    Inferred,
    Configured,
    External,
    AutoImportProvider,
  }

  /* @internal */
  export type Mutable<T> = { -readonly [K in keyof T]: T[K] };

  /* @internal */
  export function countEachFileTypes(
    infos: ScriptInfo[],
    includeSizes = false,
  ): FileStats {
    const result: Mutable<FileStats> = {
      js: 0,
      jsSize: 0,
      jsx: 0,
      jsxSize: 0,
      ts: 0,
      tsSize: 0,
      tsx: 0,
      tsxSize: 0,
      dts: 0,
      dtsSize: 0,
      deferred: 0,
      deferredSize: 0,
    };
    for (const info of infos) {
      const fileSize = includeSizes ? info.getTelemetryFileSize() : 0;
      switch (info.scriptKind) {
        case ScriptKind.JS:
          result.js += 1;
          result.jsSize! += fileSize;
          break;
        case ScriptKind.JSX:
          result.jsx += 1;
          result.jsxSize! += fileSize;
          break;
        case ScriptKind.TS:
          if (fileExtensionIs(info.fileName, Extension.Dts)) {
            result.dts += 1;
            result.dtsSize! += fileSize;
          } else {
            result.ts += 1;
            result.tsSize! += fileSize;
          }
          break;
        case ScriptKind.TSX:
          result.tsx += 1;
          result.tsxSize! += fileSize;
          break;
        case ScriptKind.Deferred:
          result.deferred += 1;
          result.deferredSize! += fileSize;
          break;
      }
    }
    return result;
  }

  function hasOneOrMoreJsAndNoTsFiles(project: Project) {
    const counts = countEachFileTypes(project.getScriptInfos());
    return counts.js > 0 && counts.ts === 0 && counts.tsx === 0;
  }

  export function allRootFilesAreJsOrDts(project: Project): boolean {
    const counts = countEachFileTypes(project.getRootScriptInfos());
    return counts.ts === 0 && counts.tsx === 0;
  }

  export function allFilesAreJsOrDts(project: Project): boolean {
    const counts = countEachFileTypes(project.getScriptInfos());
    return counts.ts === 0 && counts.tsx === 0;
  }

  /* @internal */
  export function hasNoTypeScriptSource(fileNames: string[]): boolean {
    return !fileNames.some(
      (fileName) =>
        (fileExtensionIs(fileName, Extension.Ts)
          && !fileExtensionIs(fileName, Extension.Dts))
        || fileExtensionIs(fileName, Extension.Tsx),
    );
  }

  /* @internal */
  export interface ProjectFilesWithTSDiagnostics extends protocol.ProjectFiles {
    projectErrors: readonly Diagnostic[];
  }

  export interface PluginCreateInfo {
    project: Project;
    languageService: LanguageService;
    languageServiceHost: LanguageServiceHost;
    serverHost: ServerHost;
    session?: Session<unknown>;
    config: any;
  }

  export interface PluginModule {
    create(createInfo: PluginCreateInfo): LanguageService;
    getExternalFiles?(proj: Project): string[];
    onConfigurationChanged?(config: any): void;
  }

  export interface PluginModuleWithName {
    name: string;
    module: PluginModule;
  }

  export type PluginModuleFactory = (mod: {
    typescript: typeof ts;
  }) => PluginModule;

  /**
   * The project root can be script info - if root is present,
   * or it could be just normalized path if root wasn't present on the host(only for non inferred project)
   */
  /* @internal */
  export interface ProjectRootFile {
    fileName: NormalizedPath;
    info?: ScriptInfo;
  }

  interface GeneratedFileWatcher {
    generatedFilePath: Path;
    watcher: FileWatcher;
  }
  type GeneratedFileWatcherMap =
    | GeneratedFileWatcher
    | ESMap<Path, GeneratedFileWatcher>;
  function isGeneratedFileWatcher(
    watch: GeneratedFileWatcherMap,
  ): watch is GeneratedFileWatcher {
    return (watch as GeneratedFileWatcher).generatedFilePath !== undefined;
  }

  /*@internal*/
  export interface EmitResult {
    emitSkipped: boolean;
    diagnostics: readonly Diagnostic[];
  }

  export abstract class Project implements LanguageServiceHost, ModuleResolutionHost {
    private rootFiles: ScriptInfo[] = [];
    private rootFilesMap = new Map<string, ProjectRootFile>();
    private program: Program | undefined;
    private externalFiles: SortedReadonlyArray<string> | undefined;
    private missingFilesMap: ESMap<Path, FileWatcher> | undefined;
    private generatedFilesMap: GeneratedFileWatcherMap | undefined;
    private plugins: PluginModuleWithName[] = [];

    /*@internal*/
    /**
     * This is map from files to unresolved imports in it
     * Maop does not contain entries for files that do not have unresolved imports
     * This helps in containing the set of files to invalidate
     */
    cachedUnresolvedImportsPerFile = new Map<Path, readonly string[]>();

    /*@internal*/
    lastCachedUnresolvedImportsList: SortedReadonlyArray<string> | undefined;
    /*@internal*/
    private hasAddedorRemovedFiles = false;
    /*@internal*/
    private hasAddedOrRemovedSymlinks = false;

    /*@internal*/
    lastFileExceededProgramSize: string | undefined;

    // wrapper over the real language service that will suppress all semantic operations
    protected languageService: LanguageService;

    public languageServiceEnabled: boolean;

    readonly trace?: (s: string) => void;
    readonly realpath?: (path: string) => string;

    /*@internal*/
    hasInvalidatedResolution: HasInvalidatedResolution | undefined;

    /*@internal*/
    resolutionCache: ResolutionCache;

    private builderState: BuilderState | undefined;
    /**
     * Set of files names that were updated since the last call to getChangesSinceVersion.
     */
    private updatedFileNames: Set<string> | undefined;
    /**
     * Set of files that was returned from the last call to getChangesSinceVersion.
     */
    private lastReportedFileNames: ESMap<string, boolean> | undefined;
    /**
     * Last version that was reported.
     */
    private lastReportedVersion = 0;
    /**
     * Current project's program version. (incremented everytime new program is created that is not complete reuse from the old one)
     * This property is changed in 'updateGraph' based on the set of files in program
     */
    private projectProgramVersion = 0;
    /**
     * Current version of the project state. It is changed when:
     * - new root file was added/removed
     * - edit happen in some file that is currently included in the project.
     * This property is different from projectStructureVersion since in most cases edits don't affect set of files in the project
     */
    private projectStateVersion = 0;

    protected projectErrors: Diagnostic[] | undefined;

    protected isInitialLoadPending: () => boolean = returnFalse;

    /*@internal*/
    dirty = false;

    /*@internal*/
    typingFiles: SortedReadonlyArray<string> = emptyArray;

    /*@internal*/
    originalConfiguredProjects: Set<NormalizedPath> | undefined;

    /*@internal*/
    private packageJsonsForAutoImport: Set<string> | undefined;

    /*@internal*/
    getResolvedProjectReferenceToRedirect(
      _fileName: string,
    ): ResolvedProjectReference | undefined {
      return undefined;
    }

    /* @internal */ useSourceOfProjectReferenceRedirect?(): boolean;
    /* @internal */ getParsedCommandLine?(
      fileName: string,
    ): ParsedCommandLine | undefined;

    private readonly cancellationToken: ThrottledCancellationToken;

    public isNonTsProject() {
      updateProjectIfDirty(this);
      return allFilesAreJsOrDts(this);
    }

    public isJsOnlyProject() {
      updateProjectIfDirty(this);
      return hasOneOrMoreJsAndNoTsFiles(this);
    }

    public static resolveModule(
      moduleName: string,
      initialDir: string,
      host: ServerHost,
      log: (message: string) => void,
      logErrors?: (message: string) => void,
    ): {} | undefined {
      const resolvedPath = normalizeSlashes(
        host.resolvePath(combinePaths(initialDir, "node_modules")),
      );
      log(
        `Loading ${moduleName} from ${initialDir} (resolved to ${resolvedPath})`,
      );
      const result = host.require!(resolvedPath, moduleName); // TODO: GH#18217
      if (result.error) {
        const err = result.error.stack
          || result.error.message
          || JSON.stringify(result.error);
        (logErrors || log)(
          `Failed to load module '${moduleName}' from ${resolvedPath}: ${err}`,
        );
        return undefined;
      }
      return result.module;
    }

    /*@internal*/
    readonly currentDirectory: string;

    /*@internal*/
    public directoryStructureHost: DirectoryStructureHost;

    /*@internal*/
    public readonly getCanonicalFileName: GetCanonicalFileName;

    /*@internal*/
    private exportMapCache: ExportInfoMap | undefined;
    /*@internal*/
    private changedFilesForExportMapCache: Set<Path> | undefined;
    /*@internal*/
    private moduleSpecifierCache = createModuleSpecifierCache(this);
    /*@internal*/
    private symlinks: SymlinkCache | undefined;
    /*@internal*/
    autoImportProviderHost: AutoImportProviderProject | false | undefined;
    /*@internal*/
    protected typeAcquisition: TypeAcquisition | undefined;

    /*@internal*/
    constructor(
      /*@internal*/ readonly projectName: string,
      readonly projectKind: ProjectKind,
      readonly projectService: ProjectService,
      private documentRegistry: DocumentRegistry,
      hasExplicitListOfFiles: boolean,
      lastFileExceededProgramSize: string | undefined,
      private compilerOptions: CompilerOptions,
      public compileOnSaveEnabled: boolean,
      protected watchOptions: WatchOptions | undefined,
      directoryStructureHost: DirectoryStructureHost,
      currentDirectory: string | undefined,
    ) {
      this.directoryStructureHost = directoryStructureHost;
      this.currentDirectory = this.projectService.getNormalizedAbsolutePath(
        currentDirectory || "",
      );
      this.getCanonicalFileName = this.projectService.toCanonicalFileName;

      this.cancellationToken = new ThrottledCancellationToken(
        this.projectService.cancellationToken,
        this.projectService.throttleWaitMilliseconds,
      );
      if (!this.compilerOptions) {
        this.compilerOptions = getDefaultCompilerOptions();
        this.compilerOptions.allowNonTsExtensions = true;
        this.compilerOptions.allowJs = true;
      } else if (
        hasExplicitListOfFiles
        || getAllowJSCompilerOption(this.compilerOptions)
        || this.projectService.hasDeferredExtension()
      ) {
        // If files are listed explicitly or allowJs is specified, allow all extensions
        this.compilerOptions.allowNonTsExtensions = true;
      }

      switch (projectService.serverMode) {
        case LanguageServiceMode.Semantic:
          this.languageServiceEnabled = true;
          break;
        case LanguageServiceMode.PartialSemantic:
          this.languageServiceEnabled = true;
          this.compilerOptions.noResolve = true;
          this.compilerOptions.types = [];
          break;
        case LanguageServiceMode.Syntactic:
          this.languageServiceEnabled = false;
          this.compilerOptions.noResolve = true;
          this.compilerOptions.types = [];
          break;
        default:
          Debug.assertNever(projectService.serverMode);
      }

      this.setInternalCompilerOptionsForEmittingJsFiles();
      const host = this.projectService.host;
      if (this.projectService.logger.loggingEnabled()) {
        this.trace = (s) => this.writeLog(s);
      } else if (host.trace) {
        this.trace = (s) => host.trace!(s);
      }
      this.realpath = maybeBind(host, host.realpath);

      // Use the current directory as resolution root only if the project created using current directory string
      this.resolutionCache = createResolutionCache(
        this,
        currentDirectory && this.currentDirectory,
        /*logChangesWhenResolvingModule*/ true,
      );
      this.languageService = createLanguageService(
        this,
        this.documentRegistry,
        this.projectService.serverMode,
      );
      if (lastFileExceededProgramSize) {
        this.disableLanguageService(lastFileExceededProgramSize);
      }
      this.markAsDirty();
      if (projectKind !== ProjectKind.AutoImportProvider) {
        this.projectService.pendingEnsureProjectForOpenFiles = true;
      }
    }

    isKnownTypesPackageName(name: string): boolean {
      return this.typingsCache.isKnownTypesPackageName(name);
    }
    installPackage(
      options: InstallPackageOptions,
    ): Promise<ApplyCodeActionCommandResult> {
      return this.typingsCache.installPackage({
        ...options,
        projectName: this.projectName,
        projectRootPath: this.toPath(this.currentDirectory),
      });
    }

    /*@internal*/
    getGlobalTypingsCacheLocation() {
      return this.getGlobalCache();
    }

    private get typingsCache(): TypingsCache {
      return this.projectService.typingsCache;
    }

    /*@internal*/
    getSymlinkCache(): SymlinkCache {
      if (!this.symlinks) {
        this.symlinks = createSymlinkCache(
          this.getCurrentDirectory(),
          this.getCanonicalFileName,
        );
      }
      if (this.program && !this.symlinks.hasProcessedResolutions()) {
        this.symlinks.setSymlinksFromResolutions(
          this.program.getSourceFiles(),
          this.program.getResolvedTypeReferenceDirectives(),
        );
      }
      return this.symlinks;
    }

    // Method of LanguageServiceHost
    getCompilationSettings() {
      return this.compilerOptions;
    }

    // Method to support public API
    getCompilerOptions() {
      return this.getCompilationSettings();
    }

    getNewLine() {
      return this.projectService.host.newLine;
    }

    getProjectVersion() {
      return this.projectStateVersion.toString();
    }

    getProjectReferences(): readonly ProjectReference[] | undefined {
      return undefined;
    }

    getScriptFileNames() {
      if (!this.rootFiles) {
        return ts.emptyArray;
      }

      let result: string[] | undefined;
      this.rootFilesMap.forEach((value) => {
        if (
          this.languageServiceEnabled
          || (value.info && value.info.isScriptOpen())
        ) {
          // if language service is disabled - process only files that are open
          (result || (result = [])).push(value.fileName);
        }
      });

      return addRange(result, this.typingFiles) || ts.emptyArray;
    }

    private getOrCreateScriptInfoAndAttachToProject(fileName: string) {
      const scriptInfo = this.projectService.getOrCreateScriptInfoNotOpenedByClient(
        fileName,
        this.currentDirectory,
        this.directoryStructureHost,
      );
      if (scriptInfo) {
        const existingValue = this.rootFilesMap.get(scriptInfo.path);
        if (existingValue && existingValue.info !== scriptInfo) {
          // This was missing path earlier but now the file exists. Update the root
          this.rootFiles.push(scriptInfo);
          existingValue.info = scriptInfo;
        }
        scriptInfo.attachToProject(this);
      }
      return scriptInfo;
    }

    getScriptKind(fileName: string) {
      const info = this.getOrCreateScriptInfoAndAttachToProject(fileName);
      return (info && info.scriptKind)!; // TODO: GH#18217
    }

    getScriptVersion(filename: string) {
      // Don't attach to the project if version is asked

      const info = this.projectService.getOrCreateScriptInfoNotOpenedByClient(
        filename,
        this.currentDirectory,
        this.directoryStructureHost,
      );
      return (info && info.getLatestVersion())!; // TODO: GH#18217
    }

    getScriptSnapshot(filename: string): IScriptSnapshot | undefined {
      const scriptInfo = this.getOrCreateScriptInfoAndAttachToProject(filename);
      if (scriptInfo) {
        return scriptInfo.getSnapshot();
      }
    }

    getCancellationToken(): HostCancellationToken {
      return this.cancellationToken;
    }

    getCurrentDirectory(): string {
      return this.currentDirectory;
    }

    getDefaultLibFileName() {
      const nodeModuleBinDir = getDirectoryPath(
        normalizePath(this.projectService.getExecutingFilePath()),
      );
      return combinePaths(
        nodeModuleBinDir,
        getDefaultLibFileName(this.compilerOptions),
      );
    }

    useCaseSensitiveFileNames() {
      return this.projectService.host.useCaseSensitiveFileNames;
    }

    readDirectory(
      path: string,
      extensions?: readonly string[],
      exclude?: readonly string[],
      include?: readonly string[],
      depth?: number,
    ): string[] {
      return this.directoryStructureHost.readDirectory!(
        path,
        extensions,
        exclude,
        include,
        depth,
      );
    }

    readFile(fileName: string): string | undefined {
      return this.projectService.host.readFile(fileName);
    }

    writeFile(fileName: string, content: string): void {
      return this.projectService.host.writeFile(fileName, content);
    }

    fileExists(file: string): boolean {
      // As an optimization, don't hit the disks for files we already know don't exist
      // (because we're watching for their creation).
      const path = this.toPath(file);
      return (
        !this.isWatchedMissingFile(path)
        && this.directoryStructureHost.fileExists(file)
      );
    }

    resolveModuleNames(
      moduleNames: string[],
      containingFile: string,
      reusedNames?: string[],
      redirectedReference?: ResolvedProjectReference,
      _options?: CompilerOptions,
      containingSourceFile?: SourceFile,
    ): (ResolvedModuleFull | undefined)[] {
      return this.resolutionCache.resolveModuleNames(
        moduleNames,
        containingFile,
        reusedNames,
        redirectedReference,
        containingSourceFile,
      );
    }

    getModuleResolutionCache(): ModuleResolutionCache | undefined {
      return this.resolutionCache.getModuleResolutionCache();
    }

    getResolvedModuleWithFailedLookupLocationsFromCache(
      moduleName: string,
      containingFile: string,
      resolutionMode?: ModuleKind.CommonJS | ModuleKind.ESNext,
    ): ResolvedModuleWithFailedLookupLocations | undefined {
      return this.resolutionCache.getResolvedModuleWithFailedLookupLocationsFromCache(
        moduleName,
        containingFile,
        resolutionMode,
      );
    }

    resolveTypeReferenceDirectives(
      typeDirectiveNames: string[] | FileReference[],
      containingFile: string,
      redirectedReference?: ResolvedProjectReference,
      _options?: CompilerOptions,
      containingFileMode?: SourceFile["impliedNodeFormat"] | undefined,
    ): (ResolvedTypeReferenceDirective | undefined)[] {
      return this.resolutionCache.resolveTypeReferenceDirectives(
        typeDirectiveNames,
        containingFile,
        redirectedReference,
        containingFileMode,
      );
    }

    directoryExists(path: string): boolean {
      return this.directoryStructureHost.directoryExists!(path); // TODO: GH#18217
    }

    getDirectories(path: string): string[] {
      return this.directoryStructureHost.getDirectories!(path); // TODO: GH#18217
    }

    /*@internal*/
    getCachedDirectoryStructureHost(): CachedDirectoryStructureHost {
      return undefined!; // TODO: GH#18217
    }

    /*@internal*/
    toPath(fileName: string) {
      return toPath(
        fileName,
        this.currentDirectory,
        this.projectService.toCanonicalFileName,
      );
    }

    /*@internal*/
    watchDirectoryOfFailedLookupLocation(
      directory: string,
      cb: DirectoryWatcherCallback,
      flags: WatchDirectoryFlags,
    ) {
      return this.projectService.watchFactory.watchDirectory(
        directory,
        cb,
        flags,
        this.projectService.getWatchOptions(this),
        WatchType.FailedLookupLocations,
        this,
      );
    }

    /*@internal*/
    clearInvalidateResolutionOfFailedLookupTimer() {
      return this.projectService.throttledOperations.cancel(
        `${this.getProjectName()}FailedLookupInvalidation`,
      );
    }

    /*@internal*/
    scheduleInvalidateResolutionsOfFailedLookupLocations() {
      this.projectService.throttledOperations.schedule(
        `${this.getProjectName()}FailedLookupInvalidation`,
        /*delay*/ 1000,
        () => {
          if (
            this.resolutionCache.invalidateResolutionsOfFailedLookupLocations()
          ) {
            this.projectService.delayUpdateProjectGraphAndEnsureProjectStructureForOpenFiles(
              this,
            );
          }
        },
      );
    }

    /*@internal*/
    invalidateResolutionsOfFailedLookupLocations() {
      if (
        this.clearInvalidateResolutionOfFailedLookupTimer()
        && this.resolutionCache.invalidateResolutionsOfFailedLookupLocations()
      ) {
        this.markAsDirty();
        this.projectService.delayEnsureProjectForOpenFiles();
      }
    }

    /*@internal*/
    onInvalidatedResolution() {
      this.projectService.delayUpdateProjectGraphAndEnsureProjectStructureForOpenFiles(
        this,
      );
    }

    /*@internal*/
    watchTypeRootsDirectory(
      directory: string,
      cb: DirectoryWatcherCallback,
      flags: WatchDirectoryFlags,
    ) {
      return this.projectService.watchFactory.watchDirectory(
        directory,
        cb,
        flags,
        this.projectService.getWatchOptions(this),
        WatchType.TypeRoots,
        this,
      );
    }

    /*@internal*/
    hasChangedAutomaticTypeDirectiveNames() {
      return this.resolutionCache.hasChangedAutomaticTypeDirectiveNames();
    }

    /*@internal*/
    onChangedAutomaticTypeDirectiveNames() {
      this.projectService.delayUpdateProjectGraphAndEnsureProjectStructureForOpenFiles(
        this,
      );
    }

    /*@internal*/
    getGlobalCache() {
      return this.getTypeAcquisition().enable
        ? this.projectService.typingsInstaller.globalTypingsCacheLocation
        : undefined;
    }

    /*@internal*/
    globalCacheResolutionModuleName = JsTyping.nonRelativeModuleNameForTypingCache;

    /*@internal*/
    fileIsOpen(filePath: Path) {
      return this.projectService.openFiles.has(filePath);
    }

    /*@internal*/
    writeLog(s: string) {
      this.projectService.logger.info(s);
    }

    log(s: string) {
      this.writeLog(s);
    }

    error(s: string) {
      this.projectService.logger.msg(s, Msg.Err);
    }

    private setInternalCompilerOptionsForEmittingJsFiles() {
      if (
        this.projectKind === ProjectKind.Inferred
        || this.projectKind === ProjectKind.External
      ) {
        this.compilerOptions.noEmitForJsFiles = true;
      }
    }

    /**
     * Get the errors that dont have any file name associated
     */
    getGlobalProjectErrors(): readonly Diagnostic[] {
      return (
        filter(this.projectErrors, (diagnostic) => !diagnostic.file)
        || emptyArray
      );
    }

    /**
     * Get all the project errors
     */
    getAllProjectErrors(): readonly Diagnostic[] {
      return this.projectErrors || emptyArray;
    }

    setProjectErrors(projectErrors: Diagnostic[] | undefined) {
      this.projectErrors = projectErrors;
    }

    getLanguageService(ensureSynchronized = true): LanguageService {
      if (ensureSynchronized) {
        updateProjectIfDirty(this);
      }
      return this.languageService;
    }

    /** @internal */
    getSourceMapper(): SourceMapper {
      return this.getLanguageService().getSourceMapper();
    }

    /** @internal */
    clearSourceMapperCache() {
      this.languageService.clearSourceMapperCache();
    }

    /*@internal*/
    getDocumentPositionMapper(
      generatedFileName: string,
      sourceFileName?: string,
    ): DocumentPositionMapper | undefined {
      return this.projectService.getDocumentPositionMapper(
        this,
        generatedFileName,
        sourceFileName,
      );
    }

    /*@internal*/
    getSourceFileLike(fileName: string) {
      return this.projectService.getSourceFileLike(fileName, this);
    }

    /*@internal*/
    shouldEmitFile(scriptInfo: ScriptInfo | undefined) {
      return (
        scriptInfo
        && !scriptInfo.isDynamicOrHasMixedContent()
        && !this.program!.isSourceOfProjectReferenceRedirect(scriptInfo.path)
      );
    }

    getCompileOnSaveAffectedFileList(scriptInfo: ScriptInfo): string[] {
      if (!this.languageServiceEnabled) {
        return [];
      }
      updateProjectIfDirty(this);
      this.builderState = BuilderState.create(
        this.program!,
        this.projectService.toCanonicalFileName,
        this.builderState,
        /*disableUseFileVersionAsSignature*/ true,
      );
      return mapDefined(
        BuilderState.getFilesAffectedBy(
          this.builderState,
          this.program!,
          scriptInfo.path,
          this.cancellationToken,
          maybeBind(
            this.projectService.host,
            this.projectService.host.createHash,
          ),
        ),
        (sourceFile) =>
          this.shouldEmitFile(
              this.projectService.getScriptInfoForPath(sourceFile.path),
            )
            ? sourceFile.fileName
            : undefined,
      );
    }

    /**
     * Returns true if emit was conducted
     */
    emitFile(
      scriptInfo: ScriptInfo,
      writeFile: (
        path: string,
        data: string,
        writeByteOrderMark?: boolean,
      ) => void,
    ): EmitResult {
      if (!this.languageServiceEnabled || !this.shouldEmitFile(scriptInfo)) {
        return { emitSkipped: true, diagnostics: emptyArray };
      }
      const { emitSkipped, diagnostics, outputFiles } = this.getLanguageService().getEmitOutput(scriptInfo.fileName);
      if (!emitSkipped) {
        for (const outputFile of outputFiles) {
          const outputFileAbsoluteFileName = getNormalizedAbsolutePath(
            outputFile.name,
            this.currentDirectory,
          );
          writeFile(
            outputFileAbsoluteFileName,
            outputFile.text,
            outputFile.writeByteOrderMark,
          );
        }

        // Update the signature
        if (this.builderState && getEmitDeclarations(this.compilerOptions)) {
          const dtsFiles = outputFiles.filter((f) => fileExtensionIs(f.name, Extension.Dts));
          if (dtsFiles.length === 1) {
            const sourceFile = this.program!.getSourceFile(
              scriptInfo.fileName,
            )!;
            const signature = this.projectService.host.createHash
              ? this.projectService.host.createHash(dtsFiles[0].text)
              : generateDjb2Hash(dtsFiles[0].text);
            BuilderState.updateSignatureOfFile(
              this.builderState,
              signature,
              sourceFile.resolvedPath,
            );
          }
        }
      }

      return { emitSkipped, diagnostics };
    }

    enableLanguageService() {
      if (
        this.languageServiceEnabled
        || this.projectService.serverMode === LanguageServiceMode.Syntactic
      ) {
        return;
      }
      this.languageServiceEnabled = true;
      this.lastFileExceededProgramSize = undefined;
      this.projectService.onUpdateLanguageServiceStateForProject(
        this,
        /*languageServiceEnabled*/ true,
      );
    }

    disableLanguageService(lastFileExceededProgramSize?: string) {
      if (!this.languageServiceEnabled) {
        return;
      }
      Debug.assert(
        this.projectService.serverMode !== LanguageServiceMode.Syntactic,
      );
      this.languageService.cleanupSemanticCache();
      this.languageServiceEnabled = false;
      this.lastFileExceededProgramSize = lastFileExceededProgramSize;
      this.builderState = undefined;
      if (this.autoImportProviderHost) {
        this.autoImportProviderHost.close();
      }
      this.autoImportProviderHost = undefined;
      this.resolutionCache.closeTypeRootsWatch();
      this.clearGeneratedFileWatch();
      this.projectService.onUpdateLanguageServiceStateForProject(
        this,
        /*languageServiceEnabled*/ false,
      );
    }

    getProjectName() {
      return this.projectName;
    }

    protected removeLocalTypingsFromTypeAcquisition(
      newTypeAcquisition: TypeAcquisition,
    ): TypeAcquisition {
      if (!newTypeAcquisition || !newTypeAcquisition.include) {
        // Nothing to filter out, so just return as-is
        return newTypeAcquisition;
      }
      return {
        ...newTypeAcquisition,
        include: this.removeExistingTypings(newTypeAcquisition.include),
      };
    }

    getExternalFiles(): SortedReadonlyArray<string> {
      return sort(
        flatMap(this.plugins, (plugin) => {
          if (typeof plugin.module.getExternalFiles !== "function") return;
          try {
            return plugin.module.getExternalFiles(this);
          } catch (e) {
            this.projectService.logger.info(
              `A plugin threw an exception in getExternalFiles: ${e}`,
            );
            if (e.stack) {
              this.projectService.logger.info(e.stack);
            }
          }
        }),
      );
    }

    getSourceFile(path: Path) {
      if (!this.program) {
        return undefined;
      }
      return this.program.getSourceFileByPath(path);
    }

    /* @internal */
    getSourceFileOrConfigFile(path: Path): SourceFile | undefined {
      const options = this.program!.getCompilerOptions();
      return path === options.configFilePath
        ? options.configFile
        : this.getSourceFile(path);
    }

    close() {
      if (this.program) {
        // if we have a program - release all files that are enlisted in program but arent root
        // The releasing of the roots happens later
        // The project could have pending update remaining and hence the info could be in the files but not in program graph
        for (const f of this.program.getSourceFiles()) {
          this.detachScriptInfoIfNotRoot(f.fileName);
        }
        this.program.forEachResolvedProjectReference((ref) =>
          this.detachScriptInfoFromProject(ref.sourceFile.fileName)
        );
      }

      // Release external files
      forEach(this.externalFiles, (externalFile) => this.detachScriptInfoIfNotRoot(externalFile));
      // Always remove root files from the project
      for (const root of this.rootFiles) {
        root.detachFromProject(this);
      }
      this.projectService.pendingEnsureProjectForOpenFiles = true;

      this.rootFiles = undefined!;
      this.rootFilesMap = undefined!;
      this.externalFiles = undefined;
      this.program = undefined;
      this.builderState = undefined;
      this.resolutionCache.clear();
      this.resolutionCache = undefined!;
      this.cachedUnresolvedImportsPerFile = undefined!;
      this.moduleSpecifierCache = undefined!;
      this.directoryStructureHost = undefined!;
      this.exportMapCache = undefined;
      this.projectErrors = undefined;

      // Clean up file watchers waiting for missing files
      if (this.missingFilesMap) {
        clearMap(this.missingFilesMap, closeFileWatcher);
        this.missingFilesMap = undefined;
      }
      this.clearGeneratedFileWatch();
      this.clearInvalidateResolutionOfFailedLookupTimer();
      if (this.autoImportProviderHost) {
        this.autoImportProviderHost.close();
      }
      this.autoImportProviderHost = undefined;

      // signal language service to release source files acquired from document registry
      this.languageService.dispose();
      this.languageService = undefined!;
    }

    private detachScriptInfoIfNotRoot(uncheckedFilename: string) {
      const info = this.projectService.getScriptInfo(uncheckedFilename);
      // We might not find the script info in case its not associated with the project any more
      // and project graph was not updated (eg delayed update graph in case of files changed/deleted on the disk)
      if (info && !this.isRoot(info)) {
        info.detachFromProject(this);
      }
    }

    isClosed() {
      return this.rootFiles === undefined;
    }

    hasRoots() {
      return this.rootFiles && this.rootFiles.length > 0;
    }

    /*@internal*/
    isOrphan() {
      return false;
    }

    getRootFiles() {
      return this.rootFiles && this.rootFiles.map((info) => info.fileName);
    }

    /*@internal*/
    getRootFilesMap() {
      return this.rootFilesMap;
    }

    getRootScriptInfos() {
      return this.rootFiles;
    }

    getScriptInfos(): ScriptInfo[] {
      if (!this.languageServiceEnabled) {
        // if language service is not enabled - return just root files
        return this.rootFiles;
      }
      return map(this.program!.getSourceFiles(), (sourceFile) => {
        const scriptInfo = this.projectService.getScriptInfoForPath(
          sourceFile.resolvedPath,
        );
        Debug.assert(
          !!scriptInfo,
          "getScriptInfo",
          () =>
            `scriptInfo for a file '${sourceFile.fileName}' Path: '${sourceFile.path}' / '${sourceFile.resolvedPath}' is missing.`,
        );
        return scriptInfo;
      });
    }

    getExcludedFiles(): readonly NormalizedPath[] {
      return emptyArray;
    }

    getFileNames(
      excludeFilesFromExternalLibraries?: boolean,
      excludeConfigFiles?: boolean,
    ) {
      if (!this.program) {
        return [];
      }

      if (!this.languageServiceEnabled) {
        // if language service is disabled assume that all files in program are root files + default library
        let rootFiles = this.getRootFiles();
        if (this.compilerOptions) {
          const defaultLibrary = getDefaultLibFilePath(this.compilerOptions);
          if (defaultLibrary) {
            (rootFiles || (rootFiles = [])).push(
              asNormalizedPath(defaultLibrary),
            );
          }
        }
        return rootFiles;
      }
      const result: NormalizedPath[] = [];
      for (const f of this.program.getSourceFiles()) {
        if (
          excludeFilesFromExternalLibraries
          && this.program.isSourceFileFromExternalLibrary(f)
        ) {
          continue;
        }
        result.push(asNormalizedPath(f.fileName));
      }
      if (!excludeConfigFiles) {
        const configFile = this.program.getCompilerOptions().configFile;
        if (configFile) {
          result.push(asNormalizedPath(configFile.fileName));
          if (configFile.extendedSourceFiles) {
            for (const f of configFile.extendedSourceFiles) {
              result.push(asNormalizedPath(f));
            }
          }
        }
      }
      return result;
    }

    /* @internal */
    getFileNamesWithRedirectInfo(includeProjectReferenceRedirectInfo: boolean) {
      return this.getFileNames().map(
        (fileName): protocol.FileWithProjectReferenceRedirectInfo => ({
          fileName,
          isSourceOfProjectReferenceRedirect: includeProjectReferenceRedirectInfo
            && this.isSourceOfProjectReferenceRedirect(fileName),
        }),
      );
    }

    hasConfigFile(configFilePath: NormalizedPath) {
      if (this.program && this.languageServiceEnabled) {
        const configFile = this.program.getCompilerOptions().configFile;
        if (configFile) {
          if (configFilePath === asNormalizedPath(configFile.fileName)) {
            return true;
          }
          if (configFile.extendedSourceFiles) {
            for (const f of configFile.extendedSourceFiles) {
              if (configFilePath === asNormalizedPath(f)) {
                return true;
              }
            }
          }
        }
      }
      return false;
    }

    containsScriptInfo(info: ScriptInfo): boolean {
      if (this.isRoot(info)) return true;
      if (!this.program) return false;
      const file = this.program.getSourceFileByPath(info.path);
      return !!file && file.resolvedPath === info.path;
    }

    containsFile(filename: NormalizedPath, requireOpen?: boolean): boolean {
      const info = this.projectService.getScriptInfoForNormalizedPath(filename);
      if (info && (info.isScriptOpen() || !requireOpen)) {
        return this.containsScriptInfo(info);
      }
      return false;
    }

    isRoot(info: ScriptInfo) {
      return (
        this.rootFilesMap && this.rootFilesMap.get(info.path)?.info === info
      );
    }

    // add a root file to project
    addRoot(info: ScriptInfo, fileName?: NormalizedPath) {
      Debug.assert(!this.isRoot(info));
      this.rootFiles.push(info);
      this.rootFilesMap.set(info.path, {
        fileName: fileName || info.fileName,
        info,
      });
      info.attachToProject(this);

      this.markAsDirty();
    }

    // add a root file that doesnt exist on host
    addMissingFileRoot(fileName: NormalizedPath) {
      const path = this.projectService.toPath(fileName);
      this.rootFilesMap.set(path, { fileName });
      this.markAsDirty();
    }

    removeFile(
      info: ScriptInfo,
      fileExists: boolean,
      detachFromProject: boolean,
    ) {
      if (this.isRoot(info)) {
        this.removeRoot(info);
      }
      if (fileExists) {
        // If file is present, just remove the resolutions for the file
        this.resolutionCache.removeResolutionsOfFile(info.path);
      } else {
        this.resolutionCache.invalidateResolutionOfFile(info.path);
      }
      this.cachedUnresolvedImportsPerFile.delete(info.path);

      if (detachFromProject) {
        info.detachFromProject(this);
      }

      this.markAsDirty();
    }

    registerFileUpdate(fileName: string) {
      (
        this.updatedFileNames || (this.updatedFileNames = new Set<string>())
      ).add(fileName);
    }

    /*@internal*/
    markFileAsDirty(changedFile: Path) {
      this.markAsDirty();
      if (this.exportMapCache && !this.exportMapCache.isEmpty()) {
        (this.changedFilesForExportMapCache ||= new Set()).add(changedFile);
      }
    }

    markAsDirty() {
      if (!this.dirty) {
        this.projectStateVersion++;
        this.dirty = true;
      }
    }

    /*@internal*/
    onAutoImportProviderSettingsChanged() {
      if (this.autoImportProviderHost === false) {
        this.autoImportProviderHost = undefined;
      } else {
        this.autoImportProviderHost?.markAsDirty();
      }
    }

    /*@internal*/
    onPackageJsonChange(packageJsonPath: Path) {
      if (this.packageJsonsForAutoImport?.has(packageJsonPath)) {
        this.moduleSpecifierCache.clear();
        if (this.autoImportProviderHost) {
          this.autoImportProviderHost.markAsDirty();
        }
      }
    }

    /* @internal */
    onFileAddedOrRemoved(isSymlink: boolean | undefined) {
      this.hasAddedorRemovedFiles = true;
      if (isSymlink) {
        this.hasAddedOrRemovedSymlinks = true;
      }
    }

    /* @internal */
    onDiscoveredSymlink() {
      this.hasAddedOrRemovedSymlinks = true;
    }

    /**
     * Updates set of files that contribute to this project
     * @returns: true if set of files in the project stays the same and false - otherwise.
     */
    updateGraph(): boolean {
      tracing?.push(tracing.Phase.Session, "updateGraph", {
        name: this.projectName,
        kind: ProjectKind[this.projectKind],
      });
      perfLogger.logStartUpdateGraph();
      this.resolutionCache.startRecordingFilesWithChangedResolutions();

      const hasNewProgram = this.updateGraphWorker();
      const hasAddedorRemovedFiles = this.hasAddedorRemovedFiles;
      this.hasAddedorRemovedFiles = false;
      this.hasAddedOrRemovedSymlinks = false;

      const changedFiles: readonly Path[] = this.resolutionCache.finishRecordingFilesWithChangedResolutions()
        || emptyArray;

      for (const file of changedFiles) {
        // delete cached information for changed files
        this.cachedUnresolvedImportsPerFile.delete(file);
      }

      // update builder only if language service is enabled
      // otherwise tell it to drop its internal state
      if (
        this.languageServiceEnabled
        && this.projectService.serverMode === LanguageServiceMode.Semantic
      ) {
        // 1. no changes in structure, no changes in unresolved imports - do nothing
        // 2. no changes in structure, unresolved imports were changed - collect unresolved imports for all files
        // (can reuse cached imports for files that were not changed)
        // 3. new files were added/removed, but compilation settings stays the same - collect unresolved imports for all new/modified files
        // (can reuse cached imports for files that were not changed)
        // 4. compilation settings were changed in the way that might affect module resolution - drop all caches and collect all data from the scratch
        if (hasNewProgram || changedFiles.length) {
          this.lastCachedUnresolvedImportsList = getUnresolvedImports(
            this.program!,
            this.cachedUnresolvedImportsPerFile,
          );
        }

        this.projectService.typingsCache.enqueueInstallTypingsForProject(
          this,
          this.lastCachedUnresolvedImportsList,
          hasAddedorRemovedFiles,
        );
      } else {
        this.lastCachedUnresolvedImportsList = undefined;
      }

      const isFirstProgramLoad = this.projectProgramVersion === 0 && hasNewProgram;
      if (hasNewProgram) {
        this.projectProgramVersion++;
      }
      if (hasAddedorRemovedFiles) {
        if (!this.autoImportProviderHost) {
          this.autoImportProviderHost = undefined;
        }
        this.autoImportProviderHost?.markAsDirty();
      }
      if (isFirstProgramLoad) {
        // Preload auto import provider so it's not created during completions request
        this.getPackageJsonAutoImportProvider();
      }
      perfLogger.logStopUpdateGraph();
      tracing?.pop();
      return !hasNewProgram;
    }

    /*@internal*/
    updateTypingFiles(typingFiles: SortedReadonlyArray<string>) {
      if (
        enumerateInsertsAndDeletes<string, string>(
          typingFiles,
          this.typingFiles,
          getStringComparer(!this.useCaseSensitiveFileNames()),
          /*inserted*/ noop,
          (removed) => this.detachScriptInfoFromProject(removed),
        )
      ) {
        // If typing files changed, then only schedule project update
        this.typingFiles = typingFiles;
        // Invalidate files with unresolved imports
        this.resolutionCache.setFilesWithInvalidatedNonRelativeUnresolvedImports(
          this.cachedUnresolvedImportsPerFile,
        );
        this.projectService.delayUpdateProjectGraphAndEnsureProjectStructureForOpenFiles(
          this,
        );
      }
    }

    /* @internal */
    getCurrentProgram(): Program | undefined {
      return this.program;
    }

    protected removeExistingTypings(include: string[]): string[] {
      const existing = getAutomaticTypeDirectiveNames(
        this.getCompilerOptions(),
        this.directoryStructureHost,
      );
      return include.filter((i) => existing.indexOf(i) < 0);
    }

    private updateGraphWorker() {
      const oldProgram = this.program;
      Debug.assert(
        !this.isClosed(),
        "Called update graph worker of closed project",
      );
      this.writeLog(
        `Starting updateGraphWorker: Project: ${this.getProjectName()}`,
      );
      const start = timestamp();
      this.hasInvalidatedResolution = this.resolutionCache.createHasInvalidatedResolution();
      this.resolutionCache.startCachingPerDirectoryResolution();
      this.program = this.languageService.getProgram(); // TODO: GH#18217
      this.dirty = false;
      tracing?.push(
        tracing.Phase.Session,
        "finishCachingPerDirectoryResolution",
      );
      this.resolutionCache.finishCachingPerDirectoryResolution();
      tracing?.pop();

      Debug.assert(oldProgram === undefined || this.program !== undefined);

      // bump up the version if
      // - oldProgram is not set - this is a first time updateGraph is called
      // - newProgram is different from the old program and structure of the old program was not reused.
      let hasNewProgram = false;
      if (
        this.program
        && (!oldProgram
          || (this.program !== oldProgram
            && this.program.structureIsReused !== StructureIsReused.Completely))
      ) {
        hasNewProgram = true;
        if (oldProgram) {
          for (const f of oldProgram.getSourceFiles()) {
            const newFile = this.program.getSourceFileByPath(f.resolvedPath);
            if (
              !newFile
              || (f.resolvedPath === f.path && newFile.resolvedPath !== f.path)
            ) {
              // new program does not contain this file - detach it from the project
              // - remove resolutions only if the new program doesnt contain source file by the path (not resolvedPath since path is used for resolution)
              this.detachScriptInfoFromProject(
                f.fileName,
                !!this.program.getSourceFileByPath(f.path),
              );
            }
          }

          oldProgram.forEachResolvedProjectReference(
            (resolvedProjectReference) => {
              if (
                !this.program!.getResolvedProjectReferenceByPath(
                  resolvedProjectReference.sourceFile.path,
                )
              ) {
                this.detachScriptInfoFromProject(
                  resolvedProjectReference.sourceFile.fileName,
                );
              }
            },
          );
        }

        // Update the missing file paths watcher
        updateMissingFilePathsWatch(
          this.program,
          this.missingFilesMap || (this.missingFilesMap = new Map()),
          // Watch the missing files
          (missingFilePath) => this.addMissingFileWatcher(missingFilePath),
        );

        if (this.generatedFilesMap) {
          const outPath = outFile(this.compilerOptions);
          if (isGeneratedFileWatcher(this.generatedFilesMap)) {
            // --out
            if (
              !outPath
              || !this.isValidGeneratedFileWatcher(
                removeFileExtension(outPath) + Extension.Dts,
                this.generatedFilesMap,
              )
            ) {
              this.clearGeneratedFileWatch();
            }
          } else {
            // MultiFile
            if (outPath) {
              this.clearGeneratedFileWatch();
            } else {
              this.generatedFilesMap.forEach((watcher, source) => {
                const sourceFile = this.program!.getSourceFileByPath(source);
                if (
                  !sourceFile
                  || sourceFile.resolvedPath !== source
                  || !this.isValidGeneratedFileWatcher(
                    getDeclarationEmitOutputFilePathWorker(
                      sourceFile.fileName,
                      this.compilerOptions,
                      this.currentDirectory,
                      this.program!.getCommonSourceDirectory(),
                      this.getCanonicalFileName,
                    ),
                    watcher,
                  )
                ) {
                  closeFileWatcherOf(watcher);
                  (
                    this.generatedFilesMap as ESMap<
                      string,
                      GeneratedFileWatcher
                    >
                  ).delete(source);
                }
              });
            }
          }
        }

        // Watch the type locations that would be added to program as part of automatic type resolutions
        if (
          this.languageServiceEnabled
          && this.projectService.serverMode === LanguageServiceMode.Semantic
        ) {
          this.resolutionCache.updateTypeRootsWatch();
        }
      }

      if (this.exportMapCache && !this.exportMapCache.isEmpty()) {
        this.exportMapCache.releaseSymbols();
        if (
          this.hasAddedorRemovedFiles
          || (oldProgram && !this.program!.structureIsReused)
        ) {
          this.exportMapCache.clear();
        } else if (
          this.changedFilesForExportMapCache
          && oldProgram
          && this.program
        ) {
          forEachKey(this.changedFilesForExportMapCache, (fileName) => {
            const oldSourceFile = oldProgram.getSourceFileByPath(fileName);
            const sourceFile = this.program!.getSourceFileByPath(fileName);
            if (!oldSourceFile || !sourceFile) {
              this.exportMapCache!.clear();
              return true;
            }
            return this.exportMapCache!.onFileChanged(
              oldSourceFile,
              sourceFile,
              !!this.getTypeAcquisition().enable,
            );
          });
        }
      }
      if (this.changedFilesForExportMapCache) {
        this.changedFilesForExportMapCache.clear();
      }

      if (
        this.hasAddedOrRemovedSymlinks
        || (this.program
          && !this.program.structureIsReused
          && this.getCompilerOptions().preserveSymlinks)
      ) {
        // With --preserveSymlinks, we may not determine that a file is a symlink, so we never set `hasAddedOrRemovedSymlinks`
        this.symlinks = undefined;
        this.moduleSpecifierCache.clear();
      }

      const oldExternalFiles = this.externalFiles || (emptyArray as SortedReadonlyArray<string>);
      this.externalFiles = this.getExternalFiles();
      enumerateInsertsAndDeletes<string, string>(
        this.externalFiles,
        oldExternalFiles,
        getStringComparer(!this.useCaseSensitiveFileNames()),
        // Ensure a ScriptInfo is created for new external files. This is performed indirectly
        // by the host for files in the program when the program is retrieved above but
        // the program doesn't contain external files so this must be done explicitly.
        (inserted) => {
          const scriptInfo = this.projectService.getOrCreateScriptInfoNotOpenedByClient(
            inserted,
            this.currentDirectory,
            this.directoryStructureHost,
          );
          scriptInfo?.attachToProject(this);
        },
        (removed) => this.detachScriptInfoFromProject(removed),
      );
      const elapsed = timestamp() - start;
      this.sendPerformanceEvent("UpdateGraph", elapsed);
      this.writeLog(
        `Finishing updateGraphWorker: Project: ${this.getProjectName()} Version: ${this.getProjectVersion()} structureChanged: ${hasNewProgram}${
          this.program
            ? ` structureIsReused:: ${(ts as any).StructureIsReused[this.program.structureIsReused]}`
            : ""
        } Elapsed: ${elapsed}ms`,
      );
      if (this.hasAddedorRemovedFiles) {
        this.print(/*writeProjectFileNames*/ true);
      } else if (this.program !== oldProgram) {
        this.writeLog(`Different program with same set of files`);
      }
      return hasNewProgram;
    }

    /* @internal */
    sendPerformanceEvent(kind: PerformanceEvent["kind"], durationMs: number) {
      this.projectService.sendPerformanceEvent(kind, durationMs);
    }

    private detachScriptInfoFromProject(
      uncheckedFileName: string,
      noRemoveResolution?: boolean,
    ) {
      const scriptInfoToDetach = this.projectService.getScriptInfo(uncheckedFileName);
      if (scriptInfoToDetach) {
        scriptInfoToDetach.detachFromProject(this);
        if (!noRemoveResolution) {
          this.resolutionCache.removeResolutionsOfFile(scriptInfoToDetach.path);
        }
      }
    }

    private addMissingFileWatcher(missingFilePath: Path) {
      if (isConfiguredProject(this)) {
        // If this file is referenced config file, we are already watching it, no need to watch again
        const configFileExistenceInfo = this.projectService.configFileExistenceInfoCache.get(
          missingFilePath as string as NormalizedPath,
        );
        if (
          configFileExistenceInfo?.config?.projects.has(
            this.canonicalConfigFilePath,
          )
        ) {
          return noopFileWatcher;
        }
      }
      const fileWatcher = this.projectService.watchFactory.watchFile(
        missingFilePath,
        (fileName, eventKind) => {
          if (isConfiguredProject(this)) {
            this.getCachedDirectoryStructureHost().addOrDeleteFile(
              fileName,
              missingFilePath,
              eventKind,
            );
          }

          if (
            eventKind === FileWatcherEventKind.Created
            && this.missingFilesMap!.has(missingFilePath)
          ) {
            this.missingFilesMap!.delete(missingFilePath);
            fileWatcher.close();

            // When a missing file is created, we should update the graph.
            this.projectService.delayUpdateProjectGraphAndEnsureProjectStructureForOpenFiles(
              this,
            );
          }
        },
        PollingInterval.Medium,
        this.projectService.getWatchOptions(this),
        WatchType.MissingFile,
        this,
      );
      return fileWatcher;
    }

    private isWatchedMissingFile(path: Path) {
      return !!this.missingFilesMap && this.missingFilesMap.has(path);
    }

    /* @internal */
    addGeneratedFileWatch(generatedFile: string, sourceFile: string) {
      if (outFile(this.compilerOptions)) {
        // Single watcher
        if (!this.generatedFilesMap) {
          this.generatedFilesMap = this.createGeneratedFileWatcher(generatedFile);
        }
      } else {
        // Map
        const path = this.toPath(sourceFile);
        if (this.generatedFilesMap) {
          if (isGeneratedFileWatcher(this.generatedFilesMap)) {
            Debug.fail(
              `${this.projectName} Expected to not have --out watcher for generated file with options: ${
                JSON.stringify(
                  this.compilerOptions,
                )
              }`,
            );
            return;
          }
          if (this.generatedFilesMap.has(path)) return;
        } else {
          this.generatedFilesMap = new Map();
        }
        this.generatedFilesMap.set(
          path,
          this.createGeneratedFileWatcher(generatedFile),
        );
      }
    }

    private createGeneratedFileWatcher(
      generatedFile: string,
    ): GeneratedFileWatcher {
      return {
        generatedFilePath: this.toPath(generatedFile),
        watcher: this.projectService.watchFactory.watchFile(
          generatedFile,
          () => {
            this.clearSourceMapperCache();
            this.projectService.delayUpdateProjectGraphAndEnsureProjectStructureForOpenFiles(
              this,
            );
          },
          PollingInterval.High,
          this.projectService.getWatchOptions(this),
          WatchType.MissingGeneratedFile,
          this,
        ),
      };
    }

    private isValidGeneratedFileWatcher(
      generateFile: string,
      watcher: GeneratedFileWatcher,
    ) {
      return this.toPath(generateFile) === watcher.generatedFilePath;
    }

    private clearGeneratedFileWatch() {
      if (this.generatedFilesMap) {
        if (isGeneratedFileWatcher(this.generatedFilesMap)) {
          closeFileWatcherOf(this.generatedFilesMap);
        } else {
          clearMap(this.generatedFilesMap, closeFileWatcherOf);
        }
        this.generatedFilesMap = undefined;
      }
    }

    getScriptInfoForNormalizedPath(
      fileName: NormalizedPath,
    ): ScriptInfo | undefined {
      const scriptInfo = this.projectService.getScriptInfoForPath(
        this.toPath(fileName),
      );
      if (scriptInfo && !scriptInfo.isAttached(this)) {
        return Errors.ThrowProjectDoesNotContainDocument(fileName, this);
      }
      return scriptInfo;
    }

    getScriptInfo(uncheckedFileName: string) {
      return this.projectService.getScriptInfo(uncheckedFileName);
    }

    filesToString(writeProjectFileNames: boolean) {
      if (this.isInitialLoadPending()) {
        return "\tFiles (0) InitialLoadPending\n";
      }
      if (!this.program) return "\tFiles (0) NoProgram\n";
      const sourceFiles = this.program.getSourceFiles();
      let strBuilder = `\tFiles (${sourceFiles.length})\n`;
      if (writeProjectFileNames) {
        for (const file of sourceFiles) {
          strBuilder += `\t${file.fileName}\n`;
        }
        strBuilder += "\n\n";
        explainFiles(this.program, (s) => (strBuilder += `\t${s}\n`));
      }
      return strBuilder;
    }

    /*@internal*/
    print(writeProjectFileNames: boolean) {
      this.writeLog(
        `Project '${this.projectName}' (${ProjectKind[this.projectKind]})`,
      );
      this.writeLog(
        this.filesToString(
          writeProjectFileNames
            && this.projectService.logger.hasLevel(LogLevel.verbose),
        ),
      );
      this.writeLog("-----------------------------------------------");
      if (this.autoImportProviderHost) {
        this.autoImportProviderHost.print(/*writeProjectFileNames*/ false);
      }
    }

    setCompilerOptions(compilerOptions: CompilerOptions) {
      if (compilerOptions) {
        compilerOptions.allowNonTsExtensions = true;
        const oldOptions = this.compilerOptions;
        this.compilerOptions = compilerOptions;
        this.setInternalCompilerOptionsForEmittingJsFiles();
        if (changesAffectModuleResolution(oldOptions, compilerOptions)) {
          // reset cached unresolved imports if changes in compiler options affected module resolution
          this.cachedUnresolvedImportsPerFile.clear();
          this.lastCachedUnresolvedImportsList = undefined;
          this.resolutionCache.clear();
          this.moduleSpecifierCache.clear();
        }
        this.markAsDirty();
      }
    }

    /*@internal*/
    setWatchOptions(watchOptions: WatchOptions | undefined) {
      this.watchOptions = watchOptions;
    }

    /*@internal*/
    getWatchOptions(): WatchOptions | undefined {
      return this.watchOptions;
    }

    setTypeAcquisition(newTypeAcquisition: TypeAcquisition | undefined): void {
      if (newTypeAcquisition) {
        this.typeAcquisition = this.removeLocalTypingsFromTypeAcquisition(newTypeAcquisition);
      }
    }

    getTypeAcquisition() {
      return this.typeAcquisition || {};
    }

    /* @internal */
    getChangesSinceVersion(
      lastKnownVersion?: number,
      includeProjectReferenceRedirectInfo?: boolean,
    ): ProjectFilesWithTSDiagnostics {
      const includeProjectReferenceRedirectInfoIfRequested = includeProjectReferenceRedirectInfo
        ? (files: ESMap<string, boolean>) =>
          arrayFrom(
            files.entries(),
            ([
              fileName,
              isSourceOfProjectReferenceRedirect,
            ]): protocol.FileWithProjectReferenceRedirectInfo => ({
              fileName,
              isSourceOfProjectReferenceRedirect,
            }),
          )
        : (files: ESMap<string, boolean>) => arrayFrom(files.keys());

      // Update the graph only if initial configured project load is not pending
      if (!this.isInitialLoadPending()) {
        updateProjectIfDirty(this);
      }

      const info: protocol.ProjectVersionInfo = {
        projectName: this.getProjectName(),
        version: this.projectProgramVersion,
        isInferred: isInferredProject(this),
        options: this.getCompilationSettings(),
        languageServiceDisabled: !this.languageServiceEnabled,
        lastFileExceededProgramSize: this.lastFileExceededProgramSize,
      };
      const updatedFileNames = this.updatedFileNames;
      this.updatedFileNames = undefined;
      // check if requested version is the same that we have reported last time
      if (
        this.lastReportedFileNames
        && lastKnownVersion === this.lastReportedVersion
      ) {
        // if current structure version is the same - return info without any changes
        if (
          this.projectProgramVersion === this.lastReportedVersion
          && !updatedFileNames
        ) {
          return { info, projectErrors: this.getGlobalProjectErrors() };
        }
        // compute and return the difference
        const lastReportedFileNames = this.lastReportedFileNames;
        const externalFiles = this.getExternalFiles().map(
          (f): protocol.FileWithProjectReferenceRedirectInfo => ({
            fileName: toNormalizedPath(f),
            isSourceOfProjectReferenceRedirect: false,
          }),
        );
        const currentFiles = arrayToMap(
          this.getFileNamesWithRedirectInfo(
            !!includeProjectReferenceRedirectInfo,
          ).concat(externalFiles),
          (info) => info.fileName,
          (info) => info.isSourceOfProjectReferenceRedirect,
        );

        const added: ESMap<string, boolean> = new Map<string, boolean>();
        const removed: ESMap<string, boolean> = new Map<string, boolean>();

        const updated: string[] = updatedFileNames
          ? arrayFrom(updatedFileNames.keys())
          : [];
        const updatedRedirects: protocol.FileWithProjectReferenceRedirectInfo[] = [];

        forEachEntry(
          currentFiles,
          (isSourceOfProjectReferenceRedirect, fileName) => {
            if (!lastReportedFileNames.has(fileName)) {
              added.set(fileName, isSourceOfProjectReferenceRedirect);
            } else if (
              includeProjectReferenceRedirectInfo
              && isSourceOfProjectReferenceRedirect
                !== lastReportedFileNames.get(fileName)
            ) {
              updatedRedirects.push({
                fileName,
                isSourceOfProjectReferenceRedirect,
              });
            }
          },
        );
        forEachEntry(
          lastReportedFileNames,
          (isSourceOfProjectReferenceRedirect, fileName) => {
            if (!currentFiles.has(fileName)) {
              removed.set(fileName, isSourceOfProjectReferenceRedirect);
            }
          },
        );
        this.lastReportedFileNames = currentFiles;
        this.lastReportedVersion = this.projectProgramVersion;
        return {
          info,
          changes: {
            added: includeProjectReferenceRedirectInfoIfRequested(added),
            removed: includeProjectReferenceRedirectInfoIfRequested(removed),
            updated: includeProjectReferenceRedirectInfo
              ? updated.map(
                (
                  fileName,
                ): protocol.FileWithProjectReferenceRedirectInfo => ({
                  fileName,
                  isSourceOfProjectReferenceRedirect: this.isSourceOfProjectReferenceRedirect(fileName),
                }),
              )
              : updated,
            updatedRedirects: includeProjectReferenceRedirectInfo
              ? updatedRedirects
              : undefined,
          },
          projectErrors: this.getGlobalProjectErrors(),
        };
      } else {
        // unknown version - return everything
        const projectFileNames = this.getFileNamesWithRedirectInfo(
          !!includeProjectReferenceRedirectInfo,
        );
        const externalFiles = this.getExternalFiles().map(
          (f): protocol.FileWithProjectReferenceRedirectInfo => ({
            fileName: toNormalizedPath(f),
            isSourceOfProjectReferenceRedirect: false,
          }),
        );
        const allFiles = projectFileNames.concat(externalFiles);
        this.lastReportedFileNames = arrayToMap(
          allFiles,
          (info) => info.fileName,
          (info) => info.isSourceOfProjectReferenceRedirect,
        );
        this.lastReportedVersion = this.projectProgramVersion;
        return {
          info,
          files: includeProjectReferenceRedirectInfo
            ? allFiles
            : allFiles.map((f) => f.fileName),
          projectErrors: this.getGlobalProjectErrors(),
        };
      }
    }

    // remove a root file from project
    protected removeRoot(info: ScriptInfo): void {
      orderedRemoveItem(this.rootFiles, info);
      this.rootFilesMap.delete(info.path);
    }

    /*@internal*/
    isSourceOfProjectReferenceRedirect(fileName: string) {
      return (
        !!this.program
        && this.program.isSourceOfProjectReferenceRedirect(fileName)
      );
    }

    protected enableGlobalPlugins(
      options: CompilerOptions,
      pluginConfigOverrides: Map<any> | undefined,
    ) {
      const host = this.projectService.host;

      if (!host.require) {
        this.projectService.logger.info(
          "Plugins were requested but not running in environment that supports 'require'. Nothing will be loaded",
        );
        return;
      }

      // Search any globally-specified probe paths, then our peer node_modules
      const searchPaths = [
        ...this.projectService.pluginProbeLocations,
        // ../../.. to walk from X/node_modules/typescript/lib/tsserver.js to X/node_modules/
        combinePaths(this.projectService.getExecutingFilePath(), "../../.."),
      ];

      if (this.projectService.globalPlugins) {
        // Enable global plugins with synthetic configuration entries
        for (const globalPluginName of this.projectService.globalPlugins) {
          // Skip empty names from odd commandline parses
          if (!globalPluginName) continue;

          // Skip already-locally-loaded plugins
          if (
            options.plugins
            && options.plugins.some((p) => p.name === globalPluginName)
          ) {
            continue;
          }

          // Provide global: true so plugins can detect why they can't find their config
          this.projectService.logger.info(
            `Loading global plugin ${globalPluginName}`,
          );

          this.enablePlugin(
            { name: globalPluginName, global: true } as PluginImport,
            searchPaths,
            pluginConfigOverrides,
          );
        }
      }
    }

    protected enablePlugin(
      pluginConfigEntry: PluginImport,
      searchPaths: string[],
      pluginConfigOverrides: Map<any> | undefined,
    ) {
      this.projectService.logger.info(
        `Enabling plugin ${pluginConfigEntry.name} from candidate paths: ${searchPaths.join(",")}`,
      );
      if (
        !pluginConfigEntry.name
        || parsePackageName(pluginConfigEntry.name).rest
      ) {
        this.projectService.logger.info(
          `Skipped loading plugin ${
            pluginConfigEntry.name || JSON.stringify(pluginConfigEntry)
          } because only package name is allowed plugin name`,
        );
        return;
      }

      const log = (message: string) => this.projectService.logger.info(message);
      let errorLogs: string[] | undefined;
      const logError = (message: string) => {
        (errorLogs || (errorLogs = [])).push(message);
      };
      const resolvedModule = firstDefined(
        searchPaths,
        (searchPath) =>
          Project.resolveModule(
            pluginConfigEntry.name,
            searchPath,
            this.projectService.host,
            log,
            logError,
          ) as PluginModuleFactory | undefined,
      );
      if (resolvedModule) {
        const configurationOverride = pluginConfigOverrides
          && pluginConfigOverrides.get(pluginConfigEntry.name);
        if (configurationOverride) {
          // Preserve the name property since it's immutable
          const pluginName = pluginConfigEntry.name;
          pluginConfigEntry = configurationOverride;
          pluginConfigEntry.name = pluginName;
        }

        this.enableProxy(resolvedModule, pluginConfigEntry);
      } else {
        forEach(errorLogs, log);
        this.projectService.logger.info(
          `Couldn't find ${pluginConfigEntry.name}`,
        );
      }
    }

    private enableProxy(
      pluginModuleFactory: PluginModuleFactory,
      configEntry: PluginImport,
    ) {
      try {
        if (typeof pluginModuleFactory !== "function") {
          this.projectService.logger.info(
            `Skipped loading plugin ${configEntry.name} because it did not expose a proper factory function`,
          );
          return;
        }

        const info: PluginCreateInfo = {
          config: configEntry,
          project: this,
          languageService: this.languageService,
          languageServiceHost: this,
          serverHost: this.projectService.host,
          session: this.projectService.session,
        };

        const pluginModule = pluginModuleFactory({ typescript: ts });
        const newLS = pluginModule.create(info);
        for (const k of Object.keys(this.languageService)) {
          // eslint-disable-next-line no-in-operator
          if (!(k in newLS)) {
            this.projectService.logger.info(
              `Plugin activation warning: Missing proxied method ${k} in created LS. Patching.`,
            );
            (newLS as any)[k] = (this.languageService as any)[k];
          }
        }
        this.projectService.logger.info(`Plugin validation succeeded`);
        this.languageService = newLS;
        this.plugins.push({ name: configEntry.name, module: pluginModule });
      } catch (e) {
        this.projectService.logger.info(`Plugin activation failed: ${e}`);
      }
    }

    /*@internal*/
    onPluginConfigurationChanged(pluginName: string, configuration: any) {
      this.plugins
        .filter((plugin) => plugin.name === pluginName)
        .forEach((plugin) => {
          if (plugin.module.onConfigurationChanged) {
            plugin.module.onConfigurationChanged(configuration);
          }
        });
    }

    /** Starts a new check for diagnostics. Call this if some file has updated that would cause diagnostics to be changed. */
    refreshDiagnostics() {
      this.projectService.sendProjectsUpdatedInBackgroundEvent();
    }

    /*@internal*/
    getPackageJsonsVisibleToFile(
      fileName: string,
      rootDir?: string,
    ): readonly PackageJsonInfo[] {
      if (this.projectService.serverMode !== LanguageServiceMode.Semantic) {
        return emptyArray;
      }
      return this.projectService.getPackageJsonsVisibleToFile(
        fileName,
        rootDir,
      );
    }

    /*@internal*/
    getNearestAncestorDirectoryWithPackageJson(
      fileName: string,
    ): string | undefined {
      return this.projectService.getNearestAncestorDirectoryWithPackageJson(
        fileName,
      );
    }

    /*@internal*/
    getPackageJsonsForAutoImport(rootDir?: string): readonly PackageJsonInfo[] {
      const packageJsons = this.getPackageJsonsVisibleToFile(
        combinePaths(this.currentDirectory, inferredTypesContainingFile),
        rootDir,
      );
      this.packageJsonsForAutoImport = new Set(
        packageJsons.map((p) => p.fileName),
      );
      return packageJsons;
    }

    /* @internal */
    getPackageJsonCache() {
      return this.projectService.packageJsonCache;
    }

    /*@internal*/
    getCachedExportInfoMap() {
      return (this.exportMapCache ||= createCacheableExportInfoMap(this));
    }

    /*@internal*/
    clearCachedExportInfoMap() {
      this.exportMapCache?.clear();
    }

    /*@internal*/
    getModuleSpecifierCache() {
      return this.moduleSpecifierCache;
    }

    /*@internal*/
    includePackageJsonAutoImports(): PackageJsonAutoImportPreference {
      if (
        this.projectService.includePackageJsonAutoImports()
          === PackageJsonAutoImportPreference.Off
        || !this.languageServiceEnabled
        || isInsideNodeModules(this.currentDirectory)
        || !this.isDefaultProjectForOpenFiles()
      ) {
        return PackageJsonAutoImportPreference.Off;
      }
      return this.projectService.includePackageJsonAutoImports();
    }

    /*@internal*/
    getModuleResolutionHostForAutoImportProvider(): ModuleResolutionHost {
      if (this.program) {
        return {
          fileExists: this.program.fileExists,
          directoryExists: this.program.directoryExists,
          realpath: this.program.realpath
            || this.projectService.host.realpath?.bind(this.projectService.host),
          getCurrentDirectory: this.getCurrentDirectory.bind(this),
          readFile: this.projectService.host.readFile.bind(
            this.projectService.host,
          ),
          getDirectories: this.projectService.host.getDirectories.bind(
            this.projectService.host,
          ),
          trace: this.projectService.host.trace?.bind(this.projectService.host),
          useCaseSensitiveFileNames: this.program.useCaseSensitiveFileNames(),
        };
      }
      return this.projectService.host;
    }

    /*@internal*/
    getPackageJsonAutoImportProvider(): Program | undefined {
      if (this.autoImportProviderHost === false) {
        return undefined;
      }
      if (this.projectService.serverMode !== LanguageServiceMode.Semantic) {
        this.autoImportProviderHost = false;
        return undefined;
      }
      if (this.autoImportProviderHost) {
        updateProjectIfDirty(this.autoImportProviderHost);
        if (this.autoImportProviderHost.isEmpty()) {
          this.autoImportProviderHost.close();
          this.autoImportProviderHost = undefined;
          return undefined;
        }
        return this.autoImportProviderHost.getCurrentProgram();
      }

      const dependencySelection = this.includePackageJsonAutoImports();
      if (dependencySelection) {
        tracing?.push(
          tracing.Phase.Session,
          "getPackageJsonAutoImportProvider",
        );
        const start = timestamp();
        this.autoImportProviderHost = AutoImportProviderProject.create(
          dependencySelection,
          this,
          this.getModuleResolutionHostForAutoImportProvider(),
          this.documentRegistry,
        );
        if (this.autoImportProviderHost) {
          updateProjectIfDirty(this.autoImportProviderHost);
          this.sendPerformanceEvent(
            "CreatePackageJsonAutoImportProvider",
            timestamp() - start,
          );
          tracing?.pop();
          return this.autoImportProviderHost.getCurrentProgram();
        }
        tracing?.pop();
      }
    }

    /*@internal*/
    private isDefaultProjectForOpenFiles(): boolean {
      return !!forEachEntry(
        this.projectService.openFiles,
        (_, fileName) =>
          this.projectService.tryGetDefaultProjectForFile(
            toNormalizedPath(fileName),
          ) === this,
      );
    }

    /*@internal*/
    watchNodeModulesForPackageJsonChanges(directoryPath: string) {
      return this.projectService.watchPackageJsonsInNodeModules(
        this.toPath(directoryPath),
        this,
      );
    }

    /*@internal*/
    getIncompleteCompletionsCache() {
      return this.projectService.getIncompleteCompletionsCache();
    }
  }

  function getUnresolvedImports(
    program: Program,
    cachedUnresolvedImportsPerFile: ESMap<Path, readonly string[]>,
  ): SortedReadonlyArray<string> {
    const sourceFiles = program.getSourceFiles();
    tracing?.push(tracing.Phase.Session, "getUnresolvedImports", {
      count: sourceFiles.length,
    });
    const ambientModules = program
      .getTypeChecker()
      .getAmbientModules()
      .map((mod) => stripQuotes(mod.getName()));
    const result = sortAndDeduplicate(
      flatMap(sourceFiles, (sourceFile) =>
        extractUnresolvedImportsFromSourceFile(
          sourceFile,
          ambientModules,
          cachedUnresolvedImportsPerFile,
        )),
    );
    tracing?.pop();
    return result;
  }
  function extractUnresolvedImportsFromSourceFile(
    file: SourceFile,
    ambientModules: readonly string[],
    cachedUnresolvedImportsPerFile: ESMap<Path, readonly string[]>,
  ): readonly string[] {
    return getOrUpdate(cachedUnresolvedImportsPerFile, file.path, () => {
      if (!file.resolvedModules) return emptyArray;
      let unresolvedImports: string[] | undefined;
      file.resolvedModules.forEach((resolvedModule, name) => {
        // pick unresolved non-relative names
        if (
          (!resolvedModule
            || !resolutionExtensionIsTSOrJson(resolvedModule.extension))
          && !isExternalModuleNameRelative(name)
          && !ambientModules.some((m) => m === name)
        ) {
          unresolvedImports = append(
            unresolvedImports,
            parsePackageName(name).packageName,
          );
        }
      });
      return unresolvedImports || emptyArray;
    });
  }

  /**
   * If a file is opened and no tsconfig (or jsconfig) is found,
   * the file and its imports/references are put into an InferredProject.
   */
  export class InferredProject extends Project {
    private _isJsInferredProject = false;

    toggleJsInferredProject(isJsInferredProject: boolean) {
      if (isJsInferredProject !== this._isJsInferredProject) {
        this._isJsInferredProject = isJsInferredProject;
        this.setCompilerOptions();
      }
    }

    setCompilerOptions(options?: CompilerOptions) {
      // Avoid manipulating the given options directly
      if (!options && !this.getCompilationSettings()) {
        return;
      }
      const newOptions = cloneCompilerOptions(
        options || this.getCompilationSettings(),
      );
      if (
        this._isJsInferredProject
        && typeof newOptions.maxNodeModuleJsDepth !== "number"
      ) {
        newOptions.maxNodeModuleJsDepth = 2;
      } else if (!this._isJsInferredProject) {
        newOptions.maxNodeModuleJsDepth = undefined;
      }
      newOptions.allowJs = true;
      super.setCompilerOptions(newOptions);
    }

    /** this is canonical project root path */
    readonly projectRootPath: string | undefined;

    /*@internal*/
    /** stored only if their is no projectRootPath and this isnt single inferred project */
    readonly canonicalCurrentDirectory: string | undefined;

    /*@internal*/
    constructor(
      projectService: ProjectService,
      documentRegistry: DocumentRegistry,
      compilerOptions: CompilerOptions,
      watchOptions: WatchOptions | undefined,
      projectRootPath: NormalizedPath | undefined,
      currentDirectory: string | undefined,
      pluginConfigOverrides: ESMap<string, any> | undefined,
      typeAcquisition: TypeAcquisition | undefined,
    ) {
      super(
        projectService.newInferredProjectName(),
        ProjectKind.Inferred,
        projectService,
        documentRegistry,
        // TODO: GH#18217
        /*files*/ undefined!,
        /*lastFileExceededProgramSize*/ undefined,
        compilerOptions,
        /*compileOnSaveEnabled*/ false,
        watchOptions,
        projectService.host,
        currentDirectory,
      );
      this.typeAcquisition = typeAcquisition;
      this.projectRootPath = projectRootPath && projectService.toCanonicalFileName(projectRootPath);
      if (!projectRootPath && !projectService.useSingleInferredProject) {
        this.canonicalCurrentDirectory = projectService.toCanonicalFileName(
          this.currentDirectory,
        );
      }
      this.enableGlobalPlugins(
        this.getCompilerOptions(),
        pluginConfigOverrides,
      );
    }

    addRoot(info: ScriptInfo) {
      Debug.assert(info.isScriptOpen());
      this.projectService.startWatchingConfigFilesForInferredProjectRoot(info);
      if (!this._isJsInferredProject && info.isJavaScript()) {
        this.toggleJsInferredProject(/*isJsInferredProject*/ true);
      }
      super.addRoot(info);
    }

    removeRoot(info: ScriptInfo) {
      this.projectService.stopWatchingConfigFilesForInferredProjectRoot(info);
      super.removeRoot(info);
      if (this._isJsInferredProject && info.isJavaScript()) {
        if (
          every(
            this.getRootScriptInfos(),
            (rootInfo) => !rootInfo.isJavaScript(),
          )
        ) {
          this.toggleJsInferredProject(/*isJsInferredProject*/ false);
        }
      }
    }

    /*@internal*/
    isOrphan() {
      return !this.hasRoots();
    }

    isProjectWithSingleRoot() {
      // - when useSingleInferredProject is not set and projectRootPath is not set,
      //   we can guarantee that this will be the only root
      // - other wise it has single root if it has single root script info
      return (
        (!this.projectRootPath
          && !this.projectService.useSingleInferredProject)
        || this.getRootScriptInfos().length === 1
      );
    }

    close() {
      forEach(
        this.getRootScriptInfos(),
        (info) => this.projectService.stopWatchingConfigFilesForInferredProjectRoot(info),
      );
      super.close();
    }

    getTypeAcquisition(): TypeAcquisition {
      return (
        this.typeAcquisition || {
          enable: allRootFilesAreJsOrDts(this),
          include: ts.emptyArray,
          exclude: ts.emptyArray,
        }
      );
    }
  }

  export class AutoImportProviderProject extends Project {
    /*@internal*/
    private static readonly maxDependencies = 10;

    /*@internal*/
    static getRootFileNames(
      dependencySelection: PackageJsonAutoImportPreference,
      hostProject: Project,
      moduleResolutionHost: ModuleResolutionHost,
      compilerOptions: CompilerOptions,
    ): string[] {
      if (!dependencySelection) {
        return ts.emptyArray;
      }

      const program = hostProject.getCurrentProgram();
      if (!program) {
        return ts.emptyArray;
      }

      const start = timestamp();
      let dependencyNames: Set<string> | undefined;
      let rootNames: string[] | undefined;
      const rootFileName = combinePaths(
        hostProject.currentDirectory,
        inferredTypesContainingFile,
      );
      const packageJsons = hostProject.getPackageJsonsForAutoImport(
        combinePaths(hostProject.currentDirectory, rootFileName),
      );
      for (const packageJson of packageJsons) {
        packageJson.dependencies?.forEach((_, dependenyName) => addDependency(dependenyName));
        packageJson.peerDependencies?.forEach((_, dependencyName) => addDependency(dependencyName));
      }

      let dependenciesAdded = 0;
      if (dependencyNames) {
        const symlinkCache = hostProject.getSymlinkCache();
        for (const name of arrayFrom(dependencyNames.keys())) {
          // Avoid creating a large project that would significantly slow down time to editor interactivity
          if (
            dependencySelection === PackageJsonAutoImportPreference.Auto
            && dependenciesAdded > this.maxDependencies
          ) {
            hostProject.log(
              `AutoImportProviderProject: attempted to add more than ${this.maxDependencies} dependencies. Aborting.`,
            );
            return ts.emptyArray;
          }

          // 1. Try to load from the implementation package. For many dependencies, the
          //    package.json will exist, but the package will not contain any typings,
          //    so `entrypoints` will be undefined. In that case, or if the dependency
          //    is missing altogether, we will move on to trying the @types package (2).
          const packageJson = resolvePackageNameToPackageJson(
            name,
            hostProject.currentDirectory,
            compilerOptions,
            moduleResolutionHost,
            program.getModuleResolutionCache(),
          );
          if (packageJson) {
            const entrypoints = getRootNamesFromPackageJson(
              packageJson,
              program,
              symlinkCache,
            );
            if (entrypoints) {
              rootNames = concatenate(rootNames, entrypoints);
              dependenciesAdded += entrypoints.length ? 1 : 0;
              continue;
            }
          }

          // 2. Try to load from the @types package in the tree and in the global
          //    typings cache location, if enabled.
          const done = forEach(
            [
              hostProject.currentDirectory,
              hostProject.getGlobalTypingsCacheLocation(),
            ],
            (directory) => {
              if (directory) {
                const typesPackageJson = resolvePackageNameToPackageJson(
                  `@types/${name}`,
                  directory,
                  compilerOptions,
                  moduleResolutionHost,
                  program.getModuleResolutionCache(),
                );
                if (typesPackageJson) {
                  const entrypoints = getRootNamesFromPackageJson(
                    typesPackageJson,
                    program,
                    symlinkCache,
                  );
                  rootNames = concatenate(rootNames, entrypoints);
                  dependenciesAdded += entrypoints?.length ? 1 : 0;
                  return true;
                }
              }
            },
          );

          if (done) continue;

          // 3. If the @types package did not exist and the user has settings that
          //    allow processing JS from node_modules, go back to the implementation
          //    package and load the JS.
          if (
            packageJson
            && compilerOptions.allowJs
            && compilerOptions.maxNodeModuleJsDepth
          ) {
            const entrypoints = getRootNamesFromPackageJson(
              packageJson,
              program,
              symlinkCache,
              /*allowJs*/ true,
            );
            rootNames = concatenate(rootNames, entrypoints);
            dependenciesAdded += entrypoints?.length ? 1 : 0;
          }
        }
      }

      if (rootNames?.length) {
        hostProject.log(
          `AutoImportProviderProject: found ${rootNames.length} root files in ${dependenciesAdded} dependencies in ${
            timestamp() - start
          } ms`,
        );
      }
      return rootNames || ts.emptyArray;

      function addDependency(dependency: string) {
        if (!startsWith(dependency, "@types/")) {
          (dependencyNames || (dependencyNames = new Set())).add(dependency);
        }
      }

      type PackageJsonInfo = NonNullable<
        ReturnType<typeof resolvePackageNameToPackageJson>
      >;
      function getRootNamesFromPackageJson(
        packageJson: PackageJsonInfo,
        program: Program,
        symlinkCache: SymlinkCache,
        resolveJs?: boolean,
      ) {
        const entrypoints = getEntrypointsFromPackageJsonInfo(
          packageJson,
          compilerOptions,
          moduleResolutionHost,
          program.getModuleResolutionCache(),
          resolveJs,
        );
        if (entrypoints) {
          const real = moduleResolutionHost.realpath?.(
            packageJson.packageDirectory,
          );
          const isSymlink = real && real !== packageJson.packageDirectory;
          if (isSymlink) {
            symlinkCache.setSymlinkedDirectory(packageJson.packageDirectory, {
              real,
              realPath: hostProject.toPath(real),
            });
          }

          return mapDefined(entrypoints, (entrypoint) => {
            const resolvedFileName = isSymlink
              ? entrypoint.replace(packageJson.packageDirectory, real)
              : entrypoint;
            if (
              !program.getSourceFile(resolvedFileName)
              && !(isSymlink && program.getSourceFile(entrypoint))
            ) {
              return resolvedFileName;
            }
          });
        }
      }
    }

    /*@internal*/
    static readonly compilerOptionsOverrides: CompilerOptions = {
      diagnostics: false,
      skipLibCheck: true,
      sourceMap: false,
      types: ts.emptyArray,
      lib: ts.emptyArray,
      noLib: true,
    };

    /*@internal*/
    static create(
      dependencySelection: PackageJsonAutoImportPreference,
      hostProject: Project,
      moduleResolutionHost: ModuleResolutionHost,
      documentRegistry: DocumentRegistry,
    ): AutoImportProviderProject | undefined {
      if (dependencySelection === PackageJsonAutoImportPreference.Off) {
        return undefined;
      }

      const compilerOptions = {
        ...hostProject.getCompilerOptions(),
        ...this.compilerOptionsOverrides,
      };

      const rootNames = this.getRootFileNames(
        dependencySelection,
        hostProject,
        moduleResolutionHost,
        compilerOptions,
      );
      if (!rootNames.length) {
        return undefined;
      }

      return new AutoImportProviderProject(
        hostProject,
        rootNames,
        documentRegistry,
        compilerOptions,
      );
    }

    private rootFileNames: string[] | undefined;

    /*@internal*/
    constructor(
      private hostProject: Project,
      initialRootNames: string[],
      documentRegistry: DocumentRegistry,
      compilerOptions: CompilerOptions,
    ) {
      super(
        hostProject.projectService.newAutoImportProviderProjectName(),
        ProjectKind.AutoImportProvider,
        hostProject.projectService,
        documentRegistry,
        /*hasExplicitListOfFiles*/ false,
        /*lastFileExceededProgramSize*/ undefined,
        compilerOptions,
        /*compileOnSaveEnabled*/ false,
        hostProject.getWatchOptions(),
        hostProject.projectService.host,
        hostProject.currentDirectory,
      );

      this.rootFileNames = initialRootNames;
      this.useSourceOfProjectReferenceRedirect = maybeBind(
        this.hostProject,
        this.hostProject.useSourceOfProjectReferenceRedirect,
      );
      this.getParsedCommandLine = maybeBind(
        this.hostProject,
        this.hostProject.getParsedCommandLine,
      );
    }

    /*@internal*/
    isEmpty() {
      return !some(this.rootFileNames);
    }

    isOrphan() {
      return true;
    }

    updateGraph() {
      let rootFileNames = this.rootFileNames;
      if (!rootFileNames) {
        rootFileNames = AutoImportProviderProject.getRootFileNames(
          this.hostProject.includePackageJsonAutoImports(),
          this.hostProject,
          this.hostProject.getModuleResolutionHostForAutoImportProvider(),
          this.getCompilationSettings(),
        );
      }

      this.projectService.setFileNamesOfAutoImportProviderProject(
        this,
        rootFileNames,
      );
      this.rootFileNames = rootFileNames;
      const oldProgram = this.getCurrentProgram();
      const hasSameSetOfFiles = super.updateGraph();
      if (oldProgram && oldProgram !== this.getCurrentProgram()) {
        this.hostProject.clearCachedExportInfoMap();
      }
      return hasSameSetOfFiles;
    }

    hasRoots() {
      return !!this.rootFileNames?.length;
    }

    markAsDirty() {
      this.rootFileNames = undefined;
      super.markAsDirty();
    }

    getScriptFileNames() {
      return this.rootFileNames || ts.emptyArray;
    }

    getLanguageService(): never {
      throw new Error(
        "AutoImportProviderProject language service should never be used. To get the program, use `project.getCurrentProgram()`.",
      );
    }

    /*@internal*/
    onAutoImportProviderSettingsChanged(): never {
      throw new Error(
        "AutoImportProviderProject is an auto import provider; use `markAsDirty()` instead.",
      );
    }

    /*@internal*/
    onPackageJsonChange(): never {
      throw new Error(
        "package.json changes should be notified on an AutoImportProvider's host project",
      );
    }

    getModuleResolutionHostForAutoImportProvider(): never {
      throw new Error(
        "AutoImportProviderProject cannot provide its own host; use `hostProject.getModuleResolutionHostForAutomImportProvider()` instead.",
      );
    }

    getProjectReferences() {
      return this.hostProject.getProjectReferences();
    }

    /*@internal*/
    includePackageJsonAutoImports() {
      return PackageJsonAutoImportPreference.Off;
    }

    getTypeAcquisition(): TypeAcquisition {
      return { enable: false };
    }

    /*@internal*/
    getSymlinkCache() {
      return this.hostProject.getSymlinkCache();
    }

    /*@internal*/
    getModuleResolutionCache() {
      return this.hostProject.getCurrentProgram()?.getModuleResolutionCache();
    }
  }

  /**
   * If a file is opened, the server will look for a tsconfig (or jsconfig)
   * and if successful create a ConfiguredProject for it.
   * Otherwise it will create an InferredProject.
   */
  export class ConfiguredProject extends Project {
    /* @internal */
    pendingReload: ConfigFileProgramReloadLevel | undefined;
    /* @internal */
    pendingReloadReason: string | undefined;

    /* @internal */
    openFileWatchTriggered = new Map<string, ConfigFileProgramReloadLevel>();

    /*@internal*/
    canConfigFileJsonReportNoInputFiles = false;

    /** Ref count to the project when opened from external project */
    private externalProjectRefCount = 0;

    private projectReferences: readonly ProjectReference[] | undefined;

    /** Potential project references before the project is actually loaded (read config file) */
    /*@internal*/
    potentialProjectReferences: Set<string> | undefined;

    /*@internal*/
    projectOptions?: ProjectOptions | true;

    /*@internal*/
    isInitialLoadPending: () => boolean = returnTrue;

    /*@internal*/
    sendLoadingProjectFinish = false;

    /*@internal*/
    private compilerHost?: CompilerHost;

    /*@internal*/
    constructor(
      configFileName: NormalizedPath,
      readonly canonicalConfigFilePath: NormalizedPath,
      projectService: ProjectService,
      documentRegistry: DocumentRegistry,
      cachedDirectoryStructureHost: CachedDirectoryStructureHost,
    ) {
      super(
        configFileName,
        ProjectKind.Configured,
        projectService,
        documentRegistry,
        /*hasExplicitListOfFiles*/ false,
        /*lastFileExceededProgramSize*/ undefined,
        /*compilerOptions*/ {},
        /*compileOnSaveEnabled*/ false,
        /*watchOptions*/ undefined,
        cachedDirectoryStructureHost,
        getDirectoryPath(configFileName),
      );
    }

    /* @internal */
    setCompilerHost(host: CompilerHost) {
      this.compilerHost = host;
    }

    /* @internal */
    getCompilerHost(): CompilerHost | undefined {
      return this.compilerHost;
    }

    /* @internal */
    useSourceOfProjectReferenceRedirect() {
      return this.languageServiceEnabled;
    }

    /* @internal */
    getParsedCommandLine(fileName: string) {
      const configFileName = asNormalizedPath(normalizePath(fileName));
      const canonicalConfigFilePath = asNormalizedPath(
        this.projectService.toCanonicalFileName(configFileName),
      );
      // Ensure the config file existience info is cached
      let configFileExistenceInfo = this.projectService.configFileExistenceInfoCache.get(
        canonicalConfigFilePath,
      );
      if (!configFileExistenceInfo) {
        this.projectService.configFileExistenceInfoCache.set(
          canonicalConfigFilePath,
          configFileExistenceInfo = {
            exists: this.projectService.host.fileExists(configFileName),
          },
        );
      }
      // Ensure we have upto date parsed command line
      this.projectService.ensureParsedConfigUptoDate(
        configFileName,
        canonicalConfigFilePath,
        configFileExistenceInfo,
        this,
      );
      // Watch wild cards if LS is enabled
      if (
        this.languageServiceEnabled
        && this.projectService.serverMode === LanguageServiceMode.Semantic
      ) {
        this.projectService.watchWildcards(
          configFileName,
          configFileExistenceInfo,
          this,
        );
      }
      return configFileExistenceInfo.exists
        ? configFileExistenceInfo.config!.parsedCommandLine
        : undefined;
    }

    /* @internal */
    onReleaseParsedCommandLine(fileName: string) {
      this.releaseParsedConfig(
        asNormalizedPath(
          this.projectService.toCanonicalFileName(
            asNormalizedPath(normalizePath(fileName)),
          ),
        ),
      );
    }

    /* @internal */
    private releaseParsedConfig(canonicalConfigFilePath: NormalizedPath) {
      this.projectService.stopWatchingWildCards(canonicalConfigFilePath, this);
      this.projectService.releaseParsedConfig(canonicalConfigFilePath, this);
    }

    /**
     * If the project has reload from disk pending, it reloads (and then updates graph as part of that) instead of just updating the graph
     * @returns: true if set of files in the project stays the same and false - otherwise.
     */
    updateGraph(): boolean {
      const isInitialLoad = this.isInitialLoadPending();
      this.isInitialLoadPending = returnFalse;
      const reloadLevel = this.pendingReload;
      this.pendingReload = ConfigFileProgramReloadLevel.None;
      let result: boolean;
      switch (reloadLevel) {
        case ConfigFileProgramReloadLevel.Partial:
          this.openFileWatchTriggered.clear();
          result = this.projectService.reloadFileNamesOfConfiguredProject(this);
          break;
        case ConfigFileProgramReloadLevel.Full:
          this.openFileWatchTriggered.clear();
          const reason = Debug.checkDefined(this.pendingReloadReason);
          this.pendingReloadReason = undefined;
          this.projectService.reloadConfiguredProject(
            this,
            reason,
            isInitialLoad,
            /*clearSemanticCache*/ false,
          );
          result = true;
          break;
        default:
          result = super.updateGraph();
      }
      this.compilerHost = undefined;
      this.projectService.sendProjectLoadingFinishEvent(this);
      this.projectService.sendProjectTelemetry(this);
      return result;
    }

    /*@internal*/
    getCachedDirectoryStructureHost() {
      return this.directoryStructureHost as CachedDirectoryStructureHost;
    }

    getConfigFilePath() {
      return asNormalizedPath(this.getProjectName());
    }

    getProjectReferences(): readonly ProjectReference[] | undefined {
      return this.projectReferences;
    }

    updateReferences(refs: readonly ProjectReference[] | undefined) {
      this.projectReferences = refs;
      this.potentialProjectReferences = undefined;
    }

    /*@internal*/
    setPotentialProjectReference(canonicalConfigPath: NormalizedPath) {
      Debug.assert(this.isInitialLoadPending());
      (
        this.potentialProjectReferences
        || (this.potentialProjectReferences = new Set())
      ).add(canonicalConfigPath);
    }

    /*@internal*/
    getResolvedProjectReferenceToRedirect(
      fileName: string,
    ): ResolvedProjectReference | undefined {
      const program = this.getCurrentProgram();
      return program && program.getResolvedProjectReferenceToRedirect(fileName);
    }

    /*@internal*/
    forEachResolvedProjectReference<T>(
      cb: (resolvedProjectReference: ResolvedProjectReference) => T | undefined,
    ): T | undefined {
      return this.getCurrentProgram()?.forEachResolvedProjectReference(cb);
    }

    /*@internal*/
    enablePluginsWithOptions(
      options: CompilerOptions,
      pluginConfigOverrides: ESMap<string, any> | undefined,
    ) {
      const host = this.projectService.host;

      if (!host.require) {
        this.projectService.logger.info(
          "Plugins were requested but not running in environment that supports 'require'. Nothing will be loaded",
        );
        return;
      }

      // Search our peer node_modules, then any globally-specified probe paths
      // ../../.. to walk from X/node_modules/typescript/lib/tsserver.js to X/node_modules/
      const searchPaths = [
        combinePaths(this.projectService.getExecutingFilePath(), "../../.."),
        ...this.projectService.pluginProbeLocations,
      ];

      if (this.projectService.allowLocalPluginLoads) {
        const local = getDirectoryPath(this.canonicalConfigFilePath);
        this.projectService.logger.info(
          `Local plugin loading enabled; adding ${local} to search paths`,
        );
        searchPaths.unshift(local);
      }

      // Enable tsconfig-specified plugins
      if (options.plugins) {
        for (const pluginConfigEntry of options.plugins) {
          this.enablePlugin(
            pluginConfigEntry,
            searchPaths,
            pluginConfigOverrides,
          );
        }
      }

      this.enableGlobalPlugins(options, pluginConfigOverrides);
    }

    /**
     * Get the errors that dont have any file name associated
     */
    getGlobalProjectErrors(): readonly Diagnostic[] {
      return (
        filter(this.projectErrors, (diagnostic) => !diagnostic.file)
        || emptyArray
      );
    }

    /**
     * Get all the project errors
     */
    getAllProjectErrors(): readonly Diagnostic[] {
      return this.projectErrors || emptyArray;
    }

    setProjectErrors(projectErrors: Diagnostic[]) {
      this.projectErrors = projectErrors;
    }

    close() {
      this.projectService.configFileExistenceInfoCache.forEach(
        (_configFileExistenceInfo, canonicalConfigFilePath) => this.releaseParsedConfig(canonicalConfigFilePath),
      );
      this.projectErrors = undefined;
      this.openFileWatchTriggered.clear();
      this.compilerHost = undefined;
      super.close();
    }

    /* @internal */
    addExternalProjectReference() {
      this.externalProjectRefCount++;
    }

    /* @internal */
    deleteExternalProjectReference() {
      this.externalProjectRefCount--;
    }

    /* @internal */
    isSolution() {
      return (
        this.getRootFilesMap().size === 0
        && !this.canConfigFileJsonReportNoInputFiles
      );
    }

    /* @internal */
    /** Find the configured project from the project references in project which contains the info directly */
    getDefaultChildProjectFromProjectWithReferences(info: ScriptInfo) {
      return forEachResolvedProjectReferenceProject(
        this,
        info.path,
        (child) => projectContainsInfoDirectly(child, info) ? child : undefined,
        ProjectReferenceProjectLoadKind.Find,
      );
    }

    /** Returns true if the project is needed by any of the open script info/external project */
    /* @internal */
    hasOpenRef() {
      if (!!this.externalProjectRefCount) {
        return true;
      }

      // Closed project doesnt have any reference
      if (this.isClosed()) {
        return false;
      }

      const configFileExistenceInfo = this.projectService.configFileExistenceInfoCache.get(
        this.canonicalConfigFilePath,
      )!;
      if (this.projectService.hasPendingProjectUpdate(this)) {
        // If there is pending update for this project,
        // we dont know if this project would be needed by any of the open files impacted by this config file
        // In that case keep the project alive if there are open files impacted by this project
        return !!configFileExistenceInfo.openFilesImpactedByConfigFile?.size;
      }

      // If there is no pending update for this project,
      // We know exact set of open files that get impacted by this configured project as the files in the project
      // The project is referenced only if open files impacted by this project are present in this project
      return (
        (!!configFileExistenceInfo.openFilesImpactedByConfigFile
          && forEachEntry(
            configFileExistenceInfo.openFilesImpactedByConfigFile,
            (_value, infoPath) => {
              const info = this.projectService.getScriptInfoForPath(infoPath)!;
              return (
                this.containsScriptInfo(info)
                || !!forEachResolvedProjectReferenceProject(
                  this,
                  info.path,
                  (child) => child.containsScriptInfo(info),
                  ProjectReferenceProjectLoadKind.Find,
                )
              );
            },
          ))
        || false
      );
    }

    /*@internal*/
    hasExternalProjectRef() {
      return !!this.externalProjectRefCount;
    }

    getEffectiveTypeRoots() {
      return (
        getEffectiveTypeRoots(
          this.getCompilationSettings(),
          this.directoryStructureHost,
        ) || []
      );
    }

    /*@internal*/
    updateErrorOnNoInputFiles(fileNames: string[]) {
      updateErrorForNoInputFiles(
        fileNames,
        this.getConfigFilePath(),
        this.getCompilerOptions().configFile!.configFileSpecs!,
        this.projectErrors!,
        this.canConfigFileJsonReportNoInputFiles,
      );
    }
  }

  /**
   * Project whose configuration is handled externally, such as in a '.csproj'.
   * These are created only if a host explicitly calls `openExternalProject`.
   */
  export class ExternalProject extends Project {
    excludedFiles: readonly NormalizedPath[] = [];
    /*@internal*/
    constructor(
      public externalProjectName: string,
      projectService: ProjectService,
      documentRegistry: DocumentRegistry,
      compilerOptions: CompilerOptions,
      lastFileExceededProgramSize: string | undefined,
      public compileOnSaveEnabled: boolean,
      projectFilePath?: string,
      pluginConfigOverrides?: ESMap<string, any>,
      watchOptions?: WatchOptions,
    ) {
      super(
        externalProjectName,
        ProjectKind.External,
        projectService,
        documentRegistry,
        /*hasExplicitListOfFiles*/ true,
        lastFileExceededProgramSize,
        compilerOptions,
        compileOnSaveEnabled,
        watchOptions,
        projectService.host,
        getDirectoryPath(
          projectFilePath || normalizeSlashes(externalProjectName),
        ),
      );
      this.enableGlobalPlugins(
        this.getCompilerOptions(),
        pluginConfigOverrides,
      );
    }

    updateGraph() {
      const result = super.updateGraph();
      this.projectService.sendProjectTelemetry(this);
      return result;
    }

    getExcludedFiles() {
      return this.excludedFiles;
    }
  }

  /* @internal */
  export function isInferredProject(
    project: Project,
  ): project is InferredProject {
    return project.projectKind === ProjectKind.Inferred;
  }

  /* @internal */
  export function isConfiguredProject(
    project: Project,
  ): project is ConfiguredProject {
    return project.projectKind === ProjectKind.Configured;
  }

  /* @internal */
  export function isExternalProject(
    project: Project,
  ): project is ExternalProject {
    return project.projectKind === ProjectKind.External;
  }
}
