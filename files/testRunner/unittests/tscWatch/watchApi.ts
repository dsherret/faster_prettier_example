namespace ts.tscWatch {
  describe("unittests:: tsc-watch:: watchAPI:: tsc-watch with custom module resolution", () => {
    const configFileJson: any = {
      compilerOptions: { module: "commonjs", resolveJsonModule: true },
      files: ["index.ts"],
    };
    const mainFile: File = {
      path: `${projectRoot}/index.ts`,
      content: "import settings from './settings.json';",
    };
    const config: File = {
      path: `${projectRoot}/tsconfig.json`,
      content: JSON.stringify(configFileJson),
    };
    const settingsJson: File = {
      path: `${projectRoot}/settings.json`,
      content: JSON.stringify({ content: "Print this" }),
    };

    it("verify that module resolution with json extension works when returned without extension", () => {
      const files = [libFile, mainFile, config, settingsJson];
      const host = createWatchedSystem(files, {
        currentDirectory: projectRoot,
      });
      const compilerHost = createWatchCompilerHostOfConfigFile({
        configFileName: config.path,
        system: host,
      });
      const parsedCommandResult = parseJsonConfigFileContent(
        configFileJson,
        host,
        config.path
      );
      compilerHost.resolveModuleNames = (moduleNames, containingFile) =>
        moduleNames.map((m) => {
          const result = resolveModuleName(
            m,
            containingFile,
            parsedCommandResult.options,
            compilerHost
          );
          const resolvedModule = result.resolvedModule!;
          return {
            resolvedFileName: resolvedModule.resolvedFileName,
            isExternalLibraryImport: resolvedModule.isExternalLibraryImport,
            originalFileName: resolvedModule.originalPath,
          };
        });
      const watch = createWatchProgram(compilerHost);
      const program = watch.getCurrentProgram().getProgram();
      checkProgramActualFiles(program, [
        mainFile.path,
        libFile.path,
        settingsJson.path,
      ]);
    });
  });

  describe("unittests:: tsc-watch:: watchAPI:: tsc-watch expose error count to watch status reporter", () => {
    const configFileJson: any = {
      compilerOptions: { module: "commonjs" },
      files: ["index.ts"],
    };
    const config: File = {
      path: `${projectRoot}/tsconfig.json`,
      content: JSON.stringify(configFileJson),
    };
    const mainFile: File = {
      path: `${projectRoot}/index.ts`,
      content: "let compiler = new Compiler(); for (let i = 0; j < 5; i++) {}",
    };

    it("verify that the error count is correctly passed down to the watch status reporter", () => {
      const files = [libFile, mainFile, config];
      const host = createWatchedSystem(files, {
        currentDirectory: projectRoot,
      });
      let watchedErrorCount;
      const reportWatchStatus: WatchStatusReporter = (
        _,
        __,
        ___,
        errorCount
      ) => {
        watchedErrorCount = errorCount;
      };
      const compilerHost = createWatchCompilerHostOfConfigFile({
        configFileName: config.path,
        system: host,
        reportWatchStatus,
      });
      createWatchProgram(compilerHost);
      assert.equal(
        watchedErrorCount,
        2,
        "The error count was expected to be 2 for the file change"
      );
    });
  });

  describe("unittests:: tsc-watch:: watchAPI:: when watchHost does not implement setTimeout or clearTimeout", () => {
    it("verifies that getProgram gets updated program if new file is added to the program", () => {
      const config: File = {
        path: `${projectRoot}/tsconfig.json`,
        content: "{}",
      };
      const mainFile: File = {
        path: `${projectRoot}/main.ts`,
        content: "const x = 10;",
      };
      const sys = createWatchedSystem([config, mainFile, libFile]);
      const watchCompilerHost = createWatchCompilerHost(config.path, {}, sys);
      watchCompilerHost.setTimeout = undefined;
      watchCompilerHost.clearTimeout = undefined;
      const watch = createWatchProgram(watchCompilerHost);
      checkProgramActualFiles(watch.getProgram().getProgram(), [
        mainFile.path,
        libFile.path,
      ]);
      // Write new file
      const barPath = `${projectRoot}/bar.ts`;
      sys.writeFile(barPath, "const y =10;");
      checkProgramActualFiles(watch.getProgram().getProgram(), [
        mainFile.path,
        barPath,
        libFile.path,
      ]);
    });
  });

  describe("unittests:: tsc-watch:: watchAPI:: when watchHost can add extraFileExtensions to process", () => {
    it("verifies that extraFileExtensions are supported to get the program with other extensions", () => {
      const config: File = {
        path: `${projectRoot}/tsconfig.json`,
        content: "{}",
      };
      const mainFile: File = {
        path: `${projectRoot}/main.ts`,
        content: "const x = 10;",
      };
      const otherFile: File = {
        path: `${projectRoot}/other.vue`,
        content: "",
      };
      const sys = createWatchedSystem([config, mainFile, otherFile, libFile]);
      const watchCompilerHost = createWatchCompilerHost(
        config.path,
        { allowNonTsExtensions: true },
        sys,
        /*createProgram*/ undefined,
        /*reportDiagnostics*/ undefined,
        /*reportWatchStatus*/ undefined,
        /*watchOptionsToExtend*/ undefined,
        [
          {
            extension: ".vue",
            isMixedContent: true,
            scriptKind: ScriptKind.Deferred,
          },
        ]
      );
      const watch = createWatchProgram(watchCompilerHost);
      checkProgramActualFiles(watch.getProgram().getProgram(), [
        mainFile.path,
        otherFile.path,
        libFile.path,
      ]);

      const other2 = `${projectRoot}/other2.vue`;
      sys.writeFile(other2, otherFile.content);
      checkSingleTimeoutQueueLengthAndRun(sys);
      checkProgramActualFiles(watch.getProgram().getProgram(), [
        mainFile.path,
        otherFile.path,
        libFile.path,
        other2,
      ]);
    });
  });

  describe("unittests:: tsc-watch:: watchAPI:: when watchHost uses createSemanticDiagnosticsBuilderProgram", () => {
    function getWatch<T extends BuilderProgram>(
      config: File,
      optionsToExtend: CompilerOptions | undefined,
      sys: System,
      createProgram: CreateProgram<T>
    ) {
      const watchCompilerHost = createWatchCompilerHost(
        config.path,
        optionsToExtend,
        sys,
        createProgram
      );
      return createWatchProgram(watchCompilerHost);
    }

    function setup<T extends BuilderProgram>(
      createProgram: CreateProgram<T>,
      configText: string
    ) {
      const config: File = {
        path: `${projectRoot}/tsconfig.json`,
        content: configText,
      };
      const mainFile: File = {
        path: `${projectRoot}/main.ts`,
        content: "export const x = 10;",
      };
      const otherFile: File = {
        path: `${projectRoot}/other.ts`,
        content: "export const y = 10;",
      };
      const sys = createWatchedSystem([config, mainFile, otherFile, libFile]);
      const watch = getWatch(config, { noEmit: true }, sys, createProgram);
      return { sys, watch, mainFile, otherFile, config };
    }

    function verifyOutputs(sys: System, emitSys: System) {
      for (const output of [
        `${projectRoot}/main.js`,
        `${projectRoot}/main.d.ts`,
        `${projectRoot}/other.js`,
        `${projectRoot}/other.d.ts`,
        `${projectRoot}/tsconfig.tsbuildinfo`,
      ]) {
        assert.strictEqual(
          sys.readFile(output),
          emitSys.readFile(output),
          `Output file text for ${output}`
        );
      }
    }

    function verifyBuilder<T extends BuilderProgram, U extends BuilderProgram>(
      config: File,
      sys: System,
      emitSys: System,
      createProgram: CreateProgram<T>,
      createEmitProgram: CreateProgram<U>,
      optionsToExtend?: CompilerOptions
    ) {
      const watch = getWatch(
        config,
        /*optionsToExtend*/ optionsToExtend,
        sys,
        createProgram
      );
      const emitWatch = getWatch(
        config,
        /*optionsToExtend*/ optionsToExtend,
        emitSys,
        createEmitProgram
      );
      verifyOutputs(sys, emitSys);
      watch.close();
      emitWatch.close();
    }

    it("verifies that noEmit is handled on createSemanticDiagnosticsBuilderProgram and typechecking happens only on affected files", () => {
      const { sys, watch, mainFile, otherFile } = setup(
        createSemanticDiagnosticsBuilderProgram,
        "{}"
      );
      checkProgramActualFiles(watch.getProgram().getProgram(), [
        mainFile.path,
        otherFile.path,
        libFile.path,
      ]);
      sys.appendFile(mainFile.path, "\n// SomeComment");
      sys.runQueuedTimeoutCallbacks();
      const program = watch.getProgram().getProgram();
      assert.deepEqual(
        program.getCachedSemanticDiagnostics(
          program.getSourceFile(mainFile.path)
        ),
        []
      );
      // Should not retrieve diagnostics for other file thats not changed
      assert.deepEqual(
        program.getCachedSemanticDiagnostics(
          program.getSourceFile(otherFile.path)
        ),
        /*expected*/ undefined
      );
    });

    it("noEmit with composite writes the tsbuildinfo with pending affected files correctly", () => {
      const configText = JSON.stringify({
        compilerOptions: { composite: true },
      });
      const { sys, watch, config, mainFile } = setup(
        createSemanticDiagnosticsBuilderProgram,
        configText
      );
      const { sys: emitSys, watch: emitWatch } = setup(
        createEmitAndSemanticDiagnosticsBuilderProgram,
        configText
      );
      verifyOutputs(sys, emitSys);

      watch.close();
      emitWatch.close();

      // Emit on both sys should result in same output
      verifyBuilder(
        config,
        sys,
        emitSys,
        createEmitAndSemanticDiagnosticsBuilderProgram,
        createEmitAndSemanticDiagnosticsBuilderProgram
      );

      // Change file
      sys.appendFile(mainFile.path, "\n// SomeComment");
      emitSys.appendFile(mainFile.path, "\n// SomeComment");

      // Verify noEmit results in same output
      verifyBuilder(
        config,
        sys,
        emitSys,
        createSemanticDiagnosticsBuilderProgram,
        createEmitAndSemanticDiagnosticsBuilderProgram,
        { noEmit: true }
      );

      // Emit on both sys should result in same output
      verifyBuilder(
        config,
        sys,
        emitSys,
        createEmitAndSemanticDiagnosticsBuilderProgram,
        createEmitAndSemanticDiagnosticsBuilderProgram
      );

      // Change file
      sys.appendFile(mainFile.path, "\n// SomeComment");
      emitSys.appendFile(mainFile.path, "\n// SomeComment");

      // Emit on both the builders should result in same files
      verifyBuilder(
        config,
        sys,
        emitSys,
        createSemanticDiagnosticsBuilderProgram,
        createEmitAndSemanticDiagnosticsBuilderProgram
      );
    });

    it("noEmitOnError with composite writes the tsbuildinfo with pending affected files correctly", () => {
      const config: File = {
        path: `${projectRoot}/tsconfig.json`,
        content: JSON.stringify({ compilerOptions: { composite: true } }),
      };
      const mainFile: File = {
        path: `${projectRoot}/main.ts`,
        content: "export const x: string = 10;",
      };
      const otherFile: File = {
        path: `${projectRoot}/other.ts`,
        content: "export const y = 10;",
      };
      const sys = createWatchedSystem([config, mainFile, otherFile, libFile]);
      const emitSys = createWatchedSystem([
        config,
        mainFile,
        otherFile,
        libFile,
      ]);

      // Verify noEmit results in same output
      verifyBuilder(
        config,
        sys,
        emitSys,
        createSemanticDiagnosticsBuilderProgram,
        createEmitAndSemanticDiagnosticsBuilderProgram,
        { noEmitOnError: true }
      );

      // Change file
      sys.appendFile(mainFile.path, "\n// SomeComment");
      emitSys.appendFile(mainFile.path, "\n// SomeComment");

      // Verify noEmit results in same output
      verifyBuilder(
        config,
        sys,
        emitSys,
        createSemanticDiagnosticsBuilderProgram,
        createEmitAndSemanticDiagnosticsBuilderProgram,
        { noEmitOnError: true }
      );

      // Fix error
      const fixed = "export const x = 10;";
      sys.appendFile(mainFile.path, fixed);
      emitSys.appendFile(mainFile.path, fixed);

      // Emit on both the builders should result in same files
      verifyBuilder(
        config,
        sys,
        emitSys,
        createSemanticDiagnosticsBuilderProgram,
        createEmitAndSemanticDiagnosticsBuilderProgram,
        { noEmitOnError: true }
      );
    });
  });

  describe("unittests:: tsc-watch:: watchAPI:: when getParsedCommandLine is implemented", () => {
    function setup(useSourceOfProjectReferenceRedirect?: () => boolean) {
      const config1: File = {
        path: `${projectRoot}/projects/project1/tsconfig.json`,
        content: JSON.stringify({
          compilerOptions: {
            module: "none",
            composite: true,
          },
          exclude: ["temp"],
        }),
      };
      const class1: File = {
        path: `${projectRoot}/projects/project1/class1.ts`,
        content: `class class1 {}`,
      };
      const class1Dts: File = {
        path: `${projectRoot}/projects/project1/class1.d.ts`,
        content: `declare class class1 {}`,
      };
      const config2: File = {
        path: `${projectRoot}/projects/project2/tsconfig.json`,
        content: JSON.stringify({
          compilerOptions: {
            module: "none",
            composite: true,
          },
          references: [{ path: "../project1" }],
        }),
      };
      const class2: File = {
        path: `${projectRoot}/projects/project2/class2.ts`,
        content: `class class2 {}`,
      };
      const system = createWatchedSystem([
        config1,
        class1,
        class1Dts,
        config2,
        class2,
        libFile,
      ]);
      const baseline = createBaseline(system);
      const compilerHost = createWatchCompilerHostOfConfigFile({
        configFileName: config2.path,
        system,
        optionsToExtend: { extendedDiagnostics: true },
      });
      compilerHost.useSourceOfProjectReferenceRedirect =
        useSourceOfProjectReferenceRedirect;
      const calledGetParsedCommandLine = new Set<string>();
      compilerHost.getParsedCommandLine = (fileName) => {
        assert.isFalse(
          calledGetParsedCommandLine.has(fileName),
          `Already called on ${fileName}`
        );
        calledGetParsedCommandLine.add(fileName);
        return getParsedCommandLineOfConfigFile(
          fileName,
          /*optionsToExtend*/ undefined,
          {
            useCaseSensitiveFileNames: true,
            fileExists: (path) => system.fileExists(path),
            readFile: (path) => system.readFile(path),
            getCurrentDirectory: () => system.getCurrentDirectory(),
            readDirectory: (path, extensions, excludes, includes, depth) =>
              system.readDirectory(path, extensions, excludes, includes, depth),
            onUnRecoverableConfigFileDiagnostic: noop,
          }
        );
      };
      const watch = createWatchProgram(compilerHost);
      return { watch, baseline, config2, calledGetParsedCommandLine };
    }

    it("when new file is added to the referenced project with host implementing getParsedCommandLine", () => {
      const { watch, baseline, config2, calledGetParsedCommandLine } =
        setup(returnTrue);
      runWatchBaseline({
        scenario: "watchApi",
        subScenario:
          "when new file is added to the referenced project with host implementing getParsedCommandLine",
        commandLineArgs: ["--w", "-p", config2.path, "--extendedDiagnostics"],
        ...baseline,
        getPrograms: () => [
          [watch.getCurrentProgram().getProgram(), watch.getCurrentProgram()],
        ],
        changes: [
          {
            caption: "Add class3 to project1",
            change: (sys) => {
              calledGetParsedCommandLine.clear();
              sys.writeFile(
                `${projectRoot}/projects/project1/class3.ts`,
                `class class3 {}`
              );
            },
            timeouts: checkSingleTimeoutQueueLengthAndRun,
          },
          {
            caption: "Add excluded file to project1",
            change: (sys) =>
              sys.ensureFileOrFolder({
                path: `${projectRoot}/projects/project1/temp/file.d.ts`,
                content: `declare class file {}`,
              }),
            timeouts: (sys) => sys.checkTimeoutQueueLength(0),
          },
          {
            caption: "Add output of class3",
            change: (sys) =>
              sys.writeFile(
                `${projectRoot}/projects/project1/class3.d.ts`,
                `declare class class3 {}`
              ),
            timeouts: (sys) => sys.checkTimeoutQueueLength(0),
          },
        ],
        watchOrSolution: watch,
      });
    });

    it("when new file is added to the referenced project with host implementing getParsedCommandLine without implementing useSourceOfProjectReferenceRedirect", () => {
      const { watch, baseline, config2, calledGetParsedCommandLine } = setup();
      runWatchBaseline({
        scenario: "watchApi",
        subScenario:
          "when new file is added to the referenced project with host implementing getParsedCommandLine without implementing useSourceOfProjectReferenceRedirect",
        commandLineArgs: ["--w", "-p", config2.path, "--extendedDiagnostics"],
        ...baseline,
        getPrograms: () => [
          [watch.getCurrentProgram().getProgram(), watch.getCurrentProgram()],
        ],
        changes: [
          {
            caption: "Add class3 to project1",
            change: (sys) => {
              calledGetParsedCommandLine.clear();
              sys.writeFile(
                `${projectRoot}/projects/project1/class3.ts`,
                `class class3 {}`
              );
            },
            timeouts: checkSingleTimeoutQueueLengthAndRun,
          },
          {
            caption: "Add class3 output to project1",
            change: (sys) =>
              sys.writeFile(
                `${projectRoot}/projects/project1/class3.d.ts`,
                `declare class class3 {}`
              ),
            timeouts: checkSingleTimeoutQueueLengthAndRun,
          },
          {
            caption: "Add excluded file to project1",
            change: (sys) =>
              sys.ensureFileOrFolder({
                path: `${projectRoot}/projects/project1/temp/file.d.ts`,
                content: `declare class file {}`,
              }),
            timeouts: (sys) => sys.checkTimeoutQueueLength(0),
          },
          {
            caption: "Delete output of class3",
            change: (sys) =>
              sys.deleteFile(`${projectRoot}/projects/project1/class3.d.ts`),
            timeouts: checkSingleTimeoutQueueLengthAndRun,
          },
          {
            caption: "Add output of class3",
            change: (sys) =>
              sys.writeFile(
                `${projectRoot}/projects/project1/class3.d.ts`,
                `declare class class3 {}`
              ),
            timeouts: checkSingleTimeoutQueueLengthAndRun,
          },
        ],
        watchOrSolution: watch,
      });
    });
  });
}
