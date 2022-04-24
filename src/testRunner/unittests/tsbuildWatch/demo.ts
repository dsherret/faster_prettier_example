namespace ts.tscWatch {
  describe("unittests:: tsbuildWatch:: watchMode:: with demo project", () => {
    const projectLocation = `${TestFSWithWatch.tsbuildProjectsLocation}/demo`;
    let coreFiles: File[];
    let animalFiles: File[];
    let zooFiles: File[];
    let solutionFile: File;
    let baseConfig: File;
    let allFiles: File[];
    before(() => {
      coreFiles = subProjectFiles("core", ["tsconfig.json", "utilities.ts"]);
      animalFiles = subProjectFiles("animals", [
        "tsconfig.json",
        "animal.ts",
        "dog.ts",
        "index.ts",
      ]);
      zooFiles = subProjectFiles("zoo", ["tsconfig.json", "zoo.ts"]);
      solutionFile = projectFile("tsconfig.json");
      baseConfig = projectFile("tsconfig-base.json");
      allFiles = [
        ...coreFiles,
        ...animalFiles,
        ...zooFiles,
        solutionFile,
        baseConfig,
        { path: libFile.path, content: libContent },
      ];
    });

    after(() => {
      coreFiles = undefined!;
      animalFiles = undefined!;
      zooFiles = undefined!;
      solutionFile = undefined!;
      baseConfig = undefined!;
      allFiles = undefined!;
    });

    verifyTscWatch({
      scenario: "demo",
      subScenario: "updates with circular reference",
      commandLineArgs: ["-b", "-w", "-verbose"],
      sys: () => {
        const sys = createWatchedSystem(allFiles, {
          currentDirectory: projectLocation,
        });
        sys.writeFile(
          coreFiles[0].path,
          coreFiles[0].content.replace(
            "}",
            `},
  "references": [
    {
      "path": "../zoo"
    }
  ]`,
          ),
        );
        return sys;
      },
      changes: [
        {
          caption: "Fix error",
          change: (sys) => sys.writeFile(coreFiles[0].path, coreFiles[0].content),
          timeouts: (sys) => {
            sys.checkTimeoutQueueLengthAndRun(1); // build core
            sys.checkTimeoutQueueLengthAndRun(1); // build animals
            sys.checkTimeoutQueueLengthAndRun(1); // build zoo
            sys.checkTimeoutQueueLengthAndRun(1); // build solution
            sys.checkTimeoutQueueLength(0);
          },
        },
      ],
    });

    verifyTscWatch({
      scenario: "demo",
      subScenario: "updates with bad reference",
      commandLineArgs: ["-b", "-w", "-verbose"],
      sys: () => {
        const sys = createWatchedSystem(allFiles, {
          currentDirectory: projectLocation,
        });
        sys.writeFile(
          coreFiles[1].path,
          `import * as A from '../animals';
${coreFiles[1].content}`,
        );
        return sys;
      },
      changes: [
        {
          caption: "Prepend a line",
          change: (sys) =>
            sys.writeFile(
              coreFiles[1].path,
              `
import * as A from '../animals';
${coreFiles[1].content}`,
            ),
          // build core
          timeouts: checkSingleTimeoutQueueLengthAndRunAndVerifyNoTimeout,
        },
      ],
    });

    function subProjectFiles(
      subProject: string,
      fileNames: readonly string[],
    ): File[] {
      return fileNames.map((file) => projectFile(`${subProject}/${file}`));
    }

    function projectFile(fileName: string): File {
      return TestFSWithWatch.getTsBuildProjectFile("demo", fileName);
    }
  });
}
