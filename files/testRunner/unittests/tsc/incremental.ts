namespace ts {
  describe("unittests:: tsc:: incremental::", () => {
    verifyTscSerializedIncrementalEdits({
      scenario: "incremental",
      subScenario: "when passing filename for buildinfo on commandline",
      fs: () =>
        loadProjectFromFiles({
          "/src/project/src/main.ts": "export const x = 10;",
          "/src/project/tsconfig.json": Utils.dedent`
                    {
                        "compilerOptions": {
                            "target": "es5",
                            "module": "commonjs",
                        },
                        "include": [
                            "src/**/*.ts"
                        ]
                    }`,
        }),
      commandLineArgs: [
        "--incremental",
        "--p",
        "src/project",
        "--tsBuildInfoFile",
        "src/project/.tsbuildinfo",
        "--explainFiles",
      ],
      incrementalScenarios: noChangeOnlyRuns,
    });

    verifyTscSerializedIncrementalEdits({
      scenario: "incremental",
      subScenario: "when passing rootDir from commandline",
      fs: () =>
        loadProjectFromFiles({
          "/src/project/src/main.ts": "export const x = 10;",
          "/src/project/tsconfig.json": Utils.dedent`
                    {
                        "compilerOptions": {
                            "incremental": true,
                            "outDir": "dist",
                        },
                    }`,
        }),
      commandLineArgs: ["--p", "src/project", "--rootDir", "src/project/src"],
      incrementalScenarios: noChangeOnlyRuns,
    });

    verifyTscSerializedIncrementalEdits({
      scenario: "incremental",
      subScenario: "with only dts files",
      fs: () =>
        loadProjectFromFiles({
          "/src/project/src/main.d.ts": "export const x = 10;",
          "/src/project/src/another.d.ts": "export const y = 10;",
          "/src/project/tsconfig.json": "{}",
        }),
      commandLineArgs: ["--incremental", "--p", "src/project"],
      incrementalScenarios: [
        noChangeRun,
        {
          buildKind: BuildKind.IncrementalDtsUnchanged,
          modifyFs: (fs) =>
            appendText(
              fs,
              "/src/project/src/main.d.ts",
              "export const xy = 100;"
            ),
        },
      ],
    });

    verifyTscSerializedIncrementalEdits({
      scenario: "incremental",
      subScenario: "when passing rootDir is in the tsconfig",
      fs: () =>
        loadProjectFromFiles({
          "/src/project/src/main.ts": "export const x = 10;",
          "/src/project/tsconfig.json": Utils.dedent`
                    {
                        "compilerOptions": {
                            "incremental": true,
                            "outDir": "./built",
                            "rootDir": "./"
                        },
                    }`,
        }),
      commandLineArgs: ["--p", "src/project"],
      incrementalScenarios: noChangeOnlyRuns,
    });

    describe("with noEmitOnError", () => {
      let projFs: vfs.FileSystem;
      before(() => {
        projFs = loadProjectFromDisk("tests/projects/noEmitOnError");
      });
      after(() => {
        projFs = undefined!;
      });

      function verifyNoEmitOnError(
        subScenario: string,
        fixModifyFs: TscIncremental["modifyFs"],
        modifyFs?: TscIncremental["modifyFs"]
      ) {
        verifyTscSerializedIncrementalEdits({
          scenario: "incremental",
          subScenario,
          fs: () => projFs,
          commandLineArgs: ["--incremental", "-p", "src"],
          modifyFs,
          incrementalScenarios: [
            noChangeRun,
            {
              buildKind: BuildKind.IncrementalDtsUnchanged,
              modifyFs: fixModifyFs,
            },
            noChangeRun,
          ],
          baselinePrograms: true,
        });
      }
      verifyNoEmitOnError("with noEmitOnError syntax errors", (fs) =>
        fs.writeFileSync(
          "/src/src/main.ts",
          `import { A } from "../shared/types/db";
const a = {
    lastName: 'sdsd'
};`,
          "utf-8"
        )
      );

      verifyNoEmitOnError(
        "with noEmitOnError semantic errors",
        (fs) =>
          fs.writeFileSync(
            "/src/src/main.ts",
            `import { A } from "../shared/types/db";
const a: string = "hello";`,
            "utf-8"
          ),
        (fs) =>
          fs.writeFileSync(
            "/src/src/main.ts",
            `import { A } from "../shared/types/db";
const a: string = 10;`,
            "utf-8"
          )
      );
    });

    describe("when noEmit changes between compilation", () => {
      verifyNoEmitChanges({ incremental: true });
      verifyNoEmitChanges({ incremental: true, declaration: true });
      verifyNoEmitChanges({ composite: true });

      function verifyNoEmitChanges(compilerOptions: CompilerOptions) {
        const noChangeRunWithNoEmit: TscIncremental = {
          subScenario: "No Change run with noEmit",
          commandLineArgs: ["--p", "src/project", "--noEmit"],
          ...noChangeRun,
        };
        const noChangeRunWithEmit: TscIncremental = {
          subScenario: "No Change run with emit",
          commandLineArgs: ["--p", "src/project"],
          ...noChangeRun,
        };
        let optionsString = "";
        for (const key in compilerOptions) {
          if (hasProperty(compilerOptions, key)) {
            optionsString += ` ${key}`;
          }
        }

        verifyTscSerializedIncrementalEdits({
          scenario: "incremental",
          subScenario: `noEmit changes${optionsString}`,
          commandLineArgs: ["--p", "src/project"],
          fs,
          incrementalScenarios: [
            noChangeRunWithNoEmit,
            noChangeRunWithNoEmit,
            {
              subScenario: "Introduce error but still noEmit",
              commandLineArgs: ["--p", "src/project", "--noEmit"],
              modifyFs: (fs) =>
                replaceText(fs, "/src/project/src/class.ts", "prop", "prop1"),
              buildKind: BuildKind.IncrementalDtsChange,
            },
            {
              subScenario: "Fix error and emit",
              modifyFs: (fs) =>
                replaceText(fs, "/src/project/src/class.ts", "prop1", "prop"),
              buildKind: BuildKind.IncrementalDtsChange,
            },
            noChangeRunWithEmit,
            noChangeRunWithNoEmit,
            noChangeRunWithNoEmit,
            noChangeRunWithEmit,
            {
              subScenario: "Introduce error and emit",
              modifyFs: (fs) =>
                replaceText(fs, "/src/project/src/class.ts", "prop", "prop1"),
              buildKind: BuildKind.IncrementalDtsChange,
            },
            noChangeRunWithEmit,
            noChangeRunWithNoEmit,
            noChangeRunWithNoEmit,
            noChangeRunWithEmit,
            {
              subScenario: "Fix error and no emit",
              commandLineArgs: ["--p", "src/project", "--noEmit"],
              modifyFs: (fs) =>
                replaceText(fs, "/src/project/src/class.ts", "prop1", "prop"),
              buildKind: BuildKind.IncrementalDtsChange,
            },
            noChangeRunWithEmit,
            noChangeRunWithNoEmit,
            noChangeRunWithNoEmit,
            noChangeRunWithEmit,
          ],
        });

        verifyTscSerializedIncrementalEdits({
          scenario: "incremental",
          subScenario: `noEmit changes with initial noEmit${optionsString}`,
          commandLineArgs: ["--p", "src/project", "--noEmit"],
          fs,
          incrementalScenarios: [
            noChangeRunWithEmit,
            {
              subScenario: "Introduce error with emit",
              commandLineArgs: ["--p", "src/project"],
              modifyFs: (fs) =>
                replaceText(fs, "/src/project/src/class.ts", "prop", "prop1"),
              buildKind: BuildKind.IncrementalDtsChange,
            },
            {
              subScenario: "Fix error and no emit",
              modifyFs: (fs) =>
                replaceText(fs, "/src/project/src/class.ts", "prop1", "prop"),
              buildKind: BuildKind.IncrementalDtsChange,
            },
            noChangeRunWithEmit,
          ],
        });

        function fs() {
          return loadProjectFromFiles({
            "/src/project/src/class.ts": Utils.dedent`
                            export class classC {
                                prop = 1;
                            }`,
            "/src/project/src/indirectClass.ts": Utils.dedent`
                            import { classC } from './class';
                            export class indirectClass {
                                classC = new classC();
                            }`,
            "/src/project/src/directUse.ts": Utils.dedent`
                            import { indirectClass } from './indirectClass';
                            new indirectClass().classC.prop;`,
            "/src/project/src/indirectUse.ts": Utils.dedent`
                            import { indirectClass } from './indirectClass';
                            new indirectClass().classC.prop;`,
            "/src/project/src/noChangeFile.ts": Utils.dedent`
                            export function writeLog(s: string) {
                            }`,
            "/src/project/src/noChangeFileWithEmitSpecificError.ts": Utils.dedent`
                            function someFunc(arguments: boolean, ...rest: any[]) {
                            }`,
            "/src/project/tsconfig.json": JSON.stringify({ compilerOptions }),
          });
        }
      }
    });

    verifyTscSerializedIncrementalEdits({
      scenario: "incremental",
      subScenario: `when global file is added, the signatures are updated`,
      fs: () =>
        loadProjectFromFiles({
          "/src/project/src/main.ts": Utils.dedent`
                    /// <reference path="./filePresent.ts"/>
                    /// <reference path="./fileNotFound.ts"/>
                    function main() { }
                `,
          "/src/project/src/anotherFileWithSameReferenes.ts": Utils.dedent`
                    /// <reference path="./filePresent.ts"/>
                    /// <reference path="./fileNotFound.ts"/>
                    function anotherFileWithSameReferenes() { }
                `,
          "/src/project/src/filePresent.ts": `function something() { return 10; }`,
          "/src/project/tsconfig.json": JSON.stringify({
            compilerOptions: { composite: true },
            include: ["src/**/*.ts"],
          }),
        }),
      commandLineArgs: ["--p", "src/project"],
      incrementalScenarios: [
        noChangeRun,
        {
          subScenario: "Modify main file",
          buildKind: BuildKind.IncrementalDtsChange,
          modifyFs: (fs) =>
            appendText(fs, `/src/project/src/main.ts`, `something();`),
        },
        {
          subScenario: "Modify main file again",
          buildKind: BuildKind.IncrementalDtsChange,
          modifyFs: (fs) =>
            appendText(fs, `/src/project/src/main.ts`, `something();`),
        },
        {
          subScenario: "Add new file and update main file",
          buildKind: BuildKind.IncrementalDtsChange,
          modifyFs: (fs) => {
            fs.writeFileSync(
              `/src/project/src/newFile.ts`,
              "function foo() { return 20; }"
            );
            prependText(
              fs,
              `/src/project/src/main.ts`,
              `/// <reference path="./newFile.ts"/>
`
            );
            appendText(fs, `/src/project/src/main.ts`, `foo();`);
          },
        },
        {
          subScenario: "Write file that could not be resolved",
          buildKind: BuildKind.IncrementalDtsChange,
          modifyFs: (fs) =>
            fs.writeFileSync(
              `/src/project/src/fileNotFound.ts`,
              "function something2() { return 20; }"
            ),
        },
        {
          subScenario: "Modify main file",
          buildKind: BuildKind.IncrementalDtsChange,
          modifyFs: (fs) =>
            appendText(fs, `/src/project/src/main.ts`, `something();`),
        },
      ],
      baselinePrograms: true,
    });

    describe("when synthesized imports are added to files", () => {
      function getJsxLibraryContent() {
        return `
export {};
declare global {
    namespace JSX {
        interface Element {}
        interface IntrinsicElements {
            div: {
                propA?: boolean;
            };
        }
    }
}`;
      }

      verifyTsc({
        scenario: "react-jsx-emit-mode",
        subScenario: "with no backing types found doesn't crash",
        fs: () =>
          loadProjectFromFiles({
            "/src/project/node_modules/react/jsx-runtime.js": "export {}", // js needs to be present so there's a resolution result
            "/src/project/node_modules/@types/react/index.d.ts":
              getJsxLibraryContent(), // doesn't contain a jsx-runtime definition
            "/src/project/src/index.tsx": `export const App = () => <div propA={true}></div>;`,
            "/src/project/tsconfig.json": JSON.stringify({
              compilerOptions: {
                module: "commonjs",
                jsx: "react-jsx",
                incremental: true,
                jsxImportSource: "react",
              },
            }),
          }),
        commandLineArgs: ["--p", "src/project"],
      });

      verifyTsc({
        scenario: "react-jsx-emit-mode",
        subScenario: "with no backing types found doesn't crash under --strict",
        fs: () =>
          loadProjectFromFiles({
            "/src/project/node_modules/react/jsx-runtime.js": "export {}", // js needs to be present so there's a resolution result
            "/src/project/node_modules/@types/react/index.d.ts":
              getJsxLibraryContent(), // doesn't contain a jsx-runtime definition
            "/src/project/src/index.tsx": `export const App = () => <div propA={true}></div>;`,
            "/src/project/tsconfig.json": JSON.stringify({
              compilerOptions: {
                module: "commonjs",
                jsx: "react-jsx",
                incremental: true,
                jsxImportSource: "react",
              },
            }),
          }),
        commandLineArgs: ["--p", "src/project", "--strict"],
      });
    });

    verifyTscSerializedIncrementalEdits({
      scenario: "incremental",
      subScenario: "when new file is added to the referenced project",
      commandLineArgs: ["-i", "-p", `src/projects/project2`],
      fs: () =>
        loadProjectFromFiles({
          "/src/projects/project1/tsconfig.json": JSON.stringify({
            compilerOptions: {
              module: "none",
              composite: true,
            },
            exclude: ["temp"],
          }),
          "/src/projects/project1/class1.ts": `class class1 {}`,
          "/src/projects/project1/class1.d.ts": `declare class class1 {}`,
          "/src/projects/project2/tsconfig.json": JSON.stringify({
            compilerOptions: {
              module: "none",
              composite: true,
            },
            references: [{ path: "../project1" }],
          }),
          "/src/projects/project2/class2.ts": `class class2 {}`,
        }),
      incrementalScenarios: [
        {
          subScenario: "Add class3 to project1 and build it",
          buildKind: BuildKind.IncrementalDtsChange,
          modifyFs: (fs) =>
            fs.writeFileSync(
              "/src/projects/project1/class3.ts",
              `class class3 {}`,
              "utf-8"
            ),
          cleanBuildDiscrepancies: () =>
            new Map<string, CleanBuildDescrepancy>([
              // Ts buildinfo will not be updated in incremental build so it will have semantic diagnostics cached from previous build
              // But in clean build because of global diagnostics, semantic diagnostics are not queried so not cached in tsbuildinfo
              [
                "/src/projects/project2/tsconfig.tsbuildinfo",
                CleanBuildDescrepancy.CleanFileTextDifferent,
              ],
            ]),
        },
        {
          subScenario: "Add output of class3",
          buildKind: BuildKind.IncrementalDtsChange,
          modifyFs: (fs) =>
            fs.writeFileSync(
              "/src/projects/project1/class3.d.ts",
              `declare class class3 {}`,
              "utf-8"
            ),
        },
        {
          subScenario: "Add excluded file to project1",
          buildKind: BuildKind.IncrementalDtsUnchanged,
          modifyFs: (fs) => {
            fs.mkdirSync("/src/projects/project1/temp");
            fs.writeFileSync(
              "/src/projects/project1/temp/file.d.ts",
              `declare class file {}`,
              "utf-8"
            );
          },
        },
        {
          subScenario: "Delete output for class3",
          buildKind: BuildKind.IncrementalDtsUnchanged,
          modifyFs: (fs) => fs.unlinkSync("/src/projects/project1/class3.d.ts"),
          cleanBuildDiscrepancies: () =>
            new Map<string, CleanBuildDescrepancy>([
              // Ts buildinfo willbe updated but will retain lib file errors from previous build and not others because they are emitted because of change which results in clearing their semantic diagnostics cache
              // But in clean build because of global diagnostics, semantic diagnostics are not queried so not cached in tsbuildinfo
              [
                "/src/projects/project2/tsconfig.tsbuildinfo",
                CleanBuildDescrepancy.CleanFileTextDifferent,
              ],
            ]),
        },
        {
          subScenario: "Create output for class3",
          buildKind: BuildKind.IncrementalDtsUnchanged,
          modifyFs: (fs) =>
            fs.writeFileSync(
              "/src/projects/project1/class3.d.ts",
              `declare class class3 {}`,
              "utf-8"
            ),
        },
      ],
    });

    verifyTscSerializedIncrementalEdits({
      scenario: "incremental",
      subScenario: "when project has strict true",
      commandLineArgs: ["-noEmit", "-p", `src/project`],
      fs: () =>
        loadProjectFromFiles({
          "/src/project/tsconfig.json": JSON.stringify({
            compilerOptions: {
              incremental: true,
              strict: true,
            },
          }),
          "/src/project/class1.ts": `export class class1 {}`,
        }),
      incrementalScenarios: noChangeOnlyRuns,
      baselinePrograms: true,
    });

    verifyTscSerializedIncrementalEdits({
      scenario: "incremental",
      subScenario: "serializing error chains",
      commandLineArgs: ["-p", `src/project`],
      fs: () =>
        loadProjectFromFiles(
          {
            "/src/project/tsconfig.json": JSON.stringify({
              compilerOptions: {
                incremental: true,
                strict: true,
                jsx: "react",
                module: "esnext",
              },
            }),
            "/src/project/index.tsx": Utils.dedent`
                    declare namespace JSX {
                        interface ElementChildrenAttribute { children: {}; }
                        interface IntrinsicElements { div: {} }
                    }

                    declare var React: any;

                    declare function Component(props: never): any;
                    declare function Component(props: { children?: number }): any;
                    (<Component>
                        <div />
                        <div />
                    </Component>)`,
          },
          `\ninterface ReadonlyArray<T> { readonly length: number }`
        ),
      incrementalScenarios: noChangeOnlyRuns,
    });
  });
}
