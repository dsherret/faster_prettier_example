namespace ts.projectSystem {
  export function createHostWithSolutionBuild(
    files: readonly TestFSWithWatch.FileOrFolderOrSymLink[],
    rootNames: readonly string[],
  ) {
    const host = createServerHost(files);
    // ts build should succeed
    tscWatch.ensureErrorFreeBuild(host, rootNames);
    return host;
  }

  describe("unittests:: tsserver:: with project references and tsbuild", () => {
    describe("with container project", () => {
      function getProjectFiles(project: string): [File, File] {
        return [
          TestFSWithWatch.getTsBuildProjectFile(project, "tsconfig.json"),
          TestFSWithWatch.getTsBuildProjectFile(project, "index.ts"),
        ];
      }

      const project = "container";
      const containerLib = getProjectFiles("container/lib");
      const containerExec = getProjectFiles("container/exec");
      const containerCompositeExec = getProjectFiles("container/compositeExec");
      const containerConfig = TestFSWithWatch.getTsBuildProjectFile(
        project,
        "tsconfig.json",
      );
      const files = [
        libFile,
        ...containerLib,
        ...containerExec,
        ...containerCompositeExec,
        containerConfig,
      ];

      it("does not error on container only project", () => {
        const host = createHostWithSolutionBuild(files, [containerConfig.path]);

        // Open external project for the folder
        const session = createSession(host, {
          logger: createLoggerWithInMemoryLogs(),
        });
        const service = session.getProjectService();
        service.openExternalProjects([
          {
            projectFileName: TestFSWithWatch.getTsBuildProjectFilePath(
              project,
              project,
            ),
            rootFiles: files.map((f) => ({ fileName: f.path })),
            options: {},
          },
        ]);
        files.forEach((f) => {
          const args: protocol.FileRequestArgs = {
            file: f.path,
            projectFileName: endsWith(f.path, "tsconfig.json")
              ? f.path
              : undefined,
          };
          session.executeCommandSeq<protocol.SyntacticDiagnosticsSyncRequest>({
            command: protocol.CommandTypes.SyntacticDiagnosticsSync,
            arguments: args,
          });
          session.executeCommandSeq<protocol.SemanticDiagnosticsSyncRequest>({
            command: protocol.CommandTypes.SemanticDiagnosticsSync,
            arguments: args,
          });
        });
        const containerProject = service.configuredProjects.get(
          containerConfig.path,
        )!;
        session.executeCommandSeq<protocol.CompilerOptionsDiagnosticsRequest>({
          command: protocol.CommandTypes.CompilerOptionsDiagnosticsFull,
          arguments: { projectFileName: containerProject.projectName },
        });
        baselineTsserverLogs(
          "projectReferences",
          `does not error on container only project`,
          session,
        );
      });

      it("can successfully find references with --out options", () => {
        const host = createHostWithSolutionBuild(files, [containerConfig.path]);
        const session = createSession(host, {
          logger: createLoggerWithInMemoryLogs(),
        });
        openFilesForSession([containerCompositeExec[1]], session);
        const myConstStart = protocolLocationFromSubstring(
          containerCompositeExec[1].content,
          "myConst",
        );
        session.executeCommandSeq<protocol.RenameRequest>({
          command: protocol.CommandTypes.Rename,
          arguments: { file: containerCompositeExec[1].path, ...myConstStart },
        });

        baselineTsserverLogs(
          "projectReferences",
          `can successfully find references with out option`,
          session,
        );
      });

      it("ancestor and project ref management", () => {
        const tempFile: File = {
          path: `/user/username/projects/temp/temp.ts`,
          content: "let x = 10",
        };
        const host = createHostWithSolutionBuild(files.concat([tempFile]), [
          containerConfig.path,
        ]);
        const session = createSession(host, {
          logger: createLoggerWithInMemoryLogs(),
        });
        openFilesForSession([containerCompositeExec[1]], session);
        const service = session.getProjectService();

        // Open temp file and verify all projects alive
        openFilesForSession([tempFile], session);

        // Ref projects are loaded after as part of this command
        const locationOfMyConst = protocolLocationFromSubstring(
          containerCompositeExec[1].content,
          "myConst",
        );
        session.executeCommandSeq<protocol.RenameRequest>({
          command: protocol.CommandTypes.Rename,
          arguments: {
            file: containerCompositeExec[1].path,
            ...locationOfMyConst,
          },
        });

        // Open temp file and verify all projects alive
        service.closeClientFile(tempFile.path);
        openFilesForSession([tempFile], session);

        // Close all files and open temp file, only inferred project should be alive
        service.closeClientFile(containerCompositeExec[1].path);
        service.closeClientFile(tempFile.path);
        openFilesForSession([tempFile], session);
        baselineTsserverLogs(
          "projectReferences",
          `ancestor and project ref management`,
          session,
        );
      });
    });

    describe("when root file is file from referenced project", () => {
      function verify(disableSourceOfProjectReferenceRedirect: boolean) {
        const projectLocation = `/user/username/projects/project`;
        const commonConfig: File = {
          path: `${projectLocation}/src/common/tsconfig.json`,
          content: JSON.stringify({
            compilerOptions: {
              composite: true,
              declarationMap: true,
              outDir: "../../out",
              baseUrl: "..",
              disableSourceOfProjectReferenceRedirect,
            },
            include: ["./**/*"],
          }),
        };
        const keyboardTs: File = {
          path: `${projectLocation}/src/common/input/keyboard.ts`,
          content: `function bar() { return "just a random function so .d.ts location doesnt match"; }
export function evaluateKeyboardEvent() { }`,
        };
        const keyboardTestTs: File = {
          path: `${projectLocation}/src/common/input/keyboard.test.ts`,
          content: `import { evaluateKeyboardEvent } from 'common/input/keyboard';
function testEvaluateKeyboardEvent() {
    return evaluateKeyboardEvent();
}
`,
        };
        const srcConfig: File = {
          path: `${projectLocation}/src/tsconfig.json`,
          content: JSON.stringify({
            compilerOptions: {
              composite: true,
              declarationMap: true,
              outDir: "../out",
              baseUrl: ".",
              paths: {
                "common/*": ["./common/*"],
              },
              tsBuildInfoFile: "../out/src.tsconfig.tsbuildinfo",
              disableSourceOfProjectReferenceRedirect,
            },
            include: ["./**/*"],
            references: [{ path: "./common" }],
          }),
        };
        const terminalTs: File = {
          path: `${projectLocation}/src/terminal.ts`,
          content: `import { evaluateKeyboardEvent } from 'common/input/keyboard';
function foo() {
    return evaluateKeyboardEvent();
}
`,
        };
        const host = createHostWithSolutionBuild(
          [
            commonConfig,
            keyboardTs,
            keyboardTestTs,
            srcConfig,
            terminalTs,
            libFile,
          ],
          [srcConfig.path],
        );
        const session = createSession(host, {
          logger: createLoggerWithInMemoryLogs(),
        });
        openFilesForSession([keyboardTs, terminalTs], session);

        const searchStr = "evaluateKeyboardEvent";
        const importStr = `import { evaluateKeyboardEvent } from 'common/input/keyboard';`;
        const result = session.executeCommandSeq<protocol.ReferencesRequest>({
          command: protocol.CommandTypes.References,
          arguments: protocolFileLocationFromSubstring(keyboardTs, searchStr),
        }).response as protocol.ReferencesResponseBody;
        assert.deepEqual(result, {
          refs: [
            makeReferenceItem({
              file: keyboardTs,
              text: searchStr,
              contextText: `export function evaluateKeyboardEvent() { }`,
              isDefinition: true,
              lineText: `export function evaluateKeyboardEvent() { }`,
            }),
            makeReferenceItem({
              file: keyboardTestTs,
              text: searchStr,
              contextText: importStr,
              isDefinition: false,
              isWriteAccess: true,
              lineText: importStr,
            }),
            makeReferenceItem({
              file: keyboardTestTs,
              text: searchStr,
              options: { index: 1 },
              isDefinition: false,
              lineText: `    return evaluateKeyboardEvent();`,
            }),
            makeReferenceItem({
              file: terminalTs,
              text: searchStr,
              contextText: importStr,
              isDefinition: false,
              isWriteAccess: true,
              lineText: importStr,
            }),
            makeReferenceItem({
              file: terminalTs,
              text: searchStr,
              options: { index: 1 },
              isDefinition: false,
              lineText: `    return evaluateKeyboardEvent();`,
            }),
          ],
          symbolName: searchStr,
          symbolStartOffset: protocolLocationFromSubstring(
            keyboardTs.content,
            searchStr,
          ).offset,
          symbolDisplayString: "function evaluateKeyboardEvent(): void",
        });
        baselineTsserverLogs(
          "projectReferences",
          `root file is file from referenced project${
            disableSourceOfProjectReferenceRedirect
              ? " and using declaration maps"
              : ""
          }`,
          session,
        );
      }

      it(`when using declaration file maps to navigate between projects`, () => {
        verify(/*disableSourceOfProjectReferenceRedirect*/ true);
      });
      it(`when using original source files in the project`, () => {
        verify(/*disableSourceOfProjectReferenceRedirect*/ false);
      });
    });

    it("reusing d.ts files from composite and non composite projects", () => {
      const configA: File = {
        path: `${tscWatch.projectRoot}/compositea/tsconfig.json`,
        content: JSON.stringify({
          compilerOptions: {
            composite: true,
            outDir: "../dist/",
            rootDir: "../",
            baseUrl: "../",
            paths: { "@ref/*": ["./dist/*"] },
          },
        }),
      };
      const aTs: File = {
        path: `${tscWatch.projectRoot}/compositea/a.ts`,
        content: `import { b } from "@ref/compositeb/b";`,
      };
      const a2Ts: File = {
        path: `${tscWatch.projectRoot}/compositea/a2.ts`,
        content: `export const x = 10;`,
      };
      const configB: File = {
        path: `${tscWatch.projectRoot}/compositeb/tsconfig.json`,
        content: configA.content,
      };
      const bTs: File = {
        path: `${tscWatch.projectRoot}/compositeb/b.ts`,
        content: "export function b() {}",
      };
      const bDts: File = {
        path: `${tscWatch.projectRoot}/dist/compositeb/b.d.ts`,
        content: "export declare function b(): void;",
      };
      const configC: File = {
        path: `${tscWatch.projectRoot}/compositec/tsconfig.json`,
        content: JSON.stringify({
          compilerOptions: {
            composite: true,
            outDir: "../dist/",
            rootDir: "../",
            baseUrl: "../",
            paths: { "@ref/*": ["./*"] },
          },
          references: [{ path: "../compositeb" }],
        }),
      };
      const cTs: File = {
        path: `${tscWatch.projectRoot}/compositec/c.ts`,
        content: aTs.content,
      };
      const files = [
        libFile,
        aTs,
        a2Ts,
        configA,
        bDts,
        bTs,
        configB,
        cTs,
        configC,
      ];
      const host = createServerHost(files);
      const service = createProjectService(host);
      service.openClientFile(aTs.path);
      service.checkNumberOfProjects({ configuredProjects: 1 });

      // project A referencing b.d.ts without project reference
      const projectA = service.configuredProjects.get(configA.path)!;
      assert.isDefined(projectA);
      checkProjectActualFiles(projectA, [
        aTs.path,
        a2Ts.path,
        bDts.path,
        libFile.path,
        configA.path,
      ]);

      // reuses b.d.ts but sets the path and resolved path since projectC has project references
      // as the real resolution was to b.ts
      service.openClientFile(cTs.path);
      service.checkNumberOfProjects({ configuredProjects: 2 });
      const projectC = service.configuredProjects.get(configC.path)!;
      checkProjectActualFiles(projectC, [
        cTs.path,
        bTs.path,
        libFile.path,
        configC.path,
      ]);

      // Now new project for project A tries to reuse b but there is no filesByName mapping for b's source location
      host.writeFile(a2Ts.path, `${a2Ts.content}export const y = 30;`);
      assert.isTrue(projectA.dirty);
      projectA.updateGraph();
    });

    describe("when references are monorepo like with symlinks", () => {
      interface Packages {
        bPackageJson: File;
        aTest: File;
        bFoo: File;
        bBar: File;
        bSymlink: SymLink;
      }
      function verifySymlinkScenario(
        scenario: string,
        packages: () => Packages,
      ) {
        describe(`${scenario}: when solution is not built`, () => {
          it("with preserveSymlinks turned off", () => {
            verifySession(scenario, packages(), /*alreadyBuilt*/ false, {});
          });

          it("with preserveSymlinks turned on", () => {
            verifySession(scenario, packages(), /*alreadyBuilt*/ false, {
              preserveSymlinks: true,
            });
          });
        });

        describe(`${scenario}: when solution is already built`, () => {
          it("with preserveSymlinks turned off", () => {
            verifySession(scenario, packages(), /*alreadyBuilt*/ true, {});
          });

          it("with preserveSymlinks turned on", () => {
            verifySession(scenario, packages(), /*alreadyBuilt*/ true, {
              preserveSymlinks: true,
            });
          });
        });
      }

      function verifySession(
        scenario: string,
        { bPackageJson, aTest, bFoo, bBar, bSymlink }: Packages,
        alreadyBuilt: boolean,
        extraOptions: CompilerOptions,
      ) {
        const aConfig = config("A", extraOptions, ["../B"]);
        const bConfig = config("B", extraOptions);
        const files = [
          libFile,
          bPackageJson,
          aConfig,
          bConfig,
          aTest,
          bFoo,
          bBar,
          bSymlink,
        ];
        const host = alreadyBuilt
          ? createHostWithSolutionBuild(files, [aConfig.path])
          : createServerHost(files);

        // Create symlink in node module
        const session = createSession(host, {
          canUseEvents: true,
          logger: createLoggerWithInMemoryLogs(),
        });
        openFilesForSession([aTest], session);
        verifyGetErrRequest({ session, host, files: [aTest] });
        session.executeCommandSeq<protocol.UpdateOpenRequest>({
          command: protocol.CommandTypes.UpdateOpen,
          arguments: {
            changedFiles: [
              {
                fileName: aTest.path,
                textChanges: [
                  {
                    newText: "\n",
                    start: { line: 5, offset: 1 },
                    end: { line: 5, offset: 1 },
                  },
                ],
              },
            ],
          },
        });
        verifyGetErrRequest({ session, host, files: [aTest] });
        baselineTsserverLogs(
          "projectReferences",
          `monorepo like with symlinks ${scenario} and solution is ${alreadyBuilt ? "built" : "not built"}${
            extraOptions.preserveSymlinks ? " with preserveSymlinks" : ""
          }`,
          session,
        );
      }

      function config(
        packageName: string,
        extraOptions: CompilerOptions,
        references?: string[],
      ): File {
        return {
          path: `${tscWatch.projectRoot}/packages/${packageName}/tsconfig.json`,
          content: JSON.stringify({
            compilerOptions: {
              outDir: "lib",
              rootDir: "src",
              composite: true,
              ...extraOptions,
            },
            include: ["src"],
            ...(references
              ? { references: references.map((path) => ({ path })) }
              : {}),
          }),
        };
      }

      function file(
        packageName: string,
        fileName: string,
        content: string,
      ): File {
        return {
          path: `${tscWatch.projectRoot}/packages/${packageName}/src/${fileName}`,
          content,
        };
      }

      function verifyMonoRepoLike(scope = "") {
        verifySymlinkScenario(
          `when packageJson has types field and has index.ts${scope ? " with scoped package" : ""}`,
          () => ({
            bPackageJson: {
              path: `${tscWatch.projectRoot}/packages/B/package.json`,
              content: JSON.stringify({
                main: "lib/index.js",
                types: "lib/index.d.ts",
              }),
            },
            aTest: file(
              "A",
              "index.ts",
              `import { foo } from '${scope}b';
import { bar } from '${scope}b/lib/bar';
foo();
bar();
`,
            ),
            bFoo: file("B", "index.ts", `export function foo() { }`),
            bBar: file("B", "bar.ts", `export function bar() { }`),
            bSymlink: {
              path: `${tscWatch.projectRoot}/node_modules/${scope}b`,
              symLink: `${tscWatch.projectRoot}/packages/B`,
            },
          }),
        );

        verifySymlinkScenario(
          `when referencing file from subFolder${scope ? " with scoped package" : ""}`,
          () => ({
            bPackageJson: {
              path: `${tscWatch.projectRoot}/packages/B/package.json`,
              content: "{}",
            },
            aTest: file(
              "A",
              "test.ts",
              `import { foo } from '${scope}b/lib/foo';
import { bar } from '${scope}b/lib/bar/foo';
foo();
bar();
`,
            ),
            bFoo: file("B", "foo.ts", `export function foo() { }`),
            bBar: file("B", "bar/foo.ts", `export function bar() { }`),
            bSymlink: {
              path: `${tscWatch.projectRoot}/node_modules/${scope}b`,
              symLink: `${tscWatch.projectRoot}/packages/B`,
            },
          }),
        );
      }

      describe("when package is not scoped", () => {
        verifyMonoRepoLike();
      });
      describe("when package is scoped", () => {
        verifyMonoRepoLike("@issue/");
      });
    });

    it("when the referenced projects have allowJs and emitDeclarationOnly", () => {
      const compositeConfig: File = {
        path: `${tscWatch.projectRoot}/packages/emit-composite/tsconfig.json`,
        content: JSON.stringify({
          compilerOptions: {
            composite: true,
            allowJs: true,
            emitDeclarationOnly: true,
            outDir: "lib",
            rootDir: "src",
          },
          include: ["src"],
        }),
      };
      const compositePackageJson: File = {
        path: `${tscWatch.projectRoot}/packages/emit-composite/package.json`,
        content: JSON.stringify({
          name: "emit-composite",
          version: "1.0.0",
          main: "src/index.js",
          typings: "lib/index.d.ts",
        }),
      };
      const compositeIndex: File = {
        path: `${tscWatch.projectRoot}/packages/emit-composite/src/index.js`,
        content: `const testModule = require('./testModule');
module.exports = {
    ...testModule
}`,
      };
      const compositeTestModule: File = {
        path: `${tscWatch.projectRoot}/packages/emit-composite/src/testModule.js`,
        content: `/**
 * @param {string} arg
 */
 const testCompositeFunction = (arg) => {
}
module.exports = {
    testCompositeFunction
}`,
      };
      const consumerConfig: File = {
        path: `${tscWatch.projectRoot}/packages/consumer/tsconfig.json`,
        content: JSON.stringify({
          include: ["src"],
          references: [{ path: "../emit-composite" }],
        }),
      };
      const consumerIndex: File = {
        path: `${tscWatch.projectRoot}/packages/consumer/src/index.ts`,
        content: `import { testCompositeFunction } from 'emit-composite';
testCompositeFunction('why hello there');
testCompositeFunction('why hello there', 42);`,
      };
      const symlink: SymLink = {
        path: `${tscWatch.projectRoot}/node_modules/emit-composite`,
        symLink: `${tscWatch.projectRoot}/packages/emit-composite`,
      };
      const host = createServerHost(
        [
          libFile,
          compositeConfig,
          compositePackageJson,
          compositeIndex,
          compositeTestModule,
          consumerConfig,
          consumerIndex,
          symlink,
        ],
        { useCaseSensitiveFileNames: true },
      );
      const session = createSession(host, {
        canUseEvents: true,
        logger: createLoggerWithInMemoryLogs(),
      });
      openFilesForSession([consumerIndex], session);
      verifyGetErrRequest({ host, session, files: [consumerIndex] });
      baselineTsserverLogs(
        "projectReferences",
        `when the referenced projects have allowJs and emitDeclarationOnly`,
        session,
      );
    });

    it("when finding local reference doesnt load ancestor/sibling projects", () => {
      const solutionLocation = "/user/username/projects/solution";
      const solution: File = {
        path: `${solutionLocation}/tsconfig.json`,
        content: JSON.stringify({
          files: [],
          include: [],
          references: [{ path: "./compiler" }, { path: "./services" }],
        }),
      };
      const compilerConfig: File = {
        path: `${solutionLocation}/compiler/tsconfig.json`,
        content: JSON.stringify({
          compilerOptions: {
            composite: true,
            module: "none",
          },
          files: ["./types.ts", "./program.ts"],
        }),
      };
      const typesFile: File = {
        path: `${solutionLocation}/compiler/types.ts`,
        content: `
                namespace ts {
                    export interface Program {
                        getSourceFiles(): string[];
                    }
                }`,
      };
      const programFile: File = {
        path: `${solutionLocation}/compiler/program.ts`,
        content: `
                namespace ts {
                    export const program: Program = {
                        getSourceFiles: () => [getSourceFile()]
                    };
                    function getSourceFile() { return "something"; }
                }`,
      };
      const servicesConfig: File = {
        path: `${solutionLocation}/services/tsconfig.json`,
        content: JSON.stringify({
          compilerOptions: {
            composite: true,
          },
          files: ["./services.ts"],
          references: [{ path: "../compiler" }],
        }),
      };
      const servicesFile: File = {
        path: `${solutionLocation}/services/services.ts`,
        content: `
                namespace ts {
                    const result = program.getSourceFiles();
                }`,
      };

      const files = [
        libFile,
        solution,
        compilerConfig,
        typesFile,
        programFile,
        servicesConfig,
        servicesFile,
        libFile,
      ];
      const host = createServerHost(files);
      const session = createSession(host, {
        logger: createLoggerWithInMemoryLogs(),
      });
      openFilesForSession([programFile], session);

      // Find all references for getSourceFile
      // Shouldnt load more projects
      session.executeCommandSeq<protocol.ReferencesRequest>({
        command: protocol.CommandTypes.References,
        arguments: protocolFileLocationFromSubstring(
          programFile,
          "getSourceFile",
          { index: 1 },
        ),
      });

      // Find all references for getSourceFiles
      // Should load more projects
      session.executeCommandSeq<protocol.ReferencesRequest>({
        command: protocol.CommandTypes.References,
        arguments: protocolFileLocationFromSubstring(
          programFile,
          "getSourceFiles",
        ),
      });
      baselineTsserverLogs(
        "projectReferences",
        `finding local reference doesnt load ancestor/sibling projects`,
        session,
      );
    });

    describe("special handling of localness of the definitions for findAllRefs", () => {
      function verify(
        scenario: string,
        definition: string,
        usage: string,
        referenceTerm: string,
      ) {
        it(scenario, () => {
          const solutionLocation = "/user/username/projects/solution";
          const solution: File = {
            path: `${solutionLocation}/tsconfig.json`,
            content: JSON.stringify({
              files: [],
              references: [{ path: "./api" }, { path: "./app" }],
            }),
          };
          const apiConfig: File = {
            path: `${solutionLocation}/api/tsconfig.json`,
            content: JSON.stringify({
              compilerOptions: {
                composite: true,
                outDir: "dist",
                rootDir: "src",
              },
              include: ["src"],
              references: [{ path: "../shared" }],
            }),
          };
          const apiFile: File = {
            path: `${solutionLocation}/api/src/server.ts`,
            content: `import * as shared from "../../shared/dist";
${usage}`,
          };
          const appConfig: File = {
            path: `${solutionLocation}/app/tsconfig.json`,
            content: apiConfig.content,
          };
          const appFile: File = {
            path: `${solutionLocation}/app/src/app.ts`,
            content: apiFile.content,
          };
          const sharedConfig: File = {
            path: `${solutionLocation}/shared/tsconfig.json`,
            content: JSON.stringify({
              compilerOptions: {
                composite: true,
                outDir: "dist",
                rootDir: "src",
              },
              include: ["src"],
            }),
          };
          const sharedFile: File = {
            path: `${solutionLocation}/shared/src/index.ts`,
            content: definition,
          };
          const host = createServerHost([
            libFile,
            solution,
            libFile,
            apiConfig,
            apiFile,
            appConfig,
            appFile,
            sharedConfig,
            sharedFile,
          ]);
          const session = createSession(host, {
            logger: createLoggerWithInMemoryLogs(),
          });
          openFilesForSession([apiFile], session);

          // Find all references
          session.executeCommandSeq<protocol.ReferencesRequest>({
            command: protocol.CommandTypes.References,
            arguments: protocolFileLocationFromSubstring(
              apiFile,
              referenceTerm,
            ),
          });

          baselineTsserverLogs(
            "projectReferences",
            `special handling of localness ${scenario}`,
            session,
          );
        });
      }

      verify(
        "when using arrow function assignment",
        `export const dog = () => { };`,
        `shared.dog();`,
        "dog",
      );

      verify(
        "when using arrow function as object literal property types",
        `export const foo = { bar: () => { } };`,
        `shared.foo.bar();`,
        "bar",
      );

      verify(
        "when using object literal property",
        `export const foo = {  baz: "BAZ" };`,
        `shared.foo.baz;`,
        "baz",
      );

      verify(
        "when using method of class expression",
        `export const foo = class { fly() {} };`,
        `const instance = new shared.foo();
instance.fly();`,
        "fly",
      );

      verify(
        // when using arrow function as object literal property is loaded through indirect assignment with original declaration local to project is treated as local
        "when using arrow function as object literal property",
        `const local = { bar: () => { } };
export const foo = local;`,
        `shared.foo.bar();`,
        "bar",
      );
    });

    it("when disableSolutionSearching is true, solution and siblings are not loaded", () => {
      const solutionLocation = "/user/username/projects/solution";
      const solution: File = {
        path: `${solutionLocation}/tsconfig.json`,
        content: JSON.stringify({
          files: [],
          include: [],
          references: [{ path: "./compiler" }, { path: "./services" }],
        }),
      };
      const compilerConfig: File = {
        path: `${solutionLocation}/compiler/tsconfig.json`,
        content: JSON.stringify({
          compilerOptions: {
            composite: true,
            module: "none",
            disableSolutionSearching: true,
          },
          files: ["./types.ts", "./program.ts"],
        }),
      };
      const typesFile: File = {
        path: `${solutionLocation}/compiler/types.ts`,
        content: `
                namespace ts {
                    export interface Program {
                        getSourceFiles(): string[];
                    }
                }`,
      };
      const programFile: File = {
        path: `${solutionLocation}/compiler/program.ts`,
        content: `
                namespace ts {
                    export const program: Program = {
                        getSourceFiles: () => [getSourceFile()]
                    };
                    function getSourceFile() { return "something"; }
                }`,
      };
      const servicesConfig: File = {
        path: `${solutionLocation}/services/tsconfig.json`,
        content: JSON.stringify({
          compilerOptions: {
            composite: true,
          },
          files: ["./services.ts"],
          references: [{ path: "../compiler" }],
        }),
      };
      const servicesFile: File = {
        path: `${solutionLocation}/services/services.ts`,
        content: `
                namespace ts {
                    const result = program.getSourceFiles();
                }`,
      };

      const files = [
        libFile,
        solution,
        compilerConfig,
        typesFile,
        programFile,
        servicesConfig,
        servicesFile,
        libFile,
      ];
      const host = createServerHost(files);
      const session = createSession(host, {
        logger: createLoggerWithInMemoryLogs(),
      });
      openFilesForSession([programFile], session);

      // Find all references
      // No new solutions/projects loaded
      session.executeCommandSeq<protocol.ReferencesRequest>({
        command: protocol.CommandTypes.References,
        arguments: protocolFileLocationFromSubstring(
          programFile,
          "getSourceFiles",
        ),
      });
      baselineTsserverLogs(
        "projectReferences",
        `with disableSolutionSearching solution and siblings are not loaded`,
        session,
      );
    });

    describe("when default project is solution project", () => {
      interface Setup {
        scenario: string;
        solutionOptions?: CompilerOptions;
        solutionFiles?: string[];
        configRefs: string[];
        additionalFiles: readonly File[];
      }
      const main: File = {
        path: `${tscWatch.projectRoot}/src/main.ts`,
        content: `import { foo } from 'helpers/functions';
export { foo };`,
      };
      const helper: File = {
        path: `${tscWatch.projectRoot}/src/helpers/functions.ts`,
        content: `export const foo = 1;`,
      };
      const mainDts: File = {
        path: `${tscWatch.projectRoot}/target/src/main.d.ts`,
        content: `import { foo } from 'helpers/functions';
export { foo };
//# sourceMappingURL=main.d.ts.map`,
      };
      const mainDtsMap: File = {
        path: `${tscWatch.projectRoot}/target/src/main.d.ts.map`,
        content:
          `{"version":3,"file":"main.d.ts","sourceRoot":"","sources":["../../src/main.ts"],"names":[],"mappings":"AAAA,OAAO,EAAE,GAAG,EAAE,MAAM,mBAAmB,CAAC;AAExC,OAAO,EAAC,GAAG,EAAC,CAAC"}`,
      };
      const helperDts: File = {
        path: `${tscWatch.projectRoot}/target/src/helpers/functions.d.ts`,
        content: `export declare const foo = 1;
//# sourceMappingURL=functions.d.ts.map`,
      };
      const helperDtsMap: File = {
        path: `${tscWatch.projectRoot}/target/src/helpers/functions.d.ts.map`,
        content:
          `{"version":3,"file":"functions.d.ts","sourceRoot":"","sources":["../../../src/helpers/functions.ts"],"names":[],"mappings":"AAAA,eAAO,MAAM,GAAG,IAAI,CAAC"}`,
      };
      const tsconfigIndirect3: File = {
        path: `${tscWatch.projectRoot}/indirect3/tsconfig.json`,
        content: JSON.stringify({
          compilerOptions: {
            baseUrl: "../target/src/",
          },
        }),
      };
      const fileResolvingToMainDts: File = {
        path: `${tscWatch.projectRoot}/indirect3/main.ts`,
        content: `import { foo } from 'main';
foo;
export function bar() {}`,
      };
      const tsconfigSrcPath = `${tscWatch.projectRoot}/tsconfig-src.json`;
      const tsconfigPath = `${tscWatch.projectRoot}/tsconfig.json`;
      const dummyFilePath = "/dummy/dummy.ts";
      function setup({
        solutionFiles,
        solutionOptions,
        configRefs,
        additionalFiles,
      }: Setup) {
        const tsconfigSrc: File = {
          path: tsconfigSrcPath,
          content: JSON.stringify({
            compilerOptions: {
              composite: true,
              outDir: "./target/",
              baseUrl: "./src/",
            },
            include: ["./src/**/*"],
          }),
        };
        const tsconfig: File = {
          path: tsconfigPath,
          content: JSON.stringify({
            ...(solutionOptions ? { compilerOptions: solutionOptions } : {}),
            references: configRefs.map((path) => ({ path })),
            files: solutionFiles || [],
          }),
        };
        const dummyFile: File = {
          path: dummyFilePath,
          content: "let a = 10;",
        };
        const host = createServerHost([
          tsconfigSrc,
          tsconfig,
          main,
          helper,
          libFile,
          dummyFile,
          mainDts,
          mainDtsMap,
          helperDts,
          helperDtsMap,
          tsconfigIndirect3,
          fileResolvingToMainDts,
          ...additionalFiles,
        ]);
        const session = createSession(host, {
          canUseEvents: true,
          logger: createLoggerWithInMemoryLogs(),
        });
        const service = session.getProjectService();
        service.openClientFile(main.path);
        return { session, service, host };
      }

      function verifySolutionScenario(input: Setup) {
        const { session, service, host } = setup(input);

        const info = service.getScriptInfoForPath(main.path as Path)!;
        session.logger.logs.push("");
        session.logger.logs.push(
          `getDefaultProject for ${main.path}: ${info.getDefaultProject().projectName}`,
        );
        session.logger.logs.push(
          `findDefaultConfiguredProject for ${main.path}: ${service.findDefaultConfiguredProject(info)!.projectName}`,
        );
        session.logger.logs.push("");

        // Verify errors
        verifyGetErrRequest({ session, host, files: [main] });

        // Verify collection of script infos
        service.openClientFile(dummyFilePath);

        service.closeClientFile(main.path);
        service.closeClientFile(dummyFilePath);
        service.openClientFile(dummyFilePath);

        service.openClientFile(main.path);
        service.closeClientFile(dummyFilePath);
        service.openClientFile(dummyFilePath);

        // Verify Reload projects
        service.reloadProjects();

        // Find all refs
        session.executeCommandSeq<protocol.ReferencesRequest>({
          command: protocol.CommandTypes.References,
          arguments: protocolFileLocationFromSubstring(main, "foo", {
            index: 1,
          }),
        }).response as protocol.ReferencesResponseBody;

        service.closeClientFile(main.path);
        service.closeClientFile(dummyFilePath);

        // Verify when declaration map references the file
        service.openClientFile(fileResolvingToMainDts.path);

        // Find all refs from dts include
        session.executeCommandSeq<protocol.ReferencesRequest>({
          command: protocol.CommandTypes.References,
          arguments: protocolFileLocationFromSubstring(
            fileResolvingToMainDts,
            "foo",
          ),
        }).response as protocol.ReferencesResponseBody;
        baselineTsserverLogs("projectReferences", input.scenario, session);
      }

      function getIndirectProject(
        postfix: string,
        optionsToExtend?: CompilerOptions,
      ) {
        const tsconfigIndirect: File = {
          path: `${tscWatch.projectRoot}/tsconfig-indirect${postfix}.json`,
          content: JSON.stringify({
            compilerOptions: {
              composite: true,
              outDir: "./target/",
              baseUrl: "./src/",
              ...optionsToExtend,
            },
            files: [`./indirect${postfix}/main.ts`],
            references: [{ path: "./tsconfig-src.json" }],
          }),
        };
        const indirect: File = {
          path: `${tscWatch.projectRoot}/indirect${postfix}/main.ts`,
          content: fileResolvingToMainDts.content,
        };
        return { tsconfigIndirect, indirect };
      }

      function verifyDisableReferencedProjectLoad(input: Setup) {
        const { session, service } = setup(input);

        const info = service.getScriptInfoForPath(main.path as Path)!;
        session.logger.logs.push("");
        session.logger.logs.push(
          `getDefaultProject for ${main.path}: ${info.getDefaultProject().projectName}`,
        );
        session.logger.logs.push(
          `findDefaultConfiguredProject for ${main.path}: ${service.findDefaultConfiguredProject(info)?.projectName}`,
        );
        session.logger.logs.push("");

        // Verify collection of script infos
        service.openClientFile(dummyFilePath);

        service.closeClientFile(main.path);
        service.closeClientFile(dummyFilePath);
        service.openClientFile(dummyFilePath);

        service.openClientFile(main.path);

        // Verify Reload projects
        service.reloadProjects();
        baselineTsserverLogs("projectReferences", input.scenario, session);
      }

      it("when project is directly referenced by solution", () => {
        verifySolutionScenario({
          scenario: "project is directly referenced by solution",
          configRefs: ["./tsconfig-src.json"],
          additionalFiles: emptyArray,
        });
      });

      it("when project is indirectly referenced by solution", () => {
        const { tsconfigIndirect, indirect } = getIndirectProject("1");
        const { tsconfigIndirect: tsconfigIndirect2, indirect: indirect2 } = getIndirectProject("2");
        verifySolutionScenario({
          scenario: "project is indirectly referenced by solution",
          configRefs: [
            "./tsconfig-indirect1.json",
            "./tsconfig-indirect2.json",
          ],
          additionalFiles: [
            tsconfigIndirect,
            indirect,
            tsconfigIndirect2,
            indirect2,
          ],
        });
      });

      it("disables looking into the child project if disableReferencedProjectLoad is set", () => {
        verifyDisableReferencedProjectLoad({
          scenario: "disables looking into the child project if disableReferencedProjectLoad is set",
          solutionOptions: { disableReferencedProjectLoad: true },
          configRefs: ["./tsconfig-src.json"],
          additionalFiles: emptyArray,
        });
      });

      it("disables looking into the child project if disableReferencedProjectLoad is set in indirect project", () => {
        const { tsconfigIndirect, indirect } = getIndirectProject("1", {
          disableReferencedProjectLoad: true,
        });
        verifyDisableReferencedProjectLoad({
          scenario:
            "disables looking into the child project if disableReferencedProjectLoad is set in indirect project",
          configRefs: ["./tsconfig-indirect1.json"],
          additionalFiles: [tsconfigIndirect, indirect],
        });
      });

      it("disables looking into the child project if disableReferencedProjectLoad is set in first indirect project but not in another one", () => {
        const { tsconfigIndirect, indirect } = getIndirectProject("1", {
          disableReferencedProjectLoad: true,
        });
        const { tsconfigIndirect: tsconfigIndirect2, indirect: indirect2 } = getIndirectProject("2");
        verifyDisableReferencedProjectLoad({
          scenario:
            "disables looking into the child project if disableReferencedProjectLoad is set in first indirect project but not in another one",
          configRefs: [
            "./tsconfig-indirect1.json",
            "./tsconfig-indirect2.json",
          ],
          additionalFiles: [
            tsconfigIndirect,
            indirect,
            tsconfigIndirect2,
            indirect2,
          ],
        });
      });

      describe("when solution is project that contains its own files", () => {
        it("when the project found is not solution but references open file through project reference", () => {
          const ownMain: File = {
            path: `${tscWatch.projectRoot}/own/main.ts`,
            content: fileResolvingToMainDts.content,
          };
          verifySolutionScenario({
            scenario:
              "solution with its own files and project found is not solution but references open file through project reference",
            solutionFiles: [`./own/main.ts`],
            solutionOptions: {
              outDir: "./target/",
              baseUrl: "./src/",
            },
            configRefs: ["./tsconfig-src.json"],
            additionalFiles: [ownMain],
          });
        });

        it("when project is indirectly referenced by solution", () => {
          const ownMain: File = {
            path: `${tscWatch.projectRoot}/own/main.ts`,
            content: `import { bar } from 'main';
bar;`,
          };
          const { tsconfigIndirect, indirect } = getIndirectProject("1");
          const { tsconfigIndirect: tsconfigIndirect2, indirect: indirect2 } = getIndirectProject("2");
          verifySolutionScenario({
            scenario: "solution with its own files and project is indirectly referenced by solution",
            solutionFiles: [`./own/main.ts`],
            solutionOptions: {
              outDir: "./target/",
              baseUrl: "./indirect1/",
            },
            configRefs: [
              "./tsconfig-indirect1.json",
              "./tsconfig-indirect2.json",
            ],
            additionalFiles: [
              tsconfigIndirect,
              indirect,
              tsconfigIndirect2,
              indirect2,
              ownMain,
            ],
          });
        });

        it("disables looking into the child project if disableReferencedProjectLoad is set", () => {
          const ownMain: File = {
            path: `${tscWatch.projectRoot}/own/main.ts`,
            content: fileResolvingToMainDts.content,
          };
          verifyDisableReferencedProjectLoad({
            scenario:
              "solution with its own files and disables looking into the child project if disableReferencedProjectLoad is set",
            solutionFiles: [`./own/main.ts`],
            solutionOptions: {
              outDir: "./target/",
              baseUrl: "./src/",
              disableReferencedProjectLoad: true,
            },
            configRefs: ["./tsconfig-src.json"],
            additionalFiles: [ownMain],
          });
        });

        it("disables looking into the child project if disableReferencedProjectLoad is set in indirect project", () => {
          const ownMain: File = {
            path: `${tscWatch.projectRoot}/own/main.ts`,
            content: `import { bar } from 'main';
bar;`,
          };
          const { tsconfigIndirect, indirect } = getIndirectProject("1", {
            disableReferencedProjectLoad: true,
          });
          verifyDisableReferencedProjectLoad({
            scenario:
              "solution with its own files and disables looking into the child project if disableReferencedProjectLoad is set in indirect project",
            solutionFiles: [`./own/main.ts`],
            solutionOptions: {
              outDir: "./target/",
              baseUrl: "./indirect1/",
            },
            configRefs: ["./tsconfig-indirect1.json"],
            additionalFiles: [tsconfigIndirect, indirect, ownMain],
          });
        });

        it("disables looking into the child project if disableReferencedProjectLoad is set in first indirect project but not in another one", () => {
          const ownMain: File = {
            path: `${tscWatch.projectRoot}/own/main.ts`,
            content: `import { bar } from 'main';
bar;`,
          };
          const { tsconfigIndirect, indirect } = getIndirectProject("1", {
            disableReferencedProjectLoad: true,
          });
          const { tsconfigIndirect: tsconfigIndirect2, indirect: indirect2 } = getIndirectProject("2");
          verifyDisableReferencedProjectLoad({
            scenario:
              "solution with its own files and disables looking into the child project if disableReferencedProjectLoad is set in first indirect project but not in another one",
            solutionFiles: [`./own/main.ts`],
            solutionOptions: {
              outDir: "./target/",
              baseUrl: "./indirect1/",
            },
            configRefs: [
              "./tsconfig-indirect1.json",
              "./tsconfig-indirect2.json",
            ],
            additionalFiles: [
              tsconfigIndirect,
              indirect,
              tsconfigIndirect2,
              indirect2,
              ownMain,
            ],
          });
        });
      });
    });

    describe("when new file is added to the referenced project", () => {
      function setup(extendOptionsProject2?: CompilerOptions) {
        const config1: File = {
          path: `${tscWatch.projectRoot}/projects/project1/tsconfig.json`,
          content: JSON.stringify({
            compilerOptions: {
              module: "none",
              composite: true,
            },
            exclude: ["temp"],
          }),
        };
        const class1: File = {
          path: `${tscWatch.projectRoot}/projects/project1/class1.ts`,
          content: `class class1 {}`,
        };
        const class1Dts: File = {
          path: `${tscWatch.projectRoot}/projects/project1/class1.d.ts`,
          content: `declare class class1 {}`,
        };
        const config2: File = {
          path: `${tscWatch.projectRoot}/projects/project2/tsconfig.json`,
          content: JSON.stringify({
            compilerOptions: {
              module: "none",
              composite: true,
              ...(extendOptionsProject2 || {}),
            },
            references: [{ path: "../project1" }],
          }),
        };
        const class2: File = {
          path: `${tscWatch.projectRoot}/projects/project2/class2.ts`,
          content: `class class2 {}`,
        };
        const host = createServerHost([
          config1,
          class1,
          class1Dts,
          config2,
          class2,
          libFile,
        ]);
        const session = createSession(host, {
          logger: createLoggerWithInMemoryLogs(),
        });
        openFilesForSession([class2], session);
        return { host, session, class1 };
      }

      it("when referenced project is not open", () => {
        const { host, session } = setup();

        // Add new class to referenced project
        const class3 = `${tscWatch.projectRoot}/projects/project1/class3.ts`;
        host.writeFile(class3, `class class3 {}`);
        host.checkTimeoutQueueLengthAndRun(2);

        // Add excluded file to referenced project
        host.ensureFileOrFolder({
          path: `${tscWatch.projectRoot}/projects/project1/temp/file.d.ts`,
          content: `declare class file {}`,
        });
        host.checkTimeoutQueueLengthAndRun(0);

        // Add output from new class to referenced project
        const class3Dts = `${tscWatch.projectRoot}/projects/project1/class3.d.ts`;
        host.writeFile(class3Dts, `declare class class3 {}`);
        host.checkTimeoutQueueLengthAndRun(0);
        baselineTsserverLogs(
          "projectReferences",
          `new file is added to the referenced project when referenced project is not open`,
          session,
        );
      });

      it("when referenced project is open", () => {
        const { host, session, class1 } = setup();
        openFilesForSession([class1], session);

        // Add new class to referenced project
        const class3 = `${tscWatch.projectRoot}/projects/project1/class3.ts`;
        host.writeFile(class3, `class class3 {}`);
        host.checkTimeoutQueueLengthAndRun(3);
        // Add excluded file to referenced project
        host.ensureFileOrFolder({
          path: `${tscWatch.projectRoot}/projects/project1/temp/file.d.ts`,
          content: `declare class file {}`,
        });
        host.checkTimeoutQueueLengthAndRun(0);
        // Add output from new class to referenced project
        const class3Dts = `${tscWatch.projectRoot}/projects/project1/class3.d.ts`;
        host.writeFile(class3Dts, `declare class class3 {}`);
        host.checkTimeoutQueueLengthAndRun(0);
        baselineTsserverLogs(
          "projectReferences",
          `new file is added to the referenced project when referenced project is open`,
          session,
        );
      });

      it("when referenced project is not open with disableSourceOfProjectReferenceRedirect", () => {
        const { host, session } = setup({
          disableSourceOfProjectReferenceRedirect: true,
        });

        // Add new class to referenced project
        const class3 = `${tscWatch.projectRoot}/projects/project1/class3.ts`;
        host.writeFile(class3, `class class3 {}`);
        host.checkTimeoutQueueLengthAndRun(2);
        // Add output of new class to referenced project
        const class3Dts = `${tscWatch.projectRoot}/projects/project1/class3.d.ts`;
        host.writeFile(class3Dts, `declare class class3 {}`);
        host.checkTimeoutQueueLengthAndRun(2);
        // Add excluded file to referenced project
        host.ensureFileOrFolder({
          path: `${tscWatch.projectRoot}/projects/project1/temp/file.d.ts`,
          content: `declare class file {}`,
        });
        host.checkTimeoutQueueLengthAndRun(0);
        // Delete output from new class to referenced project
        host.deleteFile(class3Dts);
        host.checkTimeoutQueueLengthAndRun(2);
        // Write back output of new class to referenced project
        host.writeFile(class3Dts, `declare class class3 {}`);
        host.checkTimeoutQueueLengthAndRun(2);
        baselineTsserverLogs(
          "projectReferences",
          `new file is added to the referenced project when referenced project is not open with disableSourceOfProjectReferenceRedirect`,
          session,
        );
      });

      it("when referenced project is open with disableSourceOfProjectReferenceRedirect", () => {
        const { host, session, class1 } = setup({
          disableSourceOfProjectReferenceRedirect: true,
        });
        openFilesForSession([class1], session);

        // Add new class to referenced project
        const class3 = `${tscWatch.projectRoot}/projects/project1/class3.ts`;
        host.writeFile(class3, `class class3 {}`);
        host.checkTimeoutQueueLengthAndRun(3);
        // Add output of new class to referenced project
        const class3Dts = `${tscWatch.projectRoot}/projects/project1/class3.d.ts`;
        host.writeFile(class3Dts, `declare class class3 {}`);
        host.checkTimeoutQueueLengthAndRun(2);
        // Add excluded file to referenced project
        host.ensureFileOrFolder({
          path: `${tscWatch.projectRoot}/projects/project1/temp/file.d.ts`,
          content: `declare class file {}`,
        });
        host.checkTimeoutQueueLengthAndRun(0);
        // Delete output from new class to referenced project
        host.deleteFile(class3Dts);
        host.checkTimeoutQueueLengthAndRun(2);
        // Write back output of new class to referenced project
        host.writeFile(class3Dts, `declare class class3 {}`);
        host.checkTimeoutQueueLengthAndRun(2);
        baselineTsserverLogs(
          "projectReferences",
          `new file is added to the referenced project when referenced project is open with disableSourceOfProjectReferenceRedirect`,
          session,
        );
      });
    });

    describe("auto import with referenced project", () => {
      function verifyAutoImport(
        built: boolean,
        disableSourceOfProjectReferenceRedirect?: boolean,
      ) {
        const solnConfig: File = {
          path: `${tscWatch.projectRoot}/tsconfig.json`,
          content: JSON.stringify({
            files: [],
            references: [
              { path: "shared/src/library" },
              { path: "app/src/program" },
            ],
          }),
        };
        const sharedConfig: File = {
          path: `${tscWatch.projectRoot}/shared/src/library/tsconfig.json`,
          content: JSON.stringify({
            compilerOptions: {
              composite: true,
              outDir: "../../bld/library",
            },
          }),
        };
        const sharedIndex: File = {
          path: `${tscWatch.projectRoot}/shared/src/library/index.ts`,
          content: `export function foo() {}`,
        };
        const sharedPackage: File = {
          path: `${tscWatch.projectRoot}/shared/package.json`,
          content: JSON.stringify({
            name: "shared",
            version: "1.0.0",
            main: "bld/library/index.js",
            types: "bld/library/index.d.ts",
          }),
        };
        const appConfig: File = {
          path: `${tscWatch.projectRoot}/app/src/program/tsconfig.json`,
          content: JSON.stringify({
            compilerOptions: {
              composite: true,
              outDir: "../../bld/program",
              disableSourceOfProjectReferenceRedirect,
            },
            references: [{ path: "../../../shared/src/library" }],
          }),
        };
        const appBar: File = {
          path: `${tscWatch.projectRoot}/app/src/program/bar.ts`,
          content: `import {foo} from "shared";`,
        };
        const appIndex: File = {
          path: `${tscWatch.projectRoot}/app/src/program/index.ts`,
          content: `foo`,
        };
        const sharedSymlink: SymLink = {
          path: `${tscWatch.projectRoot}/node_modules/shared`,
          symLink: `${tscWatch.projectRoot}/shared`,
        };
        const files = [
          solnConfig,
          sharedConfig,
          sharedIndex,
          sharedPackage,
          appConfig,
          appBar,
          appIndex,
          sharedSymlink,
          libFile,
        ];
        const host = createServerHost(files);
        if (built) {
          const solutionBuilder = tscWatch.createSolutionBuilder(
            host,
            [solnConfig.path],
            {},
          );
          solutionBuilder.build();
          host.clearOutput();
        }
        const session = createSession(host, {
          logger: createLoggerWithInMemoryLogs(),
        });
        openFilesForSession([appIndex], session);
        session.executeCommandSeq<protocol.CodeFixRequest>({
          command: protocol.CommandTypes.GetCodeFixes,
          arguments: {
            file: appIndex.path,
            startLine: 1,
            startOffset: 1,
            endLine: 1,
            endOffset: 4,
            errorCodes: [Diagnostics.Cannot_find_name_0.code],
          },
        });
        baselineTsserverLogs(
          "projectReferences",
          `auto import with referenced project${built ? " when built" : ""}${
            disableSourceOfProjectReferenceRedirect
              ? " with disableSourceOfProjectReferenceRedirect"
              : ""
          }`,
          session,
        );
      }

      it("when project is built", () => {
        verifyAutoImport(/*built*/ true);
      });
      it("when project is not built", () => {
        verifyAutoImport(/*built*/ false);
      });
      it("when disableSourceOfProjectReferenceRedirect is true", () => {
        verifyAutoImport(
          /*built*/ true,
          /*disableSourceOfProjectReferenceRedirect*/ true,
        );
      });
    });

    it("when files from two projects are open and one project references", () => {
      function getPackageAndFile(
        packageName: string,
        references?: string[],
        optionsToExtend?: CompilerOptions,
      ): [file: File, config: File] {
        const file: File = {
          path: `${tscWatch.projectRoot}/${packageName}/src/file1.ts`,
          content: `export const ${packageName}Const = 10;`,
        };
        const config: File = {
          path: `${tscWatch.projectRoot}/${packageName}/tsconfig.json`,
          content: JSON.stringify({
            compilerOptions: { composite: true, ...(optionsToExtend || {}) },
            references: references?.map((path) => ({ path: `../${path}` })),
          }),
        };
        return [file, config];
      }
      const [mainFile, mainConfig] = getPackageAndFile("main", [
        "core",
        "indirect",
        "noCoreRef1",
        "indirectDisabledChildLoad1",
        "indirectDisabledChildLoad2",
        "refToCoreRef3",
        "indirectNoCoreRef",
      ]);
      const [coreFile, coreConfig] = getPackageAndFile("core");
      const [noCoreRef1File, noCoreRef1Config] = getPackageAndFile("noCoreRef1");
      const [indirectFile, indirectConfig] = getPackageAndFile("indirect", [
        "coreRef1",
      ]);
      const [coreRef1File, coreRef1Config] = getPackageAndFile("coreRef1", [
        "core",
      ]);
      const [indirectDisabledChildLoad1File, indirectDisabledChildLoad1Config] = getPackageAndFile(
        "indirectDisabledChildLoad1",
        ["coreRef2"],
        {
          disableReferencedProjectLoad: true,
        },
      );
      const [coreRef2File, coreRef2Config] = getPackageAndFile("coreRef2", [
        "core",
      ]);
      const [indirectDisabledChildLoad2File, indirectDisabledChildLoad2Config] = getPackageAndFile(
        "indirectDisabledChildLoad2",
        ["coreRef3"],
        {
          disableReferencedProjectLoad: true,
        },
      );
      const [coreRef3File, coreRef3Config] = getPackageAndFile("coreRef3", [
        "core",
      ]);
      const [refToCoreRef3File, refToCoreRef3Config] = getPackageAndFile(
        "refToCoreRef3",
        ["coreRef3"],
      );
      const [indirectNoCoreRefFile, indirectNoCoreRefConfig] = getPackageAndFile("indirectNoCoreRef", ["noCoreRef2"]);
      const [noCoreRef2File, noCoreRef2Config] = getPackageAndFile("noCoreRef2");

      const host = createServerHost(
        [
          libFile,
          mainFile,
          mainConfig,
          coreFile,
          coreConfig,
          noCoreRef1File,
          noCoreRef1Config,
          indirectFile,
          indirectConfig,
          coreRef1File,
          coreRef1Config,
          indirectDisabledChildLoad1File,
          indirectDisabledChildLoad1Config,
          coreRef2File,
          coreRef2Config,
          indirectDisabledChildLoad2File,
          indirectDisabledChildLoad2Config,
          coreRef3File,
          coreRef3Config,
          refToCoreRef3File,
          refToCoreRef3Config,
          indirectNoCoreRefFile,
          indirectNoCoreRefConfig,
          noCoreRef2File,
          noCoreRef2Config,
        ],
        { useCaseSensitiveFileNames: true },
      );
      const session = createSession(host, {
        logger: createLoggerWithInMemoryLogs(),
      });
      openFilesForSession([mainFile, coreFile], session);

      // Find all refs in coreFile
      session.executeCommandSeq<protocol.ReferencesRequest>({
        command: protocol.CommandTypes.References,
        arguments: protocolFileLocationFromSubstring(coreFile, `coreConst`),
      });
      baselineTsserverLogs(
        "projectReferences",
        `when files from two projects are open and one project references`,
        session,
      );
    });

    describe("find refs to decl in other proj", () => {
      const indexA: File = {
        path: `${tscWatch.projectRoot}/a/index.ts`,
        content: `import { B } from "../b/lib";

const b: B = new B();`,
      };

      const configB: File = {
        path: `${tscWatch.projectRoot}/b/tsconfig.json`,
        content: `{
"compilerOptions": {
    "declarationMap": true,
    "outDir": "lib",
    "composite": true
}
}`,
      };

      const indexB: File = {
        path: `${tscWatch.projectRoot}/b/index.ts`,
        content: `export class B {
    M() {}
}`,
      };

      const helperB: File = {
        path: `${tscWatch.projectRoot}/b/helper.ts`,
        content: `import { B } from ".";

const b: B = new B();`,
      };

      const dtsB: File = {
        path: `${tscWatch.projectRoot}/b/lib/index.d.ts`,
        content: `export declare class B {
    M(): void;
}
//# sourceMappingURL=index.d.ts.map`,
      };

      const dtsMapB: File = {
        path: `${tscWatch.projectRoot}/b/lib/index.d.ts.map`,
        content:
          `{"version":3,"file":"index.d.ts","sourceRoot":"","sources":["../index.ts"],"names":[],"mappings":"AAAA,qBAAa,CAAC;IACV,CAAC;CACJ"}`,
      };

      function baselineDisableReferencedProjectLoad(
        projectAlreadyLoaded: boolean,
        disableReferencedProjectLoad: boolean,
        disableSourceOfProjectReferenceRedirect: boolean,
        dtsMapPresent: boolean,
      ) {
        // Mangled to stay under windows path length limit
        const subScenario = `when proj ${projectAlreadyLoaded ? "is" : "is not"} loaded`
          + ` and refd proj loading is ${disableReferencedProjectLoad ? "disabled" : "enabled"}`
          + ` and proj ref redirects are ${disableSourceOfProjectReferenceRedirect ? "disabled" : "enabled"}`
          + ` and a decl map is ${dtsMapPresent ? "present" : "missing"}`;
        const compilerOptions: CompilerOptions = {
          disableReferencedProjectLoad,
          disableSourceOfProjectReferenceRedirect,
          composite: true,
        };

        it(subScenario, () => {
          const configA: File = {
            path: `${tscWatch.projectRoot}/a/tsconfig.json`,
            content: `{
        "compilerOptions": ${JSON.stringify(compilerOptions)},
        "references": [{ "path": "../b" }]
    }`,
          };

          const host = createServerHost([
            configA,
            indexA,
            configB,
            indexB,
            helperB,
            dtsB,
            ...(dtsMapPresent ? [dtsMapB] : []),
          ]);
          const session = createSession(host, {
            logger: createLoggerWithInMemoryLogs(),
          });
          openFilesForSession(
            [indexA, ...(projectAlreadyLoaded ? [helperB] : [])],
            session,
          );

          session.executeCommandSeq<protocol.ReferencesRequest>({
            command: protocol.CommandTypes.References,
            arguments: protocolFileLocationFromSubstring(indexA, `B`, {
              index: 1,
            }),
          });
          baselineTsserverLogs(
            "projectReferences",
            `find refs to decl in other proj ${subScenario}`,
            session,
          );
        });
      }

      /* eslint-disable boolean-trivia */

      // Pre-loaded = A file from project B is already open when FAR is invoked
      // dRPL = Project A has disableReferencedProjectLoad
      // dSOPRR = Project A has disableSourceOfProjectReferenceRedirect
      // Map = The declaration map file b/lib/index.d.ts.map exists
      // B refs = files under directory b in which references are found (all scenarios find all references in a/index.ts)

      //                                   Pre-loaded | dRPL   | dSOPRR | Map      | B state    | Notes        | B refs              | Notes
      //                                   -----------+--------+--------+----------+------------+--------------+---------------------+---------------------------------------------------
      baselineDisableReferencedProjectLoad(true, true, true, true); // Pre-loaded |              | index.ts, helper.ts | Via map and pre-loaded project
      baselineDisableReferencedProjectLoad(true, true, true, false); // Pre-loaded |              | lib/index.d.ts      | Even though project is loaded
      baselineDisableReferencedProjectLoad(true, true, false, true); // Pre-loaded |              | index.ts, helper.ts |
      baselineDisableReferencedProjectLoad(true, true, false, false); // Pre-loaded |              | index.ts, helper.ts |
      baselineDisableReferencedProjectLoad(true, false, true, true); // Pre-loaded |              | index.ts, helper.ts | Via map and pre-loaded project
      baselineDisableReferencedProjectLoad(true, false, true, false); // Pre-loaded |              | lib/index.d.ts      | Even though project is loaded
      baselineDisableReferencedProjectLoad(true, false, false, true); // Pre-loaded |              | index.ts, helper.ts |
      baselineDisableReferencedProjectLoad(true, false, false, false); // Pre-loaded |              | index.ts, helper.ts |
      baselineDisableReferencedProjectLoad(false, true, true, true); // Not loaded |              | lib/index.d.ts      | Even though map is present
      baselineDisableReferencedProjectLoad(false, true, true, false); // Not loaded |              | lib/index.d.ts      |
      baselineDisableReferencedProjectLoad(false, true, false, true); // Not loaded |              | index.ts            | But not helper.ts, which is not referenced from a
      baselineDisableReferencedProjectLoad(false, true, false, false); // Not loaded |              | index.ts            | But not helper.ts, which is not referenced from a
      baselineDisableReferencedProjectLoad(false, false, true, true); // Loaded     | Via map      | index.ts, helper.ts | Via map and newly loaded project
      baselineDisableReferencedProjectLoad(false, false, true, false); // Not loaded |              | lib/index.d.ts      |
      baselineDisableReferencedProjectLoad(false, false, false, true); // Loaded     | Via redirect | index.ts, helper.ts |
      baselineDisableReferencedProjectLoad(false, false, false, false); // Loaded     | Via redirect | index.ts, helper.ts |

      /* eslint-enable boolean-trivia */
    });
  });
}
