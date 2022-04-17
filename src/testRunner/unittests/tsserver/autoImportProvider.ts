namespace ts.projectSystem {
  const angularFormsDts: File = {
    path: "/node_modules/@angular/forms/forms.d.ts",
    content: "export declare class PatternValidator {}",
  };
  const angularFormsPackageJson: File = {
    path: "/node_modules/@angular/forms/package.json",
    content: `{ "name": "@angular/forms", "typings": "./forms.d.ts" }`,
  };
  const angularCoreDts: File = {
    path: "/node_modules/@angular/core/core.d.ts",
    content: "",
  };
  const angularCorePackageJson: File = {
    path: "/node_modules/@angular/core/package.json",
    content: `{ "name": "@angular/core", "typings": "./core.d.ts" }`,
  };
  const tsconfig: File = {
    path: "/tsconfig.json",
    content: `{ "compilerOptions": { "module": "commonjs" } }`,
  };
  const packageJson: File = {
    path: "/package.json",
    content: `{ "dependencies": { "@angular/forms": "*", "@angular/core": "*" } }`,
  };
  const indexTs: File = {
    path: "/index.ts",
    content: "",
  };

  describe("unittests:: tsserver:: autoImportProvider", () => {
    it("Auto import provider program is not created without dependencies listed in package.json", () => {
      const { projectService, session } = setup([
        angularFormsDts,
        angularFormsPackageJson,
        tsconfig,
        { path: packageJson.path, content: `{ "dependencies": {} }` },
        indexTs,
      ]);
      openFilesForSession([indexTs], session);
      assert.isUndefined(
        projectService.configuredProjects
          .get(tsconfig.path)!
          .getLanguageService()
          .getAutoImportProvider()
      );
    });

    it("Auto import provider program is not created if dependencies are already in main program", () => {
      const { projectService, session } = setup([
        angularFormsDts,
        angularFormsPackageJson,
        tsconfig,
        packageJson,
        { path: indexTs.path, content: "import '@angular/forms';" },
      ]);
      openFilesForSession([indexTs], session);
      assert.isUndefined(
        projectService.configuredProjects
          .get(tsconfig.path)!
          .getLanguageService()
          .getAutoImportProvider()
      );
    });

    it("Auto-import program is not created for projects already inside node_modules", () => {
      // Simulate browsing typings files inside node_modules: no point creating auto import program
      // for the InferredProject that gets created in there.
      const { projectService, session } = setup([
        angularFormsDts,
        {
          path: angularFormsPackageJson.path,
          content: `{ "dependencies": { "@angular/core": "*" } }`,
        },
        {
          path: "/node_modules/@angular/core/package.json",
          content: `{ "typings": "./core.d.ts" }`,
        },
        {
          path: "/node_modules/@angular/core/core.d.ts",
          content: `export namespace angular {};`,
        },
      ]);

      openFilesForSession([angularFormsDts], session);
      checkNumberOfInferredProjects(projectService, 1);
      checkNumberOfConfiguredProjects(projectService, 0);
      assert.isUndefined(
        projectService
          .getDefaultProjectForFile(
            angularFormsDts.path as server.NormalizedPath,
            /*ensureProject*/ true
          )!
          .getLanguageService()
          .getAutoImportProvider()
      );
    });

    it("Auto-importable file is in inferred project until imported", () => {
      const { projectService, session, updateFile } = setup([
        angularFormsDts,
        angularFormsPackageJson,
        tsconfig,
        packageJson,
        indexTs,
      ]);
      checkNumberOfInferredProjects(projectService, 0);
      openFilesForSession([angularFormsDts], session);
      checkNumberOfInferredProjects(projectService, 1);
      assert.equal(
        projectService.getDefaultProjectForFile(
          angularFormsDts.path as server.NormalizedPath,
          /*ensureProject*/ true
        )?.projectKind,
        server.ProjectKind.Inferred
      );

      updateFile(indexTs.path, "import '@angular/forms'");
      assert.equal(
        projectService.getDefaultProjectForFile(
          angularFormsDts.path as server.NormalizedPath,
          /*ensureProject*/ true
        )?.projectKind,
        server.ProjectKind.Configured
      );

      assert.isUndefined(
        projectService.configuredProjects
          .get(tsconfig.path)!
          .getLanguageService()
          .getAutoImportProvider()
      );
    });

    it("Responds to package.json changes", () => {
      const { projectService, session, host } = setup([
        angularFormsDts,
        angularFormsPackageJson,
        tsconfig,
        { path: "/package.json", content: "{}" },
        indexTs,
      ]);

      openFilesForSession([indexTs], session);
      assert.isUndefined(
        projectService.configuredProjects
          .get(tsconfig.path)!
          .getLanguageService()
          .getAutoImportProvider()
      );

      host.writeFile(packageJson.path, packageJson.content);
      assert.ok(
        projectService.configuredProjects
          .get(tsconfig.path)!
          .getLanguageService()
          .getAutoImportProvider()
      );
    });

    it("Reuses autoImportProvider when program structure is unchanged", () => {
      const { projectService, session, updateFile } = setup([
        angularFormsDts,
        angularFormsPackageJson,
        tsconfig,
        packageJson,
        indexTs,
      ]);

      openFilesForSession([indexTs], session);
      const autoImportProvider = projectService.configuredProjects
        .get(tsconfig.path)!
        .getLanguageService()
        .getAutoImportProvider();
      assert.ok(autoImportProvider);

      updateFile(indexTs.path, "console.log(0)");
      assert.strictEqual(
        projectService.configuredProjects
          .get(tsconfig.path)!
          .getLanguageService()
          .getAutoImportProvider(),
        autoImportProvider
      );
    });

    it("Closes AutoImportProviderProject when host project closes", () => {
      const { projectService, session } = setup([
        angularFormsDts,
        angularFormsPackageJson,
        tsconfig,
        packageJson,
        indexTs,
      ]);

      openFilesForSession([indexTs], session);
      const hostProject = projectService.configuredProjects.get(tsconfig.path)!;
      hostProject.getPackageJsonAutoImportProvider();
      const autoImportProviderProject = hostProject.autoImportProviderHost;
      assert.ok(autoImportProviderProject);

      hostProject.close();
      assert.ok(
        autoImportProviderProject && autoImportProviderProject.isClosed()
      );
      assert.isUndefined(hostProject.autoImportProviderHost);
    });

    it("Does not schedule ensureProjectForOpenFiles on AutoImportProviderProject creation", () => {
      const { projectService, session, host } = setup([
        angularFormsDts,
        angularFormsPackageJson,
        tsconfig,
        indexTs,
      ]);

      // Create configured project only, ensure !projectService.pendingEnsureProjectForOpenFiles
      openFilesForSession([indexTs], session);
      const hostProject = projectService.configuredProjects.get(tsconfig.path)!;
      projectService.delayEnsureProjectForOpenFiles();
      host.runQueuedTimeoutCallbacks();
      assert.isFalse(projectService.pendingEnsureProjectForOpenFiles);

      // Create auto import provider project, ensure still !projectService.pendingEnsureProjectForOpenFiles
      host.writeFile(packageJson.path, packageJson.content);
      hostProject.getPackageJsonAutoImportProvider();
      assert.isFalse(projectService.pendingEnsureProjectForOpenFiles);
    });

    it("Responds to automatic changes in node_modules", () => {
      const { projectService, session, host } = setup([
        angularFormsDts,
        angularFormsPackageJson,
        angularCoreDts,
        angularCorePackageJson,
        tsconfig,
        packageJson,
        indexTs,
      ]);

      openFilesForSession([indexTs], session);
      const project = projectService.configuredProjects.get(tsconfig.path)!;
      const completionsBefore = project
        .getLanguageService()
        .getCompletionsAtPosition(indexTs.path, 0, {
          includeCompletionsForModuleExports: true,
        });
      assert.isTrue(
        completionsBefore?.entries.some((c) => c.name === "PatternValidator")
      );

      // Directory watchers only fire for add/remove, not change.
      // This is ok since a real `npm install` will always trigger add/remove events.
      host.deleteFile(angularFormsDts.path);
      host.writeFile(angularFormsDts.path, "");

      const autoImportProvider = project
        .getLanguageService()
        .getAutoImportProvider();
      const completionsAfter = project
        .getLanguageService()
        .getCompletionsAtPosition(indexTs.path, 0, {
          includeCompletionsForModuleExports: true,
        });
      assert.equal(
        autoImportProvider!.getSourceFile(angularFormsDts.path)!.getText(),
        ""
      );
      assert.isFalse(
        completionsAfter?.entries.some((c) => c.name === "PatternValidator")
      );
    });

    it("Responds to manual changes in node_modules", () => {
      const { projectService, session, updateFile } = setup([
        angularFormsDts,
        angularFormsPackageJson,
        angularCoreDts,
        angularCorePackageJson,
        tsconfig,
        packageJson,
        indexTs,
      ]);

      openFilesForSession([indexTs, angularFormsDts], session);
      const project = projectService.configuredProjects.get(tsconfig.path)!;
      const completionsBefore = project
        .getLanguageService()
        .getCompletionsAtPosition(indexTs.path, 0, {
          includeCompletionsForModuleExports: true,
        });
      assert.isTrue(
        completionsBefore?.entries.some((c) => c.name === "PatternValidator")
      );

      updateFile(angularFormsDts.path, "export class ValidatorPattern {}");
      const completionsAfter = project
        .getLanguageService()
        .getCompletionsAtPosition(indexTs.path, 0, {
          includeCompletionsForModuleExports: true,
        });
      assert.isFalse(
        completionsAfter?.entries.some((c) => c.name === "PatternValidator")
      );
      assert.isTrue(
        completionsAfter?.entries.some((c) => c.name === "ValidatorPattern")
      );
    });

    it("Recovers from an unparseable package.json", () => {
      const { projectService, session, host } = setup([
        angularFormsDts,
        angularFormsPackageJson,
        tsconfig,
        { path: packageJson.path, content: "{" },
        indexTs,
      ]);

      openFilesForSession([indexTs], session);
      assert.isUndefined(
        projectService.configuredProjects
          .get(tsconfig.path)!
          .getLanguageService()
          .getAutoImportProvider()
      );

      host.writeFile(packageJson.path, packageJson.content);
      assert.ok(
        projectService.configuredProjects
          .get(tsconfig.path)!
          .getLanguageService()
          .getAutoImportProvider()
      );
    });

    it("Does not create an auto import provider if there are too many dependencies", () => {
      const createPackage = (i: number): File[] => [
        {
          path: `/node_modules/package${i}/package.json`,
          content: `{ "name": "package${i}" }`,
        },
        { path: `/node_modules/package${i}/index.d.ts`, content: `` },
      ];

      const packages = [];
      for (let i = 0; i < 11; i++) {
        packages.push(createPackage(i));
      }

      const dependencies = packages.reduce(
        (hash, p) => ({ ...hash, [JSON.parse(p[0].content).name]: "*" }),
        {}
      );
      const packageJson: File = {
        path: "/package.json",
        content: JSON.stringify(dependencies),
      };
      const { projectService, session } = setup([
        ...flatten(packages),
        indexTs,
        tsconfig,
        packageJson,
      ]);

      openFilesForSession([indexTs], session);
      const project = projectService.configuredProjects.get(tsconfig.path)!;
      assert.isUndefined(project.getPackageJsonAutoImportProvider());
    });
  });

  describe("unittests:: tsserver:: autoImportProvider - monorepo", () => {
    it("Does not create auto import providers upon opening projects for find-all-references", () => {
      const files = [
        // node_modules
        angularFormsDts,
        angularFormsPackageJson,

        // root
        {
          path: tsconfig.path,
          content: `{ "references": [{ "path": "packages/a" }, { "path": "packages/b" }] }`,
        },
        { path: packageJson.path, content: `{ "private": true }` },

        // packages/a
        { path: "/packages/a/package.json", content: packageJson.content },
        {
          path: "/packages/a/tsconfig.json",
          content: `{ "compilerOptions": { "composite": true }, "references": [{ "path": "../b" }] }`,
        },
        { path: "/packages/a/index.ts", content: "import { B } from '../b';" },

        // packages/b
        { path: "/packages/b/package.json", content: packageJson.content },
        {
          path: "/packages/b/tsconfig.json",
          content: `{ "compilerOptions": { "composite": true } }`,
        },
        { path: "/packages/b/index.ts", content: `export class B {}` },
      ];

      const { projectService, session, findAllReferences } = setup(files);

      openFilesForSession(
        [files.find((f) => f.path === "/packages/b/index.ts")!],
        session
      );
      checkNumberOfConfiguredProjects(projectService, 2); // Solution (no files), B
      findAllReferences("/packages/b/index.ts", 1, "export class B".length - 1);
      checkNumberOfConfiguredProjects(projectService, 3); // Solution (no files), A, B

      // Project for A is created - ensure it doesn't have an autoImportProvider
      assert.isUndefined(
        projectService.configuredProjects
          .get("/packages/a/tsconfig.json")!
          .getLanguageService()
          .getAutoImportProvider()
      );
    });

    it("Does not close when root files are redirects that don't actually exist", () => {
      const files = [
        // packages/a
        {
          path: "/packages/a/package.json",
          content: `{ "dependencies": { "b": "*" } }`,
        },
        {
          path: "/packages/a/tsconfig.json",
          content: `{ "compilerOptions": { "composite": true }, "references": [{ "path": "./node_modules/b" }] }`,
        },
        { path: "/packages/a/index.ts", content: "" },

        // packages/b
        {
          path: "/packages/a/node_modules/b/package.json",
          content: `{ "types": "dist/index.d.ts" }`,
        },
        {
          path: "/packages/a/node_modules/b/tsconfig.json",
          content: `{ "compilerOptions": { "composite": true, "outDir": "dist" } }`,
        },
        {
          path: "/packages/a/node_modules/b/index.ts",
          content: `export class B {}`,
        },
      ];

      const { projectService, session } = setup(files);
      openFilesForSession([files[2]], session);
      assert.isDefined(
        projectService.configuredProjects
          .get("/packages/a/tsconfig.json")!
          .getPackageJsonAutoImportProvider()
      );
      assert.isDefined(
        projectService.configuredProjects
          .get("/packages/a/tsconfig.json")!
          .getPackageJsonAutoImportProvider()
      );
    });

    it("Can use the same document registry bucket key as main program", () => {
      for (const option of sourceFileAffectingCompilerOptions) {
        assert(
          !hasProperty(
            server.AutoImportProviderProject.compilerOptionsOverrides,
            option.name
          ),
          `'${option.name}' may cause AutoImportProviderProject not to share source files with main program`
        );
      }
    });
  });

  function setup(files: File[]) {
    const host = createServerHost(files);
    const session = createSession(host);
    const projectService = session.getProjectService();
    return {
      host,
      projectService,
      session,
      updateFile,
      findAllReferences,
    };

    function updateFile(path: string, newText: string) {
      Debug.assertIsDefined(files.find((f) => f.path === path));
      session.executeCommandSeq<protocol.ApplyChangedToOpenFilesRequest>({
        command: protocol.CommandTypes.ApplyChangedToOpenFiles,
        arguments: {
          openFiles: [
            {
              fileName: path,
              content: newText,
            },
          ],
        },
      });
    }

    function findAllReferences(file: string, line: number, offset: number) {
      Debug.assertIsDefined(files.find((f) => f.path === file));
      session.executeCommandSeq<protocol.ReferencesRequest>({
        command: protocol.CommandTypes.References,
        arguments: {
          file,
          line,
          offset,
        },
      });
    }
  }
}
