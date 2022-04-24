namespace ts {
  describe("unittests:: tsbuild:: configFileExtends:: when tsconfig extends another config", () => {
    function getConfigExtendsWithIncludeFs() {
      return loadProjectFromFiles({
        "/src/tsconfig.json": JSON.stringify({
          references: [
            { path: "./shared/tsconfig.json" },
            { path: "./webpack/tsconfig.json" },
          ],
          files: [],
        }),
        "/src/shared/tsconfig-base.json": JSON.stringify({
          include: ["./typings-base/"],
        }),
        "/src/shared/typings-base/globals.d.ts": `type Unrestricted = any;`,
        "/src/shared/tsconfig.json": JSON.stringify({
          extends: "./tsconfig-base.json",
          compilerOptions: {
            composite: true,
            outDir: "../target-tsc-build/",
            rootDir: "..",
          },
          files: ["./index.ts"],
        }),
        "/src/shared/index.ts": `export const a: Unrestricted = 1;`,
        "/src/webpack/tsconfig.json": JSON.stringify({
          extends: "../shared/tsconfig-base.json",
          compilerOptions: {
            composite: true,
            outDir: "../target-tsc-build/",
            rootDir: "..",
          },
          files: ["./index.ts"],
          references: [{ path: "../shared/tsconfig.json" }],
        }),
        "/src/webpack/index.ts": `export const b: Unrestricted = 1;`,
      });
    }
    verifyTsc({
      scenario: "configFileExtends",
      subScenario: "when building solution with projects extends config with include",
      fs: getConfigExtendsWithIncludeFs,
      commandLineArgs: ["--b", "/src/tsconfig.json", "--v", "--listFiles"],
    });
    verifyTsc({
      scenario: "configFileExtends",
      subScenario: "when building project uses reference and both extend config with include",
      fs: getConfigExtendsWithIncludeFs,
      commandLineArgs: [
        "--b",
        "/src/webpack/tsconfig.json",
        "--v",
        "--listFiles",
      ],
    });
  });
}
