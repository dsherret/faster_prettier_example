/* @internal */
namespace ts.codefix {
  const errorCodes = [
    Diagnostics
      .A_type_only_import_can_specify_a_default_import_or_named_bindings_but_not_both
      .code,
  ];
  const fixId = "splitTypeOnlyImport";
  registerCodeFix({
    errorCodes,
    fixIds: [fixId],
    getCodeActions: function getCodeActionsToSplitTypeOnlyImport(context) {
      const changes = textChanges.ChangeTracker.with(context, (t) => {
        return splitTypeOnlyImport(
          t,
          getImportDeclaration(context.sourceFile, context.span),
          context,
        );
      });
      if (changes.length) {
        return [
          createCodeFixAction(
            fixId,
            changes,
            Diagnostics.Split_into_two_separate_import_declarations,
            fixId,
            Diagnostics.Split_all_invalid_type_only_imports,
          ),
        ];
      }
    },
    getAllCodeActions: (context) =>
      codeFixAll(context, errorCodes, (changes, error) => {
        splitTypeOnlyImport(
          changes,
          getImportDeclaration(context.sourceFile, error),
          context,
        );
      }),
  });

  function getImportDeclaration(sourceFile: SourceFile, span: TextSpan) {
    return findAncestor(
      getTokenAtPosition(sourceFile, span.start),
      isImportDeclaration,
    );
  }

  function splitTypeOnlyImport(
    changes: textChanges.ChangeTracker,
    importDeclaration: ImportDeclaration | undefined,
    context: CodeFixContextBase,
  ) {
    if (!importDeclaration) {
      return;
    }
    const importClause = Debug.checkDefined(importDeclaration.importClause);
    changes.replaceNode(
      context.sourceFile,
      importDeclaration,
      factory.updateImportDeclaration(
        importDeclaration,
        importDeclaration.decorators,
        importDeclaration.modifiers,
        factory.updateImportClause(
          importClause,
          importClause.isTypeOnly,
          importClause.name,
          /*namedBindings*/ undefined,
        ),
        importDeclaration.moduleSpecifier,
        importDeclaration.assertClause,
      ),
    );

    changes.insertNodeAfter(
      context.sourceFile,
      importDeclaration,
      factory.createImportDeclaration(
        /*decorators*/ undefined,
        /*modifiers*/ undefined,
        factory.updateImportClause(
          importClause,
          importClause.isTypeOnly,
          /*name*/ undefined,
          importClause.namedBindings,
        ),
        importDeclaration.moduleSpecifier,
        importDeclaration.assertClause,
      ),
    );
  }
}
