/*@internal*/
namespace ts {
  export function transformNodeModule(context: TransformationContext) {
    const previousOnSubstituteNode = context.onSubstituteNode;
    const previousOnEmitNode = context.onEmitNode;

    const esmTransform = transformECMAScriptModule(context);

    const esmOnSubstituteNode = context.onSubstituteNode;
    const esmOnEmitNode = context.onEmitNode;

    context.onSubstituteNode = previousOnSubstituteNode;
    context.onEmitNode = previousOnEmitNode;

    const cjsTransform = transformModule(context);

    const cjsOnSubstituteNode = context.onSubstituteNode;
    const cjsOnEmitNode = context.onEmitNode;

    context.onSubstituteNode = onSubstituteNode;
    context.onEmitNode = onEmitNode;
    context.enableSubstitution(SyntaxKind.SourceFile);
    context.enableEmitNotification(SyntaxKind.SourceFile);

    let currentSourceFile: SourceFile | undefined;
    return transformSourceFileOrBundle;

    function onSubstituteNode(hint: EmitHint, node: Node) {
      if (isSourceFile(node)) {
        currentSourceFile = node;
        // Neither component transform wants substitution notifications for `SourceFile`s, and, in fact, relies on
        // the source file emit notification to setup scope variables for substitutions (so we _cannot_ call their substitute
        // functions on source files safely, as that context only gets setup in a later pipeline phase!)
        return previousOnSubstituteNode(hint, node);
      } else {
        if (!currentSourceFile) {
          return previousOnSubstituteNode(hint, node);
        }
        if (currentSourceFile.impliedNodeFormat === ModuleKind.ESNext) {
          return esmOnSubstituteNode(hint, node);
        }
        return cjsOnSubstituteNode(hint, node);
      }
    }

    function onEmitNode(
      hint: EmitHint,
      node: Node,
      emitCallback: (hint: EmitHint, node: Node) => void,
    ): void {
      if (isSourceFile(node)) {
        currentSourceFile = node;
      }
      if (!currentSourceFile) {
        return previousOnEmitNode(hint, node, emitCallback);
      }
      if (currentSourceFile.impliedNodeFormat === ModuleKind.ESNext) {
        return esmOnEmitNode(hint, node, emitCallback);
      }
      return cjsOnEmitNode(hint, node, emitCallback);
    }

    function getModuleTransformForFile(file: SourceFile): typeof esmTransform {
      return file.impliedNodeFormat === ModuleKind.ESNext
        ? esmTransform
        : cjsTransform;
    }

    function transformSourceFile(node: SourceFile) {
      if (node.isDeclarationFile) {
        return node;
      }

      currentSourceFile = node;
      const result = getModuleTransformForFile(node)(node);
      currentSourceFile = undefined;
      Debug.assert(isSourceFile(result));
      return result;
    }

    function transformSourceFileOrBundle(node: SourceFile | Bundle) {
      return node.kind === SyntaxKind.SourceFile
        ? transformSourceFile(node)
        : transformBundle(node);
    }

    function transformBundle(node: Bundle) {
      return context.factory.createBundle(
        map(node.sourceFiles, transformSourceFile),
        node.prepends,
      );
    }
  }
}
