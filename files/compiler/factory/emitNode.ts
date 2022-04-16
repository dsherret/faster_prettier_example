namespace ts {
  /**
   * Associates a node with the current transformation, initializing
   * various transient transformation properties.
   * @internal
   */
  export function getOrCreateEmitNode(node: Node): EmitNode {
    if (!node.emitNode) {
      if (isParseTreeNode(node)) {
        // To avoid holding onto transformation artifacts, we keep track of any
        // parse tree node we are annotating. This allows us to clean them up after
        // all transformations have completed.
        if (node.kind === SyntaxKind.SourceFile) {
          return (node.emitNode = { annotatedNodes: [node] } as EmitNode);
        }

        const sourceFile =
          getSourceFileOfNode(getParseTreeNode(getSourceFileOfNode(node))) ??
          Debug.fail("Could not determine parsed source file.");
        getOrCreateEmitNode(sourceFile).annotatedNodes!.push(node);
      }

      node.emitNode = {} as EmitNode;
    } else {
      Debug.assert(
        !(node.emitNode.flags & EmitFlags.Immutable),
        "Invalid attempt to mutate an immutable node."
      );
    }
    return node.emitNode;
  }

  /**
   * Clears any `EmitNode` entries from parse-tree nodes.
   * @param sourceFile A source file.
   */
  export function disposeEmitNodes(sourceFile: SourceFile | undefined) {
    // During transformation we may need to annotate a parse tree node with transient
    // transformation properties. As parse tree nodes live longer than transformation
    // nodes, we need to make sure we reclaim any memory allocated for custom ranges
    // from these nodes to ensure we do not hold onto entire subtrees just for position
    // information. We also need to reset these nodes to a pre-transformation state
    // for incremental parsing scenarios so that we do not impact later emit.
    const annotatedNodes = getSourceFileOfNode(getParseTreeNode(sourceFile))
      ?.emitNode?.annotatedNodes;
    if (annotatedNodes) {
      for (const node of annotatedNodes) {
        node.emitNode = undefined;
      }
    }
  }

  /**
   * Sets `EmitFlags.NoComments` on a node and removes any leading and trailing synthetic comments.
   * @internal
   */
  export function removeAllComments<T extends Node>(node: T): T {
    const emitNode = getOrCreateEmitNode(node);
    emitNode.flags |= EmitFlags.NoComments;
    emitNode.leadingComments = undefined;
    emitNode.trailingComments = undefined;
    return node;
  }

  /**
   * Sets flags that control emit behavior of a node.
   */
  export function setEmitFlags<T extends Node>(node: T, emitFlags: EmitFlags) {
    getOrCreateEmitNode(node).flags = emitFlags;
    return node;
  }

  /**
   * Sets flags that control emit behavior of a node.
   */
  /* @internal */
  export function addEmitFlags<T extends Node>(node: T, emitFlags: EmitFlags) {
    const emitNode = getOrCreateEmitNode(node);
    emitNode.flags = emitNode.flags | emitFlags;
    return node;
  }

  /**
   * Gets a custom text range to use when emitting source maps.
   */
  export function getSourceMapRange(node: Node): SourceMapRange {
    return node.emitNode?.sourceMapRange ?? node;
  }

  /**
   * Sets a custom text range to use when emitting source maps.
   */
  export function setSourceMapRange<T extends Node>(
    node: T,
    range: SourceMapRange | undefined
  ) {
    getOrCreateEmitNode(node).sourceMapRange = range;
    return node;
  }

  /**
   * Gets the TextRange to use for source maps for a token of a node.
   */
  export function getTokenSourceMapRange(
    node: Node,
    token: SyntaxKind
  ): SourceMapRange | undefined {
    return node.emitNode?.tokenSourceMapRanges?.[token];
  }

  /**
   * Sets the TextRange to use for source maps for a token of a node.
   */
  export function setTokenSourceMapRange<T extends Node>(
    node: T,
    token: SyntaxKind,
    range: SourceMapRange | undefined
  ) {
    const emitNode = getOrCreateEmitNode(node);
    const tokenSourceMapRanges =
      emitNode.tokenSourceMapRanges ?? (emitNode.tokenSourceMapRanges = []);
    tokenSourceMapRanges[token] = range;
    return node;
  }

  /**
   * Gets a custom text range to use when emitting comments.
   */
  /*@internal*/
  export function getStartsOnNewLine(node: Node) {
    return node.emitNode?.startsOnNewLine;
  }

  /**
   * Sets a custom text range to use when emitting comments.
   */
  /*@internal*/
  export function setStartsOnNewLine<T extends Node>(
    node: T,
    newLine: boolean
  ) {
    getOrCreateEmitNode(node).startsOnNewLine = newLine;
    return node;
  }

  /**
   * Gets a custom text range to use when emitting comments.
   */
  export function getCommentRange(node: Node): TextRange {
    return node.emitNode?.commentRange ?? node;
  }

  /**
   * Sets a custom text range to use when emitting comments.
   */
  export function setCommentRange<T extends Node>(node: T, range: TextRange) {
    getOrCreateEmitNode(node).commentRange = range;
    return node;
  }

  export function getSyntheticLeadingComments(
    node: Node
  ): SynthesizedComment[] | undefined {
    return node.emitNode?.leadingComments;
  }

  export function setSyntheticLeadingComments<T extends Node>(
    node: T,
    comments: SynthesizedComment[] | undefined
  ) {
    getOrCreateEmitNode(node).leadingComments = comments;
    return node;
  }

  export function addSyntheticLeadingComment<T extends Node>(
    node: T,
    kind:
      | SyntaxKind.SingleLineCommentTrivia
      | SyntaxKind.MultiLineCommentTrivia,
    text: string,
    hasTrailingNewLine?: boolean
  ) {
    return setSyntheticLeadingComments(
      node,
      append<SynthesizedComment>(getSyntheticLeadingComments(node), {
        kind,
        pos: -1,
        end: -1,
        hasTrailingNewLine,
        text,
      })
    );
  }

  export function getSyntheticTrailingComments(
    node: Node
  ): SynthesizedComment[] | undefined {
    return node.emitNode?.trailingComments;
  }

  export function setSyntheticTrailingComments<T extends Node>(
    node: T,
    comments: SynthesizedComment[] | undefined
  ) {
    getOrCreateEmitNode(node).trailingComments = comments;
    return node;
  }

  export function addSyntheticTrailingComment<T extends Node>(
    node: T,
    kind:
      | SyntaxKind.SingleLineCommentTrivia
      | SyntaxKind.MultiLineCommentTrivia,
    text: string,
    hasTrailingNewLine?: boolean
  ) {
    return setSyntheticTrailingComments(
      node,
      append<SynthesizedComment>(getSyntheticTrailingComments(node), {
        kind,
        pos: -1,
        end: -1,
        hasTrailingNewLine,
        text,
      })
    );
  }

  export function moveSyntheticComments<T extends Node>(
    node: T,
    original: Node
  ): T {
    setSyntheticLeadingComments(node, getSyntheticLeadingComments(original));
    setSyntheticTrailingComments(node, getSyntheticTrailingComments(original));
    const emit = getOrCreateEmitNode(original);
    emit.leadingComments = undefined;
    emit.trailingComments = undefined;
    return node;
  }

  /**
   * Gets the constant value to emit for an expression representing an enum.
   */
  export function getConstantValue(
    node: AccessExpression
  ): string | number | undefined {
    return node.emitNode?.constantValue;
  }

  /**
   * Sets the constant value to emit for an expression.
   */
  export function setConstantValue(
    node: AccessExpression,
    value: string | number
  ): AccessExpression {
    const emitNode = getOrCreateEmitNode(node);
    emitNode.constantValue = value;
    return node;
  }

  /**
   * Adds an EmitHelper to a node.
   */
  export function addEmitHelper<T extends Node>(
    node: T,
    helper: EmitHelper
  ): T {
    const emitNode = getOrCreateEmitNode(node);
    emitNode.helpers = append(emitNode.helpers, helper);
    return node;
  }

  /**
   * Add EmitHelpers to a node.
   */
  export function addEmitHelpers<T extends Node>(
    node: T,
    helpers: EmitHelper[] | undefined
  ): T {
    if (some(helpers)) {
      const emitNode = getOrCreateEmitNode(node);
      for (const helper of helpers) {
        emitNode.helpers = appendIfUnique(emitNode.helpers, helper);
      }
    }
    return node;
  }

  /**
   * Removes an EmitHelper from a node.
   */
  export function removeEmitHelper(node: Node, helper: EmitHelper): boolean {
    const helpers = node.emitNode?.helpers;
    if (helpers) {
      return orderedRemoveItem(helpers, helper);
    }
    return false;
  }

  /**
   * Gets the EmitHelpers of a node.
   */
  export function getEmitHelpers(node: Node): EmitHelper[] | undefined {
    return node.emitNode?.helpers;
  }

  /**
   * Moves matching emit helpers from a source node to a target node.
   */
  export function moveEmitHelpers(
    source: Node,
    target: Node,
    predicate: (helper: EmitHelper) => boolean
  ) {
    const sourceEmitNode = source.emitNode;
    const sourceEmitHelpers = sourceEmitNode && sourceEmitNode.helpers;
    if (!some(sourceEmitHelpers)) return;

    const targetEmitNode = getOrCreateEmitNode(target);
    let helpersRemoved = 0;
    for (let i = 0; i < sourceEmitHelpers.length; i++) {
      const helper = sourceEmitHelpers[i];
      if (predicate(helper)) {
        helpersRemoved++;
        targetEmitNode.helpers = appendIfUnique(targetEmitNode.helpers, helper);
      } else if (helpersRemoved > 0) {
        sourceEmitHelpers[i - helpersRemoved] = helper;
      }
    }

    if (helpersRemoved > 0) {
      sourceEmitHelpers.length -= helpersRemoved;
    }
  }

  /**
   * Gets the SnippetElement of a node.
   */
  /* @internal */
  export function getSnippetElement(node: Node): SnippetElement | undefined {
    return node.emitNode?.snippetElement;
  }

  /**
   * Sets the SnippetElement of a node.
   */
  /* @internal */
  export function setSnippetElement<T extends Node>(
    node: T,
    snippet: SnippetElement
  ): T {
    const emitNode = getOrCreateEmitNode(node);
    emitNode.snippetElement = snippet;
    return node;
  }

  /* @internal */
  export function ignoreSourceNewlines<T extends Node>(node: T): T {
    getOrCreateEmitNode(node).flags |= EmitFlags.IgnoreSourceNewlines;
    return node;
  }

  /* @internal */
  export function setTypeNode<T extends Node>(node: T, type: TypeNode): T {
    const emitNode = getOrCreateEmitNode(node);
    emitNode.typeNode = type;
    return node;
  }

  /* @internal */
  export function getTypeNode<T extends Node>(node: T): TypeNode | undefined {
    return node.emitNode?.typeNode;
  }
}
