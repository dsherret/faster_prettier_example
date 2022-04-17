/*@internal*/
namespace ts {
  export function transformJsx(context: TransformationContext) {
    interface PerFileState {
      importSpecifier?: string;
      filenameDeclaration?: VariableDeclaration & { name: Identifier };
      utilizedImplicitRuntimeImports?: Map<Map<ImportSpecifier>>;
    }

    const { factory, getEmitHelperFactory: emitHelpers } = context;
    const compilerOptions = context.getCompilerOptions();
    let currentSourceFile: SourceFile;
    let currentFileState!: PerFileState;

    return chainBundle(context, transformSourceFile);

    function getCurrentFileNameExpression(): Identifier {
      if (currentFileState.filenameDeclaration) {
        return currentFileState.filenameDeclaration.name;
      }
      const declaration = factory.createVariableDeclaration(
        factory.createUniqueName(
          "_jsxFileName",
          GeneratedIdentifierFlags.Optimistic |
            GeneratedIdentifierFlags.FileLevel
        ),
        /*exclaimationToken*/ undefined,
        /*type*/ undefined,
        factory.createStringLiteral(currentSourceFile.fileName)
      );
      currentFileState.filenameDeclaration =
        declaration as VariableDeclaration & { name: Identifier };
      return currentFileState.filenameDeclaration.name;
    }

    function getJsxFactoryCalleePrimitive(
      isStaticChildren: boolean
    ): "jsx" | "jsxs" | "jsxDEV" {
      return compilerOptions.jsx === JsxEmit.ReactJSXDev
        ? "jsxDEV"
        : isStaticChildren
        ? "jsxs"
        : "jsx";
    }

    function getJsxFactoryCallee(isStaticChildren: boolean) {
      const type = getJsxFactoryCalleePrimitive(isStaticChildren);
      return getImplicitImportForName(type);
    }

    function getImplicitJsxFragmentReference() {
      return getImplicitImportForName("Fragment");
    }

    function getImplicitImportForName(name: string) {
      const importSource =
        name === "createElement"
          ? currentFileState.importSpecifier!
          : getJSXRuntimeImport(
              currentFileState.importSpecifier,
              compilerOptions
            )!;
      const existing = currentFileState.utilizedImplicitRuntimeImports
        ?.get(importSource)
        ?.get(name);
      if (existing) {
        return existing.name;
      }
      if (!currentFileState.utilizedImplicitRuntimeImports) {
        currentFileState.utilizedImplicitRuntimeImports = new Map();
      }
      let specifierSourceImports =
        currentFileState.utilizedImplicitRuntimeImports.get(importSource);
      if (!specifierSourceImports) {
        specifierSourceImports = new Map();
        currentFileState.utilizedImplicitRuntimeImports.set(
          importSource,
          specifierSourceImports
        );
      }
      const generatedName = factory.createUniqueName(
        `_${name}`,
        GeneratedIdentifierFlags.Optimistic |
          GeneratedIdentifierFlags.FileLevel |
          GeneratedIdentifierFlags.AllowNameSubstitution
      );
      const specifier = factory.createImportSpecifier(
        /*isTypeOnly*/ false,
        factory.createIdentifier(name),
        generatedName
      );
      generatedName.generatedImportReference = specifier;
      specifierSourceImports.set(name, specifier);
      return generatedName;
    }

    /**
     * Transform JSX-specific syntax in a SourceFile.
     *
     * @param node A SourceFile node.
     */
    function transformSourceFile(node: SourceFile) {
      if (node.isDeclarationFile) {
        return node;
      }

      currentSourceFile = node;
      currentFileState = {};
      currentFileState.importSpecifier = getJSXImplicitImportBase(
        compilerOptions,
        node
      );
      let visited = visitEachChild(node, visitor, context);
      addEmitHelpers(visited, context.readEmitHelpers());
      let statements: readonly Statement[] = visited.statements;
      if (currentFileState.filenameDeclaration) {
        statements = insertStatementAfterCustomPrologue(
          statements.slice(),
          factory.createVariableStatement(
            /*modifiers*/ undefined,
            factory.createVariableDeclarationList(
              [currentFileState.filenameDeclaration],
              NodeFlags.Const
            )
          )
        );
      }
      if (currentFileState.utilizedImplicitRuntimeImports) {
        for (const [importSource, importSpecifiersMap] of arrayFrom(
          currentFileState.utilizedImplicitRuntimeImports.entries()
        )) {
          if (isExternalModule(node)) {
            // Add `import` statement
            const importStatement = factory.createImportDeclaration(
              /*decorators*/ undefined,
              /*modifiers*/ undefined,
              factory.createImportClause(
                /*typeOnly*/ false,
                /*name*/ undefined,
                factory.createNamedImports(
                  arrayFrom(importSpecifiersMap.values())
                )
              ),
              factory.createStringLiteral(importSource),
              /*assertClause*/ undefined
            );
            setParentRecursive(importStatement, /*incremental*/ false);
            statements = insertStatementAfterCustomPrologue(
              statements.slice(),
              importStatement
            );
          } else if (isExternalOrCommonJsModule(node)) {
            // Add `require` statement
            const requireStatement = factory.createVariableStatement(
              /*modifiers*/ undefined,
              factory.createVariableDeclarationList(
                [
                  factory.createVariableDeclaration(
                    factory.createObjectBindingPattern(
                      map(arrayFrom(importSpecifiersMap.values()), (s) =>
                        factory.createBindingElement(
                          /*dotdotdot*/ undefined,
                          s.propertyName,
                          s.name
                        )
                      )
                    ),
                    /*exclaimationToken*/ undefined,
                    /*type*/ undefined,
                    factory.createCallExpression(
                      factory.createIdentifier("require"),
                      /*typeArguments*/ undefined,
                      [factory.createStringLiteral(importSource)]
                    )
                  ),
                ],
                NodeFlags.Const
              )
            );
            setParentRecursive(requireStatement, /*incremental*/ false);
            statements = insertStatementAfterCustomPrologue(
              statements.slice(),
              requireStatement
            );
          } else {
            // Do nothing (script file) - consider an error in the checker?
          }
        }
      }
      if (statements !== visited.statements) {
        visited = factory.updateSourceFile(visited, statements);
      }
      currentFileState = undefined!;
      return visited;
    }

    function visitor(node: Node): VisitResult<Node> {
      if (node.transformFlags & TransformFlags.ContainsJsx) {
        return visitorWorker(node);
      } else {
        return node;
      }
    }

    function visitorWorker(node: Node): VisitResult<Node> {
      switch (node.kind) {
        case SyntaxKind.JsxElement:
          return visitJsxElement(node as JsxElement, /*isChild*/ false);

        case SyntaxKind.JsxSelfClosingElement:
          return visitJsxSelfClosingElement(
            node as JsxSelfClosingElement,
            /*isChild*/ false
          );

        case SyntaxKind.JsxFragment:
          return visitJsxFragment(node as JsxFragment, /*isChild*/ false);

        case SyntaxKind.JsxExpression:
          return visitJsxExpression(node as JsxExpression);

        default:
          return visitEachChild(node, visitor, context);
      }
    }

    function transformJsxChildToExpression(
      node: JsxChild
    ): Expression | undefined {
      switch (node.kind) {
        case SyntaxKind.JsxText:
          return visitJsxText(node);

        case SyntaxKind.JsxExpression:
          return visitJsxExpression(node);

        case SyntaxKind.JsxElement:
          return visitJsxElement(node, /*isChild*/ true);

        case SyntaxKind.JsxSelfClosingElement:
          return visitJsxSelfClosingElement(node, /*isChild*/ true);

        case SyntaxKind.JsxFragment:
          return visitJsxFragment(node, /*isChild*/ true);

        default:
          return Debug.failBadSyntaxKind(node);
      }
    }

    /**
     * The react jsx/jsxs transform falls back to `createElement` when an explicit `key` argument comes after a spread
     */
    function hasKeyAfterPropsSpread(node: JsxOpeningLikeElement) {
      let spread = false;
      for (const elem of node.attributes.properties) {
        if (isJsxSpreadAttribute(elem)) {
          spread = true;
        } else if (
          spread &&
          isJsxAttribute(elem) &&
          elem.name.escapedText === "key"
        ) {
          return true;
        }
      }
      return false;
    }

    function shouldUseCreateElement(node: JsxOpeningLikeElement) {
      return (
        currentFileState.importSpecifier === undefined ||
        hasKeyAfterPropsSpread(node)
      );
    }

    function visitJsxElement(node: JsxElement, isChild: boolean) {
      const tagTransform = shouldUseCreateElement(node.openingElement)
        ? visitJsxOpeningLikeElementCreateElement
        : visitJsxOpeningLikeElementJSX;
      return tagTransform(
        node.openingElement,
        node.children,
        isChild,
        /*location*/ node
      );
    }

    function visitJsxSelfClosingElement(
      node: JsxSelfClosingElement,
      isChild: boolean
    ) {
      const tagTransform = shouldUseCreateElement(node)
        ? visitJsxOpeningLikeElementCreateElement
        : visitJsxOpeningLikeElementJSX;
      return tagTransform(
        node,
        /*children*/ undefined,
        isChild,
        /*location*/ node
      );
    }

    function visitJsxFragment(node: JsxFragment, isChild: boolean) {
      const tagTransform =
        currentFileState.importSpecifier === undefined
          ? visitJsxOpeningFragmentCreateElement
          : visitJsxOpeningFragmentJSX;
      return tagTransform(
        node.openingFragment,
        node.children,
        isChild,
        /*location*/ node
      );
    }

    function convertJsxChildrenToChildrenPropObject(
      children: readonly JsxChild[]
    ) {
      const prop = convertJsxChildrenToChildrenPropAssignment(children);
      return prop && factory.createObjectLiteralExpression([prop]);
    }

    function convertJsxChildrenToChildrenPropAssignment(
      children: readonly JsxChild[]
    ) {
      const nonWhitespaceChildren = getSemanticJsxChildren(children);
      if (
        length(nonWhitespaceChildren) === 1 &&
        !(nonWhitespaceChildren[0] as JsxExpression).dotDotDotToken
      ) {
        const result = transformJsxChildToExpression(nonWhitespaceChildren[0]);
        return result && factory.createPropertyAssignment("children", result);
      }
      const result = mapDefined(children, transformJsxChildToExpression);
      return length(result)
        ? factory.createPropertyAssignment(
            "children",
            factory.createArrayLiteralExpression(result)
          )
        : undefined;
    }

    function visitJsxOpeningLikeElementJSX(
      node: JsxOpeningLikeElement,
      children: readonly JsxChild[] | undefined,
      isChild: boolean,
      location: TextRange
    ) {
      const tagName = getTagName(node);
      const childrenProp =
        children && children.length
          ? convertJsxChildrenToChildrenPropAssignment(children)
          : undefined;
      const keyAttr = find(
        node.attributes.properties,
        (p) => !!p.name && isIdentifier(p.name) && p.name.escapedText === "key"
      ) as JsxAttribute | undefined;
      const attrs = keyAttr
        ? filter(node.attributes.properties, (p) => p !== keyAttr)
        : node.attributes.properties;
      const objectProperties = length(attrs)
        ? transformJsxAttributesToObjectProps(attrs, childrenProp)
        : factory.createObjectLiteralExpression(
            childrenProp ? [childrenProp] : emptyArray
          ); // When there are no attributes, React wants {}
      return visitJsxOpeningLikeElementOrFragmentJSX(
        tagName,
        objectProperties,
        keyAttr,
        children || emptyArray,
        isChild,
        location
      );
    }

    function visitJsxOpeningLikeElementOrFragmentJSX(
      tagName: Expression,
      objectProperties: Expression,
      keyAttr: JsxAttribute | undefined,
      children: readonly JsxChild[],
      isChild: boolean,
      location: TextRange
    ) {
      const nonWhitespaceChildren = getSemanticJsxChildren(children);
      const isStaticChildren =
        length(nonWhitespaceChildren) > 1 ||
        !!(nonWhitespaceChildren[0] as JsxExpression)?.dotDotDotToken;
      const args: Expression[] = [tagName, objectProperties];
      // function jsx(type, config, maybeKey) {}
      // "maybeKey" is optional. It is acceptable to use "_jsx" without a third argument
      if (keyAttr) {
        args.push(transformJsxAttributeInitializer(keyAttr.initializer));
      }
      if (compilerOptions.jsx === JsxEmit.ReactJSXDev) {
        const originalFile = getOriginalNode(currentSourceFile);
        if (originalFile && isSourceFile(originalFile)) {
          // "maybeKey" has to be replaced with "void 0" to not break the jsxDEV signature
          if (keyAttr === undefined) {
            args.push(factory.createVoidZero());
          }
          // isStaticChildren development flag
          args.push(
            isStaticChildren ? factory.createTrue() : factory.createFalse()
          );
          // __source development flag
          const lineCol = getLineAndCharacterOfPosition(
            originalFile,
            location.pos
          );
          args.push(
            factory.createObjectLiteralExpression([
              factory.createPropertyAssignment(
                "fileName",
                getCurrentFileNameExpression()
              ),
              factory.createPropertyAssignment(
                "lineNumber",
                factory.createNumericLiteral(lineCol.line + 1)
              ),
              factory.createPropertyAssignment(
                "columnNumber",
                factory.createNumericLiteral(lineCol.character + 1)
              ),
            ])
          );
          // __self development flag
          args.push(factory.createThis());
        }
      }

      const element = setTextRange(
        factory.createCallExpression(
          getJsxFactoryCallee(isStaticChildren),
          /*typeArguments*/ undefined,
          args
        ),
        location
      );

      if (isChild) {
        startOnNewLine(element);
      }

      return element;
    }

    function visitJsxOpeningLikeElementCreateElement(
      node: JsxOpeningLikeElement,
      children: readonly JsxChild[] | undefined,
      isChild: boolean,
      location: TextRange
    ) {
      const tagName = getTagName(node);
      const attrs = node.attributes.properties;
      const objectProperties = length(attrs)
        ? transformJsxAttributesToObjectProps(attrs)
        : factory.createNull(); // When there are no attributes, React wants "null"

      const callee =
        currentFileState.importSpecifier === undefined
          ? createJsxFactoryExpression(
              factory,
              context.getEmitResolver().getJsxFactoryEntity(currentSourceFile),
              compilerOptions.reactNamespace!, // TODO: GH#18217
              node
            )
          : getImplicitImportForName("createElement");

      const element = createExpressionForJsxElement(
        factory,
        callee,
        tagName,
        objectProperties,
        mapDefined(children, transformJsxChildToExpression),
        location
      );

      if (isChild) {
        startOnNewLine(element);
      }

      return element;
    }

    function visitJsxOpeningFragmentJSX(
      _node: JsxOpeningFragment,
      children: readonly JsxChild[],
      isChild: boolean,
      location: TextRange
    ) {
      let childrenProps: Expression | undefined;
      if (children && children.length) {
        const result = convertJsxChildrenToChildrenPropObject(children);
        if (result) {
          childrenProps = result;
        }
      }
      return visitJsxOpeningLikeElementOrFragmentJSX(
        getImplicitJsxFragmentReference(),
        childrenProps || factory.createObjectLiteralExpression([]),
        /*keyAttr*/ undefined,
        children,
        isChild,
        location
      );
    }

    function visitJsxOpeningFragmentCreateElement(
      node: JsxOpeningFragment,
      children: readonly JsxChild[],
      isChild: boolean,
      location: TextRange
    ) {
      const element = createExpressionForJsxFragment(
        factory,
        context.getEmitResolver().getJsxFactoryEntity(currentSourceFile),
        context
          .getEmitResolver()
          .getJsxFragmentFactoryEntity(currentSourceFile),
        compilerOptions.reactNamespace!, // TODO: GH#18217
        mapDefined(children, transformJsxChildToExpression),
        node,
        location
      );

      if (isChild) {
        startOnNewLine(element);
      }

      return element;
    }

    function transformJsxSpreadAttributeToSpreadAssignment(
      node: JsxSpreadAttribute
    ) {
      return factory.createSpreadAssignment(
        visitNode(node.expression, visitor, isExpression)
      );
    }

    function transformJsxAttributesToObjectProps(
      attrs: readonly (JsxSpreadAttribute | JsxAttribute)[],
      children?: PropertyAssignment
    ) {
      const target = getEmitScriptTarget(compilerOptions);
      return target && target >= ScriptTarget.ES2018
        ? factory.createObjectLiteralExpression(
            transformJsxAttributesToProps(attrs, children)
          )
        : transformJsxAttributesToExpression(attrs, children);
    }

    function transformJsxAttributesToProps(
      attrs: readonly (JsxSpreadAttribute | JsxAttribute)[],
      children?: PropertyAssignment
    ) {
      const props = flatten<SpreadAssignment | PropertyAssignment>(
        spanMap(attrs, isJsxSpreadAttribute, (attrs, isSpread) =>
          map(attrs, (attr) =>
            isSpread
              ? transformJsxSpreadAttributeToSpreadAssignment(
                  attr as JsxSpreadAttribute
                )
              : transformJsxAttributeToObjectLiteralElement(
                  attr as JsxAttribute
                )
          )
        )
      );
      if (children) {
        props.push(children);
      }
      return props;
    }

    function transformJsxAttributesToExpression(
      attrs: readonly (JsxSpreadAttribute | JsxAttribute)[],
      children?: PropertyAssignment
    ) {
      // Map spans of JsxAttribute nodes into object literals and spans
      // of JsxSpreadAttribute nodes into expressions.
      const expressions = flatten(
        spanMap(attrs, isJsxSpreadAttribute, (attrs, isSpread) =>
          isSpread
            ? map(attrs, transformJsxSpreadAttributeToExpression)
            : factory.createObjectLiteralExpression(
                map(attrs, transformJsxAttributeToObjectLiteralElement)
              )
        )
      );

      if (isJsxSpreadAttribute(attrs[0])) {
        // We must always emit at least one object literal before a spread
        // argument.factory.createObjectLiteral
        expressions.unshift(factory.createObjectLiteralExpression());
      }

      if (children) {
        expressions.push(factory.createObjectLiteralExpression([children]));
      }

      return (
        singleOrUndefined(expressions) ||
        emitHelpers().createAssignHelper(expressions)
      );
    }

    function transformJsxSpreadAttributeToExpression(node: JsxSpreadAttribute) {
      return visitNode(node.expression, visitor, isExpression);
    }

    function transformJsxAttributeToObjectLiteralElement(node: JsxAttribute) {
      const name = getAttributeName(node);
      const expression = transformJsxAttributeInitializer(node.initializer);
      return factory.createPropertyAssignment(name, expression);
    }

    function transformJsxAttributeInitializer(
      node: StringLiteral | JsxExpression | undefined
    ): Expression {
      if (node === undefined) {
        return factory.createTrue();
      } else if (node.kind === SyntaxKind.StringLiteral) {
        // Always recreate the literal to escape any escape sequences or newlines which may be in the original jsx string and which
        // Need to be escaped to be handled correctly in a normal string
        const singleQuote =
          node.singleQuote !== undefined
            ? node.singleQuote
            : !isStringDoubleQuoted(node, currentSourceFile);
        const literal = factory.createStringLiteral(
          tryDecodeEntities(node.text) || node.text,
          singleQuote
        );
        return setTextRange(literal, node);
      } else if (node.kind === SyntaxKind.JsxExpression) {
        if (node.expression === undefined) {
          return factory.createTrue();
        }
        return visitNode(node.expression, visitor, isExpression);
      } else {
        return Debug.failBadSyntaxKind(node);
      }
    }

    function visitJsxText(node: JsxText): StringLiteral | undefined {
      const fixed = fixupWhitespaceAndDecodeEntities(node.text);
      return fixed === undefined
        ? undefined
        : factory.createStringLiteral(fixed);
    }

    /**
     * JSX trims whitespace at the end and beginning of lines, except that the
     * start/end of a tag is considered a start/end of a line only if that line is
     * on the same line as the closing tag. See examples in
     * tests/cases/conformance/jsx/tsxReactEmitWhitespace.tsx
     * See also https://www.w3.org/TR/html4/struct/text.html#h-9.1 and https://www.w3.org/TR/CSS2/text.html#white-space-model
     *
     * An equivalent algorithm would be:
     * - If there is only one line, return it.
     * - If there is only whitespace (but multiple lines), return `undefined`.
     * - Split the text into lines.
     * - 'trimRight' the first line, 'trimLeft' the last line, 'trim' middle lines.
     * - Decode entities on each line (individually).
     * - Remove empty lines and join the rest with " ".
     */
    function fixupWhitespaceAndDecodeEntities(
      text: string
    ): string | undefined {
      let acc: string | undefined;
      // First non-whitespace character on this line.
      let firstNonWhitespace = 0;
      // Last non-whitespace character on this line.
      let lastNonWhitespace = -1;
      // These initial values are special because the first line is:
      // firstNonWhitespace = 0 to indicate that we want leading whitsepace,
      // but lastNonWhitespace = -1 as a special flag to indicate that we *don't* include the line if it's all whitespace.

      for (let i = 0; i < text.length; i++) {
        const c = text.charCodeAt(i);
        if (isLineBreak(c)) {
          // If we've seen any non-whitespace characters on this line, add the 'trim' of the line.
          // (lastNonWhitespace === -1 is a special flag to detect whether the first line is all whitespace.)
          if (firstNonWhitespace !== -1 && lastNonWhitespace !== -1) {
            acc = addLineOfJsxText(
              acc,
              text.substr(
                firstNonWhitespace,
                lastNonWhitespace - firstNonWhitespace + 1
              )
            );
          }

          // Reset firstNonWhitespace for the next line.
          // Don't bother to reset lastNonWhitespace because we ignore it if firstNonWhitespace = -1.
          firstNonWhitespace = -1;
        } else if (!isWhiteSpaceSingleLine(c)) {
          lastNonWhitespace = i;
          if (firstNonWhitespace === -1) {
            firstNonWhitespace = i;
          }
        }
      }

      return firstNonWhitespace !== -1
        ? // Last line had a non-whitespace character. Emit the 'trimLeft', meaning keep trailing whitespace.
          addLineOfJsxText(acc, text.substr(firstNonWhitespace))
        : // Last line was all whitespace, so ignore it
          acc;
    }

    function addLineOfJsxText(
      acc: string | undefined,
      trimmedLine: string
    ): string {
      // We do not escape the string here as that is handled by the printer
      // when it emits the literal. We do, however, need to decode JSX entities.
      const decoded = decodeEntities(trimmedLine);
      return acc === undefined ? decoded : acc + " " + decoded;
    }

    /**
     * Replace entities like "&nbsp;", "&#123;", and "&#xDEADBEEF;" with the characters they encode.
     * See https://en.wikipedia.org/wiki/List_of_XML_and_HTML_character_entity_references
     */
    function decodeEntities(text: string): string {
      return text.replace(
        /&((#((\d+)|x([\da-fA-F]+)))|(\w+));/g,
        (match, _all, _number, _digits, decimal, hex, word) => {
          if (decimal) {
            return utf16EncodeAsString(parseInt(decimal, 10));
          } else if (hex) {
            return utf16EncodeAsString(parseInt(hex, 16));
          } else {
            const ch = entities.get(word);
            // If this is not a valid entity, then just use `match` (replace it with itself, i.e. don't replace)
            return ch ? utf16EncodeAsString(ch) : match;
          }
        }
      );
    }

    /** Like `decodeEntities` but returns `undefined` if there were no entities to decode. */
    function tryDecodeEntities(text: string): string | undefined {
      const decoded = decodeEntities(text);
      return decoded === text ? undefined : decoded;
    }

    function getTagName(node: JsxElement | JsxOpeningLikeElement): Expression {
      if (node.kind === SyntaxKind.JsxElement) {
        return getTagName(node.openingElement);
      } else {
        const name = node.tagName;
        if (isIdentifier(name) && isIntrinsicJsxName(name.escapedText)) {
          return factory.createStringLiteral(idText(name));
        } else {
          return createExpressionFromEntityName(factory, name);
        }
      }
    }

    /**
     * Emit an attribute name, which is quoted if it needs to be quoted. Because
     * these emit into an object literal property name, we don't need to be worried
     * about keywords, just non-identifier characters
     */
    function getAttributeName(node: JsxAttribute): StringLiteral | Identifier {
      const name = node.name;
      const text = idText(name);
      if (/^[A-Za-z_]\w*$/.test(text)) {
        return name;
      } else {
        return factory.createStringLiteral(text);
      }
    }

    function visitJsxExpression(node: JsxExpression) {
      const expression = visitNode(node.expression, visitor, isExpression);
      return node.dotDotDotToken
        ? factory.createSpreadElement(expression!)
        : expression;
    }
  }

  const entities = new Map(
    getEntries({
      quot: 0x0022,
      amp: 0x0026,
      apos: 0x0027,
      lt: 0x003c,
      gt: 0x003e,
      nbsp: 0x00a0,
      iexcl: 0x00a1,
      cent: 0x00a2,
      pound: 0x00a3,
      curren: 0x00a4,
      yen: 0x00a5,
      brvbar: 0x00a6,
      sect: 0x00a7,
      uml: 0x00a8,
      copy: 0x00a9,
      ordf: 0x00aa,
      laquo: 0x00ab,
      not: 0x00ac,
      shy: 0x00ad,
      reg: 0x00ae,
      macr: 0x00af,
      deg: 0x00b0,
      plusmn: 0x00b1,
      sup2: 0x00b2,
      sup3: 0x00b3,
      acute: 0x00b4,
      micro: 0x00b5,
      para: 0x00b6,
      middot: 0x00b7,
      cedil: 0x00b8,
      sup1: 0x00b9,
      ordm: 0x00ba,
      raquo: 0x00bb,
      frac14: 0x00bc,
      frac12: 0x00bd,
      frac34: 0x00be,
      iquest: 0x00bf,
      Agrave: 0x00c0,
      Aacute: 0x00c1,
      Acirc: 0x00c2,
      Atilde: 0x00c3,
      Auml: 0x00c4,
      Aring: 0x00c5,
      AElig: 0x00c6,
      Ccedil: 0x00c7,
      Egrave: 0x00c8,
      Eacute: 0x00c9,
      Ecirc: 0x00ca,
      Euml: 0x00cb,
      Igrave: 0x00cc,
      Iacute: 0x00cd,
      Icirc: 0x00ce,
      Iuml: 0x00cf,
      ETH: 0x00d0,
      Ntilde: 0x00d1,
      Ograve: 0x00d2,
      Oacute: 0x00d3,
      Ocirc: 0x00d4,
      Otilde: 0x00d5,
      Ouml: 0x00d6,
      times: 0x00d7,
      Oslash: 0x00d8,
      Ugrave: 0x00d9,
      Uacute: 0x00da,
      Ucirc: 0x00db,
      Uuml: 0x00dc,
      Yacute: 0x00dd,
      THORN: 0x00de,
      szlig: 0x00df,
      agrave: 0x00e0,
      aacute: 0x00e1,
      acirc: 0x00e2,
      atilde: 0x00e3,
      auml: 0x00e4,
      aring: 0x00e5,
      aelig: 0x00e6,
      ccedil: 0x00e7,
      egrave: 0x00e8,
      eacute: 0x00e9,
      ecirc: 0x00ea,
      euml: 0x00eb,
      igrave: 0x00ec,
      iacute: 0x00ed,
      icirc: 0x00ee,
      iuml: 0x00ef,
      eth: 0x00f0,
      ntilde: 0x00f1,
      ograve: 0x00f2,
      oacute: 0x00f3,
      ocirc: 0x00f4,
      otilde: 0x00f5,
      ouml: 0x00f6,
      divide: 0x00f7,
      oslash: 0x00f8,
      ugrave: 0x00f9,
      uacute: 0x00fa,
      ucirc: 0x00fb,
      uuml: 0x00fc,
      yacute: 0x00fd,
      thorn: 0x00fe,
      yuml: 0x00ff,
      OElig: 0x0152,
      oelig: 0x0153,
      Scaron: 0x0160,
      scaron: 0x0161,
      Yuml: 0x0178,
      fnof: 0x0192,
      circ: 0x02c6,
      tilde: 0x02dc,
      Alpha: 0x0391,
      Beta: 0x0392,
      Gamma: 0x0393,
      Delta: 0x0394,
      Epsilon: 0x0395,
      Zeta: 0x0396,
      Eta: 0x0397,
      Theta: 0x0398,
      Iota: 0x0399,
      Kappa: 0x039a,
      Lambda: 0x039b,
      Mu: 0x039c,
      Nu: 0x039d,
      Xi: 0x039e,
      Omicron: 0x039f,
      Pi: 0x03a0,
      Rho: 0x03a1,
      Sigma: 0x03a3,
      Tau: 0x03a4,
      Upsilon: 0x03a5,
      Phi: 0x03a6,
      Chi: 0x03a7,
      Psi: 0x03a8,
      Omega: 0x03a9,
      alpha: 0x03b1,
      beta: 0x03b2,
      gamma: 0x03b3,
      delta: 0x03b4,
      epsilon: 0x03b5,
      zeta: 0x03b6,
      eta: 0x03b7,
      theta: 0x03b8,
      iota: 0x03b9,
      kappa: 0x03ba,
      lambda: 0x03bb,
      mu: 0x03bc,
      nu: 0x03bd,
      xi: 0x03be,
      omicron: 0x03bf,
      pi: 0x03c0,
      rho: 0x03c1,
      sigmaf: 0x03c2,
      sigma: 0x03c3,
      tau: 0x03c4,
      upsilon: 0x03c5,
      phi: 0x03c6,
      chi: 0x03c7,
      psi: 0x03c8,
      omega: 0x03c9,
      thetasym: 0x03d1,
      upsih: 0x03d2,
      piv: 0x03d6,
      ensp: 0x2002,
      emsp: 0x2003,
      thinsp: 0x2009,
      zwnj: 0x200c,
      zwj: 0x200d,
      lrm: 0x200e,
      rlm: 0x200f,
      ndash: 0x2013,
      mdash: 0x2014,
      lsquo: 0x2018,
      rsquo: 0x2019,
      sbquo: 0x201a,
      ldquo: 0x201c,
      rdquo: 0x201d,
      bdquo: 0x201e,
      dagger: 0x2020,
      Dagger: 0x2021,
      bull: 0x2022,
      hellip: 0x2026,
      permil: 0x2030,
      prime: 0x2032,
      Prime: 0x2033,
      lsaquo: 0x2039,
      rsaquo: 0x203a,
      oline: 0x203e,
      frasl: 0x2044,
      euro: 0x20ac,
      image: 0x2111,
      weierp: 0x2118,
      real: 0x211c,
      trade: 0x2122,
      alefsym: 0x2135,
      larr: 0x2190,
      uarr: 0x2191,
      rarr: 0x2192,
      darr: 0x2193,
      harr: 0x2194,
      crarr: 0x21b5,
      lArr: 0x21d0,
      uArr: 0x21d1,
      rArr: 0x21d2,
      dArr: 0x21d3,
      hArr: 0x21d4,
      forall: 0x2200,
      part: 0x2202,
      exist: 0x2203,
      empty: 0x2205,
      nabla: 0x2207,
      isin: 0x2208,
      notin: 0x2209,
      ni: 0x220b,
      prod: 0x220f,
      sum: 0x2211,
      minus: 0x2212,
      lowast: 0x2217,
      radic: 0x221a,
      prop: 0x221d,
      infin: 0x221e,
      ang: 0x2220,
      and: 0x2227,
      or: 0x2228,
      cap: 0x2229,
      cup: 0x222a,
      int: 0x222b,
      there4: 0x2234,
      sim: 0x223c,
      cong: 0x2245,
      asymp: 0x2248,
      ne: 0x2260,
      equiv: 0x2261,
      le: 0x2264,
      ge: 0x2265,
      sub: 0x2282,
      sup: 0x2283,
      nsub: 0x2284,
      sube: 0x2286,
      supe: 0x2287,
      oplus: 0x2295,
      otimes: 0x2297,
      perp: 0x22a5,
      sdot: 0x22c5,
      lceil: 0x2308,
      rceil: 0x2309,
      lfloor: 0x230a,
      rfloor: 0x230b,
      lang: 0x2329,
      rang: 0x232a,
      loz: 0x25ca,
      spades: 0x2660,
      clubs: 0x2663,
      hearts: 0x2665,
      diams: 0x2666,
    })
  );
}
