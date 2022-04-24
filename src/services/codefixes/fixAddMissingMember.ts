/* @internal */
namespace ts.codefix {
  const fixMissingMember = "fixMissingMember";
  const fixMissingProperties = "fixMissingProperties";
  const fixMissingAttributes = "fixMissingAttributes";
  const fixMissingFunctionDeclaration = "fixMissingFunctionDeclaration";

  const errorCodes = [
    Diagnostics.Property_0_does_not_exist_on_type_1.code,
    Diagnostics.Property_0_does_not_exist_on_type_1_Did_you_mean_2.code,
    Diagnostics.Property_0_is_missing_in_type_1_but_required_in_type_2.code,
    Diagnostics.Type_0_is_missing_the_following_properties_from_type_1_Colon_2
      .code,
    Diagnostics
      .Type_0_is_missing_the_following_properties_from_type_1_Colon_2_and_3_more
      .code,
    Diagnostics.Argument_of_type_0_is_not_assignable_to_parameter_of_type_1
      .code,
    Diagnostics.Cannot_find_name_0.code,
  ];

  registerCodeFix({
    errorCodes,
    getCodeActions(context) {
      const typeChecker = context.program.getTypeChecker();
      const info = getInfo(
        context.sourceFile,
        context.span.start,
        context.errorCode,
        typeChecker,
        context.program,
      );
      if (!info) {
        return undefined;
      }
      if (info.kind === InfoKind.ObjectLiteral) {
        const changes = textChanges.ChangeTracker.with(context, (t) => addObjectLiteralProperties(t, context, info));
        return [
          createCodeFixAction(
            fixMissingProperties,
            changes,
            Diagnostics.Add_missing_properties,
            fixMissingProperties,
            Diagnostics.Add_all_missing_properties,
          ),
        ];
      }
      if (info.kind === InfoKind.JsxAttributes) {
        const changes = textChanges.ChangeTracker.with(context, (t) => addJsxAttributes(t, context, info));
        return [
          createCodeFixAction(
            fixMissingAttributes,
            changes,
            Diagnostics.Add_missing_attributes,
            fixMissingAttributes,
            Diagnostics.Add_all_missing_attributes,
          ),
        ];
      }
      if (info.kind === InfoKind.Function) {
        const changes = textChanges.ChangeTracker.with(context, (t) => addFunctionDeclaration(t, context, info));
        return [
          createCodeFixAction(
            fixMissingFunctionDeclaration,
            changes,
            [Diagnostics.Add_missing_function_declaration_0, info.token.text],
            fixMissingFunctionDeclaration,
            Diagnostics.Add_all_missing_function_declarations,
          ),
        ];
      }
      if (info.kind === InfoKind.Enum) {
        const changes = textChanges.ChangeTracker.with(
          context,
          (t) => addEnumMemberDeclaration(t, context.program.getTypeChecker(), info),
        );
        return [
          createCodeFixAction(
            fixMissingMember,
            changes,
            [Diagnostics.Add_missing_enum_member_0, info.token.text],
            fixMissingMember,
            Diagnostics.Add_all_missing_members,
          ),
        ];
      }
      return concatenate(
        getActionsForMissingMethodDeclaration(context, info),
        getActionsForMissingMemberDeclaration(context, info),
      );
    },
    fixIds: [
      fixMissingMember,
      fixMissingFunctionDeclaration,
      fixMissingProperties,
      fixMissingAttributes,
    ],
    getAllCodeActions: (context) => {
      const { program, fixId } = context;
      const checker = program.getTypeChecker();
      const seen = new Map<string, true>();
      const typeDeclToMembers = new Map<
        ClassOrInterface,
        ClassOrInterfaceInfo[]
      >();

      return createCombinedCodeActions(
        textChanges.ChangeTracker.with(context, (changes) => {
          eachDiagnostic(context, errorCodes, (diag) => {
            const info = getInfo(
              diag.file,
              diag.start,
              diag.code,
              checker,
              context.program,
            );
            if (
              !info
              || !addToSeen(
                seen,
                getNodeId(info.parentDeclaration) + "#" + info.token.text,
              )
            ) {
              return;
            }

            if (
              fixId === fixMissingFunctionDeclaration
              && info.kind === InfoKind.Function
            ) {
              addFunctionDeclaration(changes, context, info);
            } else if (
              fixId === fixMissingProperties
              && info.kind === InfoKind.ObjectLiteral
            ) {
              addObjectLiteralProperties(changes, context, info);
            } else if (
              fixId === fixMissingAttributes
              && info.kind === InfoKind.JsxAttributes
            ) {
              addJsxAttributes(changes, context, info);
            } else {
              if (info.kind === InfoKind.Enum) {
                addEnumMemberDeclaration(changes, checker, info);
              }
              if (info.kind === InfoKind.ClassOrInterface) {
                const { parentDeclaration, token } = info;
                const infos = getOrUpdate(
                  typeDeclToMembers,
                  parentDeclaration,
                  () => [],
                );
                if (!infos.some((i) => i.token.text === token.text)) {
                  infos.push(info);
                }
              }
            }
          });

          typeDeclToMembers.forEach((infos, classDeclaration) => {
            const supers = getAllSupers(classDeclaration, checker);
            for (const info of infos) {
              // If some superclass added this property, don't add it again.
              if (
                supers.some((superClassOrInterface) => {
                  const superInfos = typeDeclToMembers.get(
                    superClassOrInterface,
                  );
                  return (
                    !!superInfos
                    && superInfos.some(
                      ({ token }) => token.text === info.token.text,
                    )
                  );
                })
              ) {
                continue;
              }

              const {
                parentDeclaration,
                declSourceFile,
                modifierFlags,
                token,
                call,
                isJSFile,
              } = info;
              // Always prefer to add a method declaration if possible.
              if (call && !isPrivateIdentifier(token)) {
                addMethodDeclaration(
                  context,
                  changes,
                  call,
                  token,
                  modifierFlags & ModifierFlags.Static,
                  parentDeclaration,
                  declSourceFile,
                );
              } else {
                if (isJSFile && !isInterfaceDeclaration(parentDeclaration)) {
                  addMissingMemberInJs(
                    changes,
                    declSourceFile,
                    parentDeclaration,
                    token,
                    !!(modifierFlags & ModifierFlags.Static),
                  );
                } else {
                  const typeNode = getTypeNode(
                    program.getTypeChecker(),
                    parentDeclaration,
                    token,
                  );
                  addPropertyDeclaration(
                    changes,
                    declSourceFile,
                    parentDeclaration,
                    token.text,
                    typeNode,
                    modifierFlags & ModifierFlags.Static,
                  );
                }
              }
            }
          });
        }),
      );
    },
  });

  const enum InfoKind {
    Enum,
    ClassOrInterface,
    Function,
    ObjectLiteral,
    JsxAttributes,
  }
  type Info =
    | EnumInfo
    | ClassOrInterfaceInfo
    | FunctionInfo
    | ObjectLiteralInfo
    | JsxAttributesInfo;

  interface EnumInfo {
    readonly kind: InfoKind.Enum;
    readonly token: Identifier;
    readonly parentDeclaration: EnumDeclaration;
  }

  interface ClassOrInterfaceInfo {
    readonly kind: InfoKind.ClassOrInterface;
    readonly call: CallExpression | undefined;
    readonly token: Identifier | PrivateIdentifier;
    readonly modifierFlags: ModifierFlags;
    readonly parentDeclaration: ClassOrInterface;
    readonly declSourceFile: SourceFile;
    readonly isJSFile: boolean;
  }

  interface FunctionInfo {
    readonly kind: InfoKind.Function;
    readonly call: CallExpression;
    readonly token: Identifier;
    readonly sourceFile: SourceFile;
    readonly modifierFlags: ModifierFlags;
    readonly parentDeclaration: SourceFile | ModuleDeclaration;
  }

  interface ObjectLiteralInfo {
    readonly kind: InfoKind.ObjectLiteral;
    readonly token: Identifier;
    readonly properties: Symbol[];
    readonly parentDeclaration: ObjectLiteralExpression;
    readonly indentation?: number;
  }

  interface JsxAttributesInfo {
    readonly kind: InfoKind.JsxAttributes;
    readonly token: Identifier;
    readonly attributes: Symbol[];
    readonly parentDeclaration: JsxOpeningLikeElement;
  }

  function getInfo(
    sourceFile: SourceFile,
    tokenPos: number,
    errorCode: number,
    checker: TypeChecker,
    program: Program,
  ): Info | undefined {
    // The identifier of the missing property. eg:
    // this.missing = 1;
    //      ^^^^^^^
    const token = getTokenAtPosition(sourceFile, tokenPos);
    const parent = token.parent;

    if (
      errorCode
        === Diagnostics.Argument_of_type_0_is_not_assignable_to_parameter_of_type_1
          .code
    ) {
      if (
        !(
          token.kind === SyntaxKind.OpenBraceToken
          && isObjectLiteralExpression(parent)
          && isCallExpression(parent.parent)
        )
      ) {
        return undefined;
      }

      const argIndex = findIndex(
        parent.parent.arguments,
        (arg) => arg === parent,
      );
      if (argIndex < 0) return undefined;

      const signature = singleOrUndefined(
        checker.getSignaturesOfType(
          checker.getTypeAtLocation(parent.parent.expression),
          SignatureKind.Call,
        ),
      );
      if (
        !(signature && signature.declaration && signature.parameters[argIndex])
      ) {
        return undefined;
      }

      const param = signature.parameters[argIndex].valueDeclaration;
      if (!(param && isParameter(param) && isIdentifier(param.name))) {
        return undefined;
      }

      const properties = arrayFrom(
        checker.getUnmatchedProperties(
          checker.getTypeAtLocation(parent),
          checker.getTypeAtLocation(param),
          /* requireOptionalProperties */ false,
          /* matchDiscriminantProperties */ false,
        ),
      );
      if (!length(properties)) return undefined;
      return {
        kind: InfoKind.ObjectLiteral,
        token: param.name,
        properties,
        parentDeclaration: parent,
      };
    }

    if (!isMemberName(token)) return undefined;

    if (
      isIdentifier(token)
      && hasInitializer(parent)
      && parent.initializer
      && isObjectLiteralExpression(parent.initializer)
    ) {
      const properties = arrayFrom(
        checker.getUnmatchedProperties(
          checker.getTypeAtLocation(parent.initializer),
          checker.getTypeAtLocation(token),
          /* requireOptionalProperties */ false,
          /* matchDiscriminantProperties */ false,
        ),
      );
      if (!length(properties)) return undefined;

      return {
        kind: InfoKind.ObjectLiteral,
        token,
        properties,
        parentDeclaration: parent.initializer,
      };
    }

    if (isIdentifier(token) && isJsxOpeningLikeElement(token.parent)) {
      const target = getEmitScriptTarget(program.getCompilerOptions());
      const attributes = getUnmatchedAttributes(checker, target, token.parent);
      if (!length(attributes)) return undefined;
      return {
        kind: InfoKind.JsxAttributes,
        token,
        attributes,
        parentDeclaration: token.parent,
      };
    }

    if (isIdentifier(token) && isCallExpression(parent)) {
      return {
        kind: InfoKind.Function,
        token,
        call: parent,
        sourceFile,
        modifierFlags: ModifierFlags.None,
        parentDeclaration: sourceFile,
      };
    }

    if (!isPropertyAccessExpression(parent)) return undefined;

    const leftExpressionType = skipConstraint(
      checker.getTypeAtLocation(parent.expression),
    );
    const symbol = leftExpressionType.symbol;
    if (!symbol || !symbol.declarations) return undefined;

    if (isIdentifier(token) && isCallExpression(parent.parent)) {
      const moduleDeclaration = find(symbol.declarations, isModuleDeclaration);
      const moduleDeclarationSourceFile = moduleDeclaration?.getSourceFile();
      if (
        moduleDeclaration
        && moduleDeclarationSourceFile
        && !isSourceFileFromLibrary(program, moduleDeclarationSourceFile)
      ) {
        return {
          kind: InfoKind.Function,
          token,
          call: parent.parent,
          sourceFile,
          modifierFlags: ModifierFlags.Export,
          parentDeclaration: moduleDeclaration,
        };
      }

      const moduleSourceFile = find(symbol.declarations, isSourceFile);
      if (sourceFile.commonJsModuleIndicator) return undefined;

      if (
        moduleSourceFile
        && !isSourceFileFromLibrary(program, moduleSourceFile)
      ) {
        return {
          kind: InfoKind.Function,
          token,
          call: parent.parent,
          sourceFile: moduleSourceFile,
          modifierFlags: ModifierFlags.Export,
          parentDeclaration: moduleSourceFile,
        };
      }
    }

    const classDeclaration = find(symbol.declarations, isClassLike);
    // Don't suggest adding private identifiers to anything other than a class.
    if (!classDeclaration && isPrivateIdentifier(token)) return undefined;

    // Prefer to change the class instead of the interface if they are merged
    const classOrInterface = classDeclaration || find(symbol.declarations, isInterfaceDeclaration);
    if (
      classOrInterface
      && !isSourceFileFromLibrary(program, classOrInterface.getSourceFile())
    ) {
      const makeStatic = ((leftExpressionType as TypeReference).target || leftExpressionType)
        !== checker.getDeclaredTypeOfSymbol(symbol);
      if (
        makeStatic
        && (isPrivateIdentifier(token) || isInterfaceDeclaration(classOrInterface))
      ) {
        return undefined;
      }

      const declSourceFile = classOrInterface.getSourceFile();
      const modifierFlags = (makeStatic ? ModifierFlags.Static : 0)
        | (startsWithUnderscore(token.text) ? ModifierFlags.Private : 0);
      const isJSFile = isSourceFileJS(declSourceFile);
      const call = tryCast(parent.parent, isCallExpression);
      return {
        kind: InfoKind.ClassOrInterface,
        token,
        call,
        modifierFlags,
        parentDeclaration: classOrInterface,
        declSourceFile,
        isJSFile,
      };
    }

    const enumDeclaration = find(symbol.declarations, isEnumDeclaration);
    if (
      enumDeclaration
      && !isPrivateIdentifier(token)
      && !isSourceFileFromLibrary(program, enumDeclaration.getSourceFile())
    ) {
      return { kind: InfoKind.Enum, token, parentDeclaration: enumDeclaration };
    }

    return undefined;
  }

  function isSourceFileFromLibrary(program: Program, node: SourceFile) {
    return (
      program.isSourceFileFromExternalLibrary(node)
      || program.isSourceFileDefaultLibrary(node)
    );
  }

  function getActionsForMissingMemberDeclaration(
    context: CodeFixContext,
    info: ClassOrInterfaceInfo,
  ): CodeFixAction[] | undefined {
    return info.isJSFile
      ? singleElementArray(
        createActionForAddMissingMemberInJavascriptFile(context, info),
      )
      : createActionsForAddMissingMemberInTypeScriptFile(context, info);
  }

  function createActionForAddMissingMemberInJavascriptFile(
    context: CodeFixContext,
    {
      parentDeclaration,
      declSourceFile,
      modifierFlags,
      token,
    }: ClassOrInterfaceInfo,
  ): CodeFixAction | undefined {
    if (isInterfaceDeclaration(parentDeclaration)) {
      return undefined;
    }

    const changes = textChanges.ChangeTracker.with(context, (t) =>
      addMissingMemberInJs(
        t,
        declSourceFile,
        parentDeclaration,
        token,
        !!(modifierFlags & ModifierFlags.Static),
      ));
    if (changes.length === 0) {
      return undefined;
    }

    const diagnostic = modifierFlags & ModifierFlags.Static
      ? Diagnostics.Initialize_static_property_0
      : isPrivateIdentifier(token)
      ? Diagnostics.Declare_a_private_field_named_0
      : Diagnostics.Initialize_property_0_in_the_constructor;

    return createCodeFixAction(
      fixMissingMember,
      changes,
      [diagnostic, token.text],
      fixMissingMember,
      Diagnostics.Add_all_missing_members,
    );
  }

  function addMissingMemberInJs(
    changeTracker: textChanges.ChangeTracker,
    declSourceFile: SourceFile,
    classDeclaration: ClassLikeDeclaration,
    token: Identifier | PrivateIdentifier,
    makeStatic: boolean,
  ): void {
    const tokenName = token.text;
    if (makeStatic) {
      if (classDeclaration.kind === SyntaxKind.ClassExpression) {
        return;
      }
      const className = classDeclaration.name!.getText();
      const staticInitialization = initializePropertyToUndefined(
        factory.createIdentifier(className),
        tokenName,
      );
      changeTracker.insertNodeAfter(
        declSourceFile,
        classDeclaration,
        staticInitialization,
      );
    } else if (isPrivateIdentifier(token)) {
      const property = factory.createPropertyDeclaration(
        /*decorators*/ undefined,
        /*modifiers*/ undefined,
        tokenName,
        /*questionToken*/ undefined,
        /*type*/ undefined,
        /*initializer*/ undefined,
      );

      const lastProp = getNodeToInsertPropertyAfter(classDeclaration);
      if (lastProp) {
        changeTracker.insertNodeAfter(declSourceFile, lastProp, property);
      } else {
        changeTracker.insertNodeAtClassStart(
          declSourceFile,
          classDeclaration,
          property,
        );
      }
    } else {
      const classConstructor = getFirstConstructorWithBody(classDeclaration);
      if (!classConstructor) {
        return;
      }
      const propertyInitialization = initializePropertyToUndefined(
        factory.createThis(),
        tokenName,
      );
      changeTracker.insertNodeAtConstructorEnd(
        declSourceFile,
        classConstructor,
        propertyInitialization,
      );
    }
  }

  function initializePropertyToUndefined(
    obj: Expression,
    propertyName: string,
  ) {
    return factory.createExpressionStatement(
      factory.createAssignment(
        factory.createPropertyAccessExpression(obj, propertyName),
        createUndefined(),
      ),
    );
  }

  function createActionsForAddMissingMemberInTypeScriptFile(
    context: CodeFixContext,
    {
      parentDeclaration,
      declSourceFile,
      modifierFlags,
      token,
    }: ClassOrInterfaceInfo,
  ): CodeFixAction[] | undefined {
    const memberName = token.text;
    const isStatic = modifierFlags & ModifierFlags.Static;
    const typeNode = getTypeNode(
      context.program.getTypeChecker(),
      parentDeclaration,
      token,
    );
    const addPropertyDeclarationChanges = (modifierFlags: ModifierFlags) =>
      textChanges.ChangeTracker.with(context, (t) =>
        addPropertyDeclaration(
          t,
          declSourceFile,
          parentDeclaration,
          memberName,
          typeNode,
          modifierFlags,
        ));

    const actions = [
      createCodeFixAction(
        fixMissingMember,
        addPropertyDeclarationChanges(modifierFlags & ModifierFlags.Static),
        [
          isStatic
            ? Diagnostics.Declare_static_property_0
            : Diagnostics.Declare_property_0,
          memberName,
        ],
        fixMissingMember,
        Diagnostics.Add_all_missing_members,
      ),
    ];
    if (isStatic || isPrivateIdentifier(token)) {
      return actions;
    }

    if (modifierFlags & ModifierFlags.Private) {
      actions.unshift(
        createCodeFixActionWithoutFixAll(
          fixMissingMember,
          addPropertyDeclarationChanges(ModifierFlags.Private),
          [Diagnostics.Declare_private_property_0, memberName],
        ),
      );
    }

    actions.push(
      createAddIndexSignatureAction(
        context,
        declSourceFile,
        parentDeclaration,
        token.text,
        typeNode,
      ),
    );
    return actions;
  }

  function getTypeNode(
    checker: TypeChecker,
    classDeclaration: ClassOrInterface,
    token: Node,
  ) {
    let typeNode: TypeNode | undefined;
    if (token.parent.parent.kind === SyntaxKind.BinaryExpression) {
      const binaryExpression = token.parent.parent as BinaryExpression;
      const otherExpression = token.parent === binaryExpression.left
        ? binaryExpression.right
        : binaryExpression.left;
      const widenedType = checker.getWidenedType(
        checker.getBaseTypeOfLiteralType(
          checker.getTypeAtLocation(otherExpression),
        ),
      );
      typeNode = checker.typeToTypeNode(
        widenedType,
        classDeclaration,
        NodeBuilderFlags.NoTruncation,
      );
    } else {
      const contextualType = checker.getContextualType(
        token.parent as Expression,
      );
      typeNode = contextualType
        ? checker.typeToTypeNode(
          contextualType,
          /*enclosingDeclaration*/ undefined,
          NodeBuilderFlags.NoTruncation,
        )
        : undefined;
    }
    return typeNode || factory.createKeywordTypeNode(SyntaxKind.AnyKeyword);
  }

  function addPropertyDeclaration(
    changeTracker: textChanges.ChangeTracker,
    declSourceFile: SourceFile,
    classDeclaration: ClassOrInterface,
    tokenName: string,
    typeNode: TypeNode,
    modifierFlags: ModifierFlags,
  ): void {
    const property = factory.createPropertyDeclaration(
      /*decorators*/ undefined,
      /*modifiers*/ modifierFlags
        ? factory.createNodeArray(
          factory.createModifiersFromModifierFlags(modifierFlags),
        )
        : undefined,
      tokenName,
      /*questionToken*/ undefined,
      typeNode,
      /*initializer*/ undefined,
    );

    const lastProp = getNodeToInsertPropertyAfter(classDeclaration);
    if (lastProp) {
      changeTracker.insertNodeAfter(declSourceFile, lastProp, property);
    } else {
      changeTracker.insertNodeAtClassStart(
        declSourceFile,
        classDeclaration,
        property,
      );
    }
  }

  // Gets the last of the first run of PropertyDeclarations, or undefined if the class does not start with a PropertyDeclaration.
  function getNodeToInsertPropertyAfter(
    cls: ClassOrInterface,
  ): PropertyDeclaration | undefined {
    let res: PropertyDeclaration | undefined;
    for (const member of cls.members) {
      if (!isPropertyDeclaration(member)) break;
      res = member;
    }
    return res;
  }

  function createAddIndexSignatureAction(
    context: CodeFixContext,
    declSourceFile: SourceFile,
    classDeclaration: ClassOrInterface,
    tokenName: string,
    typeNode: TypeNode,
  ): CodeFixAction {
    // Index signatures cannot have the static modifier.
    const stringTypeNode = factory.createKeywordTypeNode(
      SyntaxKind.StringKeyword,
    );
    const indexingParameter = factory.createParameterDeclaration(
      /*decorators*/ undefined,
      /*modifiers*/ undefined,
      /*dotDotDotToken*/ undefined,
      "x",
      /*questionToken*/ undefined,
      stringTypeNode,
      /*initializer*/ undefined,
    );
    const indexSignature = factory.createIndexSignature(
      /*decorators*/ undefined,
      /*modifiers*/ undefined,
      [indexingParameter],
      typeNode,
    );

    const changes = textChanges.ChangeTracker.with(
      context,
      (t) => t.insertNodeAtClassStart(declSourceFile, classDeclaration, indexSignature),
    );
    // No fixId here because code-fix-all currently only works on adding individual named properties.
    return createCodeFixActionWithoutFixAll(fixMissingMember, changes, [
      Diagnostics.Add_index_signature_for_property_0,
      tokenName,
    ]);
  }

  function getActionsForMissingMethodDeclaration(
    context: CodeFixContext,
    info: ClassOrInterfaceInfo,
  ): CodeFixAction[] | undefined {
    const { parentDeclaration, declSourceFile, modifierFlags, token, call } = info;
    if (call === undefined) {
      return undefined;
    }

    // Private methods are not implemented yet.
    if (isPrivateIdentifier(token)) {
      return undefined;
    }

    const methodName = token.text;
    const addMethodDeclarationChanges = (modifierFlags: ModifierFlags) =>
      textChanges.ChangeTracker.with(context, (t) =>
        addMethodDeclaration(
          context,
          t,
          call,
          token,
          modifierFlags,
          parentDeclaration,
          declSourceFile,
        ));
    const actions = [
      createCodeFixAction(
        fixMissingMember,
        addMethodDeclarationChanges(modifierFlags & ModifierFlags.Static),
        [
          modifierFlags & ModifierFlags.Static
            ? Diagnostics.Declare_static_method_0
            : Diagnostics.Declare_method_0,
          methodName,
        ],
        fixMissingMember,
        Diagnostics.Add_all_missing_members,
      ),
    ];
    if (modifierFlags & ModifierFlags.Private) {
      actions.unshift(
        createCodeFixActionWithoutFixAll(
          fixMissingMember,
          addMethodDeclarationChanges(ModifierFlags.Private),
          [Diagnostics.Declare_private_method_0, methodName],
        ),
      );
    }
    return actions;
  }

  function addMethodDeclaration(
    context: CodeFixContextBase,
    changes: textChanges.ChangeTracker,
    callExpression: CallExpression,
    name: Identifier,
    modifierFlags: ModifierFlags,
    parentDeclaration: ClassOrInterface,
    sourceFile: SourceFile,
  ): void {
    const importAdder = createImportAdder(
      sourceFile,
      context.program,
      context.preferences,
      context.host,
    );
    const methodDeclaration = createSignatureDeclarationFromCallExpression(
      SyntaxKind.MethodDeclaration,
      context,
      importAdder,
      callExpression,
      name,
      modifierFlags,
      parentDeclaration,
    ) as MethodDeclaration;
    const containingMethodDeclaration = findAncestor(
      callExpression,
      (n) => isMethodDeclaration(n) || isConstructorDeclaration(n),
    );
    if (
      containingMethodDeclaration
      && containingMethodDeclaration.parent === parentDeclaration
    ) {
      changes.insertNodeAfter(
        sourceFile,
        containingMethodDeclaration,
        methodDeclaration,
      );
    } else {
      changes.insertNodeAtClassStart(
        sourceFile,
        parentDeclaration,
        methodDeclaration,
      );
    }
    importAdder.writeFixes(changes);
  }

  function addEnumMemberDeclaration(
    changes: textChanges.ChangeTracker,
    checker: TypeChecker,
    { token, parentDeclaration }: EnumInfo,
  ) {
    /**
     * create initializer only literal enum that has string initializer.
     * value of initializer is a string literal that equal to name of enum member.
     * numeric enum or empty enum will not create initializer.
     */
    const hasStringInitializer = some(parentDeclaration.members, (member) => {
      const type = checker.getTypeAtLocation(member);
      return !!(type && type.flags & TypeFlags.StringLike);
    });

    const enumMember = factory.createEnumMember(
      token,
      hasStringInitializer ? factory.createStringLiteral(token.text) : undefined,
    );
    changes.replaceNode(
      parentDeclaration.getSourceFile(),
      parentDeclaration,
      factory.updateEnumDeclaration(
        parentDeclaration,
        parentDeclaration.decorators,
        parentDeclaration.modifiers,
        parentDeclaration.name,
        concatenate(parentDeclaration.members, singleElementArray(enumMember)),
      ),
      {
        leadingTriviaOption: textChanges.LeadingTriviaOption.IncludeAll,
        trailingTriviaOption: textChanges.TrailingTriviaOption.Exclude,
      },
    );
  }

  function addFunctionDeclaration(
    changes: textChanges.ChangeTracker,
    context: CodeFixContextBase,
    info: FunctionInfo,
  ) {
    const importAdder = createImportAdder(
      context.sourceFile,
      context.program,
      context.preferences,
      context.host,
    );
    const functionDeclaration = createSignatureDeclarationFromCallExpression(
      SyntaxKind.FunctionDeclaration,
      context,
      importAdder,
      info.call,
      idText(info.token),
      info.modifierFlags,
      info.parentDeclaration,
    ) as FunctionDeclaration;
    changes.insertNodeAtEndOfScope(
      info.sourceFile,
      info.parentDeclaration,
      functionDeclaration,
    );
  }

  function addJsxAttributes(
    changes: textChanges.ChangeTracker,
    context: CodeFixContextBase,
    info: JsxAttributesInfo,
  ) {
    const importAdder = createImportAdder(
      context.sourceFile,
      context.program,
      context.preferences,
      context.host,
    );
    const quotePreference = getQuotePreference(
      context.sourceFile,
      context.preferences,
    );
    const checker = context.program.getTypeChecker();
    const jsxAttributesNode = info.parentDeclaration.attributes;
    const hasSpreadAttribute = some(
      jsxAttributesNode.properties,
      isJsxSpreadAttribute,
    );
    const attrs = map(info.attributes, (attr) => {
      const value = tryGetValueFromType(
        context,
        checker,
        importAdder,
        quotePreference,
        checker.getTypeOfSymbol(attr),
      );
      const name = factory.createIdentifier(attr.name);
      const jsxAttribute = factory.createJsxAttribute(
        name,
        factory.createJsxExpression(/*dotDotDotToken*/ undefined, value),
      );
      // formattingScanner requires the Identifier to have a context for scanning attributes with "-" (data-foo).
      setParent(name, jsxAttribute);
      return jsxAttribute;
    });
    const jsxAttributes = factory.createJsxAttributes(
      hasSpreadAttribute
        ? [...attrs, ...jsxAttributesNode.properties]
        : [...jsxAttributesNode.properties, ...attrs],
    );
    const options = {
      prefix: jsxAttributesNode.pos === jsxAttributesNode.end ? " " : undefined,
    };
    changes.replaceNode(
      context.sourceFile,
      jsxAttributesNode,
      jsxAttributes,
      options,
    );
  }

  function addObjectLiteralProperties(
    changes: textChanges.ChangeTracker,
    context: CodeFixContextBase,
    info: ObjectLiteralInfo,
  ) {
    const importAdder = createImportAdder(
      context.sourceFile,
      context.program,
      context.preferences,
      context.host,
    );
    const quotePreference = getQuotePreference(
      context.sourceFile,
      context.preferences,
    );
    const target = getEmitScriptTarget(context.program.getCompilerOptions());
    const checker = context.program.getTypeChecker();
    const props = map(info.properties, (prop) => {
      const initializer = tryGetValueFromType(
        context,
        checker,
        importAdder,
        quotePreference,
        checker.getTypeOfSymbol(prop),
      );
      return factory.createPropertyAssignment(
        createPropertyNameNodeForIdentifierOrLiteral(
          prop.name,
          target,
          quotePreference === QuotePreference.Single,
        ),
        initializer,
      );
    });
    const options = {
      leadingTriviaOption: textChanges.LeadingTriviaOption.Exclude,
      trailingTriviaOption: textChanges.TrailingTriviaOption.Exclude,
      indentation: info.indentation,
    };
    changes.replaceNode(
      context.sourceFile,
      info.parentDeclaration,
      factory.createObjectLiteralExpression(
        [...info.parentDeclaration.properties, ...props],
        /*multiLine*/ true,
      ),
      options,
    );
  }

  function tryGetValueFromType(
    context: CodeFixContextBase,
    checker: TypeChecker,
    importAdder: ImportAdder,
    quotePreference: QuotePreference,
    type: Type,
  ): Expression {
    if (type.flags & TypeFlags.AnyOrUnknown) {
      return createUndefined();
    }
    if (type.flags & (TypeFlags.String | TypeFlags.TemplateLiteral)) {
      return factory.createStringLiteral(
        "",
        /* isSingleQuote */ quotePreference === QuotePreference.Single,
      );
    }
    if (type.flags & TypeFlags.Number) {
      return factory.createNumericLiteral(0);
    }
    if (type.flags & TypeFlags.BigInt) {
      return factory.createBigIntLiteral("0n");
    }
    if (type.flags & TypeFlags.Boolean) {
      return factory.createFalse();
    }
    if (type.flags & TypeFlags.EnumLike) {
      const enumMember = type.symbol.exports
        ? firstOrUndefined(arrayFrom(type.symbol.exports.values()))
        : type.symbol;
      const name = checker.symbolToExpression(
        type.symbol.parent ? type.symbol.parent : type.symbol,
        SymbolFlags.Value,
        /*enclosingDeclaration*/ undefined,
        /*flags*/ undefined,
      );
      return enumMember === undefined || name === undefined
        ? factory.createNumericLiteral(0)
        : factory.createPropertyAccessExpression(
          name,
          checker.symbolToString(enumMember),
        );
    }
    if (type.flags & TypeFlags.NumberLiteral) {
      return factory.createNumericLiteral((type as NumberLiteralType).value);
    }
    if (type.flags & TypeFlags.BigIntLiteral) {
      return factory.createBigIntLiteral((type as BigIntLiteralType).value);
    }
    if (type.flags & TypeFlags.StringLiteral) {
      return factory.createStringLiteral(
        (type as StringLiteralType).value,
        /* isSingleQuote */ quotePreference === QuotePreference.Single,
      );
    }
    if (type.flags & TypeFlags.BooleanLiteral) {
      return type === checker.getFalseType()
          || type === checker.getFalseType(/*fresh*/ true)
        ? factory.createFalse()
        : factory.createTrue();
    }
    if (type.flags & TypeFlags.Null) {
      return factory.createNull();
    }
    if (type.flags & TypeFlags.Union) {
      const expression = firstDefined(
        (type as UnionType).types,
        (t) => tryGetValueFromType(context, checker, importAdder, quotePreference, t),
      );
      return expression ?? createUndefined();
    }
    if (checker.isArrayLikeType(type)) {
      return factory.createArrayLiteralExpression();
    }
    if (isObjectLiteralType(type)) {
      const props = map(checker.getPropertiesOfType(type), (prop) => {
        const initializer = prop.valueDeclaration
          ? tryGetValueFromType(
            context,
            checker,
            importAdder,
            quotePreference,
            checker.getTypeAtLocation(prop.valueDeclaration),
          )
          : createUndefined();
        return factory.createPropertyAssignment(prop.name, initializer);
      });
      return factory.createObjectLiteralExpression(props, /*multiLine*/ true);
    }
    if (getObjectFlags(type) & ObjectFlags.Anonymous) {
      const decl = find(
        type.symbol.declarations || emptyArray,
        or(isFunctionTypeNode, isMethodSignature, isMethodDeclaration),
      );
      if (decl === undefined) return createUndefined();

      const signature = checker.getSignaturesOfType(type, SignatureKind.Call);
      if (signature === undefined) return createUndefined();

      const func = createSignatureDeclarationFromSignature(
        SyntaxKind.FunctionExpression,
        context,
        quotePreference,
        signature[0],
        createStubbedBody(
          Diagnostics.Function_not_implemented.message,
          quotePreference,
        ),
        /*name*/ undefined,
        /*modifiers*/ undefined,
        /*optional*/ undefined,
        /*enclosingDeclaration*/ undefined,
        importAdder,
      ) as FunctionExpression | undefined;
      return func ?? createUndefined();
    }
    if (getObjectFlags(type) & ObjectFlags.Class) {
      const classDeclaration = getClassLikeDeclarationOfSymbol(type.symbol);
      if (
        classDeclaration === undefined
        || hasAbstractModifier(classDeclaration)
      ) {
        return createUndefined();
      }

      const constructorDeclaration = getFirstConstructorWithBody(classDeclaration);
      if (constructorDeclaration && length(constructorDeclaration.parameters)) {
        return createUndefined();
      }

      return factory.createNewExpression(
        factory.createIdentifier(type.symbol.name),
        /*typeArguments*/ undefined,
        /*argumentsArray*/ undefined,
      );
    }
    return createUndefined();
  }

  function createUndefined() {
    return factory.createIdentifier("undefined");
  }

  function isObjectLiteralType(type: Type) {
    return (
      type.flags & TypeFlags.Object
      && (getObjectFlags(type) & ObjectFlags.ObjectLiteral
        || (type.symbol
          && tryCast(
            singleOrUndefined(type.symbol.declarations),
            isTypeLiteralNode,
          )))
    );
  }

  function getUnmatchedAttributes(
    checker: TypeChecker,
    target: ScriptTarget,
    source: JsxOpeningLikeElement,
  ) {
    const attrsType = checker.getContextualType(source.attributes);
    if (attrsType === undefined) return emptyArray;

    const targetProps = attrsType.getProperties();
    if (!length(targetProps)) return emptyArray;

    const seenNames = new Set<__String>();
    for (const sourceProp of source.attributes.properties) {
      if (isJsxAttribute(sourceProp)) {
        seenNames.add(sourceProp.name.escapedText);
      }
      if (isJsxSpreadAttribute(sourceProp)) {
        const type = checker.getTypeAtLocation(sourceProp.expression);
        for (const prop of type.getProperties()) {
          seenNames.add(prop.escapedName);
        }
      }
    }
    return filter(
      targetProps,
      (targetProp) =>
        isIdentifierText(targetProp.name, target, LanguageVariant.JSX)
        && !(
          targetProp.flags & SymbolFlags.Optional
          || getCheckFlags(targetProp) & CheckFlags.Partial
          || seenNames.has(targetProp.escapedName)
        ),
    );
  }
}
