package guinep

import guinep.model.*
import scala.quoted.*
import scala.collection.mutable
import com.softwaremill.quicklens.*

private[guinep] object macros {
  inline def funInfos(inline fs: Any): Seq[Fun] =
    ${ Macros.funInfosImpl('fs) }

  object Macros {
    def funInfosImpl(fs: Expr[Any])(using Quotes): Expr[Seq[Fun]] =
      Macros().funInfosImpl(fs)
  }

  class Macros(using Quotes) {
    import quotes.reflect.*

    private def getMostInnerApply(term: Term): Option[String] = term match {
      case Apply(fun, _) => getMostInnerApply(fun)
      case TypeApply(fun, _) => getMostInnerApply(fun)
      case Ident(name) => Some(name)
      case This(name) => name
      case Select(_, name) => Some(name)
      case _ => None
    }

    private def wrongParamsListError(f: Expr[Any]): Nothing =
      report.errorAndAbort(s"Wrong params list, expected a function reference, got: ${f.show}", f.asTerm.pos)

    private def unsupportedFunctionParamType(t: TypeRepr, pos: Option[Position] = None): Nothing = pos match {
      case Some(p) => report.errorAndAbort(s"Unsupported function param type: ${t.show}", p)
      case None => report.errorAndAbort(s"Unsupported function param type: ${t.show}")
    }

    extension (t: Term)
      private def select(s: Term): Term = Select(t, s.symbol)
      private def select(s: String): Term =
        t.select(
          t.tpe
            .typeSymbol
            .methodMember(s)
            .headOption.
            getOrElse(report.errorAndAbort(s"PANIC: No member $s in term ${t.show} with type ${t.tpe.show}"))
        )

    extension (s: Symbol)
      private def prettyName: String =
        s.name.stripSuffix("$")

    extension (tpe: TypeRepr)
      private def stripAnnots: TypeRepr = tpe match {
        case AnnotatedType(tpe, _) => tpe.stripAnnots
        case _ => tpe
      }

    private def functionNameImpl(f: Expr[Any]): Expr[String] = {
      val name = f.asTerm match {
        case Lambda(_, body) =>
          getMostInnerApply(body).getOrElse(wrongParamsListError(f))
        case tree =>
          getMostInnerApply(tree).getOrElse(wrongParamsListError(f))
      }
      Expr(name)
    }

    private def functionParams(f: Expr[Any]): Seq[ValDef] = f.asTerm match {
      case Lambda(params, _) =>
        params.map (param => param)
      case Ident(_) =>
        Nil
      case Select(_, _) =>
        Nil
      case Apply(Ident(_), Nil) =>
        Nil
      case _ =>
        wrongParamsListError(f)
    }

    private def isProductTpe(tpe: TypeRepr): Boolean =
      val typeSymbol = tpe.typeSymbol
      val typeIsSingleton = tpe.isSingleton
      val typeIsCaseClass = typeSymbol.flags.is(Flags.Case)
      val typeIsAnyVal = tpe.baseClasses.contains(defn.AnyValClass)
      typeIsSingleton || typeIsCaseClass || typeIsAnyVal

    private def isSumTpe(tpe: TypeRepr): Boolean =
      val typeSymbol = tpe.typeSymbol
      val typeIsSingleton = tpe.isSingleton
      val typeIsEnum = typeSymbol.flags.is(Flags.Enum)
      val typeIsSealedTraitOrAbstractClass = typeSymbol.flags.is(Flags.Sealed) && (typeSymbol.flags.is(Flags.Trait) || typeSymbol.flags.is(Flags.Abstract))
      val typeIsNonCaseClassWithChildren = !typeSymbol.flags.is(Flags.Case) && typeSymbol.children.nonEmpty
      !typeIsSingleton && (typeIsEnum || typeIsSealedTraitOrAbstractClass || typeIsNonCaseClassWithChildren)

    private def isCaseObjectTpe(tpe: TypeRepr): Boolean =
      val typeSymbol = tpe.typeSymbol
      val isModule = typeSymbol.flags.is(Flags.Module)
      val isEnumCaseNonClassDef = typeSymbol.flags.is(Flags.Enum) && typeSymbol.flags.is(Flags.Case) && !typeSymbol.isClassDef
      isModule || isEnumCaseNonClassDef

    private case class FormConstrContext(constructedTpes: mutable.Map[String, Option[FormElement]], referencedTpes: mutable.Set[String])
    private def formConstrCtx(using FormConstrContext) = summon[FormConstrContext]

    extension (tpe: TypeRepr)
      private def namedRef: String = tpe match
        case ntpe: NamedType => ntpe.typeSymbol.fullName
        case AppliedType(tpe, args) => s"${tpe.namedRef}[${args.map(_.namedRef).mkString(", ")}]"
        case AnnotatedType(tpe, _) => tpe.namedRef
        case _ => tpe.show

    private def functionFormElementFromTreeWithCaching(paramName: String, paramTpe: TypeRepr)(using FormConstrContext): FormElement =
      formConstrCtx.constructedTpes.get(paramTpe.namedRef) match
        case Some(_) =>
          formConstrCtx.referencedTpes.add(paramTpe.namedRef)
          FormElement.NamedRef(paramName, paramTpe.namedRef)
        case _ =>
          formConstrCtx.constructedTpes.update(paramTpe.namedRef, None)
          val formElement = functionFormElementFromTree(paramName, paramTpe)
          formConstrCtx.constructedTpes.update(paramTpe.namedRef, Some(formElement.modify(_.name).setTo("value")))
          formElement

    private def functionFormElementFromTree(paramName: String, paramType: TypeRepr)(using FormConstrContext): FormElement = paramType match {
      case ntpe: NamedType if ntpe =:= TypeRepr.of[String] =>
        FormElement.TextInput(paramName)
      case ntpe: NamedType if ntpe =:= TypeRepr.of[Char] =>
        FormElement.CharInput(paramName)
      case ntpe: NamedType if ntpe =:= TypeRepr.of[Int] =>
        FormElement.NumberInput(paramName, Types.IntType.Int)
      case ntpe: NamedType if ntpe =:= TypeRepr.of[Long] =>
        FormElement.NumberInput(paramName, Types.IntType.Long)
      case ntpe: NamedType if ntpe =:= TypeRepr.of[Short] =>
        FormElement.NumberInput(paramName, Types.IntType.Short)
      case ntpe: NamedType if ntpe =:= TypeRepr.of[Byte] =>
        FormElement.NumberInput(paramName, Types.IntType.Byte)
      case ntpe: NamedType if ntpe =:= TypeRepr.of[Boolean] =>
        FormElement.CheckboxInput(paramName)
      case ntpe: NamedType if ntpe =:= TypeRepr.of[Float] =>
        FormElement.FloatingNumberInput(paramName, Types.FloatingType.Float)
      case ntpe: NamedType if ntpe =:= TypeRepr.of[Double] =>
        FormElement.FloatingNumberInput(paramName, Types.FloatingType.Double)
      case AppliedType(ntpe: NamedType, List(tpeArg)) if listLikeSymbolsTypes.contains(ntpe.typeSymbol) =>
        FormElement.ListInput(paramName, functionFormElementFromTreeWithCaching("elem", tpeArg), listLikeSymbolsTypes(ntpe.typeSymbol))
      case OrType(ltpe, rtpe) if ltpe =:= TypeRepr.of[Null] || rtpe =:= TypeRepr.of[Null] =>
        val innerForm = if ltpe =:= TypeRepr.of[Null] then
          functionFormElementFromTreeWithCaching(paramName, rtpe)
        else
          functionFormElementFromTreeWithCaching(paramName, ltpe)
        FormElement.Nullable(paramName, innerForm)
      case ntpe if isProductTpe(ntpe) =>
        val classSymbol = ntpe.typeSymbol
        val typeDefParams = classSymbol.primaryConstructor.paramSymss.flatten.filter(_.isTypeParam)
        val fields = classSymbol.primaryConstructor.paramSymss.flatten.filter(_.isValDef).map(_.tree).collect { case v: ValDef => v }
        FormElement.FieldSet(
          paramName,
          fields.map { valdef =>
            functionFormElementFromTreeWithCaching(
              valdef.name,
              valdef.tpt.tpe.substituteTypes(typeDefParams, ntpe.typeArgs).stripAnnots
            )
          }
        )
      case ntpe if isSumTpe(ntpe) =>
        val classSymbol = ntpe.typeSymbol
        val childrenAppliedTpes = classSymbol.children.map(child => appliedChild(child, classSymbol, ntpe.typeArgs)).map(_.stripAnnots)
        val childrenFormElements = childrenAppliedTpes.map(t => functionFormElementFromTreeWithCaching("value", t))
        val options = classSymbol.children.map(_.prettyName).zip(childrenFormElements)
        FormElement.Dropdown(paramName, options)
      case _ =>
        unsupportedFunctionParamType(paramType)
    }

    private def formImpl(f: Expr[Any]): Expr[Form] =
      given FormConstrContext = FormConstrContext(mutable.Map.empty, mutable.Set.empty)
      val inputs = functionParams(f)
        .map {
          case ValDef(name, tpt, _) =>
            functionFormElementFromTreeWithCaching(name, tpt.tpe)
        }
      val usedFormDecls =
        formConstrCtx.constructedTpes
          .toList.filter( (ref, formElement) => formConstrCtx.referencedTpes.contains(ref)  )
          .collect {
            case (ref, Some(formElement)) => ref -> formElement
          }
          .toMap
      val form = Form(inputs, usedFormDecls)
      Expr(form)

    private def appliedChild(childSym: Symbol, parentSym: Symbol, parentArgs: List[TypeRepr]): TypeRepr = childSym.tree match {
      case classDef @ ClassDef(_, _, parents, _, _) =>
        parents
          .collect {
            case tpt: TypeTree => tpt.tpe
          }
          .collectFirst {
            case AppliedType(tpe, args) if tpe.typeSymbol == parentSym => args
            case tpe if tpe.typeSymbol == parentSym => Nil
          }.match
            case None =>
              report.errorAndAbort(s"""PANIC: Could not find applied parent for ${childSym.name}, parents: ${parents.map(_.show).mkString(",")}""", classDef.pos)
            case Some(parentExtendsArgs) =>
              val childDefArgs = classDef.symbol.primaryConstructor.paramSymss.flatten.filter(_.isTypeParam).map(_.typeRef)
              val childArgTpes = childDefArgs.map { arg =>
                arg.substituteTypes(parentExtendsArgs.map(_.typeSymbol), parentArgs)
              }
              // TODO(kπ) might want to handle the case when there are unsubstituted type parameters left
              val childTpe = childSym.typeRef.appliedTo(childArgTpes)
              childTpe
      case _ =>
        childSym.typeRef
    }

    private case class ConstrEntry(definition: Option[Statement], ref: Term)
    private case class ConstrContext(constrMap: mutable.Map[String, ConstrEntry])
    private def constrCtx(using ConstrContext) = summon[ConstrContext]

    private def constructArgWithCaching(paramTpe: TypeRepr, param: Term)(using ConstrContext): Term =
      constrCtx.constrMap.get(paramTpe.namedRef) match
        case Some(ConstrEntry(_, ref)) =>
          ref.appliedTo(param)
        case None =>
          val ConstrEntry(_, ref) = constructFunction(paramTpe)
          ref.appliedTo(param)

    private def constructFunction(paramTpe: TypeRepr)(using ConstrContext): ConstrEntry =
      val defdefSymbol =
        Symbol.newMethod(
          Symbol.spliceOwner,
          s"constrFunFor${paramTpe.namedRef}",
          MethodType(List("inputs"))(_ => List(TypeRepr.of[Any]),  _ => paramTpe)
        )
      constrCtx.constrMap.update(paramTpe.namedRef, ConstrEntry(None, Ref(defdefSymbol)))
      val defdef = DefDef(defdefSymbol, {
        case List(List(param: Term)) =>
          Some(constructArg(paramTpe, param))
      })
      val constrEntry = ConstrEntry(Some(defdef), Ref(defdefSymbol))
      val newMap = constrCtx.constrMap.update(paramTpe.namedRef, constrEntry)
      constrEntry

    // TODO(kπ) add all the missing symbols to it
    private val listLikeSymbolsTypes: Map[Symbol, Types.ListType] = Map(
      Symbol.classSymbol("scala.collection.immutable.List") -> Types.ListType.List,
      Symbol.classSymbol("scala.collection.immutable.Seq") -> Types.ListType.Seq,
      Symbol.classSymbol("scala.collection.immutable.Vector") -> Types.ListType.Vector
    )

    private def constructArg(paramTpe: TypeRepr, param: Term)(using ConstrContext): Term = {
      paramTpe match {
        case ntpe: NamedType if ntpe =:= TypeRepr.of[String] =>
          param.select("asInstanceOf").appliedToType(ntpe)
        case ntpe: NamedType if ntpe =:= TypeRepr.of[Char] =>
          param.select("asInstanceOf").appliedToType(ntpe)
        case ntpe: NamedType
        if ntpe =:= TypeRepr.of[Int] || ntpe =:= TypeRepr.of[Long] || ntpe =:= TypeRepr.of[Short] || ntpe =:= TypeRepr.of[Byte] =>
          param.select(s"asInstanceOf").appliedToType(ntpe)
        case ntpe: NamedType if ntpe =:= TypeRepr.of[Boolean] =>
          param.select("asInstanceOf").appliedToType(ntpe)
        case ntpe: NamedType if ntpe =:= TypeRepr.of[Double] || ntpe =:= TypeRepr.of[Float] =>
          param.select(s"asInstanceOf").appliedToType(ntpe)
        case AppliedType(ntpe: NamedType, List(tpeArg)) if listLikeSymbolsTypes.contains(ntpe.typeSymbol) =>
          param.select("asInstanceOf").appliedToType(paramTpe)
        case OrType(ltpe, rtpe) if ltpe =:= TypeRepr.of[Null] || rtpe =:= TypeRepr.of[Null] =>
          val castedParam = param.select("asInstanceOf").appliedToType(paramTpe)
          '{ if ${param.asExpr} == null then null else ${castedParam.asExpr} }.asTerm
        case ntpe if isCaseObjectTpe(ntpe) && ntpe.typeSymbol.flags.is(Flags.Module) =>
          Ref(ntpe.typeSymbol.companionModule)
        case ntpe if isCaseObjectTpe(ntpe) =>
          Ref(ntpe.typeSymbol)
        case ntpe if isProductTpe(ntpe) =>
          val classSymbol = ntpe.typeSymbol
          val typeDefParams = classSymbol.primaryConstructor.paramSymss.flatten.filter(_.isTypeParam)
          val fields = classSymbol.primaryConstructor.paramSymss.flatten.filter(_.isValDef).map(_.tree)
          val paramValue = '{ ${param.asExpr}.asInstanceOf[Map[String, Any]] }.asTerm
          val args = fields.collect { case field: ValDef =>
            val fieldName = field.name
            val fieldValue = paramValue.select("apply").appliedTo(Literal(StringConstant(fieldName)))
            constructArgWithCaching(
              field.tpt.tpe.substituteTypes(typeDefParams, ntpe.typeArgs),
              fieldValue
            )
          }
          New(Inferred(ntpe.typeSymbol.typeRef)).select(classSymbol.primaryConstructor).appliedToTypes(ntpe.typeArgs).appliedToArgs(args)
        case ntpe if isSumTpe(ntpe) =>
          val classSymbol = ntpe.typeSymbol
          val className = classSymbol.name
          val children = classSymbol.children
          val childrenAppliedTpes = children.map(child => appliedChild(child, classSymbol, ntpe.typeArgs)).map(_.stripAnnots)
          val paramMap = '{ ${param.asExpr}.asInstanceOf[Map[String, Any]] }.asTerm
          val paramName = paramMap.select("apply").appliedTo(Literal(StringConstant("name")))
          val paramValue = paramMap.select("apply").appliedTo(Literal(StringConstant("value")))
          children.zip(childrenAppliedTpes).foldRight[Term]{
            '{ throw new RuntimeException(s"Class ${${paramName.asExpr}} is not a child of ${${Expr(className)}}") }.asTerm
          } { case ((child, childAppliedTpe), acc) =>
            val childName = Literal(StringConstant(child.prettyName))
            If(
              paramName.select("equals").appliedTo(childName),
              constructArgWithCaching(childAppliedTpe, paramValue),
              acc
            )
          }
        case _ =>
          unsupportedFunctionParamType(paramTpe, Some(param.pos))
      }
    }

    @scala.annotation.nowarn("msg=match may not be exhaustive")
    private def functionRunImpl(f: Expr[Any]): Expr[List[Any] => String] = f.asTerm match {
      case l@Lambda(params, _) =>
        /* (params: List[Any]) => l.apply(constructArg(params(0)), constructArg(params(1)), ...) */
        given ConstrContext = ConstrContext(mutable.Map.empty)
        val resLambda = Lambda(
          Symbol.spliceOwner,
          MethodType(List("inputs"))(_ => List(TypeRepr.of[List[Any]]),  _ => TypeRepr.of[String]),
          { case (sym, List(params: Term)) =>
            val args = functionParams(f).zipWithIndex.map { case (valdef, i) =>
              val paramTpe = valdef.tpt.tpe
              val param = params.select("apply").appliedTo(Literal(IntConstant(i)))
              constructArgWithCaching(paramTpe, param)
            }.toList
            val aply = l.select("apply")
            val res =
              if args.isEmpty then
                aply.appliedToNone
              else
                aply.appliedToArgs(args)
            res.select("toString").appliedToNone
          }
        )
        Block(
          constrCtx.constrMap.toList.flatMap(_._2.definition),
          resLambda
        ).asExprOf[List[Any] => String]
      case t@Ident(_) =>
        Lambda(
          Symbol.spliceOwner,
          MethodType(List("inputs"))(_ => List(TypeRepr.of[List[Any]]),  _ => TypeRepr.of[String]),
          { case (sym, List(params: Term)) =>
            t.select("toString").appliedToNone
          }
        ).asExprOf[List[Any] => String]
      case a@Apply(Ident(_), Nil) =>
        Lambda(
          Symbol.spliceOwner,
          MethodType(List("inputs"))(_ => List(TypeRepr.of[List[Any]]),  _ => TypeRepr.of[String]),
          { case (sym, List(params: Term)) =>
            a.select("toString").appliedToNone
          }
        ).asExprOf[List[Any] => String]
      case _ =>
        wrongParamsListError(f)
    }

    private def stripInlined(term: Term): Term = term match {
      case Inlined(_, _, body) => stripInlined(body)
      case _ => term
    }

    def funInfosImpl(fs: Expr[Any]): Expr[Seq[Fun]] = {
      val functions = fs.match {
        case Varargs(args) => args
        case f => Seq(f)
      }.map { expr =>
        stripInlined(expr.asTerm).asExpr
      }
      if (functions.isEmpty)
        report.errorAndAbort("No functions provided", fs.asTerm.pos)
      Expr.ofSeq(functions.map(funInfoImpl))
    }

    def funInfoImpl(f: Expr[Any]): Expr[Fun] = {
      val name = functionNameImpl(f)
      val form = formImpl(f)
      val run = functionRunImpl(f)
      '{ Fun($name, $form, $run) }
    }
  }
}
