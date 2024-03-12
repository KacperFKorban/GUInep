package guinep

import guinep.model.*
import scala.quoted.*

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
      case _ => None
    }

    def wrongParamsListError(f: Expr[Any]): Nothing =
      report.errorAndAbort(s"Wrong params list, expected a function reference, got: ${f.show}", f.asTerm.pos)

    private def unsupportedFunctionParamType(t: TypeRepr, pos: Option[Position] = None): Nothing = pos match {
      case Some(p) => report.errorAndAbort(s"Unsupported function param type: ${t.show}", p)
      case None => report.errorAndAbort(s"Unsupported function param type: ${t.show}")
    }

    extension (t: Term)
      private def select(s: Term): Term = Select(t, s.symbol)
      private def select(s: String): Term =
        t.select(t.tpe.typeSymbol.methodMember(s).head)

    private def functionNameImpl(f: Expr[Any]): Expr[String] = {
      val name = f.asTerm match {
        case Inlined(_, _, Lambda(_, body)) =>
          getMostInnerApply(body).getOrElse(wrongParamsListError(f))
        case Lambda(_, body) =>
          getMostInnerApply(body).getOrElse(wrongParamsListError(f))
        case _ =>
          wrongParamsListError(f)
      }
      Expr(name)
    }

    private def functionParams(f: Expr[Any]): Seq[ValDef] = f.asTerm match {
      case Lambda(params, body) =>
        params.map (param => param)
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
      val isObject = typeSymbol.flags.is(Flags.Module)
      val isEnumCase = typeSymbol.flags.is(Flags.Enum) && typeSymbol.flags.is(Flags.Case)
      isObject || isEnumCase

    private def functionFormElementFromTree(paramName: String, paramType: TypeRepr): FormElement = paramType match {
      case ntpe: NamedType if ntpe.name == "String" => FormElement.TextInput(paramName)
      case ntpe: NamedType if ntpe.name == "Int" => FormElement.NumberInput(paramName)
      case ntpe: NamedType if ntpe.name == "Boolean" => FormElement.CheckboxInput(paramName)
      case ntpe if isProductTpe(ntpe) =>
        val classSymbol = ntpe.typeSymbol
        val fields = classSymbol.primaryConstructor.paramSymss.flatten.filter(_.isValDef).map(_.tree).collect { case v: ValDef => v }
        FormElement.FieldSet(paramName, fields.map(v => functionFormElementFromTree(v.name, v.tpt.tpe)))
      case ntpe if isSumTpe(ntpe) =>
        val classSymbol = ntpe.typeSymbol
        val childrenFormElements = classSymbol.children.map(_.typeRef).map(t => functionFormElementFromTree("value", t))
        val options = classSymbol.children.map(_.name).zip(childrenFormElements)
        FormElement.Dropdown(paramName, options)
      case _ =>
        unsupportedFunctionParamType(paramType)
    }

    private def functionFormElementsImpl(f: Expr[Any]): Expr[Seq[FormElement]] =
      Expr.ofSeq(
        functionParams(f).map { case ValDef(name, tpt, _) => functionFormElementFromTree(name, tpt.tpe) } .map(Expr(_))
      )

    private def constructArg(paramTpe: TypeRepr, param: Term): Term = {
      paramTpe match {
        case ntpe: NamedType if ntpe.name == "String" => param.select("asInstanceOf").appliedToType(ntpe)
        case ntpe: NamedType if ntpe.name == "Int" => param.select("asInstanceOf").appliedToType(ntpe)
        case ntpe: NamedType if ntpe.name == "Boolean" => param.select("asInstanceOf").appliedToType(ntpe)
        case ntpe if isCaseObjectTpe(ntpe) =>
          Ident(ntpe.typeSymbol.termRef)
        case ntpe if isProductTpe(ntpe) =>
          val classSymbol = ntpe.classSymbol.getOrElse(unsupportedFunctionParamType(paramTpe, Some(param.pos)))
          val fields = classSymbol.primaryConstructor.paramSymss.flatten.filter(_.isValDef).map(_.tree)
          val paramValue = '{ ${param.asExpr}.asInstanceOf[Map[String, Any]] }.asTerm
          val args = fields.collect { case field: ValDef =>
            val fieldName = field.asInstanceOf[ValDef].name
            val fieldValue = paramValue.select("apply").appliedTo(Literal(StringConstant(fieldName)))
            constructArg(field.tpt.tpe, fieldValue)
          }
          New(Inferred(ntpe)).select(classSymbol.primaryConstructor).appliedToArgs(args)
        case ntpe if isSumTpe(ntpe) =>
          val classSymbol = ntpe.classSymbol.getOrElse(unsupportedFunctionParamType(paramTpe, Some(param.pos)))
          val className = classSymbol.name
          val children = classSymbol.children
          val paramMap = '{ ${param.asExpr}.asInstanceOf[Map[String, Any]] }.asTerm
          val paramName = paramMap.select("apply").appliedTo(Literal(StringConstant("name")))
          val paramValue = paramMap.select("apply").appliedTo(Literal(StringConstant("value")))
          children.foldRight[Term]{
            '{ throw new RuntimeException(s"Class ${${paramName.asExpr}} is not a child of ${${Expr(className)}}") }.asTerm
          } { (child, acc) =>
            val childName = Literal(StringConstant(child.name))
            If(
              paramName.select("equals").appliedTo(childName),
              constructArg(child.typeRef, paramValue),
              acc
            )
          }
        case _ =>
          println(paramTpe.show)
          unsupportedFunctionParamType(paramTpe, Some(param.pos))
      }
    }

    private def functionRunImpl(f: Expr[Any]): Expr[List[Any] => String] = {
      val fTerm = f.asTerm
      f.asTerm match {
        case l@Lambda(params, body) =>
          /* (params: List[Any]) => l.apply(params(0).asInstanceOf[String], params(1).asInstanceOf[Int], ...) */
          Lambda(
            Symbol.spliceOwner,
            MethodType(List("inputs"))(_ => List(TypeRepr.of[List[Any]]),  _ => TypeRepr.of[String]),
            { case (sym, List(params: Term)) =>
              l.select("apply").appliedToArgs(
                functionParams(f).zipWithIndex.map { case (valdef, i) =>
                  val paramTpe = valdef.tpt.tpe
                  val param = params.select("apply").appliedTo(Literal(IntConstant(i)))
                  constructArg(paramTpe, param)
                }.toList
              ).select("toString").appliedToNone
            }
          ).asExprOf[List[Any] => String]
        case _ =>
          wrongParamsListError(f)
      }
    }

    def funInfosImpl(fs: Expr[Any]): Expr[Seq[Fun]] = {
      val functions = fs match {
        case Varargs(args) => args
        case _ => wrongParamsListError(fs)
      }
      if (functions.isEmpty)
        report.errorAndAbort("No functions provided", fs.asTerm.pos)
      Expr.ofSeq(functions.map(funInfoImpl))
    }

    def funInfoImpl(f: Expr[Any]): Expr[Fun] = {
      val name = functionNameImpl(f)
      val params = functionFormElementsImpl(f)
      val run = functionRunImpl(f)
      '{ Fun($name, $params, $run) }
    }
  }
}
