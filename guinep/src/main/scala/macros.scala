package guinep

import guinep.model.*
import scala.quoted.*

private[guinep] object macros {
  inline def scriptInfos(inline fs: Any): Seq[Script] =
    ${ Macros.scriptInfosImpl('fs) }

  object Macros {
    def scriptInfosImpl(fs: Expr[Any])(using Quotes): Expr[Seq[Script]] =
      Macros().scriptInfosImpl(fs)
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

    private def unsupportedFunctionParamType(t: TypeRepr, pos: Position): Nothing =
      report.errorAndAbort(s"Unsupported function param type: ${t.show}", pos)

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

    private def functionFormElementFromTree(tree: Tree): FormElement = tree match {
      case ValDef(name, tpt, _) =>
        val paramType = tpt.tpe
        val paramName = name
        paramType match {
          case ntpe: NamedType if ntpe.name == "String" => FormElement.TextInput(paramName)
          case ntpe: NamedType if ntpe.name == "Int" => FormElement.NumberInput(paramName)
          case ntpe: NamedType if ntpe.name == "Boolean" => FormElement.CheckboxInput(paramName)
          case ntpe: NamedType =>
            val classSymbol = ntpe.classSymbol.getOrElse(unsupportedFunctionParamType(paramType, tree.pos))
            val fields = classSymbol.primaryConstructor.paramSymss.flatten.filter(_.isValDef).map(_.tree)
            FormElement.FieldSet(paramName, fields.map(functionFormElementFromTree))
          case _ => unsupportedFunctionParamType(paramType, tree.pos)
        }
    }

    private def functionFormElementsImpl(f: Expr[Any]): Expr[Seq[FormElement]] = {
      Expr.ofSeq(
        functionParams(f).map(functionFormElementFromTree).map(Expr(_))
      )
    }

    private def constructArg(paramTpe: TypeRepr, param: Term): Term = {
      paramTpe match {
        case ntpe: NamedType if ntpe.name == "String" => param.select("asInstanceOf").appliedToType(ntpe)
        case ntpe: NamedType if ntpe.name == "Int" => param.select("asInstanceOf").appliedToType(ntpe)
        case ntpe: NamedType if ntpe.name == "Boolean" => param.select("asInstanceOf").appliedToType(ntpe)
        case ntpe: NamedType =>
          val classSymbol = ntpe.classSymbol.getOrElse(unsupportedFunctionParamType(paramTpe, param.pos))
          val fields = classSymbol.primaryConstructor.paramSymss.flatten.filter(_.isValDef).map(_.tree)
          val paramValue = '{ ${param.asExpr}.asInstanceOf[Map[String, Any]] }.asTerm
          val args = fields.collect { case field: ValDef =>
            val fieldName = field.asInstanceOf[ValDef].name
            val fieldValue = paramValue.select("apply").appliedTo(Literal(StringConstant(fieldName)))
            constructArg(field.tpt.tpe, fieldValue)
          }
          New(Inferred(ntpe)).select(classSymbol.primaryConstructor).appliedToArgs(args)
        case _ => unsupportedFunctionParamType(paramTpe, param.pos)
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

    def scriptInfosImpl(fs: Expr[Any]): Expr[Seq[Script]] = {
      val functions = fs match {
        case Varargs(args) => args
        case _ => wrongParamsListError(fs)
      }
      if (functions.isEmpty)
        report.errorAndAbort("No functions provided", fs.asTerm.pos)
      Expr.ofSeq(functions.map(scriptInfoImpl))
    }

    def scriptInfoImpl(f: Expr[Any]): Expr[Script] = {
      val name = functionNameImpl(f)
      val params = functionFormElementsImpl(f)
      val run = functionRunImpl(f)
      '{ Script($name, $params, $run) }
    }
  }
}
