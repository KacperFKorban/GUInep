package guinep.internal

import scala.quoted.*

transparent inline def scriptInfos(inline fs: Any): Seq[Script] =
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

  private def functionInputsImpl(f: Expr[Any]): Expr[Seq[Input]] = {
    Expr.ofSeq(
      functionParams(f).map { valdef =>
        val paramType = valdef.tpt.tpe
        val paramName = valdef.name
        paramType match {
          case ntpe: NamedType if ntpe.name == "String" => '{ Input(${Expr(paramName)}, FieldType.String) }
          case ntpe: NamedType if ntpe.name == "Int" => '{ Input(${Expr(paramName)}, FieldType.Int) }
          case t => unsupportedFunctionParamType(paramType, f.asTerm.pos)
        }
      }
    )
  }

  /*
   * This function is used to chain a List[Either[String, ?]] into a single Either[String, List[?]], dealing with Terms
   */
  private def chainEitherTermsToList(eithers: List[Term]): Term = eithers match {
    case Nil => '{ Right(Nil) }.asTerm
    case x :: xs =>
      val rest = chainEitherTermsToList(xs)
      '{ ${x.asExprOf[Either[String, ?]]}.flatMap { v => ${rest.asExprOf[Either[String, List[?]]]}.map { vs => v :: vs } } }.asTerm
  }

  private def functionRunImpl(f: Expr[Any]): Expr[List[String] => String] = {
    val fTerm = f.asTerm
    f.asTerm match {
      case l@Lambda(params, body) =>
        /* (params: List[Any]) => l.apply(params(0).asInstanceOf[String], params(1).toInt, ...) */
        Lambda(
          Symbol.spliceOwner,
          MethodType(List("inputs"))(_ => List(TypeRepr.of[List[String]]),  _ => TypeRepr.of[String]),
          { case (sym, List(params: Term)) =>
            val aply = l.select("apply")
            val args = functionParams(f).zipWithIndex.map { case (valdef, i) =>
            val paramTpe = valdef.tpt.tpe
            val paramName = valdef.name
              val param = params.select("apply").appliedTo(Literal(IntConstant(i)))
              val compiletimeModule = Symbol.requiredModule("scala.compiletime")
              val summonInlineTerm = Select.unique(Ref(compiletimeModule), "summonInline")
              val deserializerAppTpe = AppliedType(TypeRepr.of[Deserializer], List(paramTpe))
              val deserializerInstanceTerm = summonInlineTerm.appliedToType(deserializerAppTpe)
              deserializerInstanceTerm.select("deserialize").appliedTo(param)
            }.toList
            val argsGet = args.map(_.select("right").select("get"))
            aply.appliedToArgs(argsGet).select("toString").appliedToNone
          }
        ).asExprOf[List[String] => String]
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
    val params = functionInputsImpl(f)
    val run = functionRunImpl(f)
    '{ Script($name, $params, $run) }
  }
}
