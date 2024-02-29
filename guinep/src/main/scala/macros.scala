package guinep.internal

import scala.quoted.*
import javax.crypto.Mac

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

  private def functionParamTypes(f: Expr[Any]): Seq[TypeRepr] = f.asTerm match {
    case Lambda(params, body) =>
      params.map (param => param.tpt.tpe)
    case _ =>
      wrongParamsListError(f)
  }

  private def functionFieldsImpl(f: Expr[Any]): Expr[Seq[FieldType]] = {
    Expr.ofSeq(
      functionParamTypes(f).map { paramType =>
        paramType match {
          case ntpe: NamedType if ntpe.name == "String" => '{ FieldType.String }
          case ntpe: NamedType if ntpe.name == "Int" => '{ FieldType.Int }
          case t => unsupportedFunctionParamType(paramType, f.asTerm.pos)
        }
      }
    )
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
              functionParamTypes(f).zipWithIndex.map { case (paramTpe, i) =>
                val param = params.select("apply").appliedTo(Literal(IntConstant(i)))
                paramTpe match {
                  case ntpe: NamedType if ntpe.name == "String" => param.select("asInstanceOf").appliedToType(ntpe)
                  case ntpe: NamedType if ntpe.name == "Int" => '{ ${param.asExprOf[Any]}.asInstanceOf[String].toInt }.asTerm
                }
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
    val params = functionFieldsImpl(f)
    val run = functionRunImpl(f)
    '{ Script($name, $params, $run) }
  }
}
