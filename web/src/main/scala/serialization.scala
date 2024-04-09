package guinep

import guinep.*
import guinep.model.*
import zio.*
import zio.json.*
import zio.json.ast.*
import zio.json.ast.Json.*
import scala.util.chaining.*
import com.softwaremill.quicklens.*

private[guinep] object serialization:
  def sequenceEither[A, B](eithers: List[Either[A, B]]): Either[A, List[B]] =
    eithers.foldLeft(Right(List.empty): Either[A, List[B]]) { (acc, e) =>
      acc.flatMap(a => e.map(b => a :+ b))
    }

  extension (formElements: List[FormElement])
    def parseJSONValue(value: Obj)(using formElementLookup: Map[String, FormElement]): Either[String, Map[String, Any]] =
      formElements
        .map { element =>
          val v = value.get(element.name).toRight(s"Missing value for ${element.name}").flatMap(element.parseJSONValue)
          v.map(element.name -> _)
        }
        .pipe(sequenceEither).map(_.toMap)

    def sortByArgs(values: Map[String, Any]): List[Any] =
      formElements.map { element =>
        values(element.name)
      }

  extension (formElement: FormElement)
    def parseJSONValue(value: Json)(using formElementLookup: Map[String, FormElement]): Either[String, Any] = formElement match
      case FormElement.FieldSet(name, elements) =>
        for {
          m <- value.asObject.toRight(s"Invalid object: $value")
          res <- elements.parseJSONValue(m)
        } yield res
      case FormElement.TextInput(_) => value.asString.toRight(s"Invalid string: $value")
      case FormElement.CharInput(_) => value.asString.flatMap(_.headOption).toRight(s"Invalid char: $value")
      case FormElement.NumberInput(_, tpe) => tpe match
        case guinep.model.Types.IntType.Int =>
          value.asString.flatMap(_.toIntOption).toRight(s"Invalid int: $value")
        case guinep.model.Types.IntType.Long =>
          value.asString.flatMap(_.toLongOption).toRight(s"Invalid long: $value")
        case guinep.model.Types.IntType.Byte =>
          value.asString.flatMap(_.toByteOption).toRight(s"Invalid byte: $value")
        case guinep.model.Types.IntType.Short =>
          value.asString.flatMap(_.toShortOption).toRight(s"Invalid short: $value")
      case FormElement.FloatingNumberInput(_, tpe) => tpe match
        case guinep.model.Types.FloatingType.Double =>
          value.asString.flatMap(_.toDoubleOption).toRight(s"Invalid double: $value")
        case guinep.model.Types.FloatingType.Float =>
          value.asString.flatMap(_.toFloatOption).toRight(s"Invalid float: $value")
      case FormElement.CheckboxInput(_) => value.asBoolean.toRight(s"Invalid boolean: $value")
      case FormElement.Dropdown(_, options) =>
        for {
          v <- value.asObject.toRight(s"Invalid object: $value")
          ddName <- v.get("name").flatMap(_.asString).toRight(s"Invalid name: $value")
          ddValue = v.get("value").getOrElse(Obj())
          ddValueObj <- ddValue.asObject.toRight(s"Invalid object: $value")
          foundOption <- options.find(_._1 == ddName).toRight(s"Invalid option: $value")
          res <- foundOption._2.parseJSONValue(ddValueObj)
        } yield Map(
          "name" -> ddName,
          "value" -> res
        )
      case FormElement.TextArea(_, _, _) => Right(value)
      case FormElement.NamedRef(name, ref) =>
        val formElementFromLookup = formElementLookup(ref).modify(_.name).setTo(name)
        formElementFromLookup.parseJSONValue(value)
      case FormElement.ListInput(_, element, tpe) =>
        for {
          jsonLst <- value.asArray.map(_.toList).toRight(s"Invalid array $value")
          res <- sequenceEither(jsonLst.map(element.parseJSONValue))
        } yield tpe match
          case guinep.model.Types.ListType.List => res
          case guinep.model.Types.ListType.Seq => res.toSeq
          case guinep.model.Types.ListType.Vector => res.toVector
      case _ => Left(s"Unsupported form element: $formElement")
