package guinep

/**
  * Starts a web server with the endpoints for running the given functions and
  * an automatically derived frontend for calling them.
  *
  * @param functions the functions to be exposed
  * @example {{{
  *  def add(a: Int, b: Int) = a + b
  *  def greet(name: String) = s"Hello, $name!"
  *  @main def run = guinep.web(add, greet)
  * }}}
  */
inline def web(inline functions: Any*): Unit =
  val functionsInfos = macros.funInfos(functions)
  val functionsInfosMap = functionsInfos.groupBy(_.name)
  if functionsInfosMap.exists(_._2.size > 1) then
    println(
      s"""|Duplicate function names found: ${functionsInfosMap.filter(_._2.size > 1).keys.mkString(", ")}
          |Ignoring duplicates""".stripMargin
    )
  println("Starting GUInep web server at http://localhost:8090/")
  webgen.genWeb(functionsInfosMap.view.mapValues(_.head).toMap)
