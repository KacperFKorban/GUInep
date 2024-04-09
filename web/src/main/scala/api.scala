package guinep

/**
  * Creates and instance of GUInep web interface with the given configuration.
  * 
  * To start the web server, call the `apply` method with the functions to be exposed.
  * 
  * To change the configuration, call the `withSetConfig` or `withModifyConfig` methods.
  * 
  * @param config the configuration for the web interface
  * @example {{{
  *  def add(a: Int, b: Int) = a + b
  *  def greet(name: String) = s"Hello, $name!"
  *  @main def run = guinep.web(add, greet)
  * }}}
  */
def web: GuinepWeb =
  GuinepWeb()

/**
  * Configuration for the GUInep web interface.
  *
  * @param requireNonNullableInputs when set, all non-nullable types will correspond to required inputs in the frontend
  */
case class GuinepWebConfig(
  requireNonNullableInputs: Boolean = false
)

object GuinepWebConfig:
  def default: GuinepWebConfig =
    GuinepWebConfig()

private case class GuinepWeb(config: GuinepWebConfig = GuinepWebConfig.default) {
  def withSetConfig(config: GuinepWebConfig): GuinepWeb =
    this.copy(config = config)

  def withModifyConfig(f: GuinepWebConfig => GuinepWebConfig): GuinepWeb =
    withSetConfig(f(config))

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
  inline def apply(inline functions: Any*): Unit =
    val functionsInfos = macros.funInfos(functions)
    val functionsInfosMap = functionsInfos.groupBy(_.name)
    if functionsInfosMap.exists(_._2.size > 1) then
      println(
        s"""|Duplicate function names found: ${functionsInfosMap.filter(_._2.size > 1).keys.mkString(", ")}
            |Ignoring duplicates""".stripMargin
      )
    println("Starting GUInep web server at http://localhost:8090/")
    webgen.genWeb(
      funs = functionsInfos.distinct.map(fun => fun.name -> fun),
      config = config
    )
}
