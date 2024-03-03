package guinep

inline def web(inline scripts: Any*): Unit =
  val scriptInfos = internal.scriptInfos(scripts)
  val scriptInfosMap = scriptInfos.groupBy(_.name)
  if scriptInfosMap.exists(_._2.size > 1) then
    println(
      s"""|Duplicate script names found: ${scriptInfosMap.filter(_._2.size > 1).keys.mkString(", ")}
          |Ignoring duplicates""".stripMargin
    )
  println("Starting GUInep web server at http://localhost:8090/scripts")
  internal.genWeb(scriptInfosMap.mapValues(_.head).toMap)
