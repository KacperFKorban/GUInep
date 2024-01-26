package guinep

inline def guinep(inline scripts: Any*): Unit =
  val scriptInfos = internal.scriptInfos(scripts)
  val scriptInfosMap = scriptInfos.groupBy(_.name)
  if scriptInfosMap.exists(_._2.size > 1) then
    println(
      s"""|Duplicate script names found: ${scriptInfosMap.filter(_._2.size > 1).keys.mkString(", ")}
          |Ignoring duplicates""".stripMargin
    )
  internal.guinep(scriptInfosMap.mapValues(_.head).toMap)
