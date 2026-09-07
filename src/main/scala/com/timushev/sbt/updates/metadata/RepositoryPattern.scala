package com.timushev.sbt.updates.metadata

import sbt.ModuleID

private[metadata] object RepositoryPattern {
  def revisionPrefix(pattern: String, module: ModuleID, organization: String): Option[String] = {
    val tokens = Map("organisation" -> organization, "organization" -> organization, "module" -> module.name) ++
      module.configurations.map("conf" -> _) ++
      module.extraAttributes.map { case (key, value) => key.stripPrefix("e:") -> value }
    val substituted = substitute(pattern, tokens)
    val first       = substituted.indexOf('[')
    if (
      first >= 0 && substituted.indexOf(']', first) >= 0 &&
      substituted.substring(first + 1, substituted.indexOf(']', first)) == "revision"
    ) {
      val optional = substituted.indexOf('(')
      Some(substituted.substring(0, if (optional >= 0) math.min(first, optional) else first))
    } else None
  }

  private[metadata] def substitute(pattern: String, tokens: Map[String, String]): String = {
    val result                            = new StringBuilder
    val optional                          = new StringBuilder
    var inOptional                        = false
    var keepOptional: Option[Boolean]     = None
    var index                             = 0
    def append(value: String): Unit       = if (inOptional) optional.append(value) else result.append(value)
    def invalid(message: String): Nothing = throw new IllegalArgumentException(message + " in pattern " + pattern)
    while (index < pattern.length) {
      pattern.charAt(index) match {
        case '(' =>
          if (inOptional) invalid("invalid start of optional part")
          inOptional = true
          optional.clear()
          keepOptional = None
        case ')' =>
          if (!inOptional) invalid("invalid end of optional part")
          inOptional = false
          keepOptional match {
            case Some(true) => result.append(optional)
            case None       => result.append('(').append(optional).append(')')
            case _          => ()
          }
        case '[' =>
          val end = pattern.indexOf(']', index + 1)
          if (end < 0) invalid("last token hasn't been closed")
          val token = pattern.substring(index + 1, end)
          if (token.contains('[')) invalid("invalid start of token")
          val value = tokens.get(token)
          if (inOptional) {
            keepOptional = Some(value.exists(_.nonEmpty))
            append(value.getOrElse("null"))
          } else append(value.getOrElse(pattern.substring(index, end + 1)))
          index = end
        case ']'  => invalid("invalid end of token")
        case char => append(char.toString)
      }
      index += 1
    }
    if (inOptional) invalid("optional part hasn't been closed")
    result.toString
  }
}
