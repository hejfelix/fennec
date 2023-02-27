package fennec.examples.macros

object SourceFileMacro {
  import scala.quoted.*
  inline def getContent[A]: (String, String) = ${ getContentImpl[A] }

  private def getContentImpl[A: Type](using Quotes): Expr[(String, String)] =
    import quotes.reflect.*
    val typeSymbol = TypeRepr
      .of[A]
      .typeSymbol
    val position = typeSymbol.pos
      .getOrElse(
        report.errorAndAbort("no symbol position"),
      )
    val file = position.sourceFile

    val str = file.content
      .getOrElse(
        report.errorAndAbort("no source-file content"),
      )
    Expr((typeSymbol.fullName, str))

}
