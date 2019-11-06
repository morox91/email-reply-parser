package net.methvin.email

object EmailReplyParser {
  /**
   * Get the text of the visible portions of the email body
   *
   * @param text the String email body
   * @param includeSignatures a boolean indicating whether to include signatures
   * @return the parsed-out reply
   */
  def parseReply(text: String, includeSignatures: Boolean = false): String = {
    new EmailReplyParser(text).parsedText(includeSignatures = includeSignatures)
  }

  /**
   * Create a new email reply parser for an email body
   * @param text the email body
   * @return the new reply parser instance
   */
  def apply(text: String) = new EmailReplyParser(text)

  /**
   * A fragment of text in the email, with certain suspected properties
   */
  case class Fragment(
    lines: Seq[String],
    quoted: Boolean = false,
    signature: Boolean = false,
    hidden: Boolean = false
  ) {
    lazy val content = lines mkString "\n"
    override def toString = content
  }

  private val OnLine = """(?smi)(?!On.*On\s.+?wrote:)On\s.+?wrote:\s*$""".r
  private val OnLinePl = """(?smi)(?!.*<\S+@\S+\.\S+> napisał\(a\):)[^\n]*<\S+@\S+\.\S+>\s+napisał\(a\):\s*$""".r
  private val Signature = s"""(?smi)(^Sent from (\\w+\\s*){1,5}\\.?\\s*$$)|^(--|__|-\\w)""".r
  private val QuoteHeader = """(?smi)^\s*On.*?wrote:\s*$""".r
  private val QuoteHeader2 = """(?smi)^.*<\S+@\S+\.\S+> napisał\(a\):\s*$""".r
  private val Quoted = """(?smi)^\s*>""".r
  private val QuotedWp = """(?smi)^\s{8,}""".r
  private val OnetQuote = """(?smi)^\x{00A0}.*\x{00A0}$""".r
  private val OutlookQuote = """(?smi)^From:.*Sent:.*To:.*Subject:.*$""".r
}

class EmailReplyParser(val text: String) {

  def parsedText: String = parsedText()

  def parsedText(includeSignatures: Boolean = false): String = {
    fragments.filter(f => !f.hidden || includeSignatures && f.signature).mkString("\n").trim
  }

  import EmailReplyParser._

  val fragments: Seq[Fragment] = {
    var fragments: Seq[Fragment] = Seq()
    var fragment: Option[Fragment] = None
    var foundVisible = false

    val onLineReplaced = OnLine.replaceAllIn(text.replace("\r\n", "\n"), _.group(0).replace("\n", " "))
    val onLinePlReplaced = OnLinePl.replaceAllIn(onLineReplaced, _.group(0).replace("\n", " "))
    val onetQuoteRemoved = OnetQuote.replaceAllIn(onLinePlReplaced, "")
    val outlookQuoteRemoved = OutlookQuote.replaceAllIn(onetQuoteRemoved, "")

    for (reverseLine <- outlookQuoteRemoved.reverse.lines) {
      val line = reverseLine.reverse
      val isQuoted = Quoted.findFirstIn(line).isDefined || QuotedWp.findFirstIn(line).isDefined
      val isQuoteHeader = QuoteHeader.findFirstIn(line).isDefined || QuoteHeader2.findFirstIn(line).isDefined
      val isEmpty = line.trim.isEmpty
      if (isEmpty) checkSignature()
      fragment = fragment collect {
        case f if f.quoted == isQuoted || f.quoted && (isEmpty || isQuoteHeader) =>
          f.copy(lines = line +: f.lines)
      } orElse {
        finishFragment()
        Some(Fragment(Seq(line), quoted = isQuoted))
      }
    }

    checkSignature()
    finishFragment()

    def checkSignature(): Unit = {
      fragment = fragment map { f =>
        f.copy(signature = Signature.findFirstIn(f.lines.head).isDefined)
      }
      if (fragment.exists(_.signature)) finishFragment()
    }

    def finishFragment(): Unit = {
      for (f <- fragment) {
        val hidden = !foundVisible && (f.quoted || f.signature || f.content.trim.isEmpty)
        foundVisible ||= !hidden
        fragments = f.copy(hidden = hidden) +: fragments
      }
      fragment = None
    }

    fragments
  }
}
