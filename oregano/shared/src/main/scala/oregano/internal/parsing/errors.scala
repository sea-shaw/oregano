package oregano.internal.parsing

import parsley.errors.{DefaultErrorBuilder, ErrorBuilder}
import parsley.errors.tokenextractors.TillNextWhitespace

object errors {
  case class Pos(line: Int, offset: Int, width: Int)
  case class PosError(msg: String, pos: Pos)
  case class PosErrorInfoLines(lines: Seq[String], pos: Pos)

  object PosErrorBuilder extends ErrorBuilder[PosError] with TillNextWhitespace {

    override def build(pos: Position, source: Source, lines: ErrorInfoLines): PosError = {
      PosError(lines.lines.mkString("\n"), lines.pos)
    }

    type Position = Unit

    type Source = Unit

    override def pos(line: Int, col: Int): Position = ()

    override def source(sourceName: Option[String]): Source = ()

    type ErrorInfoLines = PosErrorInfoLines

    override def vanillaError(unexpected: UnexpectedLine, expected: ExpectedLine, reasons: Messages, line: LineInfo): ErrorInfoLines = {
      PosErrorInfoLines(Seq.concat(unexpected, expected, reasons), line)
    }

    override def specializedError(msgs: Messages, line: LineInfo): ErrorInfoLines = PosErrorInfoLines(msgs, line)

    type ExpectedItems = Option[String]
    type Messages = Seq[Message]

    override def combineExpectedItems(alts: Set[Item]): ExpectedItems = DefaultErrorBuilder.disjunct(alts)

    override def combineMessages(alts: Seq[Message]): Messages = DefaultErrorBuilder.combineMessages(alts)

    type UnexpectedLine = Option[String]
    type ExpectedLine = Option[String]
    type Message = String
    type LineInfo = Pos

    override def unexpected(item: Option[Item]): UnexpectedLine = DefaultErrorBuilder.unexpected(item)

    override def expected(alts: ExpectedItems): ExpectedLine = DefaultErrorBuilder.expected(alts)

    override def reason(reason: String): Message = DefaultErrorBuilder.reason(reason)

    override def message(msg: String): Message = DefaultErrorBuilder.message(msg)

    override def lineInfo(line: String, linesBefore: Seq[String], linesAfter: Seq[String], lineNum: Int, errorPointsAt: Int, errorWidth: Int): LineInfo = {
      Pos(lineNum - 1, errorPointsAt, errorWidth)
    }

    override val numLinesBefore: Int = 0

    override val numLinesAfter: Int = 0

    type Item = String
    type Raw = String
    type Named = String
    type EndOfInput = String

    override def raw(item: String): Raw = DefaultErrorBuilder.raw(item)

    override def named(item: String): Named = DefaultErrorBuilder.named(item)

    override val endOfInput: EndOfInput = DefaultErrorBuilder.EndOfInput

    override val trimToParserDemand: Boolean = true
  }
}
