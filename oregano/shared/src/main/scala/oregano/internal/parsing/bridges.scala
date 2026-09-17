package oregano.internal.parsing

import cats.collections.{Diet, Range}
import cats.data.NonEmptyList
import oregano.internal.ast.{AST, QuantifierType}
import parsley.Parsley
import parsley.Parsley.pure
import parsley.bridges.ParserSingletonBridge
import parsley.errors.combinator.*
import parsley.templates.{PureParserBridge1, PureParserBridge2, PureParserBridge3}

object bridges {

  type ToRegex = (ast: AST) ?=> ast.Regex[?]

  object Dot extends ParserSingletonBridge[ToRegex] {
    override protected def singleton: Parsley[ToRegex] = pure(ast.Class(allSet -- Diet.one('\n'.toInt)))
  }

  object Lit extends PureParserBridge1[Int, ToRegex] {
    override def apply(c: Int): ToRegex = ast.Lit(c)
  }

  object Class extends PureParserBridge1[Diet[Int], ToRegex] {
    override def apply(cs: Diet[Int]): ToRegex = ast.Class(cs)
  }

  object LineStart extends ParserSingletonBridge[ToRegex] {
    override protected def singleton: Parsley[ToRegex] = pure(ast.LineStart)
  }

  object LineEnd extends ParserSingletonBridge[ToRegex] {
    override protected def singleton: Parsley[ToRegex] = pure(ast.LineEnd)
  }

  object NegativeLookahead extends PureParserBridge1[ToRegex, ToRegex] {
    override def apply(inner: ToRegex): ToRegex = ast.NegativeLookahead(inner)
  }

  object NegativeLookbehind extends PureParserBridge1[ToRegex, ToRegex] {
    override def apply(inner: ToRegex): ToRegex = ast.NegativeLookbehind(inner)
  }

  object Backreference extends PureParserBridge1[Int, ToRegex] {
    override def apply(group: Int): ToRegex = ast.Backreference(group)
  }

  object Capture extends PureParserBridge1[ToRegex, ToRegex] {
    override def apply(inner: ToRegex): ToRegex = ast.Capture(inner)
  }

  object NamedCapture extends PureParserBridge2[String, ToRegex, ToRegex] {
    override def apply(name: String, inner: ToRegex): ToRegex = ast.NamedCapture(name, inner)
  }

  object PositiveLookahead extends PureParserBridge1[ToRegex, ToRegex] {
    override def apply(inner: ToRegex): ToRegex = ast.PositiveLookahead(inner)
  }

  object PositiveLookbehind extends PureParserBridge1[ToRegex, ToRegex] {
    override def apply(inner: ToRegex): ToRegex = ast.PositiveLookbehind(inner)
  }

  object Independent extends PureParserBridge1[ToRegex, ToRegex] {
    override def apply(inner: ToRegex): ToRegex = ast.Independent(inner)
  }

  object Cat extends PureParserBridge1[NonEmptyList[ToRegex], ToRegex] {
    override def apply(regexes: NonEmptyList[ToRegex]): ToRegex = {
      val NonEmptyList(head, tail) = regexes
      tail.foldLeft(head)(ast.Cat(_, _))
    }
  }

  object Alt extends PureParserBridge2[ToRegex, ToRegex, ToRegex] {
    override def apply(left: ToRegex, right: ToRegex): ToRegex = ast.Alt(left, right)
  }

  object Opt extends PureParserBridge2[ToRegex, QuantifierType, ToRegex] {
    override def apply(inner: ToRegex, quantifierType: QuantifierType): ToRegex = ast.Opt(inner, quantifierType)
  }

  object Star extends PureParserBridge2[ToRegex, QuantifierType, ToRegex] {
    override def apply(inner: ToRegex, quantifierType: QuantifierType): ToRegex =  ast.Star(inner, quantifierType)
  }

  object Plus extends PureParserBridge2[ToRegex, QuantifierType, ToRegex] {
    override def apply(inner: ToRegex, quantifierType: QuantifierType): ToRegex =  ast.Plus(inner, quantifierType)
  }

  object NumericalQuantifier {
    def apply(start: Parsley[Int], end: Parsley[Option[Option[Int]]]): Parsley[(ToRegex, QuantifierType) => ToRegex] =  (start <~> end).mapFilterMsg {
      case (0, None | Some(Some(0))) => Right((toRegex, _) => ast.Zero(toRegex))
      case (1, None | Some(Some(1))) => Right((toRegex, _) => toRegex)
      case (n, None)                 => Right(ast.Exactly(_, n, _))
      case (0, Some(None))           => Right(ast.Star(_, _))
      case (n, Some(None))           => Right(ast.AtLeast(_, n, _))
      case (0, Some(Some(1)))        => Right(ast.Opt(_, _))
      case (0, Some(Some(m)))        => Right(ast.AtMost(_, m, _))
      case (n, Some(Some(m)))        => if n == m then Right(ast.Exactly(_, n, _))
                                        else if n < m then Right(ast.Between(_, n, m, _))
                                        else Left(Seq("Upper bound cannot be less than lower bound"))
    }
  }

  object WithFlags extends PureParserBridge3[List[Char], Option[NonEmptyList[Char]], Option[ToRegex], ToRegex] {
    override def apply(on: List[Char], off: Option[NonEmptyList[Char]], mInner: Option[ToRegex]): ToRegex = {
      val (onSet, offSet) = flags(on, off)
      mInner match {
        case None        => ast.Flags(onSet, offSet)
        case Some(inner) => ast.NonCapture(onSet, offSet, inner)
      }
    }
  }

  private [internal] val allSet = Diet.fromRange(Range(0x00000, 0x1ffff))

  private inline def ast(using ast: AST): ast.type = ast

  private def flags(on: List[Char], off: Option[NonEmptyList[Char]]): (Set[Char], Set[Char]) = {
    val onSet = on.toSet
    val offSet = off.fold(Nil)(_.toList).toSet
    (onSet -- offSet, offSet)
  }
}
