package oregano.internal.parsing

import cats.collections.{Diet, Range}
import cats.data.NonEmptyList
import oregano.internal.ast.{AST, Greedy, Lazy, Possessive}
import oregano.internal.parsing.bridges.*
import parsley.{Parsley, Result}
import parsley.character.{hexDigit, octDigit}
import parsley.combinator.{choice, option, range, sepBy1}
import parsley.errors.ErrorBuilder
import parsley.errors.combinator.*
import parsley.expr.chain
import parsley.quick.{atomic, empty, eof, many, noneOf, oneOf, notFollowedBy, pure}
import parsley.syntax.character.{charLift, stringLift}
import parsley.syntax.all.*
import parsley.token.Lexer
import parsley.token.descriptions.{LexicalDesc, NumericDesc}

private [internal] object parser {

    def parse[Err: ErrorBuilder](s: String)(using ast: AST): Result[Err, ast.Regex[?]] = {
        regex.parse(s).map(_(using ast))
    }

    private[internal] def parseClass[Err: ErrorBuilder](s: String)(using ast: AST): Result[Err, ast.Regex[?]] = {
        cls.parse(s).map(_(using ast))
    }

    private def some[A](p: Parsley[A]): Parsley[NonEmptyList[A]] = (p, many(p)).zipped(NonEmptyList(_, _))

    private val regexDesc = LexicalDesc.plain.copy(
      numericDesc = NumericDesc.plain.copy(
        integerNumbersCanBeOctal = false,
        integerNumbersCanBeHexadecimal = false
      )
    )

    private val lexer = Lexer(regexDesc)

    private lazy val regex = expr <~ eof
    private lazy val expr: Parsley[ToRegex] = chain.right1(term)(Alt from '|')
    private lazy val term = Cat(some(atom))
    private lazy val atom = boundary | quantified

    private lazy val boundary = (LineStart from '^') | (LineEnd from '$')

    private lazy val quantified: Parsley[ToRegex] = quantifiable <~> option(postfixOps <~> quantifierType) map {
        case (regex, None)                 => regex
        case (regex, Some(postfix, qType)) => postfix(regex, qType)
    }
    private lazy val quantifiable = choice(positiveLookahead, negativeLookahead, positiveLookbehind, negativeLookbehind, independent, withFlags, capture, lit, dot, predefinedEsc, cls, backreference)

    private lazy val positiveLookahead = PositiveLookahead(atomic("(?=") ~> expr <~ ')')
    private lazy val negativeLookahead = NegativeLookahead(atomic("(?!" ~> expr <~ ')'))
    private lazy val positiveLookbehind = PositiveLookbehind(atomic("(?<=") ~> expr <~ ')')
    private lazy val negativeLookbehind = NegativeLookbehind(atomic("(?<!" ~> expr <~ ')'))
    private lazy val independent = Independent(atomic("(?>") ~> expr <~ ')')
    private lazy val withFlags = WithFlags(atomic("(?") ~> many(flag), option('-' ~> some(flag)), option(':' ~> expr) <~ ')')
    private lazy val capture = Capture('(' ~> expr <~ ')')
    private lazy val lit = Lit(noneOf(keyChars).map(_.toInt) | charEsc)
    private lazy val dot = Dot from '.'

    private lazy val flag = oneOf('i', 'd', 'm', 's', 'u', 'x', 'U') // TODO: U is not allowed for non-capturing groups

    private lazy val charEsc: Parsley[Int] = {
        // corresponds with \x{ ... }
        val hexArbEscX = hexDigit
            .foldLeft1[BigInt](0)((n, d) => n * 16 + d.asDigit)
            .collectMsg("characters cannot exceed largest codepoint 0x1ffff") {
                case n if n <= Character.MAX_CODE_POINT => n.toInt
            }
        // corresponds with \xhh
        val hexFixedEscX = (hexDigit, hexDigit).zipped((d1, d2) => d1.asDigit * 16 + d2.asDigit)
        // corresponds with \uHHHH
        val hexFixedEscU = (hexDigit, hexDigit, hexDigit, hexDigit).zipped((d1, d2, d3, d4) => d1.asDigit * 4096 + d2.asDigit * 256 + d3.asDigit * 16 + d4.asDigit)
        val hexCodeEscX = '{' ~> hexArbEscX <~ '}' | hexFixedEscX
        val octCode = range(min = 1, max = 3)(octDigit).mapFilterMsg { ds =>
            val n = ds.foldLeft(0)((n, d) => n * 8 + d.asDigit)
            if (n > 255) Left(Seq("octal escape sequences cannot be greater than 0377 (255 in decimal)"))
            else Right(n)
        }
        val numeric = 'x' ~> hexCodeEscX | '0' ~> octCode | 'u' ~> hexFixedEscU
        // `\cx`: the control character corresponding to x (@-?) -- space is somehow valid for this, but don't know what to
        val control = 'c' ~> empty
        // TODO: Use `keyChars` to define this
        val single = choice(
          't' as 0x00009,
          'n' as 0x0000a,
          'r' as 0x0000d,
          'f' as 0x0000c,
          'a' as 0x00007,
          'e' as 0x0001b,
          '.' as '.'.toInt,
          '(' as '('.toInt,
          ')' as ')'.toInt,
          '^' as '^'.toInt,
          '|' as '|'.toInt,
          '/' as '/'.toInt // TODO: This isn't a metacharacter but it can be escaped? '/' is also valid without '\'?
        ) // probably a nicer way of escaping dot
        atomic('\\' ~> (single | numeric | control | '\\'.map(_.toInt)))
    }
    private lazy val setEsc: Parsley[Diet[Int]] = empty

    private lazy val cls = {
        lazy val clsSet: Parsley[Diet[Int]] = '[' ~> ('^' ~> clsBody.map(allSet -- _) | clsBody) <~ ']'
        // classes may not be empty, and ] can be used as part of one in that instance: []] is ], but [a]] is a] and [] is an error
        // although []a] is also treated as `a|]`...
        lazy val clsBody = clsIntersect | ']' ~> clsIntersect
        lazy val clsAtom = noneOf(']', '[', '\\', '&').map(_.toInt) | atomic('&'.map(_.toInt) <~ notFollowedBy('&')) | charEsc
        lazy val clsRange = clsAtom.zip(option(atomic('-' ~> clsAtom))).mapFilterMsg {
            case (l, Some(r)) if l < r => Right(Diet.fromRange(Range(l, r)))
            case (l, Some(r))          => Left(Seq(s"ranges must be ascending, but '$l' is greater than '$r'")) // TODO: whitespace in message!
            case (l, None)             => Right(Diet.one(l))
        } | setEsc
        lazy val clsUnion = (clsRange | clsSet).reduceLeft(_ | _)
        // intersection is lowest precedence, but it's a bit of a pain, because [&&X] and [X&&] are legal, but [&&] is not.
        // similarly, [X&&..&&Y] is the same as [X&&Y].
        lazy val clsIntersect = sepBy1(option(clsUnion), "&&").mapFilterMsg { css =>
            css.flatten.reduceOption(_ & _) match
                case Some(cs) => Right(cs)
                case None     => Left(Seq("class intersections cannot be empty on both sides"))
        }
        Class(clsSet)
    }

    private lazy val predefinedEsc = atomic('\\' ~> predefined)
    private lazy val predefined = Class(
      choice(
        'd' as Diet.fromRange(Range('0'.toInt, '9'.toInt)),
        'D' as allSet -- Diet.fromRange(Range('0'.toInt, '9'.toInt)),
        'w' as Diet.fromRange(Range('a'.toInt, 'z'.toInt)) | Diet.fromRange(Range('A'.toInt, 'Z'.toInt)) | Diet.fromRange(Range('0'.toInt, '9'.toInt)) | Diet.one('_'.toInt),
        'W' as allSet -- (Diet.fromRange(Range('a'.toInt, 'z'.toInt)) | Diet.fromRange(Range('A'.toInt, 'Z'.toInt)) | Diet.fromRange(Range('0'.toInt, '9'.toInt)) | Diet.one('_'.toInt)),
        's' as Diet.one(' '.toInt) | Diet.one('\t'.toInt) | Diet.one('\n'.toInt) | Diet.one('\u000B'.toInt) | Diet.one('\r'.toInt) | Diet.one('\f'.toInt),
        'S' as allSet -- (Diet.one(' '.toInt) | Diet.one('\t'.toInt) | Diet.one('\n'.toInt) | Diet.one('\u000B'.toInt) | Diet.one('\r'.toInt) | Diet.one('\f'.toInt))
      )
    )

    private lazy val backreference = Backreference(atomic('\\' ~> int))

    private val keyChars = Set('(', ')', '{', '}', '[', '.', '*', '+', '?', '\\', '|', '$', '^')

    private lazy val postfixOps = (Opt from '?') | (Star from '*') | (Plus from '+') | numericalQuantifier
    private lazy val quantifierType = ('?' as Lazy) | ('+' as Possessive) | pure(Greedy)

    private lazy val numericalQuantifier = NumericalQuantifier('{' ~> int, option(',' ~> option(int)) <~ '}')
    private lazy val int = lexer.lexeme.natural.decimal32[Int]
}
