package oregano.internal.ast

import cats.collections.Diet
import oregano.internal.hchain.*
import oregano.internal.sanitised.*
import scala.quoted.{Expr, Type, Quotes}

/* Trait containig the definition of the `AST` nodes. Implemented by `Oregano`
   and `Catnip`. Path-dependent types prevent mixing nodes between the two. */
trait AST extends Tidy, BuildFunction, EmptyTypes, CapturingTypes, CatTypes, AltTypes, OptTypes, Rep1Types, Rep0Types {
    sealed abstract class Regex[F[_ <: Rep] <: HChain](nodeType: NodeType[F]) extends Tidiable[F](nodeType) {
        /* Number of capturing groups, including discarded ones. */
        val numCaptures: Int

        /* Returns the code to construct an `HChain` from `groups` starting with
       group `i`. `R` is true if this node is repeated and false otherwise. */
        def sanitiseCode[R <: Rep: Type](str: Expr[CharSequence], groups: Expr[Groups], i: Int)(using RepType[R])(using Quotes): SanitiseExpr[F[R]]

        def getCode[R <: Rep: Type](str: Expr[CharSequence], groups: Expr[Groups], i: Int)(using RepType[R])(using Quotes): Expr[F[R]]
    }

    /* Node with no capturing groups. */
    sealed abstract class Empty extends Regex[Const[HEmpty]](EmptyType) {
        override final def sanitiseCode[R <: Rep: Type](str: Expr[CharSequence], groups: Expr[Groups], i: Int)(using RepType[R])(using Quotes): SanitiseExpr[Const[HEmpty][R]] = {
            sanitiseEmpty
        }

        override final def getCode[R <: Rep: Type](str: Expr[CharSequence], groups: Expr[Groups], i: Int)(using RepType[R])(using Quotes): Expr[HEmpty] = {
            '{ HEmpty }
        }
    }

    /* Leaf node with no capturing groups and no children. */
    sealed abstract class EmptyLeaf extends Empty {
        override final val numCaptures: Int = 0
    }

    /* a */
    case class Lit(c: Int) extends EmptyLeaf

    /* [a-z] */
    case class Class(cs: Diet[Int]) extends EmptyLeaf

    /* ^ */
    case object LineStart extends EmptyLeaf

    /* $ */
    case object LineEnd extends EmptyLeaf

    /* \n */
    case class Backreference(group: Int) extends EmptyLeaf

    /* (?idmsuxU-idmsuxU) */
    case class Flags(flagsOn: Set[Char], flagsOff: Set[Char]) extends EmptyLeaf

    /* Wrapper that discards the capturing groups of `inner`. */
    sealed abstract class EmptyWrapper[F[_ <: Rep] <: HChain] protected (inner: Regex[F]) extends Empty {
        override final val numCaptures: Int = inner.numCaptures
    }

    /* A{0} or A{0,0} */
    case class Zero[F[_ <: Rep] <: HChain](inner: Regex[F]) extends EmptyWrapper[F](inner)

    /* (?!A) */
    case class NegativeLookahead[F[_ <: Rep] <: HChain](inner: Regex[F]) extends EmptyWrapper[F](inner)

    /* (?<!A) */
    case class NegativeLookbehind[F[_ <: Rep] <: HChain](inner: Regex[F]) extends EmptyWrapper[F](inner)

    /* Capturing group. */
    sealed abstract class Capturing[F[_ <: Rep] <: HChain, G[_ <: Rep] <: HChain] protected (inner: Regex[F])(capturingType: CapturingType[F, G]) extends Regex[G](capturingType.asNodeType) {
        override final val numCaptures: Int = inner.numCaptures + 1

        override final def sanitiseCode[R <: Rep: Type](str: Expr[CharSequence], groups: Expr[Groups], i: Int)(using RepType[R])(using Quotes): SanitiseExpr[G[R]] = {
            val sanitisedCapture = '{
                val l = $groups(${ Expr(2 * i) })
                val u = $groups(${ Expr(2 * i + 1) })
                if l >= 0 && u >= 0 then Some($str.subSequence(l, u).toString) else None
            }
            capturingType.sanitiseCode(sanitisedCapture, inner.getCode(str, groups, i + 1))
        }

        override final def getCode[R <: Rep: Type](str: Expr[CharSequence], groups: Expr[Groups], i: Int)(using RepType[R])(using Quotes): Expr[G[R]] = {
            val capture = '{ HSingleton($str.subSequence($groups(${ Expr(2 * i) }), $groups(${ Expr(2 * i + 1) })).toString) }
            capturingType.getCode(capture, inner.getCode(str, groups, i + 1))
        }
    }

    /* (A) */
    case class Capture[F[_ <: Rep] <: HChain, G[_ <: Rep] <: HChain] private (inner: Regex[F])(capturingType: CapturingType[F, G]) extends Capturing[F, G](inner)(capturingType)
    object Capture {
        def apply[F[_ <: Rep] <: HChain](inner: Regex[F]): Capture[F, ?] = {
            new Capture(inner)(CapturingType(inner))
        }
    }

    /* (?<name>A) */
    case class NamedCapture[F[_ <: Rep] <: HChain, G[_ <: Rep] <: HChain] private (name: String, inner: Regex[F])(capturingType: CapturingType[F, G]) extends Capturing[F, G](inner)(capturingType)
    object NamedCapture {
        def apply[F[_ <: Rep] <: HChain](name: String, inner: Regex[F]): NamedCapture[F, ?] = {
            new NamedCapture(name, inner)(CapturingType(inner))
        }
    }

    /* Wrapper around `inenr` that doesn't affect its type. */
    sealed abstract class Wrapper[F[_ <: Rep] <: HChain] protected (inner: Regex[F]) extends Regex[F](inner.nodeType) {
        override final val numCaptures: Int = inner.numCaptures

        override final def sanitiseCode[R <: Rep: Type](str: Expr[CharSequence], groups: Expr[Groups], i: Int)(using RepType[R])(using Quotes): SanitiseExpr[F[R]] = {
            inner.sanitiseCode(str, groups, i)
        }

        override final def getCode[R <: Rep: Type](str: Expr[CharSequence], groups: Expr[Groups], i: Int)(using RepType[R])(using Quotes): Expr[F[R]] = {
            inner.getCode(str, groups, i)
        }
    }

    /* (?:A) */
    case class NonCapture[F[_ <: Rep] <: HChain](flagsOn: Set[Char], flagsOff: Set[Char], inner: Regex[F]) extends Wrapper[F](inner)
    case class PositiveLookahead[F[_ <: Rep] <: HChain](inner: Regex[F]) extends Wrapper[F](inner)
    case class PositiveLookbehind[F[_ <: Rep] <: HChain](inner: Regex[F]) extends Wrapper[F](inner)
    case class Independent[F[_ <: Rep] <: HChain](inner: Regex[F]) extends Wrapper[F](inner)

    /* AB */
    case class Cat[F[_ <: Rep] <: HChain, G[_ <: Rep] <: HChain, H[_ <: Rep] <: HChain] private (left: Regex[F], right: Regex[G])(catType: CatType[F, G, H]) extends Regex[H](catType.asNodeType) {
        override val numCaptures: Int = left.numCaptures + right.numCaptures

        override def sanitiseCode[R <: Rep: Type](str: Expr[CharSequence], groups: Expr[Groups], i: Int)(using RepType[R])(using Quotes): SanitiseExpr[H[R]] = {
            lazy val sanitisedLeft = left.sanitiseCode(str, groups, i)
            lazy val sanitisedRight = right.sanitiseCode(str, groups, i + left.numCaptures)
            catType.sanitiseCode(sanitisedLeft, sanitisedRight)
        }

        override def getCode[R <: Rep: Type](str: Expr[CharSequence], groups: Expr[Groups], i: Int)(using RepType[R])(using Quotes): Expr[H[R]] = {
            lazy val getLeft = left.getCode(str, groups, i)
            lazy val getRight = right.getCode(str, groups, i + left.numCaptures)
            catType.getCode(getLeft, getRight)
        }
    }

    object Cat {
        def apply[F[_ <: Rep] <: HChain, G[_ <: Rep] <: HChain](left: Regex[F], right: Regex[G]): Cat[F, G, ?] = {
            new Cat(left, right)(CatType(left, right))
        }
    }

    /* A|B */
    case class Alt[F[_ <: Rep] <: HChain, G[_ <: Rep] <: HChain, H[_ <: Rep] <: HChain] private (left: Regex[F], right: Regex[G])(altType: AltType[F, G, H]) extends Regex[H](altType.asNodeType) {
        override val numCaptures: Int = left.numCaptures + right.numCaptures

        override def sanitiseCode[R <: Rep: Type](str: Expr[CharSequence], groups: Expr[Groups], i: Int)(using rep: RepType[R])(using Quotes): SanitiseExpr[H[R]] = {
            lazy val sanitisedLeft = left.sanitiseCode(str, groups, i)
            lazy val sanitisedRight = right.sanitiseCode(str, groups, i + left.numCaptures)

            altType.sanitiseCode(sanitisedLeft, sanitisedRight)
        }

        override def getCode[R <: Rep: Type](str: Expr[CharSequence], groups: Expr[Groups], i: Int)(using RepType[R])(using Quotes): Expr[H[R]] = {
            given Type[H] = nodeType.tpe
            '{ ${ sanitiseCode(str, groups, i) }.get.captures }
        }
    }

    object Alt {
        def apply[F[_ <: Rep] <: HChain, G[_ <: Rep] <: HChain](left: Regex[F], right: Regex[G]): Alt[F, G, ?] = {
            new Alt(left, right)(AltType(left, right))
        }
    }

    /* A? */
    case class Opt[F[_ <: Rep] <: HChain, G[_ <: Rep] <: HChain] private (inner: Regex[F], quantifierType: QuantifierType)(optType: OptType[F, G]) extends Regex[G](optType.asNodeType) {
        override val numCaptures: Int = inner.numCaptures

        override def sanitiseCode[R <: Rep: Type](str: Expr[CharSequence], groups: Expr[Groups], i: Int)(using RepType[R])(using Quotes): SanitiseExpr[G[R]] = {
            lazy val sanitisedInner = inner.sanitiseCode(str, groups, i)
            optType.sanitiseCode(sanitisedInner)
        }

        override def getCode[R <: Rep: Type](str: Expr[CharSequence], groups: Expr[Groups], i: Int)(using RepType[R])(using Quotes): Expr[G[R]] = {
            lazy val sanitisedInner = inner.sanitiseCode(str, groups, i)
            optType.getCode(sanitisedInner)
        }
    }

    object Opt {
        def apply[F[_ <: Rep] <: HChain](inner: Regex[F], quantifierType: QuantifierType): Opt[F, ?] = {
            new Opt(inner, quantifierType)(OptType(inner))
        }
    }

    /* Repeat `inner` one or more times. */
    sealed abstract class Rep1[F[_ <: Rep] <: HChain, G[_ <: Rep] <: HChain](inner: Regex[F])(rep1Type: Rep1Type[F, G]) extends Regex[G](rep1Type.asNodeType) {
        override final val numCaptures: Int = inner.numCaptures

        override final def sanitiseCode[R <: Rep: Type](str: Expr[CharSequence], groups: Expr[Groups], i: Int)(using RepType[R])(using Quotes): SanitiseExpr[G[R]] = {
            lazy val sanitisedInner = inner.sanitiseCode(str, groups, i)(using RepTrue)
            rep1Type.sanitiseCode(sanitisedInner)
        }

        override final def getCode[R <: Rep: Type](str: Expr[CharSequence], groups: Expr[Groups], i: Int)(using RepType[R])(using Quotes): Expr[G[R]] = {
            lazy val getInner = inner.getCode(str, groups, i)(using RepTrue)
            rep1Type.getCode(getInner)
        }
    }

    /* A+ */
    case class Plus[F[_ <: Rep] <: HChain, G[_ <: Rep] <: HChain](inner: Regex[F], quantifierType: QuantifierType)(nodeType: Rep1Type[F, G]) extends Rep1(inner)(nodeType)
    object Plus {
        def apply[F[_ <: Rep] <: HChain](inner: Regex[F], quantifierType: QuantifierType): Plus[F, ?] = {
            new Plus(inner, quantifierType)(Rep1Type(inner))
        }
    }

    /* A{n} for n >= 2. */
    case class Exactly[F[_ <: Rep] <: HChain, G[_ <: Rep] <: HChain] private (inner: Regex[F], n: Int, quantifierType: QuantifierType)(nodeType: Rep1Type[F, G]) extends Rep1[F, G](inner)(nodeType)
    object Exactly {
        def apply[F[_ <: Rep] <: HChain](inner: Regex[F], n: Int, quantifierType: QuantifierType): Exactly[F, ?] = {
            new Exactly(inner, n, quantifierType)(Rep1Type(inner))
        }
    }

    /* A{n,} for n >= 1. Use `Star` for {0,} */
    case class AtLeast[F[_ <: Rep] <: HChain, G[_ <: Rep] <: HChain] private (inner: Regex[F], n: Int, quantifierType: QuantifierType)(nodeType: Rep1Type[F, G]) extends Rep1[F, G](inner)(nodeType)
    object AtLeast {
        def apply[F[_ <: Rep] <: HChain](inner: Regex[F], n: Int, quantifierType: QuantifierType): AtLeast[F, ?] = {
            new AtLeast(inner, n, quantifierType)(Rep1Type(inner))
        }
    }

    /* A{n, m} for n >= 1, m >= 2. */
    case class Between[F[_ <: Rep] <: HChain, G[_ <: Rep] <: HChain] private (inner: Regex[F], n: Int, m: Int, quantifierType: QuantifierType)(nodeType: Rep1Type[F, G]) extends Rep1[F, G](inner)(nodeType)
    object Between {
        def apply[F[_ <: Rep] <: HChain](inner: Regex[F], n: Int, m: Int, quantifierType: QuantifierType): Between[F, ?] = {
            new Between(inner, n, m, quantifierType)(Rep1Type(inner))
        }
    }

    /* Repeat `inner` zero or more times. */
    sealed abstract class Rep0[F[_ <: Rep] <: HChain, G[_ <: Rep] <: HChain](inner: Regex[F])(rep0Type: Rep0Type[F, G]) extends Regex[G](rep0Type.asNodeType) {
        override final val numCaptures: Int = inner.numCaptures

        override final def sanitiseCode[R <: Rep: Type](str: Expr[CharSequence], groups: Expr[Groups], i: Int)(using RepType[R])(using Quotes): SanitiseExpr[G[R]] = {
            lazy val sanitisedInner = inner.sanitiseCode(str, groups, i)(using RepTrue)
            rep0Type.sanitiseCode(sanitisedInner)
        }

        override final def getCode[R <: Rep: Type](str: Expr[CharSequence], groups: Expr[Groups], i: Int)(using RepType[R])(using Quotes): Expr[G[R]] = {
            lazy val sanitisedInner = inner.sanitiseCode(str, groups, i)(using RepTrue)
            rep0Type.getCode(sanitisedInner)
        }
    }

    /* A* */
    case class Star[F[_ <: Rep] <: HChain, G[_ <: Rep] <: HChain] private (inner: Regex[F], quantifierType: QuantifierType)(nodeType: Rep0Type[F, G]) extends Rep0[F, G](inner)(nodeType)
    object Star {
        def apply[F[_ <: Rep] <: HChain](inner: Regex[F], quantifierType: QuantifierType) = {
            new Star(inner, quantifierType)(Rep0Type(inner))
        }
    }

    /* A{0, m} for m >= 2. Use `Opt` for {0, 1} and `Zero` for {0, 0}. */
    case class AtMost[F[_ <: Rep] <: HChain, G[_ <: Rep] <: HChain] private (inner: Regex[F], n: Int, quantifierType: QuantifierType)(nodeType: Rep0Type[F, G]) extends Rep0[F, G](inner)(nodeType)
    object AtMost {
        def apply[F[_ <: Rep] <: HChain](inner: Regex[F], n: Int, quantifierType: QuantifierType) = {
            new AtMost(inner, n, quantifierType)(Rep0Type(inner))
        }
    }
}
