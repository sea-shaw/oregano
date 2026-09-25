package oregano.internal.ast

import oregano.internal.hchain.*
import oregano.internal.sanitised.*
import scala.quoted.{Expr, Quotes, Type}

private trait Rep1Types { this: Tidy =>
    /* Type of a `Rep1` node. */
    protected sealed trait Rep1Type[F[_ <: Rep] <: HChain, G[_ <: Rep] <: HChain] { this: NodeType[G] =>
        final val asNodeType: NodeType[G] & Rep1Type[F, G] = this
        def sanitiseCode[R <: Rep](sanitisedInner: => SanitiseExpr[F[true]])(using Quotes): SanitiseExpr[G[R]]
        def getCode[R <: Rep](getInner: => Expr[F[true]])(using Quotes): Expr[G[R]]
    }

    protected object Rep1Type {
        def apply[F[_ <: Rep] <: HChain](inner: Tidiable[F]): Rep1Type[F, ?] = {
            inner.nodeType match {
                case _: HEmptyType       => Rep1Empty
                case _: HNonEmptyType[_] => Rep1NonEmpty(inner)
            }
        }
    }

    /* A+ */
    private object Rep1Empty extends Rep1Type[Const[HEmpty], Const[HEmpty]] with HEmptyType {
        override def sanitiseCode[R <: Rep](sanitisedInner: => SanitiseExpr[Const[HEmpty][true]])(using Quotes): SanitiseExpr[Const[HEmpty][R]] = {
            sanitiseEmpty
        }

        override def getCode[R <: Rep](getInner: => Expr[Const[HEmpty][true]])(using Quotes): Expr[HEmpty] = {
            '{ HEmpty }
        }
    }

    /* (A)+ */
    private type Rep1NonEmptyType[F[_ <: Rep] <: HNonEmpty] = Const[F[true]]
    private class Rep1NonEmpty[F[_ <: Rep] <: HNonEmpty](inner: Tidiable[F]) extends Rep1Type[F, Rep1NonEmptyType[F]] with HNonEmptyType[Rep1NonEmptyType[F]] {
        override def tpe(using Quotes): Type[Rep1NonEmptyType[F]] = {
            given Type[F] = inner.tpe

            Type.of[Rep1NonEmptyType[F]]
        }

        override def sanitiseCode[R <: Rep](sanitisedInner: => SanitiseExpr[F[true]])(using Quotes): SanitiseExpr[Rep1NonEmptyType[F][R]] = {
            sanitisedInner
        }

        override def getCode[R <: Rep](getInner: => Expr[F[true]])(using Quotes): Expr[F[true]] = {
            getInner
        }

        override def flattenFunction[C <: Chains, L <: Leaves, R <: Rep: Type](nodes: Nodes[C], types: Types[L])(using RepType[R])(using Quotes): FlattenFunction[CCons[F[true], C], L, ?] = {
            inner.flattenFunction(nodes, types)(using RepTrue)
        }
    }
}
