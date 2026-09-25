package oregano.internal.ast

import oregano.internal.hchain.*
import oregano.internal.sanitised.*
import scala.quoted.{Expr, Quotes, Type}

private trait OptTypes { this: Tidy =>
    /* Type of an `Opt` node. */
    protected sealed trait OptType[F[_ <: Rep] <: HChain, G[_ <: Rep] <: HChain] { this: NodeType[G] =>
        final val asNodeType: NodeType[G] & OptType[F, G] = this
        def sanitiseCode[R <: Rep: Type](sanitisedInner: => SanitiseExpr[F[R]])(using Quotes): SanitiseExpr[G[R]]
        def getCode[R <: Rep: Type](sanitisedInner: => SanitiseExpr[F[R]])(using Quotes): Expr[G[R]]
    }

    protected object OptType {
        def apply[F[_ <: Rep] <: HChain](inner: Tidiable[F]): OptType[F, ?] = {
            inner.nodeType match {
                case _: HEmptyType                       => OptEmpty
                case singletonOption: SingletonOption[f] => OptNested(singletonOption)
                case _: HNonEmptyType[f]                 => OptSingleton(inner)
            }
        }
    }

    /* A? */
    private object OptEmpty extends OptType[Const[HEmpty], Const[HEmpty]] with HEmptyType {
        override def sanitiseCode[R <: Rep: Type](sanitisedInner: => SanitiseExpr[Const[HEmpty][R]])(using Quotes): SanitiseExpr[Const[HEmpty][R]] = {
            sanitiseEmpty
        }

        override def getCode[R <: Rep: Type](sanitisedInner: => SanitiseExpr[Const[HEmpty][R]])(using Quotes): Expr[HEmpty] = {
            '{ HEmpty }
        }
    }

    /* (A)? */
    private type OptSingletonType = SingletonOptionType
    private class OptSingleton[F[_ <: Rep] <: HNonEmpty](inner: Tidiable[F]) extends OptType[F, OptSingletonType[F]] with SingletonOption[F] {
        override def innerType(using Quotes): Type[F] = inner.tpe

        override def sanitiseCode[R <: Rep: Type](sanitisedInner: => SanitiseExpr[F[R]])(using Quotes): SanitiseExpr[OptSingletonType[F][R]] = {
            given Type[F] = innerType

            sanitiseOpt(sanitisedInner)
        }

        override def getCode[R <: Rep: Type](sanitisedInner: => SanitiseExpr[F[R]])(using Quotes): Expr[HSingleton[Option[F[R]]]] = {
            given Type[F] = innerType

            getOpt(sanitisedInner)
        }

        override def tidyInner[R <: Rep: Type](using RepType[R])(using Quotes): TidyFunction[F[R], ?] = inner.tidyFunction
    }

    /* (A?)? */
    private type OptNestedType = SingletonOptionType
    private class OptNested[F[_ <: Rep] <: HNonEmpty](inner: SingletonOption[F]) extends OptType[OptNestedType[F], OptNestedType[F]] with SingletonOption[F] {
        override def innerType(using Quotes): Type[F] = inner.innerType

        override def sanitiseCode[R <: Rep: Type](sanitisedInner: => SanitiseExpr[OptNestedType[F][R]])(using Quotes): SanitiseExpr[OptNestedType[F][R]] = {
            sanitisedInner
        }

        override def getCode[R <: Rep: Type](sanitisedInner: => SanitiseExpr[OptNestedType[F][R]])(using Quotes): Expr[HSingleton[Option[F[R]]]] = {
            given Type[F] = innerType

            '{ $sanitisedInner.get.captures }
        }

        override def tidyInner[R <: Rep: Type](using RepType[R])(using Quotes): TidyFunction[F[R], ?] = inner.tidyInner
    }
}
