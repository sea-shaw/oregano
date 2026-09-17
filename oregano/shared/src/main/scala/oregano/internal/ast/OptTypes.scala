package oregano.internal.ast

import oregano.internal.hchain.*
import oregano.internal.sanitised.*
import scala.quoted.{Expr, Quotes, Type}

trait OptTypes { this: Tidy =>
  /* Type of an `Opt` node. */
  protected sealed trait OptType[F[_ <: Rep] <: HChain, G[_ <: Rep] <: HChain] { this: NodeType[G] =>
    final val asNodeType: NodeType[G] & OptType[F, G] = this
    def sanitiseCode[R <: Rep: Type](sanitisedInner: => SanitiseExpr[F[R]])(using Quotes): SanitiseExpr[G[R]]
    def getCode[R <: Rep: Type](sanitisedInner: => SanitiseExpr[F[R]])(using Quotes): Expr[G[R]]
  }

  protected object OptType {
    def apply[F[_ <: Rep] <: HChain](inner: Tidiable[F])(using Quotes): OptType[F, ?] = {
      inner.nodeType match {
        case _: HEmptyType => OptEmpty()
        case singletonOption: SingletonOption[f] => {
          given Type[f] = singletonOption.innerType
          OptNested(singletonOption)
        }
        case nonEmpty: HNonEmptyType[f] => {
          given Type[f] = nonEmpty.tpe
          OptSingleton(inner)
        }
      }
    }
  }

  /* A? */
  private class OptEmpty(using Type[Const[HEmpty]]) extends OptType[Const[HEmpty], Const[HEmpty]] with HEmptyType {
    override def sanitiseCode[R <: Rep: Type](sanitisedInner: => SanitiseExpr[Const[HEmpty][R]])(using Quotes): SanitiseExpr[Const[HEmpty][R]] = {
      sanitiseEmpty
    }

    override def getCode[R <: Rep: Type](sanitisedInner: => SanitiseExpr[Const[HEmpty][R]])(using Quotes): Expr[HEmpty] = {
      '{ HEmpty }
    }
  }

  /* (A)? */
  private type OptSingletonType = SingletonOptionType
  private class OptSingleton[F[_ <: Rep] <: HNonEmpty](inner: Tidiable[F])(using Type[F], Type[OptSingletonType[F]]) extends OptType[F, OptSingletonType[F]] with SingletonOption[F] {
    override def sanitiseCode[R <: Rep: Type](sanitisedInner: => SanitiseExpr[F[R]])(using Quotes): SanitiseExpr[OptSingletonType[F][R]] = {
      sanitiseOpt(sanitisedInner)
    }

    override def getCode[R <: Rep: Type](sanitisedInner: => SanitiseExpr[F[R]])(using Quotes): Expr[HSingleton[Option[F[R]]]] = {
      getOpt(sanitisedInner)
    }

    override def tidyInner[R <: Rep: Type](using RepType[R])(using Quotes): TidyFunction[F[R], ?] = inner.tidyFunction
  }

  /* (A?)? */
  private type OptNestedType = SingletonOptionType
  private class OptNested[F[_ <: Rep] <: HNonEmpty](innerType: SingletonOption[F])(using Type[F], Type[OptNestedType[F]]) extends OptType[OptNestedType[F], OptNestedType[F]] with SingletonOption[F] {
    override def sanitiseCode[R <: Rep: Type](sanitisedInner: => SanitiseExpr[OptNestedType[F][R]])(using Quotes): SanitiseExpr[OptNestedType[F][R]] = {
      sanitisedInner
    }

    override def getCode[R <: Rep: Type](sanitisedInner: => SanitiseExpr[OptNestedType[F][R]])(using Quotes): Expr[HSingleton[Option[F[R]]]] = {
      '{ $sanitisedInner.get.captures }
    }

    override def tidyInner[R <: Rep: Type](using RepType[R])(using Quotes): TidyFunction[F[R], ?] = innerType.tidyInner
  }
}
