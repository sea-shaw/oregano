package oregano.internal.ast

import oregano.internal.hchain.*
import oregano.internal.sanitised.*
import scala.quoted.{Expr, Quotes, Type}

trait Rep0Types { this: Tidy =>
  /* Type of a `Rep0` node. */
  protected sealed trait Rep0Type[F[_ <: Rep] <: HChain, G[_ <: Rep] <: HChain] { this: NodeType[G] =>
    final val asNodeType: NodeType[G] & Rep0Type[F, G] = this
    def sanitiseCode[R <: Rep: Type](sanitisedInner: => SanitiseExpr[F[true]])(using Quotes): SanitiseExpr[G[R]]
    def getCode[R <: Rep: Type](sanitisedInner: => SanitiseExpr[F[true]])(using Quotes): Expr[G[R]]
  }

  protected object Rep0Type {
    def apply[F[_ <: Rep] <: HChain](inner: Tidiable[F])(using Quotes): Rep0Type[F, ?] = {
      given Type[F] = inner.nodeType.tpe
      inner.nodeType match {
        case _: HEmptyType => Rep0Empty()
        case option: SingletonOption[f] => {
          given Type[f] = option.innerType
          Rep0Opt(option)
        }
        case _: HNonEmptyType[_] => Rep0NonEmpty(inner)
      }
    }
  }

  /* A* */
  private class Rep0Empty(using Type[Const[HEmpty]]) extends Rep0Type[Const[HEmpty], Const[HEmpty]] with HEmptyType {
    override def sanitiseCode[R <: Rep: Type](sanitisedInner: => SanitiseExpr[Const[HEmpty][true]])(using Quotes): SanitiseExpr[Const[HEmpty][R]] = {
      sanitiseEmpty
    }

    override def getCode[R <: Rep: Type](sanitisedInner: => SanitiseExpr[Const[HEmpty][true]])(using Quotes): Expr[HEmpty] = {
      '{ HEmpty }
    }
  }

  /* (?:(A)?)* */
  private type Rep0OptType[F[_ <: Rep] <: HNonEmpty] = SingletonOptionType[Const[F[true]]]
  private class Rep0Opt[F[_ <: Rep] <: HNonEmpty: Type](innerType: SingletonOption[F])(using Type[Const[F[true]]], Type[Rep0OptType[F]]) extends Rep0Type[SingletonOptionType[F], Rep0OptType[F]] with SingletonOption[Const[F[true]]] {
    override def sanitiseCode[R <: Rep: Type](sanitisedInner: => SanitiseExpr[SingletonOptionType[F][true]])(using Quotes): SanitiseExpr[Rep0OptType[F][R]] = {
      sanitisedInner
    }

    override def getCode[R <: Rep: Type](sanitisedInner: => SanitiseExpr[SingletonOptionType[F][true]])(using Quotes): Expr[HSingleton[Option[F[true]]]] = {
      '{ $sanitisedInner.get.captures }
    }

    override def tidyInner[R <: Rep: Type](using RepType[R])(using Quotes): TidyFunction[F[true], ?] = innerType.tidyInner(using RepTrue)
  }

  /* (A)* */
  private type Rep0NonEmptyType[F[_ <: Rep] <: HNonEmpty] = SingletonOptionType[Const[F[true]]]
  private class Rep0NonEmpty[F[_ <: Rep] <: HNonEmpty: Type](inner: Tidiable[F])(using Type[Const[F[true]]], Type[Rep0NonEmptyType[F]]) extends Rep0Type[F, Rep0NonEmptyType[F]] with SingletonOption[Const[F[true]]] {
    override def sanitiseCode[R <: Rep: Type](sanitisedInner: => SanitiseExpr[F[true]])(using Quotes): SanitiseExpr[Rep0NonEmptyType[F][R]] = {
      sanitiseOpt(sanitisedInner)
    }

    override def getCode[R <: Rep: Type](sanitisedInner: => SanitiseExpr[F[true]])(using Quotes): Expr[HSingleton[Option[F[true]]]] = {
      getOpt(sanitisedInner)
    }

    override def tidyInner[R <: Rep: Type](using RepType[R])(using Quotes): TidyFunction[F[true], ?] = inner.tidyFunction(using RepTrue)
  }
}
