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
    def apply[F[_ <: Rep] <: HChain](inner: Tidiable[F]): Rep0Type[F, ?] = {
      inner.nodeType match {
        case _: HEmptyType              => Rep0Empty
        case option: SingletonOption[f] => Rep0Opt(option)
        case _: HNonEmptyType[_]        => Rep0NonEmpty(inner)
      }
    }
  }

  /* A* */
  private object Rep0Empty extends Rep0Type[Const[HEmpty], Const[HEmpty]] with HEmptyType {
    override def sanitiseCode[R <: Rep: Type](sanitisedInner: => SanitiseExpr[Const[HEmpty][true]])(using Quotes): SanitiseExpr[Const[HEmpty][R]] = {
      sanitiseEmpty
    }

    override def getCode[R <: Rep: Type](sanitisedInner: => SanitiseExpr[Const[HEmpty][true]])(using Quotes): Expr[HEmpty] = {
      '{ HEmpty }
    }
  }

  /* (?:(A)?)* */
  private type Rep0OptType[F[_ <: Rep] <: HNonEmpty] = SingletonOptionType[Const[F[true]]]
  private class Rep0Opt[F[_ <: Rep] <: HNonEmpty](inner: SingletonOption[F]) extends Rep0Type[SingletonOptionType[F], Rep0OptType[F]] with SingletonOption[Const[F[true]]] {
    override def innerType(using Quotes): Type[Const[F[true]]] = {
      given Type[F] = inner.innerType

      Type.of[Const[F[true]]]
    }

    override def sanitiseCode[R <: Rep: Type](sanitisedInner: => SanitiseExpr[SingletonOptionType[F][true]])(using Quotes): SanitiseExpr[Rep0OptType[F][R]] = {
      sanitisedInner
    }

    override def getCode[R <: Rep: Type](sanitisedInner: => SanitiseExpr[SingletonOptionType[F][true]])(using Quotes): Expr[HSingleton[Option[F[true]]]] = {
      given Type[F] = inner.innerType

      '{ $sanitisedInner.get.captures }
    }

    override def tidyInner[R <: Rep: Type](using RepType[R])(using Quotes): TidyFunction[F[true], ?] = inner.tidyInner(using RepTrue)
  }

  /* (A)* */
  private type Rep0NonEmptyType[F[_ <: Rep] <: HNonEmpty] = SingletonOptionType[Const[F[true]]]
  private class Rep0NonEmpty[F[_ <: Rep] <: HNonEmpty](inner: Tidiable[F]) extends Rep0Type[F, Rep0NonEmptyType[F]] with SingletonOption[Const[F[true]]] {
    override def innerType(using Quotes): Type[Const[F[true]]] = {
      given Type[F] = inner.tpe

      Type.of[Const[F[true]]]
    }

    override def sanitiseCode[R <: Rep: Type](sanitisedInner: => SanitiseExpr[F[true]])(using Quotes): SanitiseExpr[Rep0NonEmptyType[F][R]] = {
      given Type[F] = inner.tpe

      sanitiseOpt(sanitisedInner)
    }

    override def getCode[R <: Rep: Type](sanitisedInner: => SanitiseExpr[F[true]])(using Quotes): Expr[HSingleton[Option[F[true]]]] = {
      given Type[F] = inner.tpe

      getOpt(sanitisedInner)
    }

    override def tidyInner[R <: Rep: Type](using RepType[R])(using Quotes): TidyFunction[F[true], ?] = inner.tidyFunction(using RepTrue)
  }
}
