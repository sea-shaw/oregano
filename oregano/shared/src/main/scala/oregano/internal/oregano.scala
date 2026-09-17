package oregano.internal

import oregano.internal.ast.AST
import scala.quoted.{Expr, Quotes, Type}

type EitherIor[+A, +B] = Either[Either[A, B], (A, B)]

private class Oregano(using Type[EitherIor]) extends AST {
  type InclusiveOr = EitherIor
  
  protected def fromLeft[A: Type](left: Expr[A])(using Quotes): Expr[InclusiveOr[A, Nothing]] = {
    '{ Left(Left($left)) }
  }

  protected def fromRight[B: Type](right: Expr[B])(using Quotes): Expr[InclusiveOr[Nothing, B]] = {
    '{ Left(Right($right)) }
  }

  protected def fromBoth[A: Type, B: Type](left: Expr[A], right: Expr[B])(using Quotes): Expr[InclusiveOr[A, B]] = {
    '{ Right(($left, $right)) }
  }

  override protected def bimap[A: Type, B: Type, C: Type, D: Type](f: Expr[A] => Quotes ?=> Expr[C], g: Expr[B] => Quotes ?=> Expr[D])(expr: Expr[InclusiveOr[A, B]])(using Quotes): Expr[InclusiveOr[C, D]] = {
    '{
      $expr match {
        case Left(Left(left))     => Left(Left(${ f('left) }))
        case Left(Right(right))   => Left(Right(${ g('right) }))
        case Right((left, right)) => Right((${ f('left) }, ${ g('right) }))
      }
    }
  }
}
