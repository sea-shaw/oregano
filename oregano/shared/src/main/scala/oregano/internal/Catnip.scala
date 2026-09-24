package oregano.internal

import cats.data.Ior
import oregano.internal.ast.AST
import scala.quoted.{Expr, Quotes, Type}

/* Implementation of `AST` using `cats.data.Ior`. */
private object Catnip extends AST {
    type InclusiveOr = Ior

    override protected def inclusiveOrType(using Quotes): Type[InclusiveOr] = Type.of[InclusiveOr]

    override protected def fromLeft[A: Type](left: Expr[A])(using Quotes): Expr[InclusiveOr[A, Nothing]] = {
        '{ Ior.Left($left) }
    }

    override protected def fromRight[B: Type](right: Expr[B])(using Quotes): Expr[InclusiveOr[Nothing, B]] = {
        '{ Ior.Right($right) }
    }

    override protected def fromBoth[A: Type, B: Type](left: Expr[A], right: Expr[B])(using Quotes): Expr[InclusiveOr[A, B]] = {
        '{ Ior.Both($left, $right) }
    }

    override protected def bimap[A: Type, B: Type, C: Type, D: Type](f: Expr[A] => Quotes ?=> Expr[C], g: Expr[B] => Quotes ?=> Expr[D])(expr: Expr[Ior[A, B]])(using Quotes): Expr[InclusiveOr[C, D]] = {
        '{
            $expr match {
                case Ior.Left(left)        => Ior.Left(${ f('left) })
                case Ior.Right(right)      => Ior.Right(${ g('right) })
                case Ior.Both(left, right) => Ior.Both(${ f('left) }, ${ g('right) })
            }
        }
    }
}
