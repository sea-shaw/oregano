package oregano.internal

import scala.quoted.Expr

object sanitised {
    type SanitiseExpr[A] = Expr[Option[Sanitised[A]]]

    case class Sanitised[+A](captures: A, any: Boolean)
}
