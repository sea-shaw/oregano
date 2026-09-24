package oregano.internal

import scala.quoted.Expr

/* Used to construct the output type. */
object sanitised {
    type SanitiseExpr[A] = Expr[Option[Sanitised[A]]]

    /* Writer monad using OR monoid for `Boolean`. `captures` contains data,
       `any` is true if `captures` includes a capture group.  */
    case class Sanitised[+A](captures: A, any: Boolean)
}
