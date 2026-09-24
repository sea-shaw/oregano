package oregano.internal.ast

// TODO: Should these be parameters or separate nodes? E.g. `GreedyOpt`,
// `LazyOpt`, `PossessiveOpt`.
/* Quantifiers can be greedy, lazy, or posessive. */
sealed trait QuantifierType
case object Greedy extends QuantifierType /* A?, A*, A+ */
case object Lazy extends QuantifierType /* A??, A*?, A+? */
case object Possessive extends QuantifierType /* A?+, A*+, A++ */
