package oregano.internal.ast

// TODO: Should these be parameters or separate nodes? E.g. `GreedyOpt`,
// `ReluctantOpt`, `PossessiveOpt`.
sealed trait QuantifierType
case object Greedy extends QuantifierType /* A?, A*, A+ */
case object Reluctant extends QuantifierType /* A??, A*?, A+? */
case object Possessive extends QuantifierType /* A?+, A*+, A++ */
