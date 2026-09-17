package oregano.internal.ast

trait EmptyTypes { this: Tidy =>
  /* Type of an empty node. In it's own trait for consistency. */
  protected object EmptyType extends HEmptyType
}
