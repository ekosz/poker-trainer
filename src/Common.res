/**
 * The community cards that everyone uses in combination with their pocket
 * cards to form the best hand.
 */
type board = {
  flop1: Card.t,
  flop2: Card.t,
  flop3: Card.t,
  turn: option<Card.t>,
  river: option<Card.t>,
}

/**
 * The cards in one's hand that are not part of the community cards
 */
type pocketCards = {
  p1: Card.t,
  p2: Card.t,
}

/**
 * Five cards, made of a player's pocket cards and the community cards. The
 * cards are ordered so that c1 is always the highest ranked card and c5 is the
 * lowest
 */
type hand = {
  c1: Card.t,
  c2: Card.t,
  c3: Card.t,
  c4: Card.t,
  c5: Card.t,
}
