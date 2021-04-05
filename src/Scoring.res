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

exception Invariant(string)
type score =
  | StraitFlush
  | FourOfAKind
  | FullHouse
  | Flush
  | Strait
  | ThreeOfAKind
  | TwoPair
  | Pair
  | HighCard

let scoreToIndex = score =>
  switch score {
  | StraitFlush => 9
  | FourOfAKind => 8
  | FullHouse => 7
  | Flush => 6
  | Strait => 5
  | ThreeOfAKind => 4
  | TwoPair => 3
  | Pair => 2
  | HighCard => 1
  }

let compareScores = (scoreA, scoreB) => scoreToIndex(scoreB) - scoreToIndex(scoreA)
let compareRanks = (rankA, rankB) => Card.intOfRank(rankB) - Card.intOfRank(rankA)
let compareSuites = (suitA, suitB) => Card.intOfSuite(suitB) - Card.intOfSuite(suitA)
// By default cards are sorted by their rank
let compareCards = (cardA: Card.t, cardB: Card.t) => compareRanks(cardA.rank, cardB.rank)

let combinePocketAndBoard = (p, b) => {
  Belt.Array.concatMany([
    [p.p1, p.p2, b.flop1, b.flop2, b.flop3],
    switch b {
    | {turn: Some(turn), river: Some(river)} => [turn, river]
    | {turn: Some(turn)} => [turn]
    | _ => []
    },
  ])->Belt.SortArray.stableSortBy(compareCards)
}

let makeFlush = (pocketCards: pocketCards, board: board) => {
  open Belt.Array
  combinePocketAndBoard(pocketCards, board)
  ->reduce(Js.Dict.empty(), (acc, card) => {
    acc->Js.Dict.set(
      card.suite->Card.stringOfSuite,
      switch acc->Js.Dict.get(card.suite->Card.stringOfSuite) {
      | None => [card]
      // If we've already found the top 5 suited cards, stop
      | Some(cards) if cards->length === 5 => cards
      | Some(cards) => cards->concat([card])
      },
    )
    acc
  })
  ->Js.Dict.values
  ->keep(cards => cards->length === 5)
  ->get(0)
  ->Belt.Option.map(@warning("-8")
  ([c1, c2, c3, c4, c5]) => {c1: c1, c2: c2, c3: c3, c4: c4, c5: c5})
}

let makeStrait = (pocketCards: pocketCards, board: board) => {
  let sortedCards = combinePocketAndBoard(pocketCards, board)
  let contiguasCards = sortedCards->Belt.Array.reduce(list{}, (acc, nextCard) => {
    switch acc {
    | list{} => list{nextCard}
    // If we've already found the top 5 contiguas cards, stop
    | hand if hand->Belt.List.size === 5 => hand
    // Ace is special as it can be used in the 1s place for a strait
    | list{{Card.rank: Card.Ace} as lastCard, ...rest} if nextCard.rank == Card.Num(2) => list{
        nextCard,
        lastCard,
        ...rest,
      }
    // Add to list if contiguas
    | list{lastCard, ...rest}
      if lastCard.rank->Card.intOfRank - nextCard.rank->Card.intOfRank === 1 => list{
        nextCard,
        lastCard,
        ...rest,
      }
    // Otherwise restart list
    | _ => list{nextCard}
    }
  })

  if contiguasCards->Belt.List.length === 5 {
    // We know the list is 5 long and reversed in order of rank

    @warning("-8")
    let list{c5, c4, c3, c2, c1} = contiguasCards
    {c1: c1, c2: c2, c3: c3, c4: c4, c5: c5}->Some
  } else {
    None
  }
}

let makePairs = (pocketCards, board) => {
  open Belt.Array
  let pairs =
    combinePocketAndBoard(pocketCards, board)
    ->reduce(Js.Dict.empty(), (acc, card) => {
      acc->Js.Dict.set(
        card.rank->Card.stringOfRank,
        switch acc->Js.Dict.get(card.rank->Card.stringOfRank) {
        | None => list{card}
        | Some(rest) => list{card, ...rest}
        },
      )
      acc
    })
    ->Js.Dict.values
    ->Belt.SortArray.stableSortBy(@warning("-8")
    (list{a, ..._} as aCards, list{b, ..._} as bCards) =>
      switch Pervasives.compare(aCards->Belt.List.size, bCards->Belt.List.size) {
      | 0 => compareRanks(a.rank, b.rank)
      | 1 => -1
      | -1 => 1
      | _ => raise(Invariant("Invalid Pervasives.compare"))
      }
    )
    ->Belt.List.fromArray

  switch pairs {
  | list{list{c1, c2, c3, c4}, list{c5, ..._}, ..._} => (
      FourOfAKind,
      {c1: c1, c2: c2, c3: c3, c4: c4, c5: c5},
    )
  | list{list{c1, c2, c3}, list{c4, c5, ..._}, ..._} => (
      FullHouse,
      {c1: c1, c2: c2, c3: c3, c4: c4, c5: c5},
    )
  | list{list{c1, c2, c3}, list{c4}, list{c5}, ..._} => (
      ThreeOfAKind,
      {c1: c1, c2: c2, c3: c3, c4: c4, c5: c5},
    )
  | list{list{c1, c2}, list{c3, c4}, list{c5, ..._}, ..._} => (
      TwoPair,
      {c1: c1, c2: c2, c3: c3, c4: c4, c5: c5},
    )
  | list{list{c1, c2}, list{c3}, list{c4}, list{c5}, ..._} => (
      Pair,
      {c1: c1, c2: c2, c3: c3, c4: c4, c5: c5},
    )
  | list{list{c1}, list{c2}, list{c3}, list{c4}, list{c5}, ..._} => (
      HighCard,
      {c1: c1, c2: c2, c3: c3, c4: c4, c5: c5},
    )
  | _ => {
      Js.log2("InvalidPairs", pairs->Belt.List.toArray)
      raise(Invariant("InvalidPairs"))
    }
  }
}

let stringOfScore = (score: score, hand: hand) => {
  open Card
  switch (score, hand) {
  | (StraitFlush, {c1: {rank: Ace}}) => "Royal Flush"
  | (StraitFlush, _) => "Strait Flush"
  | (FourOfAKind, _) => "Four of a Kind"
  | (FullHouse, _) => "Full House"
  | (Flush, _) => "Flush"
  | (Strait, _) => "Strait"
  | (ThreeOfAKind, _) => "Three of a Kind"
  | (TwoPair, _) => "Two Pair"
  | (Pair, _) => "One Pair"
  | (HighCard, {c1: {rank: high}}) => `High of ${high->stringOfRank}`
  }
}

exception InvalidHandSize(int)
let make = (board: board, pocketCards: pocketCards): (score, hand) => {
  switch (
    makeFlush(pocketCards, board),
    makeStrait(pocketCards, board),
    makePairs(pocketCards, board),
  ) {
  | (Some(flushHand), Some(straitHand), _) if flushHand == straitHand => (StraitFlush, straitHand)
  | (_, _, (FourOfAKind, _) as x) => x
  | (_, _, (FullHouse, _) as x) => x
  | (Some(hand), _, _) => (Flush, hand)
  | (_, Some(hand), _) => (Strait, hand)
  | (_, _, x) => x
  }
}

let compare = (board, pocketCardsA, pocketCardsB) => {
  let (score1, hand1) = make(board, pocketCardsA)
  let (score2, hand2) = make(board, pocketCardsB)

  if score1 == score2 {
    switch score1 {
    | StraitFlush => compareRanks(hand1.c1.rank, hand2.c1.rank)
    | FourOfAKind if hand1.c1.rank != hand2.c1.rank => compareRanks(hand1.c1.rank, hand2.c1.rank)
    | FourOfAKind => compareRanks(hand1.c5.rank, hand2.c5.rank)
    | FullHouse if hand1.c1.rank != hand2.c1.rank => compareRanks(hand1.c1.rank, hand2.c1.rank)
    | FullHouse => compareRanks(hand1.c4.rank, hand2.c4.rank)
    | Flush if hand1.c1.rank != hand2.c1.rank => compareRanks(hand1.c1.rank, hand2.c1.rank)
    | Flush if hand1.c2.rank != hand2.c2.rank => compareRanks(hand1.c2.rank, hand2.c2.rank)
    | Flush if hand1.c3.rank != hand2.c3.rank => compareRanks(hand1.c3.rank, hand2.c3.rank)
    | Flush if hand1.c4.rank != hand2.c4.rank => compareRanks(hand1.c4.rank, hand2.c4.rank)
    | Flush => compareRanks(hand1.c5.rank, hand2.c5.rank)
    | Strait if hand1.c1.rank != hand2.c1.rank => compareRanks(hand1.c1.rank, hand2.c1.rank)
    | Strait if hand1.c2.rank != hand2.c2.rank => compareRanks(hand1.c2.rank, hand2.c2.rank)
    | Strait if hand1.c3.rank != hand2.c3.rank => compareRanks(hand1.c3.rank, hand2.c3.rank)
    | Strait if hand1.c4.rank != hand2.c4.rank => compareRanks(hand1.c4.rank, hand2.c4.rank)
    | Strait => compareRanks(hand1.c5.rank, hand2.c5.rank)
    | ThreeOfAKind if hand1.c1.rank != hand2.c1.rank => compareRanks(hand1.c1.rank, hand2.c1.rank)
    | ThreeOfAKind if hand1.c4.rank != hand2.c4.rank => compareRanks(hand1.c4.rank, hand2.c4.rank)
    | ThreeOfAKind => compareRanks(hand1.c5.rank, hand2.c5.rank)
    | TwoPair if hand1.c1.rank != hand2.c1.rank => compareRanks(hand1.c1.rank, hand2.c1.rank)
    | TwoPair if hand1.c3.rank != hand2.c3.rank => compareRanks(hand1.c3.rank, hand2.c3.rank)
    | TwoPair => compareRanks(hand1.c5.rank, hand2.c5.rank)
    | Pair if hand1.c1.rank != hand2.c1.rank => compareRanks(hand1.c1.rank, hand2.c1.rank)
    | Pair if hand1.c3.rank != hand2.c3.rank => compareRanks(hand1.c3.rank, hand2.c3.rank)
    | Pair if hand1.c4.rank != hand2.c4.rank => compareRanks(hand1.c4.rank, hand2.c4.rank)
    | Pair => compareRanks(hand1.c5.rank, hand2.c5.rank)
    | HighCard if hand1.c1.rank != hand2.c1.rank => compareRanks(hand1.c1.rank, hand2.c2.rank)
    | HighCard if hand1.c2.rank != hand2.c2.rank => compareRanks(hand1.c2.rank, hand2.c2.rank)
    | HighCard if hand1.c3.rank != hand2.c3.rank => compareRanks(hand1.c3.rank, hand2.c2.rank)
    | HighCard if hand1.c4.rank != hand2.c4.rank => compareRanks(hand1.c4.rank, hand2.c2.rank)
    | HighCard => compareRanks(hand1.c5.rank, hand2.c5.rank)
    }
  } else {
    compareScores(score1, score2)
  }
}
