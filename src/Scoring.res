type hand = array<Card.t>

exception Invariant(string)
type score =
  | StraitFlush(hand)
  | FourOfAKind({hand: hand, rank: Card.rank})
  | FullHouse({hand: hand, threeOf: Card.rank, twoOf: Card.rank})
  | Flush(hand)
  | Strait(hand)
  | ThreeOfAKind({hand: hand, threeOf: Card.rank})
  | TwoPair({hand: hand, pair1: Card.rank, pair2: Card.rank})
  | Pair({hand: hand, twoOf: Card.rank})
  | HighCard(hand)

let intOfScore = score =>
  switch score {
  | StraitFlush(_) => 9
  | FourOfAKind(_) => 8
  | FullHouse(_) => 7
  | Flush(_) => 6
  | Strait(_) => 5
  | ThreeOfAKind(_) => 4
  | TwoPair(_) => 3
  | Pair(_) => 2
  | HighCard(_) => 1
  }

let compareScores = (scoreA, scoreB) => intOfScore(scoreB) - intOfScore(scoreA)
let compareRanks = (rankA, rankB) => Card.intOfRank(rankB) - Card.intOfRank(rankA)
let compareSuites = (suitA, suitB) => Card.intOfSuite(suitB) - Card.intOfSuite(suitA)
// By default cards are sorted by their rank
let compareCards = (cardA: Card.t, cardB: Card.t) => compareRanks(cardA.rank, cardB.rank)

let isSameScore = (s1, s2) => s1->intOfScore == s2->intOfScore

let getHigh = (h: hand) => {
  h
  ->Belt.Array.map(({rank}) => rank)
  ->Belt.SortArray.stableSortBy(compareRanks)
  ->Belt.Array.getUnsafe(0)
}

let makeFlush = (h: hand) => {
  open Belt.Array
  let flushedSuite =
    h
    ->map(({suite}) => suite->Card.stringOfSuite)
    ->reduce(Js.Dict.empty(), (acc, suite) => {
      acc->Js.Dict.set(
        suite,
        switch acc->Js.Dict.get(suite) {
        | Some(x) => x + 1
        | None => 1
        },
      )
      acc
    })
    ->Js.Dict.entries
    ->keep(((_, count)) => count >= 5)
    ->map(((suiteString, _)) => suiteString->Card.suiteOfString)
    ->get(0)

  switch flushedSuite {
  | None => None
  | Some(suite) =>
    h
    ->keep(card => card.suite == suite)
    ->Belt.SortArray.stableSortBy(compareCards)
    // Take top 5 ranked cards
    ->slice(~offset=0, ~len=5)
    ->Flush
    ->Some
  }
}

let makeStrait = (h: hand) => {
  open Belt.Array
  let sortedCards = h->Belt.SortArray.stableSortBy(compareCards)
  let contiguasCards = sortedCards->reduce(list{}, (acc, card) => {
    switch acc {
    | list{} => list{[card]}
    | list{currentGroup, ...rest} => {
        let lastCard = currentGroup->getUnsafe(0)
        if lastCard.rank->Card.intOfRank - card.rank->Card.intOfRank == 1 {
          list{currentGroup->concat([card]), ...rest}
        } else {
          list{[card], currentGroup, ...rest}
        }
      }
    }
  })

  switch contiguasCards->Belt.List.getBy(a => a->length == 5) {
  | Some(hand) => hand->Strait->Some
  | None =>
    switch (sortedCards->getUnsafe(0), contiguasCards->Belt.List.getBy(a => a->length == 4)) {
    | ({rank: Card.Ace} as card, Some([{rank: Card.Num(5)}, _, _, _] as arr)) =>
      arr->concat([card])->Strait->Some
    | _ => None
    }
  }
}

let makePairs = (h: hand) => {
  open Belt.Array
  let pairs: list<(Card.rank, int, array<Card.t>)> =
    h
    ->reduce(Js.Dict.empty(), (acc, card) => {
      let rankString = card.rank->Card.stringOfRank
      acc->Js.Dict.set(
        rankString,
        switch acc->Js.Dict.get(rankString) {
        | Some(arr) => arr->concat([card])
        | None => [card]
        },
      )
      acc
    })
    ->Js.Dict.entries
    ->map(((rankString, cards)) => (rankString->Card.rankOfString, cards->Belt.Array.length, cards))
    ->Belt.SortArray.stableSortBy(((rankA, countA, _), (rankB, countB, _)) =>
      switch Pervasives.compare(countA, countB) {
      | 0 => compareRanks(rankA, rankB)
      | 1 => -1
      | -1 => 1
      | c => c
      }
    )
    ->Belt.List.fromArray

  switch pairs {
  | list{(rank, 4, cards), (_, _, kickers), ..._} => {
      let kicker = kickers->getUnsafe(0)
      {hand: cards->concat([kicker]), rank: rank}->FourOfAKind
    }
  | list{(threeOf, 3, cardsA), (twoOf, 3, cardsB), ..._}
  | list{(threeOf, 3, cardsA), (twoOf, 2, cardsB), ..._} =>
    {
      hand: cardsA->concat(cardsB->slice(~offset=0, ~len=2)),
      threeOf: threeOf,
      twoOf: twoOf,
    }->FullHouse
  | list{(threeOf, 3, cards), (_, 1, kicker1), (_, 1, kicker2), ..._} =>
    {
      hand: concatMany([cards, kicker1, kicker2]),
      threeOf: threeOf,
    }->ThreeOfAKind
  | list{(pair1, 2, pair1Cards), (pair2, 2, pair2Cards), (_, _, kickers), ..._} => {
      let kicker = kickers->getUnsafe(0)
      {hand: concatMany([pair1Cards, pair2Cards, [kicker]]), pair1: pair1, pair2: pair2}->TwoPair
    }
  | list{(twoOf, 2, cards), (_, 1, kicker1), (_, 1, kicker2), (_, 1, kicker3), ..._} =>
    {
      hand: concatMany([cards, kicker1, kicker2, kicker3]),
      twoOf: twoOf,
    }->Pair
  | list{
      (_, 1, kicker1),
      (_, 1, kicker2),
      (_, 1, kicker3),
      (_, 1, kicker4),
      (_, 1, kicker5),
      ..._,
    } =>
    concatMany([kicker1, kicker2, kicker3, kicker4, kicker5])->HighCard
  | _ => {
      Js.log2("InvalidPairs", pairs->Belt.List.toArray)
      raise(Invariant("InvalidPairs"))
    }
  }
}

let longName = f =>
  switch f {
  | Card.Ace => "Aces"
  | King => "Kings"
  | Queen => "Queens"
  | Jack => "Jacks"
  | Num(10) => "Tens"
  | Num(9) => "Nines"
  | Num(8) => "Eights"
  | Num(7) => "Sevens"
  | Num(6) => "Sixes"
  | Num(5) => "Fives"
  | Num(4) => "Fours"
  | Num(3) => "Threes"
  | Num(2) => "Duces"
  | _ => "Unknown"
  }

let stringOfScore = s => {
  open Card
  switch s {
  | StraitFlush(hand) =>
    switch hand->getHigh {
    | Ace => "Royal Flush"
    | x => `Strait Flush (${stringOfRank(x)} high)`
    }
  | FourOfAKind({hand, rank}) => {
      let kicker =
        (hand->Belt.Array.keep(c => c.rank != rank)->Belt.Array.getUnsafe(0)).rank->stringOfRank
      `Four of a Kind: Quads of ${stringOfRank(rank)} (kicker: ${kicker})`
    }
  | FullHouse({threeOf, twoOf}) =>
    `Full House: ${stringOfRank(threeOf)}s full of ${stringOfRank(twoOf)}s`
  | Flush(hand) => `Flush (${(hand->Belt.Array.getUnsafe(0)).rank->stringOfRank} high)`
  | Strait(hand) => `Strait (${(hand->Belt.Array.getUnsafe(0)).rank->stringOfRank} high)`
  | ThreeOfAKind({hand, threeOf}) =>
    let kickers =
      hand
      ->Belt.Array.map(({rank}) => rank)
      ->Belt.Array.keep(r => r != threeOf)
      ->Belt.Array.map(stringOfRank)
      ->Belt.Array.joinWith("", x => x)
    `Three of a Kind: Set of ${stringOfRank(threeOf)} (kickers: ${kickers})`
  | TwoPair({hand, pair1, pair2}) =>
    let kicker =
      hand
      ->Belt.Array.map(({rank}) => rank)
      ->Belt.Array.keep(r => r != pair1 && r != pair2)
      ->Belt.Array.map(stringOfRank)
      ->Belt.Array.joinWith("", x => x)
    `Two Pair: ${longName(pair1)} and ${longName(pair2)} (kicker: ${kicker})`
  | Pair({hand, twoOf}) => {
      let kickers =
        hand
        ->Belt.Array.map(({rank}) => rank)
        ->Belt.Array.keep(r => r != twoOf)
        ->Belt.Array.map(stringOfRank)
        ->Belt.Array.joinWith("", x => x)
      `One Pair: ${longName(twoOf)} (kickers: ${kickers})`
    }
  | HighCard(hand) => {
      let high = (hand->Belt.Array.getUnsafe(0)).rank
      let kickers =
        hand
        ->Belt.Array.map(({rank}) => rank)
        ->Belt.Array.keep(r => r != high)
        ->Belt.Array.map(stringOfRank)
        ->Belt.Array.joinWith("", x => x)
      `High of ${stringOfRank(high)} (kickers: ${kickers})`
    }
  }
}

exception InvalidHandSize(int)
let make = (h: hand): score => {
  let length = h->Belt.Array.length
  if length < 1 || length > 7 {
    raise(InvalidHandSize(length))
  }
  switch (makeFlush(h), makeStrait(h), makePairs(h)) {
  | (Some(Flush(_)), Some(Strait(hand)), _) => StraitFlush(hand)
  | (_, _, FourOfAKind(_) as a) => a
  | (_, _, FullHouse(_) as a) => a
  | (Some(Flush(_) as a), _, _) => a
  | (_, Some(Strait(_) as a), _) => a
  | (_, _, x) => x
  }
}

let compare = (score1, score2) => {
  if isSameScore(score1, score2) {
    switch (score1, score2) {
    | (StraitFlush(handA), StraitFlush(handB))
    | (Flush(handA), Flush(handB))
    | (Strait(handA), Strait(handB))
    | (HighCard(handA), HighCard(handB)) =>
      compareRanks(handA->getHigh, handB->getHigh)
    | (FourOfAKind({rank: a}), FourOfAKind({rank: b}))
    | (FullHouse({threeOf: a}), FullHouse({threeOf: b}))
    | (ThreeOfAKind({threeOf: a}), ThreeOfAKind({threeOf: b}))
    | (TwoPair({pair1: a}), TwoPair({pair1: b}))
    | (Pair({twoOf: a}), Pair({twoOf: b})) if a != b =>
      compareRanks(a, b)
    | (FourOfAKind({rank, hand: handA}), FourOfAKind({hand: handB})) => {
        open Belt.Array
        let kickerA = handA->keep(({rank: cRank}) => cRank != rank)->getUnsafe(0)
        let kickerB = handB->keep(({rank: cRank}) => cRank != rank)->getUnsafe(0)
        compareCards(kickerA, kickerB)
      }
    | (FullHouse({twoOf: a}), FullHouse({twoOf: b})) => compareRanks(a, b)
    | (ThreeOfAKind({threeOf, hand: handA}), ThreeOfAKind({hand: handB})) => {
        open Belt.Array
        let kickerA = handA->keep(({rank: cRank}) => cRank != threeOf)->getUnsafe(0)
        let kickerB = handB->keep(({rank: cRank}) => cRank != threeOf)->getUnsafe(0)
        compareCards(kickerA, kickerB)
      }
    | (TwoPair({pair1, pair2, hand: handA}), TwoPair({hand: handB})) => {
        open Belt.Array
        let kickerA = handA->keep(({rank: cRank}) => cRank != pair1 && cRank != pair2)->getUnsafe(0)
        let kickerB = handB->keep(({rank: cRank}) => cRank != pair1 && cRank != pair2)->getUnsafe(0)
        compareCards(kickerA, kickerB)
      }
    | (Pair({twoOf, hand: handA}), Pair({hand: handB})) => {
        open Belt.Array
        let kickerA = handA->keep(({rank: cRank}) => cRank != twoOf)->getUnsafe(0)
        let kickerB = handB->keep(({rank: cRank}) => cRank != twoOf)->getUnsafe(0)
        compareCards(kickerA, kickerB)
      }
    | (x, y) => {
        Js.log2(x, y)
        raise(Invariant("Should not get here"))
      }
    }
  } else {
    compareScores(score1, score2)
  }
}
