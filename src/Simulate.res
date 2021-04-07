type result = {
  idx: int,
  pocketCards: Game.pocketCards,
  hand: Game.hand,
  score: Scoring.score,
}

let classify = ({Game.p1: p1, p2}) => {
  open Card

  @warning("-8")
  let [card1, card2] = [p1, p2]->Belt.SortArray.stableSortBy(Scoring.compareCards)
  if card1.rank == card2.rank {
    let rankString = card1.rank->stringOfRank
    `${rankString}${rankString}`
  } else if card1.suite == card2.suite {
    `${card1.rank->stringOfRank}${card2.rank->stringOfRank}s`
  } else {
    `${card1.rank->stringOfRank}${card2.rank->stringOfRank}o`
  }
}

let playGame = playerCount => {
  open Belt.Array
  let d = Game.genDeck()
  let hands = makeBy(playerCount, i => {
    {Game.p1: d->getUnsafe(i), p2: d->getUnsafe(playerCount + i)}
  })
  let board = {
    Game.flop1: d->getUnsafe(playerCount * 2 + 1),
    flop2: d->getUnsafe(playerCount * 2 + 2),
    flop3: d->getUnsafe(playerCount * 2 + 3),
    turn: Some(d->getUnsafe(playerCount * 2 + 5)),
    river: Some(d->getUnsafe(playerCount * 2 + 7)),
  }

  let make = Scoring.make(board)
  let compare = Scoring.compare(board)

  let results =
    hands
    ->mapWithIndex((idx, pocketCards) => {
      let (score, hand) = make(pocketCards)
      {idx: idx, hand: hand, score: score, pocketCards: pocketCards}
    })
    ->Belt.SortArray.stableSortBy(({pocketCards: a}, {pocketCards: b}) => compare(a, b))

  let first = results->getUnsafe(0)

  let (winners, losers) =
    results->partition(({pocketCards}) => compare(first.pocketCards, pocketCards) === 0)
  (winners, losers, board)
}

let run = (simulations, playerCount) => {
  open Belt.Array
  make(simulations, ())
  ->reduce(Js.Dict.empty(), (acc, _) => {
    let (winners, losers, _) = playGame(playerCount)

    winners->forEach(({pocketCards}) => {
      let classification = pocketCards->classify
      acc->Js.Dict.set(
        classification,
        switch acc->Js.Dict.get(classification) {
        | None => (1, 0)
        | Some((wins, loses)) => (wins + 1, loses)
        },
      )
    })

    losers->forEach(({pocketCards}) => {
      let classification = pocketCards->classify
      acc->Js.Dict.set(
        classification,
        switch acc->Js.Dict.get(classification) {
        | None => (0, 1)
        | Some((wins, loses)) => (wins, loses + 1)
        },
      )
    })

    acc
  })
  ->Js.Dict.entries
  ->reduce([], (acc: array<(string, float)>, (key, (wins, loses))) => {
    acc->concat([(key, float_of_int(wins) /. float_of_int(wins + loses))])
  })
}
