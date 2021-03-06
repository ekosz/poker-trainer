type result = {
  idx: int,
  pocketCards: Common.pocketCards,
  hand: Common.hand,
  score: Scoring.score,
}

let playGame = playerCount => {
  open Belt.Array
  let d = Game.genDeck()
  let hands = makeBy(playerCount, i => {
    {Common.p1: d->getUnsafe(i), p2: d->getUnsafe(playerCount + i)}
  })
  let board = {
    Common.flop1: d->getUnsafe(playerCount * 2 + 1),
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
      let classification = pocketCards->Classification.make->Classification.toString
      acc->Js.Dict.set(
        classification,
        switch acc->Js.Dict.get(classification) {
        | None => (1, 0)
        | Some((wins, loses)) => (wins + 1, loses)
        },
      )
    })

    losers->forEach(({pocketCards}) => {
      let classification = pocketCards->Classification.make->Classification.toString
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
