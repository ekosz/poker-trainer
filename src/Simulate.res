type url

@new external makeURL: (string, 'a) => url = "URL"

@module("fs")
external writeFile: (url, string, @as(json`{"encoding": "utf8"}`) _) => unit = "writeFileSync"
@module("csv-stringify/lib/sync.js") external stringifyCSV: (. array<'a>) => string = "default"

// new URL('./foo.txt', import.meta.url)

type deck = array<Card.t>

type result = {
  idx: int,
  hand: array<Card.t>,
  score: Scoring.score,
}

let simulations = 1_000_000
let playerCount = 9

let genDeck = (): deck => {
  Belt.Array.makeBy(52, i => {
    Card.suite: Card.suiteOfInt(i / 13),
    rank: Card.rankOfInt(mod(i, 13) + 1),
  })
}

let classify = hand => {
  open Card

  @warning("-8")
  let [card1, card2] = hand->Belt.SortArray.stableSortBy(Scoring.compareCards)
  if card1.rank == card2.rank {
    let rankString = card1.rank->stringOfRank
    `${rankString}${rankString}`
  } else if card1.suite == card2.suite {
    `${card1.rank->stringOfRank}${card2.rank->stringOfRank}s`
  } else {
    `${card1.rank->stringOfRank}${card2.rank->stringOfRank}o`
  }
}

let playGame = () => {
  open Belt.Array
  let d = genDeck()->shuffle
  let hands = makeBy(playerCount, i => {
    [d->getUnsafe(i), d->getUnsafe(playerCount + i)]
  })
  let table = [
    d->getUnsafe(playerCount * 2 + 1),
    d->getUnsafe(playerCount * 2 + 2),
    d->getUnsafe(playerCount * 2 + 3),
    d->getUnsafe(playerCount * 2 + 5),
    d->getUnsafe(playerCount * 2 + 7),
  ]

  let results =
    hands
    ->mapWithIndex((idx, hand) => {
      let score = hand->concat(table)->Scoring.make
      {idx: idx, hand: hand, score: score}
    })
    ->Belt.SortArray.stableSortBy(({score: a}, {score: b}) => Scoring.compare(a, b))

  let first = results->getUnsafe(0)

  let (winners, losers) = results->partition(({score}) => Scoring.compare(first.score, score) === 0)
  (winners, losers, table)
}

let run = () => {
  open Belt.Array
  make(simulations, ())
  ->reduce(Js.Dict.empty(), (acc, _) => {
    let (winners, losers, _) = playGame()

    winners->forEach(({hand}) => {
      let classification = hand->classify
      acc->Js.Dict.set(
        classification,
        switch acc->Js.Dict.get(classification) {
        | None => (1, 0)
        | Some((wins, loses)) => (wins + 1, loses)
        },
      )
    })

    losers->forEach(({hand}) => {
      let classification = hand->classify
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
  ->stringifyCSV(. _)
  ->writeFile(
    makeURL(
      `../data/odds_${playerCount->string_of_int}_handed.csv`,
      @warning("-103") %raw(`import.meta.url`),
    ),
    _,
  )
}
