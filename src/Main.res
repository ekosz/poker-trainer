let printResult = ({Simulate.idx: idx, hand, score}) => {
  Js.log3(
    `Player ${string_of_int(idx + 1)}`,
    hand->Belt.Array.map(Card.stringOfCard),
    score->Scoring.stringOfScore,
  )
}

let playOneGame = () => {
  let (winners, losers, table) = Simulate.playGame()
  Js.log2("Table", table->Belt.Array.map(Card.stringOfCard))
  Js.log(winners->Belt.Array.length === 1 ? "Winner" : "Winners")
  winners->Belt.Array.forEach(printResult)
  Js.log("Losers")
  losers->Belt.Array.forEach(printResult)
}

Simulate.run()
