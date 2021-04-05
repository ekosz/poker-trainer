let printResult = ({Simulate.idx: idx, pocketCards, hand, score}) => {
  let {c1, c2, c3, c4, c5} = hand
  Js.log4(
    `Player ${string_of_int(idx + 1)}`,
    [pocketCards.p1, pocketCards.p2]->Belt.Array.map(Card.stringOfCard),
    score->Scoring.stringOfScore(hand),
    [c1, c2, c3, c4, c5]->Belt.Array.map(Card.stringOfCard),
  )
}

let playOneGame = () => {
  let (winners, losers, board) = Simulate.playGame()

  @warning("-8")
  let {Scoring.flop1: flop1, flop2, flop3, turn: Some(turn), river: Some(river)} = board
  Js.log2("Board", [flop1, flop2, flop3, turn, river]->Belt.Array.map(Card.stringOfCard))
  Js.log(winners->Belt.Array.length === 1 ? "Winner" : "Winners")
  winners->Belt.Array.forEach(printResult)
  Js.log("Losers")
  losers->Belt.Array.forEach(printResult)
}

playOneGame()
