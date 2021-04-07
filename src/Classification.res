type t = string

let make = ({Common.p1: p1, p2}): t => {
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

let isPair = classification => {
  @warning("-8")
  switch classification->Js.String2.split("") {
  | [a, b] if a === b => true
  | _ => false
  }
}

let isSuited = classification => classification->Js.String2.endsWith("s")

let handCount = classification => {
  if isPair(classification) {
    6 // 6 ways to make a pair (sh, sd, sc, hd, hc, dc)
  } else if isSuited(classification) {
    4 // 4 suites
  } else {
    12 // 12 ways to make any other combo (4 x 4 suites)
  }
}

let toString = x => x
