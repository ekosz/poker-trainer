let bluffHand = "A5s"
let smallBindCallingEquity = 0.666

exception Invariant(string)

type position =
  | BigBlind
  | SmallBlind
  | Button
  | Cutoff
  | Hijack
  | Lojack
  | MiddlePosition
  | UnderTheGun
  | UnderTheGunPlus1
  | UnderTheGunPlus2

type previousAction =
  | Limp
  | LoseRaise
  | StrongRaise

type action =
  | Check
  | Call
  | Raise
  | Fold

type scenario = {
  numberOfPlayers: int,
  previousAction: previousAction,
  positionNum: int,
  position: position,
  pocketCards: Scoring.pocketCards,
}

type preFlopPcts = {
  raise: float,
  strongCallRaise: float,
  strongReRaise: float,
  loseCallRaise: float,
  loseReRaise: float,
}

// Any position right of the cutoff
let earlyPosition = {
  raise: 0.143,
  strongCallRaise: 0.113,
  strongReRaise: 0.009,
  loseCallRaise: 0.143,
  loseReRaise: 0.044,
}

// Position right of the button
let cutOff = {
  raise: 0.222,
  strongCallRaise: 0.113,
  strongReRaise: 0.009,
  loseCallRaise: 0.186,
  loseReRaise: 0.054,
}

// All positions are relative to the button. Acts second to last pre-flop and last post-flop
let button = {
  raise: 0.333,
  strongCallRaise: 0.113,
  strongReRaise: 0.009,
  loseCallRaise: 0.222,
  loseReRaise: 0.087,
}

// The blinds are left of the button. They are last to act pre-flop and first
// to act post-flop. For now we're using the same percents for both big and
// small blind
let blinds = {
  raise: 0.063,
  strongCallRaise: 0.101,
  strongReRaise: 0.009,
  loseCallRaise: 0.143,
  loseReRaise: 0.044,
}

let totalCombos = 1326 // total combinations of pocket cards

let isPair = classification => {
  @warning("-8")
  switch classification->Js.String2.split("") {
  | [a, b] if a === b => true
  | _ => false
  }
}

let isSuited = classification => classification->Js.String2.endsWith("s")
let pctOfTotal = pct => pct->float_of_int /. totalCombos->float_of_int
let roundToThousandths = num => Js.Math.round(num *. 1000.0) /. 1000.0
let roundToTenths = num => Js.Math.round(num *. 10.0) /. 10.0

let classificationToHandCount = classification => {
  if isPair(classification) {
    6 // 6 ways to make a pair (sh, sd, sc, hd, hc, dc)
  } else if isSuited(classification) {
    4 // 4 suites
  } else {
    12 // 12 ways to make any other combo (4 x 4 suites)
  }
}

let calcPct = (handToEval, hands) => {
  let (_, totalHands) = hands->Belt.Array.reduce((false, 0), ((found, acc), hand) => {
    if found {
      (found, acc)
    } else if hand === handToEval {
      (true, acc + hand->classificationToHandCount)
    } else {
      (false, acc + hand->classificationToHandCount)
    }
  })
  totalHands->pctOfTotal
}

let positionOf = (numberOfPlayers, positionNum) => {
  switch (numberOfPlayers, positionNum) {
  | (_, 1) => Button
  | (_, 2) => SmallBlind
  | (_, 3) => BigBlind
  | (_, 4) => UnderTheGun
  | (6, 5) => UnderTheGunPlus1
  | (7, 5) => Lojack
  | (7, 6) => Hijack
  | (8, 5) => MiddlePosition
  | (8, 6) => Lojack
  | (8, 7) => Hijack
  | (9, 5) => UnderTheGunPlus1
  | (9, 6) => MiddlePosition
  | (9, 7) => Lojack
  | (9, 8) => Hijack
  | (10, 5) => UnderTheGunPlus1
  | (10, 6) => UnderTheGunPlus2
  | (10, 7) => MiddlePosition
  | (10, 8) => Lojack
  | (10, 9) => Hijack
  | (5, 5) | (6, 6) | (7, 7) | (8, 8) | (9, 9) | (10, 10) => Cutoff
  | _ => raise(Invariant("Invalid position"))
  }
}

let stringOfPosition = p =>
  switch p {
  | Button => "the button"
  | BigBlind => "the big blind"
  | SmallBlind => "the small blind"
  | UnderTheGun => "under the gun"
  | UnderTheGunPlus1 => "under the gun +1"
  | UnderTheGunPlus2 => "under the gun +2"
  | MiddlePosition => "middle position"
  | Lojack => "the lojack"
  | Hijack => "the highjack"
  | Cutoff => "the cutoff"
  }

let genSceanario = numberOfPlayers => {
  @warning("-8")
  let [p1, p2] =
    Simulate.genDeck()->Belt.Array.shuffle->Belt.Array.slice(~offset=0, ~len=2)
  let positionNum = Js.Math.random_int(1, numberOfPlayers + 1)
  let seed = Js.Math.random()
  {
    numberOfPlayers: numberOfPlayers,
    pocketCards: {p1: p1, p2: p2},
    positionNum: positionNum,
    position: positionOf(numberOfPlayers, positionNum),
    previousAction: positionNum === 4 || (positionNum === 1 && numberOfPlayers === 3)
      ? Limp
      : seed >= 0.4
      ? Limp
      : seed >= 0.3
      ? StrongRaise
      : LoseRaise,
  }
}

let describeScenario = (scenario: scenario) => {
  let prevPosition =
    scenario.positionNum === 1
      ? positionOf(scenario.numberOfPlayers, scenario.numberOfPlayers)
      : positionOf(scenario.numberOfPlayers, scenario.positionNum - 1)
  let prevAction = switch scenario.previousAction {
  | Limp => " limps in"
  | StrongRaise => ", who is normally tight, raises"
  | LoseRaise => ", who has been playing lose, raises"
  }

  [
    `You are playing ${scenario.numberOfPlayers->string_of_int} handed and are ${scenario.position->stringOfPosition}.`,
    `You are delt ${scenario.pocketCards.p1->Card.stringOfCard}, ${scenario.pocketCards.p2->Card.stringOfCard}.`,
    scenario.position == UnderTheGun ||
      (scenario.numberOfPlayers === 3 && scenario.position == Button)
      ? ""
      : prevPosition->stringOfPosition ++ prevAction ++ ".",
    "What do you do?",
  ]
  ->Js.Array2.filter(x => x != "")
  ->Js.Array2.joinWith(" ")
}

let scenarioChoices = (scenario: scenario) => {
  if scenario.position == BigBlind && scenario.previousAction == Limp {
    [{"name": "Check", "value": Check}, {"name": "Raise", "value": Raise}]
  } else {
    [
      {"name": "Call", "value": Call},
      {"name": scenario.previousAction == Limp ? "Raise" : "Re-raise", "value": Raise},
      {"name": "Fold", "value": Fold},
    ]
  }
}

let toEquityString = x => ((1.0 -. x) *. 100.0)->roundToTenths->Js.Float.toString ++ "%"

let testScenario = (action: action, scenario: scenario, hands: array<string>) => {
  let checker = switch scenario.position {
  | Button => button
  | Cutoff => cutOff
  | BigBlind | SmallBlind => blinds
  | _ => earlyPosition
  }
  let pocketEquity = scenario.pocketCards->Simulate.classify->calcPct(hands)->roundToThousandths

  switch (action, scenario.previousAction) {
  | (Check, Limp) if scenario.position == BigBlind && pocketEquity <= checker.raise => {
      let msg =
        [
          "You should have raised.",
          `To raise you should have at least ${checker.raise->toEquityString} equity.`,
          `You had ${pocketEquity->toEquityString}`,
        ]->Js.Array2.joinWith(" ")
      (false, msg)
    }
  | (Check, Limp) if scenario.position == BigBlind => (true, "Good check")
  | (Check, _) => raise(Invariant("Should not get here yet"))
  | (Raise, Limp) if scenario.position == BigBlind && pocketEquity > checker.raise => {
      let msg =
        [
          "You should have chcked.",
          `To raise you should have at least ${checker.raise->toEquityString} equity.`,
          `You had ${pocketEquity->toEquityString}`,
        ]->Js.Array2.joinWith(" ")
      (false, msg)
    }
  | (Call, Limp) if scenario.position == SmallBlind && pocketEquity > smallBindCallingEquity => {
      let msg =
        [
          "You should have folded.",
          `To call you should have at least ${smallBindCallingEquity->toEquityString} equity.`,
          `You had ${pocketEquity->toEquityString}`,
        ]->Js.Array2.joinWith(" ")
      (false, msg)
    }
  | (Call, Limp) if scenario.position == SmallBlind && pocketEquity > checker.raise => (
      true,
      "Good call",
    )
  | (Call, Limp) | (Raise, Limp) if pocketEquity > checker.raise => {
      let msg =
        [
          "You should have folded.",
          "In this situation you should either be folding or raising.",
          `To raise you should have at least ${checker.raise->toEquityString} equity.`,
          `You had ${pocketEquity->toEquityString}`,
        ]->Js.Array2.joinWith(" ")
      (false, msg)
    }
  | (Call, Limp) => {
      let msg =
        [
          "You should have raised.",
          "In this situation you should either be folding or raising.",
          `To raise you should have at least ${checker.raise->toEquityString} equity.`,
          `You had ${pocketEquity->toEquityString}`,
        ]->Js.Array2.joinWith(" ")
      (false, msg)
    }
  | (Fold, Limp) => (true, `Good fold`)
  | (Raise, Limp) => (true, `Good raise`)
  | (Fold, LoseRaise) | (Call, LoseRaise) if pocketEquity <= checker.loseReRaise => {
      let msg =
        [
          "You should have re-raised.",
          `To re-raise you should have at least ${checker.loseReRaise->toEquityString} equity.`,
          `You had ${pocketEquity->toEquityString}`,
        ]->Js.Array2.joinWith(" ")
      (false, msg)
    }
  | (Call, LoseRaise) | (Raise, LoseRaise) if pocketEquity > checker.loseCallRaise => {
      let msg =
        [
          "You should have folded.",
          `To call you should have at least ${checker.loseCallRaise->toEquityString} equity.`,
          `You had ${pocketEquity->toEquityString}`,
        ]->Js.Array2.joinWith(" ")
      (false, msg)
    }
  | (Fold, LoseRaise) | (Raise, LoseRaise)
    if pocketEquity <= checker.loseCallRaise && pocketEquity > checker.loseReRaise => {
      let msg =
        [
          "You should have called.",
          `To call you should have at least ${checker.loseCallRaise->toEquityString} equity.`,
          `You had ${pocketEquity->toEquityString}`,
        ]->Js.Array2.joinWith(" ")
      (false, msg)
    }
  | (Fold, LoseRaise) => (true, `Good fold`)
  | (Call, LoseRaise) => (true, `Good call`)
  | (Raise, LoseRaise) => (true, `Good raise`)
  | (Fold, StrongRaise) | (Call, StrongRaise) if pocketEquity <= checker.strongReRaise => {
      let msg =
        [
          "You should have re-raised.",
          `To re-raise you should have at least ${checker.strongReRaise->toEquityString} equity.`,
          `You had ${pocketEquity->toEquityString}`,
        ]->Js.Array2.joinWith(" ")
      (false, msg)
    }
  | (Fold, StrongRaise) | (Call, StrongRaise)
    if scenario.pocketCards->Simulate.classify === bluffHand => {
      let msg =
        [
          "You should have re-raised.",
          "You want to keep at least one bluff hand in your highest range. This is that bluffing hand.",
        ]->Js.Array2.joinWith(" ")
      (false, msg)
    }
  | (Call, StrongRaise) | (Raise, StrongRaise) if pocketEquity > checker.strongCallRaise => {
      let msg =
        [
          "You should have folded.",
          `To call you should have at least ${checker.strongCallRaise->toEquityString} equity.`,
          `You had ${pocketEquity->toEquityString}`,
        ]->Js.Array2.joinWith(" ")
      (false, msg)
    }
  | (Fold, StrongRaise) | (Raise, StrongRaise)
    if pocketEquity <= checker.strongCallRaise && pocketEquity > checker.strongReRaise => {
      let msg =
        [
          "You should have called.",
          `To call you should have at least ${checker.strongCallRaise->toEquityString} equity.`,
          `You had ${pocketEquity->toEquityString}`,
        ]->Js.Array2.joinWith(" ")
      (false, msg)
    }
  | (Fold, StrongRaise) => (true, `Good fold`)
  | (Call, StrongRaise) => (true, `Good call`)
  | (Raise, StrongRaise) => (true, `Good raise`)
  }
}
