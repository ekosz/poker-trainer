exception Invariant(string)

type previousAction =
  | Limp
  | LoseRaise
  | StrongRaise

type scenario = {
  numberOfPlayers: int,
  previousAction: previousAction,
  positionNum: int,
  position: Game.position,
  pocketCards: Common.pocketCards,
}

let pctOfTotal = pct => pct->float_of_int /. AI.totalCombos->float_of_int
let roundToThousandths = num => Js.Math.round(num *. 1000.0) /. 1000.0
let roundToTenths = num => Js.Math.round(num *. 10.0) /. 10.0

let calcPct = (handToEval: Classification.t, hands: array<Classification.t>) => {
  let (_, totalHands) = hands->Belt.Array.reduce((false, 0), ((found, acc), hand) => {
    if found {
      (found, acc)
    } else if hand === handToEval {
      (true, acc + hand->Classification.handCount)
    } else {
      (false, acc + hand->Classification.handCount)
    }
  })
  totalHands->pctOfTotal
}

let genSceanario = numberOfPlayers => {
  @warning("-8")
  let [p1, p2] = Game.genDeck()->Belt.Array.slice(~offset=0, ~len=2)
  let positionNum = Js.Math.random_int(1, numberOfPlayers + 1)
  let seed = Js.Math.random()
  {
    numberOfPlayers: numberOfPlayers,
    pocketCards: {p1: p1, p2: p2},
    positionNum: positionNum,
    position: Game.positionOf(numberOfPlayers, positionNum),
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
      ? Game.positionOf(scenario.numberOfPlayers, scenario.numberOfPlayers)
      : Game.positionOf(scenario.numberOfPlayers, scenario.positionNum - 1)
  let prevAction = switch scenario.previousAction {
  | Limp => " limps in"
  | StrongRaise => ", who is normally tight, raises"
  | LoseRaise => ", who has been playing lose, raises"
  }

  [
    `You are playing ${scenario.numberOfPlayers->string_of_int} handed and are ${scenario.position->Game.stringOfPosition}.`,
    `You are delt ${scenario.pocketCards.p1->Card.stringOfCard}, ${scenario.pocketCards.p2->Card.stringOfCard}.`,
    scenario.position == UnderTheGun ||
      (scenario.numberOfPlayers === 3 && scenario.position == Button)
      ? ""
      : prevPosition->Game.stringOfPosition ++ prevAction ++ ".",
    "What do you do?",
  ]
  ->Js.Array2.filter(x => x != "")
  ->Js.Array2.joinWith(" ")
}

let scenarioChoices = (scenario: scenario) => {
  if scenario.position == BigBlind && scenario.previousAction == Limp {
    [{"name": "Check", "value": Game.Check}, {"name": "Raise", "value": Game.Raise}]
  } else {
    [
      {"name": "Call", "value": Game.Call},
      {"name": scenario.previousAction == Limp ? "Raise" : "Re-raise", "value": Game.Raise},
      {"name": "Fold", "value": Game.Fold},
    ]
  }
}

let toEquityString = x => ((1.0 -. x) *. 100.0)->roundToTenths->Js.Float.toString ++ "%"

let testScenario = (action: Game.action, scenario: scenario, hands: array<Classification.t>) => {
  let checker = switch scenario.position {
  | Button => AI.preflopButton
  | Cutoff => AI.preflopCutOff
  | BigBlind | SmallBlind => AI.preflopBlinds
  | _ => AI.preflopEarlyPosition
  }
  let pocketEquity = scenario.pocketCards->Classification.make->calcPct(hands)->roundToThousandths

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
  | (Call, Limp)
    if scenario.position == SmallBlind && pocketEquity > AI.preFlopSmallBindCallingEquity => {
      let msg =
        [
          "You should have folded.",
          `To call you should have at least ${AI.preFlopSmallBindCallingEquity->toEquityString} equity.`,
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
    if scenario.pocketCards->Simulate.classify === AI.bluffHand => {
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
