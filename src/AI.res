exception Invariant(string)

let bluffHand = "A5s"
let preFlopSmallBindCallingEquity = 0.666

type preFlopPcts = {
  raise: float,
  strongCallRaise: float,
  strongReRaise: float,
  loseCallRaise: float,
  loseReRaise: float,
}

// Any position right of the cutoff
let preflopEarlyPosition = {
  raise: 0.143,
  strongCallRaise: 0.113,
  strongReRaise: 0.009,
  loseCallRaise: 0.143,
  loseReRaise: 0.044,
}

// Position right of the button
let preflopCutOff = {
  raise: 0.222,
  strongCallRaise: 0.113,
  strongReRaise: 0.009,
  loseCallRaise: 0.186,
  loseReRaise: 0.054,
}

// All positions are relative to the button. Acts second to last pre-flop and last post-flop
let preflopButton = {
  raise: 0.333,
  strongCallRaise: 0.113,
  strongReRaise: 0.009,
  loseCallRaise: 0.222,
  loseReRaise: 0.087,
}

// The blinds are left of the button. They are last to act pre-flop and first
// to act post-flop. For now we're using the same percents for both big and
// small blind
let preflopBlinds = {
  raise: 0.063,
  strongCallRaise: 0.101,
  strongReRaise: 0.009,
  loseCallRaise: 0.143,
  loseReRaise: 0.044,
}

let totalCombos = 1326 // total combinations of pocket cards

let pctOfTotal = pct => pct->float_of_int /. totalCombos->float_of_int

let calcPct = (handToEval, hands) => {
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

let calculatePreflopAction = (
  hands: array<Classification.t>,
  losenessFactor: float,
  gameState: Game.gameState,
): Game.pricedAction => {
  let {activeBet, pot} = gameState
  let playerState = gameState.players->Js.Array2.unsafe_get(gameState.playerTurn)
  let equity = calcPct(playerState.pocketCards->Classification.make, hands)
  let checker = switch playerState.position {
  | Button => preflopButton
  | Cutoff => preflopCutOff
  | BigBlind | SmallBlind => preflopBlinds
  | _ => preflopEarlyPosition
  }

  switch (activeBet, playerState.lastBet) {
  | (Some(x), Some(y)) if x == y =>
    // BigBlind limp. Check or raise
    if equity < checker.raise {
      let raise =
        Js.Math.max_int(x * 2, (losenessFactor *. 3.0 *. x->float_of_int)->int_of_float) - y
      (Raise, raise)
    } else {
      (Check, 0)
    }
  | (Some(x), Some(y)) if x === gameState.bigBlindAmount && y === gameState.smallBlindAmout =>
    // SmallBlind limp
    if equity < checker.raise {
      let raise =
        Js.Math.max_int(x * 2, (losenessFactor *. 3.0 *. x->float_of_int)->int_of_float) - y
      (Raise, raise)
    } else if equity < preFlopSmallBindCallingEquity {
      (Call, x - y)
    } else {
      (Fold, 0)
    }
  | (Some(x), None) if x == gameState.bigBlindAmount =>
    // Limp to us
    if equity < checker.raise {
      let raise = Js.Math.max_int(x * 2, (losenessFactor *. 3.0 *. x->float_of_int)->int_of_float)
      (Raise, raise)
    } else {
      (Fold, 0)
    }
  | (Some(x), None) => {
      // Pot has already been raised
      let potOdds = x->float_of_int /. (x + pot)->float_of_int
      if playerState.pocketCards->Classification.make->Classification.toString === bluffHand {
        // re-raise our bluff hand
        (Raise, x * 2)
      } else if equity < potOdds {
        let pctOfRange = (checker.raise -. equity) /. checker.raise *. losenessFactor
        if pctOfRange > 0.5 {
          let raise = Js.Math.max_int(
            x * 2,
            (pctOfRange *. 2.0 *. pot->float_of_int)->int_of_float - x,
          )
          (Raise, raise)
        } else {
          (Call, x)
        }
      } else {
        (Fold, 0)
      }
    }
  | (Some(x), Some(y)) => {
      // Already raised
      let potOdds = (x - y)->float_of_int /. (x - y + pot)->float_of_int
      if equity < potOdds {
        (Call, x - y)
      } else {
        (Fold, 0)
      }
    }
  | (None, _) => raise(Invariant("Shold not get here"))
  }
}

let calculateAction = (
  hands: array<Classification.t>,
  losenessFactor: float,
  gameState: Game.gameState,
): Game.pricedAction => {
  switch gameState.gameTurn {
  | Preflop => calculatePreflopAction(hands, losenessFactor, gameState)
  | _ =>
    switch gameState.activeBet {
    | None => (Check, 0)
    | Some(x) => (Call, x)
    }
  }
}
