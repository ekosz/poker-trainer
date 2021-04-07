let bluffHand: string
let preFlopSmallBindCallingEquity: float

type preFlopPcts = {
  raise: float,
  strongCallRaise: float,
  strongReRaise: float,
  loseCallRaise: float,
  loseReRaise: float,
}

// Any position right of the cutoff
let preflopEarlyPosition: preFlopPcts

// Position right of the button
let preflopCutOff: preFlopPcts

// All positions are relative to the button. Acts second to last pre-flop and last post-flop
let preflopButton: preFlopPcts

// The blinds are left of the button. They are last to act pre-flop and first
// to act post-flop. For now we're using the same percents for both big and
// small blind
let preflopBlinds: preFlopPcts

let calcPct: (Classification.t, array<Classification.t>) => float

let calculateAction: (array<Classification.t>, float, Game.gameState) => Game.pricedAction
