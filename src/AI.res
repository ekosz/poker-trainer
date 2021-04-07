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
