let genDeck: unit => array<Card.t>

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

let positionOf: (int, int) => position
let stringOfPosition: position => string

type gameTurn =
  | Preflop
  | Flop
  | Turn
  | River

type action =
  | Check
  | Call
  | Raise
  | Fold

type pricedAction = (action, int)
type actionHistory = array<pricedAction>

type playHistory = {
  preflop: option<actionHistory>,
  flop: option<actionHistory>,
  turn: option<actionHistory>,
  river: option<actionHistory>,
}

type playerMode =
  | On
  | Folded

type playerState = {
  idx: int,
  name: string,
  playerMode: playerMode,
  stack: int,
  lastBet: option<int>,
  position: position,
  pocketCards: Common.pocketCards,
  playHistory: playHistory,
}

type gameMode =
  | NotStarted
  | On
  | Ended({winners: array<int>})

type gameState = {
  smallBlindAmout: int,
  bigBlindAmount: int,
  gameTurn: gameTurn,
  gameMode: gameMode,
  activeBet: option<int>,
  pot: int,
  playerTurn: int,
  buttonIdx: int,
  players: array<playerState>,
  board: option<Common.board>,
  deck: array<Card.t>,
}

let make: (int, int, int, int) => gameState
let start: gameState => gameState
let reset: gameState => gameState
let applyAction: (gameState, action, int) => gameState
let describeAction: (gameState, action, int) => string
