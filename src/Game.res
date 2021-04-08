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

let emptyHistory = {preflop: None, flop: None, turn: None, river: None}

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

let addToHistory = (playHistory, gameTurn: gameTurn, pricedAction) => {
  switch (gameTurn, playHistory) {
  | (Preflop, {preflop: None}) => {...playHistory, preflop: [pricedAction]->Some}
  | (Preflop, {preflop: Some(acc)}) => {
      ...playHistory,
      preflop: acc->Js.Array2.concat([pricedAction])->Some,
    }
  | (Flop, {flop: None}) => {...playHistory, flop: [pricedAction]->Some}
  | (Flop, {flop: Some(acc)}) => {
      ...playHistory,
      flop: acc->Js.Array2.concat([pricedAction])->Some,
    }
  | (Turn, {turn: None}) => {...playHistory, turn: [pricedAction]->Some}
  | (Turn, {turn: Some(acc)}) => {
      ...playHistory,
      turn: acc->Js.Array2.concat([pricedAction])->Some,
    }
  | (River, {river: None}) => {...playHistory, river: [pricedAction]->Some}
  | (River, {river: Some(acc)}) => {
      ...playHistory,
      river: acc->Js.Array2.concat([pricedAction])->Some,
    }
  }
}

let applyPlayerAction = (
  playerState: playerState,
  currentBet: option<int>,
  gameTurn: gameTurn,
  action: action,
  amount: int,
) => {
  let lastBet = switch currentBet {
  | _ if amount === 0 => playerState.lastBet
  | Some(x) => (x + amount)->Some
  | None => amount->Some
  }

  {
    ...playerState,
    lastBet: lastBet,
    playerMode: action == Fold ? Folded : playerState.playerMode,
    playHistory: playerState.playHistory->addToHistory(gameTurn, (action, amount)),
    stack: playerState.stack - amount,
  }
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

let deck = Belt.Array.makeBy(52, i => {
  Card.suite: Card.suiteOfInt(i / 13),
  rank: Card.rankOfInt(mod(i, 13) + 1),
})

let genDeck = () => deck->Belt.Array.shuffle

exception InvaildAction(string)
let validateAction = (
  gameState: gameState,
  playerState: playerState,
  action: action,
  amount: int,
) => {
  if gameState.gameMode != On {
    raise(InvaildAction("Game must be on"))
  }
  if playerState.playerMode != On {
    raise(InvaildAction("Player must be still in the game"))
  }
  if amount < 0 {
    raise(InvaildAction("Amount cannot be negative"))
  }
  switch (action, gameState.activeBet, playerState.lastBet) {
  | (Fold, _, _) | (Check, _, _) if amount != 0 =>
    raise(InvaildAction("Amount must be 0 for check or fold"))
  | (Check, Some(x), Some(y)) if x > y =>
    raise(InvaildAction("Cannot check when there is an active bet larger than last bet"))
  | (Check, Some(_), None) => raise(InvaildAction("Cannot check when there is an active bet"))
  | (Call, None, _) => raise(InvaildAction("Cannot call when there is no active bet"))
  | (Call, Some(x), None) if amount != x =>
    raise(
      InvaildAction(
        `Call of ${amount->string_of_int} does not match active bet of ${x->string_of_int}`,
      ),
    )
  | (Call, Some(x), Some(y)) if amount + y != x => {
      Js.log({"amount": amount, "x": x, "y": y})
      raise(InvaildAction("Must match current bet"))
    }
  | (Raise, Some(x), None) if amount < x * 2 =>
    raise(InvaildAction("Cannot raise less than double current bet"))
  | (Raise, Some(x), Some(y)) if amount + y < x * 2 =>
    raise(InvaildAction("Cannot raise less than double current bet"))
  | (Call, _, _) | (Raise, _, _) if amount > playerState.stack =>
    raise(InvaildAction("Cannot bet more than stack"))
  | _ => ()
  }
}

let calcWinners = gameState => {
  let nonFoldedPlayers = gameState.players->Js.Array2.filter(p => p.playerMode == On)

  @warning("-8")
  let Some(board) = gameState.board
  let compare = Scoring.compare(board)

  let bestHand =
    nonFoldedPlayers
    ->Belt.SortArray.stableSortBy((playerA, playerB) => {
      compare(playerA.pocketCards, playerB.pocketCards)
    })
    ->Js.Array2.unsafe_get(0)

  nonFoldedPlayers
  ->Js.Array2.filter(p => compare(bestHand.pocketCards, p.pocketCards) === 0)
  ->Js.Array2.map(p => p.idx)
}

let make = (numberOfPlayers, startingStack, smallBlindAmout, bigBlindAmount) => {
  let buttonIdx = 0
  let deck = genDeck()
  let players = Belt.Array.makeBy(numberOfPlayers, i => {
    let position = positionOf(numberOfPlayers, i + 1)
    {
      idx: i,
      name: `Player ${(i + 1)->string_of_int}`,
      position: position,
      playerMode: On,
      lastBet: None,
      stack: startingStack,
      playHistory: emptyHistory,
      pocketCards: {
        p1: deck->Belt.Array.getUnsafe(i),
        p2: deck->Belt.Array.getUnsafe(numberOfPlayers + i),
      },
    }
  })

  {
    gameMode: NotStarted,
    gameTurn: Preflop,
    players: players,
    smallBlindAmout: smallBlindAmout,
    bigBlindAmount: bigBlindAmount,
    pot: 0,
    // UnderTheGun | Button
    playerTurn: mod(buttonIdx + 3, numberOfPlayers),
    buttonIdx: buttonIdx,
    activeBet: None,
    board: None,
    deck: deck,
  }
}

let start = (gameState: gameState) => {
  let players = gameState.players->Js.Array2.map(player =>
    switch player.position {
    | BigBlind =>
      player->applyPlayerAction(None, gameState.gameTurn, Raise, gameState.bigBlindAmount)
    | SmallBlind =>
      player->applyPlayerAction(None, gameState.gameTurn, Raise, gameState.smallBlindAmout)
    | _ => player
    }
  )

  {
    ...gameState,
    gameMode: On,
    players: players,
    pot: gameState.bigBlindAmount + gameState.smallBlindAmout,
    activeBet: gameState.bigBlindAmount->Some,
  }
}

let reset = (gameState: gameState) => {
  let numberOfPlayers = gameState.players->Js.Array2.length
  // Button rotates clockwise around the table
  let buttonIdx = mod(gameState.buttonIdx + 1, numberOfPlayers)
  let deck = genDeck()

  let players = gameState.players->Js.Array2.map(player => {
    // Position rotates counter-clockwise relative to the button
    let positionNum =
      player.idx - buttonIdx >= 0
        ? player.idx - buttonIdx
        : numberOfPlayers + player.idx - buttonIdx
    let position = positionOf(numberOfPlayers, positionNum + 1)

    // Give winning to player if won last game
    let stack = switch gameState.gameMode {
    | Ended({winners}) if winners->Js.Array2.includes(player.idx) =>
      player.stack + gameState.pot / winners->Js.Array2.length
    | _ => player.stack
    }

    {
      ...player,
      stack: stack,
      position: position,
      playerMode: On,
      lastBet: None,
      playHistory: emptyHistory,
      pocketCards: {
        p1: deck->Belt.Array.getUnsafe(player.idx),
        p2: deck->Belt.Array.getUnsafe(numberOfPlayers + player.idx),
      },
    }
  })

  {
    ...gameState,
    board: None,
    activeBet: None,
    gameMode: NotStarted,
    gameTurn: Preflop,
    players: players,
    pot: 0,
    // UnderTheGun | Button
    playerTurn: mod(buttonIdx + 3, numberOfPlayers),
    buttonIdx: buttonIdx,
    deck: deck,
  }
}

let progressBoard = (board: option<Common.board>, numberOfPlayers, deck) => {
  switch board {
  | None =>
    {
      Common.flop1: deck->Js.Array2.unsafe_get(numberOfPlayers * 2 + 1),
      flop2: deck->Js.Array2.unsafe_get(numberOfPlayers * 2 + 2),
      flop3: deck->Js.Array2.unsafe_get(numberOfPlayers * 2 + 3),
      turn: None,
      river: None,
    }->Some
  | Some({turn: None, river: None} as x) =>
    {
      ...x,
      turn: deck->Js.Array2.unsafe_get(numberOfPlayers * 2 + 1 + 3 + 1)->Some,
    }->Some
  | Some({river: None} as x) =>
    {
      ...x,
      river: deck->Js.Array2.unsafe_get(numberOfPlayers * 2 + 1 + 3 + 1 + 1)->Some,
    }->Some
  | _ => board
  }
}

let progressTurn = gameState => {
  let {gameTurn} = gameState
  if gameTurn == River {
    {...gameState, gameMode: Ended({winners: gameState->calcWinners})}
  } else {
    let {gameTurn} = gameState
    let numberOfPlayers = gameState.players->Js.Array2.length
    let nonFoldedPlayers = gameState.players->Js.Array2.filter(p => p.playerMode != Folded)
    let firstToActIdx = mod(gameState.buttonIdx + 1, numberOfPlayers)
    let firstToAct = switch nonFoldedPlayers->Js.Array2.find(p => p.idx >= firstToActIdx) {
    | Some(x) => x
    | None => nonFoldedPlayers->Js.Array2.unsafe_get(0)
    }

    {
      ...gameState,
      activeBet: None,
      players: gameState.players->Js.Array2.map(player => {...player, lastBet: None}),
      playerTurn: firstToAct.idx,
      board: gameState.board->progressBoard(numberOfPlayers, gameState.deck),
      gameTurn: gameTurn == Preflop
        ? Flop
        : gameTurn == Flop
        ? Turn
        : gameTurn == Turn
        ? River
        : raise(Invariant("Should not get here")),
    }
  }
}

let progressState = (gameState: gameState): gameState => {
  let nonFoldedPlayers = gameState.players->Js.Array2.filter(p => p.playerMode != Folded)
  if nonFoldedPlayers->Js.Array2.length === 1 {
    let winners = nonFoldedPlayers->Js.Array2.map(p => p.idx)
    {...gameState, gameMode: Ended({winners: winners})}
  } else {
    let nextPlayer = switch nonFoldedPlayers->Js.Array2.find(p => p.idx > gameState.playerTurn) {
    | Some(x) => x
    | None => nonFoldedPlayers->Js.Array2.unsafe_get(0)
    }
    switch (gameState.gameTurn, nextPlayer.playHistory) {
    | (Preflop, {preflop: None})
    | (Flop, {flop: None})
    | (Turn, {turn: None})
    | (River, {river: None}) => {...gameState, playerTurn: nextPlayer.idx}
    | (Preflop, _)
      if gameState.activeBet == Some(gameState.bigBlindAmount) &&
        nextPlayer.position ==
          BigBlind => // Only in the preflop phase the big blind has the option to check or
      // raise instead of the round ending
      {...gameState, playerTurn: nextPlayer.idx}
    | _ if gameState.activeBet == nextPlayer.lastBet => // Betting is over
      gameState->progressTurn
    | _ => {...gameState, playerTurn: nextPlayer.idx}
    }
  }
}

let applyAction = (gameState, action, amount: int) => {
  let {playerTurn, pot} = gameState
  let player = gameState.players->Js.Array2.unsafe_get(playerTurn)
  gameState->validateAction(player, action, amount)

  let players = gameState.players->Js.Array2.map(player =>
    if player.idx === playerTurn {
      player->applyPlayerAction(gameState.activeBet, gameState.gameTurn, action, amount)
    } else {
      player
    }
  )

  // If raising, set the new active bet for the table
  let activeBet = switch (action, gameState.activeBet) {
  | (Raise, Some(x)) => Some(x + amount)
  | (Raise, None) => Some(amount)
  | (_, x) => x
  }

  {
    ...gameState,
    players: players,
    activeBet: activeBet,
    pot: pot + amount,
  }->progressState
}

let describeAction = (gameState: gameState, action: action, amount: int) => {
  let player = gameState.players->Js.Array2.unsafe_get(gameState.playerTurn)

  let actionString = switch (action, gameState.activeBet, player.lastBet) {
  | (Check, _, _) => "checks"
  | (Fold, _, _) => "folds"
  | (Call, _, None) => `calls ${amount->string_of_int}`
  | (Call, _, Some(x)) => `adds ${amount->string_of_int} to call ${(x + amount)->string_of_int}`
  | (Raise, Some(x), None) => `raises to ${(amount + x)->string_of_int} from ${x->string_of_int}`
  | (Raise, None, None) => `bets ${amount->string_of_int}`
  | (Raise, Some(x), Some(y)) =>
    `adds ${amount->string_of_int} to raise to ${(y + amount)
        ->string_of_int} from ${x->string_of_int}`
  | (Raise, None, Some(_)) => raise(Invariant("Should not get here"))
  }

  [player.name, `(${player.position->stringOfPosition})`, actionString]->Js.Array2.joinWith(" ")
}

let avaibleActions = gameState => {
  let player = gameState.players->Js.Array2.unsafe_get(gameState.playerTurn)
  switch (gameState.activeBet, player.lastBet) {
  | (None, None) => [{"name": "Check", "value": Check}, {"name": "Raise", "value": Raise}]
  | (Some(_), None) => [
      {"name": "Fold", "value": Fold},
      {"name": "Call", "value": Call},
      {"name": "Raise", "value": Raise},
    ]
  | (Some(x), Some(y)) if x === y => [
      {"name": "Check", "value": Check},
      {"name": "Raise", "value": Raise},
    ]
  | (Some(_), Some(_)) => [
      {"name": "Fold", "value": Fold},
      {"name": "Call", "value": Call},
      {"name": "Raise", "value": Raise},
    ]
  | (None, Some(_)) => raise(Invariant("Should not get here"))
  }
}

let raiseRange = gameState => {
  let player = gameState.players->Js.Array2.unsafe_get(gameState.playerTurn)
  switch gameState.activeBet {
  | Some(x) => (x * 2, player.stack)
  | None => (gameState.bigBlindAmount, player.stack)
  }
}
