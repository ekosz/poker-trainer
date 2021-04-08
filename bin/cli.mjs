#!/usr/bin/env node

import { readFileSync } from 'fs';
import parse from 'csv-parse/lib/sync.js';
import inquirer from 'inquirer';
import chalk from 'chalk';
import { Command, Option } from 'commander/esm.mjs';
// Make sure to run `re:build` first or these imports will fail
import * as AI from '../src/AI.mjs';
import * as Game from '../src/Game.mjs';
import * as PreflopTraining from '../src/PreflopTraining.mjs';
import * as Simulate from '../src/Simulate.mjs';
import * as Card from '../src/Card.mjs';
import * as Scoring from '../src/Scoring.mjs';

const packageJSON = JSON.parse(
  readFileSync(new URL('../package.json', import.meta.url), { encoding: 'utf8' })
);

const program = new Command();
program.version(packageJSON.version);

const loadComputedHands = (() => {
  const cache = {};
  return numberOfPlayers => {
    if (cache[numberOfPlayers]) return cache[numberOfPlayers];
    // array<(string, float)>
    const data = parse(
      readFileSync(new URL('../data/odds_3_handed.csv', import.meta.url), { encoding: 'utf8' })
    );
    // Sort by winning freqency desc
    data.sort((a, b) => b[1] - a[1]);
    // Only store the classifications themselves
    cache[numberOfPlayers] = data.map(x => x[0]);
    return cache[numberOfPlayers];
  }
})();

// The maximum is exclusive and the minimum is inclusive
const randomInt = (min, max) => {
  return Math.floor(Math.random() * (max - min) + min);
}

async function preFlopTraining(numberOfPlayers) {
  const scenario = PreflopTraining.genSceanario(numberOfPlayers);
  const question = {
    type: 'list',
    name: 'action',
    message: PreflopTraining.describeScenario(scenario),
    choices: PreflopTraining.scenarioChoices(scenario),
  }
  const response = await inquirer.prompt([question]);
  const [isCorrect, why] = PreflopTraining.testScenario(
    response.action,
    scenario,
    loadComputedHands(numberOfPlayers)
  );
  if (isCorrect) {
    process.stdout.write(chalk.green.bold('Correct!') + ' ' + why + '\n');
  } else {
    process.stdout.write(chalk.red.bold('Incorrect!') + ' ' + why + '\n');
  }
}

let makeAIGame = ({ numberOfPlayers, playerPosition, ais }) => {
  let hands = loadComputedHands(numberOfPlayers);

  return async initialState => {
    let gameState = Game.start(initialState);
    let player = gameState.players[playerPosition];
    console.log('Starting stack sizes');
    gameState.players.forEach(p => console.log(p.name, p.stack))
    console.log(`You are ${Game.stringOfPosition(player.position)}.`);
    console.log(`You are delt ${Card.stringOfCard(player.pocketCards.p1)}, ${Card.stringOfCard(player.pocketCards.p2)}.`);
    while(!gameState.gameMode.winners) {
      let action;
      let amount;
      if (gameState.playerTurn === playerPosition) {
        console.log("Pot size", gameState.pot)
        const response = await inquirer.prompt([{
          type: 'list',
          name: 'action',
          message: 'What would you like to do?',
          choices: Game.avaibleActions(gameState),
        }, {
          type: 'number',
          name: 'amount',
          validate: x => {
            const [min, max] = Game.raiseRange(gameState);
            if (x < min) return `Cannot raise less than ${min}`;
            if (x > max) return `Cannot raise more than ${max}`;
            return true;
          },
          when: ({ action }) => action === 2 /* Raise */,
        }]);
        action = response.action;
        if (action === 2 /* Raise */) {
          amount = response.amount
        } else if (action === 3 /* Fold */ || action === 0 /* Check */) {
          amount = 0
        } else {
          amount = gameState.activeBet;
        }
      } else {
        [action, amount] = AI.calculateAction(hands, ais[gameState.playerTurn], gameState);
      }
      const nextState = Game.applyAction(gameState, action, amount)
      console.log(Game.describeAction(gameState, action, amount));
      if (gameState.board !== nextState.board) {
        console.log('The board is', [
          nextState.board.flop1,
          nextState.board.flop2,
          nextState.board.flop3,
          nextState.board.turn,
          nextState.board.river,
        ].filter(Boolean).map(Card.stringOfCard));
      }
      gameState = nextState;
      // Sleep for 200ms to make it seem like real work is happening
      await new Promise(resolve => setTimeout(resolve, 200));
    }
    gameState.gameMode.winners.forEach(winningIdx => {
      const winner = gameState.players[winningIdx];
      console.log(winner.name, 'won with', [Card.stringOfCard(winner.pocketCards.p1), Card.stringOfCard(winner.pocketCards.p2)])
    });
    return gameState;
  }
}

function recalculateOdds(numberOfPlayers, simulations) {
  console.log(`Running ${simulations} ${numberOfPlayers} handed games...`)
  const results = Simulate.run(simulations, numberOfPlayers);

  writeFileSync(
    new URL(`../data/odds_${numberOfPlayers}_handed.csv`, import.meta.url),
    stringify(results),
    { encoding: 'utf8' }
  );
}

function printResult({idx, hand, pocketCards, score}) {
  console.log(
    `Player ${idx + 1}`,
    [pocketCards.p1, pocketCards.p2].map(c => Card.stringOfCard(c)),
    Scoring.stringOfScore(score, hand),
    [hand.c1, hand.c2, hand.c3, hand.c4, hand.c5].map(c => Card.stringOfCard(c))
  );
}

function simulateGame(numberOfPlayers) {
  var [winners, losers, board] = Simulate.playGame(numberOfPlayers);
  console.log("Board", [
    board.flop1,
    board.flop2,
    board.flop3,
    board.turn,
    board.river
  ].map(c => Card.stringOfCard(c)));
  console.log(winners.length === 1 ? "Winner" : "Winners");
  winners.forEach(printResult);
  console.log("Losers");
  losers.forEach(printResult)
}

program
  .command('train', { isDefault: true })
  .description('starts a new training session')
  .addOption(new Option('-m, --mode <mode>', 'training mode').choices(['pre-flop']).default('pre-flop'))
  .addOption(new Option('-h, --hands <size>', 'hand size').choices(['3','4','5','6','7','8','9','10','random']).default('random'))
  .action(async options => {
    const numberOfPlayers = options.hands !== 'random' ? parseInt(options.hands, 10) : randomInt(3, 11);
    while(true) {
      await preFlopTraining(numberOfPlayers);
      if (!(await inquirer.prompt([{type: 'confirm', name: 'again', default: true }])).again) {
        break;
      }
    }
  });

program
  .command('play')
  .description('players a game with (pretty darn bad) AI')
  .addOption(new Option('-h, --hands <size>', 'hand size').choices(['3','4','5','6','7','8','9','10','random']).default('9'))
  .addOption(new Option('-s, --stack <amount>', 'starting stack size').default('200'))
  .addOption(new Option('-bb, --big-blind <amount>', 'big blind').default('2'))
  .addOption(new Option('-ss, --small-blind <amount>', 'small blind').default('1'))
  .action(async options => {
    const numberOfPlayers = options.hands !== 'random' ? parseInt(options.hands, 10) : randomInt(3, 11);
    let playerPosition = randomInt(0, numberOfPlayers)
    const ais = Array.from(new Array(numberOfPlayers)).map(() => Math.random() + 0.5)
    let playAIGame = makeAIGame({ numberOfPlayers, playerPosition, ais })
    let gameState = Game.make(
      numberOfPlayers,
      parseInt(options.stack, 10),
      parseInt(options.smallBlind, 10),
      parseInt(options.bigBlind, 10),
    );
    // Override name for the non-NPC
    gameState.players[playerPosition].name = 'You'

    while(true) {
      try {
        gameState = await playAIGame(gameState);
      } catch (err) {
        console.error(err);
        process.exit(1);
      }
      if (!(await inquirer.prompt([{type: 'confirm', name: 'again', default: true }])).again) {
        break;
      }
      try {
        gameState = Game.reset(gameState)
      } catch (err) {
        console.error(err);
        process.exit(1);
      }
    }
  });

program
  .command('recalculateOdds')
  .description('Recalculates the odds csv for a given hand size. These odds are later used by the trainer for giving feedback. The odds are calculated by running many many simulations and seeing which pocket cards lead to the best odds of winning.')
  .addOption(new Option('-h, --hands <size>', 'hand size').makeOptionMandatory(true))
  .addOption(new Option('-s, --simulations <amount>', 'number of simulations').default(1_000_000))
  .action(({ hands, simulations }) => {
    recalculateOdds(hands, simulations)
  });

program
  .command('simulateGame')
  .description('Plays a game with X players and prints the results to stdout. This script is mostly just used for testing purposes.')
  .addOption(new Option('-h, --hands <size>', 'hand size').default(9))
  .action(({ hands }) => {
    simulateGame(hands)
  });

program.parse(process.argv);
