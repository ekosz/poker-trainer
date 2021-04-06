#!/usr/bin/env node

import { readFileSync } from 'fs';
import parse from 'csv-parse/lib/sync.js';
import inquirer from 'inquirer';
import chalk from 'chalk';
import { Command, Option } from 'commander/esm.mjs';
// Make sure to run `re:build` first or these imports will fail
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
  numberOfPlayers = numberOfPlayers || randomInt(3, 11);
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
    let numberOfPlayers;
    if (options.hands !== 'random') {
      numberOfPlayers = parseInt(options.hands, 10);
    }
    while(true) {
      await preFlopTraining(numberOfPlayers);
      if (!(await inquirer.prompt([{type: 'confirm', name: 'again', default: true }])).again) {
        break;
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
