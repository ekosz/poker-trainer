#!/usr/bin/env node

import { readFileSync } from 'fs';
import parse from 'csv-parse/lib/sync.js';
import inquirer from 'inquirer';
import chalk from 'chalk';
import { Command, Option } from 'commander/esm.mjs';
// Make sure to run `re:build` first or these imports will fail
import * as PreflopTraining from '../src/PreflopTraining.mjs';

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

async function preFlopTraining() {
  const numberOfPlayers = randomInt(3, 11);
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

program
  .command('train', { isDefault: true })
  .description('starts a new training session')
  .addOption(new Option('-m, --mode <mode>', 'training mode').choices(['pre-flop']).default('pre-flop'))
  .action(async _options => {
    while(true) {
      await preFlopTraining();
      if (!(await inquirer.prompt([{type: 'confirm', name: 'again', default: true }])).again) {
        break;
      }
    }
  });

program.parse(process.argv);
