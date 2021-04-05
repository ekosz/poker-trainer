/**
 * Recalculates the odds csv for a given hand size. These odds are later used
 * by the trainer for giving feedback. The odds are calculated by running many
 * many simulations and seeing which pocket cards lead to the best odds of
 * winning.
 *
 * Usage: node ./scripts/recalculateOdds.mjs numberOfPlayers [simulations (default 1_000_000)]
 */

import { writeFileSync } from 'fs';
import stringify from 'csv-stringify/lib/sync.js';
// Make sure to run `re:build` first or these imports will fail
import * as Simulate from '../src/Simulate.mjs';

(() => {
  const numberOfPlayers = process.argv[2]
    ? Number(process.argv[2])
    : null;
  if (!numberOfPlayers) throw new Error('Must provide number of players');
  const simulations = process.argv[3] ? Number(process.argv[3]) : 1_000_000;
  console.log(`Running ${simulations} ${numberOfPlayers} handed games...`)
  const results = Simulate.run(simulations, numberOfPlayers);

  writeFileSync(
    new URL(`../data/odds_${numberOfPlayers}_handed.csv`, import.meta.url),
    stringify(results),
    { encoding: 'utf8' }
  );
})();
