/**
 * Plays a game with X players and prints the results to stdout. This script is
 * mostly just used for testing purposes.
 *
 * Usage: node ./scripts/simulateGame.mjs [numberOfPlayers (default 9)]
 */

// Make sure to run `re:build` first or these imports will fail
import * as Simulate from '../src/Simulate.mjs';
import * as Card from '../src/Card.mjs';
import * as Scoring from '../src/Scoring.mjs';

function printResult({idx, hand, pocketCards, score}) {
  console.log(
    `Player ${idx + 1}`,
    [pocketCards.p1, pocketCards.p2].map(c => Card.stringOfCard(c)),
    Scoring.stringOfScore(score, hand),
    [hand.c1, hand.c2, hand.c3, hand.c4, hand.c5].map(c => Card.stringOfCard(c))
  );
}

(() => {
  const numberOfPlayers = process.argv[2] ? Number(process.argv[2]) : 9;

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
})();
