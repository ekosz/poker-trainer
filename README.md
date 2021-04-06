# Poker Trainer

This is a little test application made by me (and for me) to learn
[Rescript](https://rescript-lang.org/) better. It is _highly_ opinionated and
ranges are hard coded at the moment. If you don't like the numbers I chose,
feel free to fork and add your own!

## Usage

Currently this package isn't yet uploaded to `npm`. To use it you will have the
following the "Developing" steps below then run the bin file via `node
./bin/cli.mjs`.

## Developing

This repository current uses [pnpm](https://pnpm.io/) and comes with
a `pnpm-lock.yaml` file.  But `npm` / `yarn` should also work. Feel free to
swap out for those instead.

1. Clone this repo
2. `pnpm install`
3. `pnpm run re:start` or `pnpm run re:build` to build without watch mode

## How the ranges were calculated

For the training to work properly it needed to stack rank all the pocket cards
could be dealt. The way how I chose to stack rank for was each player count,
I simulated 1,000,000 games of poker and recorded each hand's winning
frequency. The results are saved in the `data/` folder currently. This may not
be the _best_ way of doing this but it worked well for me.

## Why Rescript?

Why not?
