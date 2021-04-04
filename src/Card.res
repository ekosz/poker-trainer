type suite =
  | Club
  | Heart
  | Diamond
  | Spade

exception InvalidSuiteInt(int)
let suiteOfInt = (x: int): suite => {
  switch x {
  | 0 => Club
  | 1 => Heart
  | 2 => Diamond
  | 3 => Spade
  | _ => raise(InvalidSuiteInt(x))
  }
}

let stringOfSuite = s => {
  switch s {
  | Club => "c"
  | Heart => "h"
  | Spade => "s"
  | Diamond => "d"
  }
}

let intOfSuite = s => {
  switch s {
  | Club => 4
  | Heart => 3
  | Spade => 2
  | Diamond => 1
  }
}

exception InvalidSuiteString(string)
let suiteOfString = c => {
  switch c {
  | "c" => Club
  | "h" => Heart
  | "s" => Spade
  | "d" => Diamond
  | x => raise(InvalidSuiteString(x))
  }
}

type rank =
  | Ace
  | King
  | Queen
  | Jack
  | Num(int)

exception InvalidRankInt(int)
let rankOfInt = (x: int): rank => {
  if x < 1 || x > 13 {
    raise(InvalidRankInt(x))
  }
  switch x {
  | 13 => King
  | 12 => Queen
  | 11 => Jack
  | 1 => Ace
  | _ => Num(x)
  }
}

let stringOfRank = f => {
  switch f {
  | Ace => "A"
  | King => "K"
  | Queen => "Q"
  | Jack => "J"
  | Num(10) => "T"
  | Num(x) => string_of_int(x)
  }
}

let rankOfString = s => {
  switch s {
  | "A" => Ace
  | "K" => King
  | "Q" => Queen
  | "J" => Jack
  | "T" => Num(10)
  | x => Num(int_of_string(x))
  }
}

let intOfRank = f => {
  switch f {
  | Num(x) => x
  | Jack => 11
  | Queen => 12
  | King => 13
  | Ace => 14
  }
}

type t = {suite: suite, rank: rank}
let stringOfCard = (card: t) => {
  stringOfRank(card.rank) ++ stringOfSuite(card.suite)
}
