# PPL

This the repo I'm using for [a probabalistic programming class](http://www.cs.tufts.edu/comp/150PP/)

## Usage

Most of this so far is in Clojure and you need [Leiningen](https://leiningen.org/) installed to run it.  You can also do this with Homebrew `brew install leiningen`.

You can use `lein repl` to call the functions interactively, and will need to for the coin problems.  The Monte Carlo die simulation for the dice guessing game in class can be run from the ds directory in the terminal using `lein run num-games num-throws pivot`.  This runs a simulation for a tally sheet with the pivot provied, the specified number of throws per game, and the number of games to simulate.  It will print the odds of each of the four possabilities (guessing correctly in either the first three attempts or failing to do so and losing the turn).