package com.example.model

import java.util.UUID

sealed case class State private (name: Name, guesses: Set[Guess], word: Word) {
  def failuresCount: Int            = (guesses.map(_.char) -- word.toSet).size
  def playerLost: Boolean           = failuresCount > 5
  def playerWon: Boolean            = (word.toSet -- guesses.map(_.char)).isEmpty
  def addGuess(guess: Guess): State = new State(name, guesses + guess, word) {}
}

object State {
  def initial(name: Name, word: Word): State = new State(name, Set.empty, word) {}

  def make(name: Name, guesses: Set[Guess], word: Word): State = State(name, guesses, word)

  final case class StateId private (value: String) extends AnyVal
  object StateId {
    def make(id: String): StateId = StateId(id)
  }
}
