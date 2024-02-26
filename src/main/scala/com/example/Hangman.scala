package com.example

import com.example.model.State.StateId
import com.example.model.Word.WordId
import com.example.model.error._
import com.example.model.{ Guess, GuessResult, Name, State, Word }
import com.example.repository.{ StateRepository, WordRepository }

import java.io.IOException
import zio._

object Hangman extends ZIOAppDefault {

  def getUserInput(message: String): IO[IOException, String] = Console.readLine(message)

  lazy val getGuess: IO[IOException, Guess] =
    for {
      input <- getUserInput("What's your next guess? ")
      guess <- ZIO.from(Guess.make(input)) <> (Console.printLine("Invalid input. Please try again...") <*> getGuess)
    } yield guess

  lazy val getName: IO[IOException, Name] =
    for {
      input <- getUserInput("What's your name? ")
      name  <- ZIO.from(Name.make(input)) <> (Console.printLine("Invalid input. Please try again...") <*> getName)
    } yield name

  val chooseWord: ZIO[WordRepository, NotFoundError, Word] = {
    for {
      repo   <- ZIO.service[WordRepository]
      index  <- Random.nextLongBounded(repo.count)
      wordId = WordId.make(index)
      word   <- repo.get(wordId)
    } yield word
  }

  def gameLoop(stateId: StateId): ZIO[StateRepository, Throwable, Unit] =
    for {
      stateRepo   <- ZIO.service[StateRepository]
      stateWriter = stateRepo.write(stateId)(_)
      oldState    <- stateRepo.read(stateId).mapError(_.asThrowable)

      guess                    <- renderState(oldState) <*> getGuess
      newState: State          = oldState.addGuess(guess)
      guessResult: GuessResult = analyzeNewGuess(oldState, newState, guess)

      _ <- guessResult match {
            case GuessResult.Won =>
              Console.printLine(s"Congratulations ${newState.name.name}! You won!") <*>
                renderState(newState)
            case GuessResult.Lost =>
              Console.printLine(s"Sorry ${newState.name.name}! You Lost! Word was: ${newState.word.word}") <*>
                renderState(newState)
            case GuessResult.Correct =>
              for {
                _ <- stateWriter(newState).mapError(_.asThrowable)
                _ <- Console.printLine(s"Good guess, ${newState.name.name}!")
                _ <- gameLoop(stateId)
              } yield ()
            case GuessResult.Incorrect =>
              for {
                _ <- stateWriter(newState).mapError(_.asThrowable)
                _ <- Console.printLine(s"Bad guess, ${newState.name.name}!")
                _ <- gameLoop(stateId)
              } yield ()
            case GuessResult.Unchanged =>
              Console.printLine(s"${newState.name.name}, You've already tried that letter!") <*>
                gameLoop(stateId)
          }
    } yield ()

  def analyzeNewGuess(oldState: State, newState: State, guess: Guess): GuessResult =
    if (oldState.guesses.contains(guess)) GuessResult.Unchanged
    else if (newState.playerWon) GuessResult.Won
    else if (newState.playerLost) GuessResult.Lost
    else if (oldState.word.contains(guess.char)) GuessResult.Correct
    else GuessResult.Incorrect

  def renderState(state: State): IO[IOException, Unit] = {

    /*
        --------
        |      |
        |      0
        |     \|/
        |      |
        |     / \
        -

        f     n  c  t  o
        -  -  -  -  -  -  -
        Guesses: a, z, y, x
     */
    val hangman: UIO[String] = ZIO.attempt(hangmanStages(state.failuresCount)).orDie
    val word: String =
      state.word.toList
        .map(c => if (state.guesses.map(_.char).contains(c)) s" $c " else "   ")
        .mkString

    val line    = List.fill(state.word.length)(" - ").mkString
    val guesses = s" Guesses: ${state.guesses.map(_.char).mkString(", ")}"

    hangman.flatMap { hangman =>
      Console.printLine {
        s"""
         #$hangman
         #
         #$word
         #$line
         #
         #$guesses
         #
         #""".stripMargin('#')
      }
    }
  }

  private type Env = WordRepository with StateRepository

  private val program: ZIO[Env, Throwable, Unit] =
    for {
      name <- Console.printLine("Welcome to ZIO Hangman!") <*> getName
      word <- chooseWord.mapError(_.asThrowable)

      initialState = State.initial(name, word)
      stateRepo    <- ZIO.service[StateRepository]
      stateId      <- stateRepo.create(initialState).mapError(_.asThrowable)
      _            <- Console.printLine(s"Your Id: ${stateId.value}")

      _ <- gameLoop(stateId)
    } yield ()

  override val run = program.provide(
    Layers.wordRepositoryLayer,
    Layers.stareRepositoryLayer
  )
}
