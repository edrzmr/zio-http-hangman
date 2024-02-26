package com.example.repository
import com.example.extension.model._
import com.example.model.error.RepositoryError
import com.example.model.{ Guess, Name, State, Word }
import com.example.model.State.StateId
import zio.{ IO, Layer, UIO, ZIO, ZLayer }

import java.io.{ BufferedReader, BufferedWriter, Closeable, FileReader, FileWriter, InputStreamReader }
import java.nio.file.{ Files, Path, Paths }
import java.util.UUID
import scala.io.{ BufferedSource, Source }

class FileStateRepository private (storagePath: Path) extends StateRepository {

  import FileStateRepository._

  override def create(initial: State): IO[RepositoryError, StateId] =
    for {
      stateId  <- StateId.make(UUID.randomUUID.toString).succeed
      filePath <- resolvePath(storagePath, stateId)
      exists   <- existsPath(filePath)
      _ <- {
        if (exists) RepositoryError(s"State: '$initial' already exist").fail
        else attempt(Files.createFile(filePath))(t => RepositoryError(t, s"Fail to create file: '$filePath'"))
      }

      _ <- write(stateId)(initial)

    } yield stateId

  override def write(stateId: StateId)(state: State): IO[RepositoryError, Unit] = {

    def writeLine(bufferedWriter: BufferedWriter): IO[RepositoryError, Unit] =
      for {
        line <- format(state).succeed
        _ <- attempt(bufferedWriter.append(line).flush()) { t =>
              RepositoryError(
                t,
                s"Fail to append state: '$state', storage path: '$storagePath', state id '$stateId'"
              )
            }
      } yield ()

    for {
      filePath <- resolvePath(storagePath, stateId)
      exists   <- existsPath(filePath)
      isRegularFile <- attempt(Files.isRegularFile(filePath)) { t =>
                        RepositoryError(t, s"Fail to verify if path is regular file")
                      }
      isWritable <- attempt(Files.isWritable(filePath)) { t =>
                     RepositoryError(s"Fail to verify if file is writable: '${storagePath.toString}'", t)
                   }

      validatedPath <- (exists, isRegularFile, isWritable) match {
                        case (true, true, true)  => filePath.succeed
                        case (false, _, _)       => RepositoryError(s"File '$filePath' not found").fail
                        case (true, false, _)    => RepositoryError(s"Path '$filePath' must be a regular file").fail
                        case (true, true, false) => RepositoryError(s"File '$filePath' must be writable").fail
                      }

      _ <- ZIO.acquireReleaseWith(openFileToAppend(validatedPath))(close)(writeLine)

    } yield ()

  }

  override def read(stateId: StateId): IO[RepositoryError, State] = {

    def readLastLine(bufferedSource: BufferedSource): IO[RepositoryError, String] =
      for {
        lines <- bufferedSource.getLines().toArray.succeed
        lastLine <- lines.lastOption
                     .map(_.succeed)
                     .getOrElse(RepositoryError(s"State not found with stateId: '$stateId").fail)
      } yield lastLine

    for {
      filePath <- resolvePath(storagePath, stateId)
      exists   <- existsPath(filePath)
      isRegularFile <- attempt(Files.isRegularFile(filePath)) { t =>
                        RepositoryError(t, s"Fail to verify if path is regular file")
                      }
      isReadable <- attempt(Files.isReadable(filePath)) { t =>
                     RepositoryError(s"Fail to verify if file is readable: '${storagePath.toString}'", t)
                   }

      validatedPath <- (exists, isRegularFile, isReadable) match {
                        case (true, true, true)  => filePath.succeed
                        case (false, _, _)       => RepositoryError(s"File '$filePath' not found").fail
                        case (true, false, _)    => RepositoryError(s"Path '$filePath' must be a regular file").fail
                        case (true, true, false) => RepositoryError(s"File '$filePath' must be readable").fail
                      }

      lastLine <- ZIO.acquireReleaseWith(openFileToRead(validatedPath))(close)(readLastLine)
      state    <- parse(lastLine)

    } yield state
  }
}

object FileStateRepository {

  val DEFAULT_STATE_PATH: String = "/tmp/zio-http-hangman"

  def live(storagePath: String): Layer[RepositoryError, FileStateRepository] = ZLayer {

    for {
      path   <- attempt(Paths.get(storagePath))(t => RepositoryError(s"Fail to get Path from '$storagePath'", t))
      exists <- attempt(Files.exists(path))(t => RepositoryError(s"Fail to verify if path exists: '$storagePath'", t))
      isDirectory <- attempt(Files.isDirectory(path)) { t =>
                      RepositoryError(s"Fail to verify if path is directory: '$storagePath'", t)
                    }
      isWritable <- attempt(Files.isWritable(path)) { t =>
                     RepositoryError(s"Fail to verify if directory if writable: '$storagePath'", t)
                   }

      validatedPath <- (exists, isDirectory, isWritable) match {
                        case (true, false, _) =>
                          RepositoryError(s"Path '$storagePath' must be a writable directory").fail
                        case (true, true, false) =>
                          RepositoryError(s"Directory '$storagePath' must be writable").fail
                        case (false, _, _) =>
                          attempt(Files.createDirectories(path)) { t =>
                            RepositoryError(s"Fail to create directory: $storagePath", t)
                          }
                        case (true, true, true) =>
                          path.succeed
                      }

    } yield new FileStateRepository(validatedPath)

  }

  private def format(state: State): String =
    s"name=${state.name.name}," +
      s"guesses=${state.guesses.map(_.char).mkString}," +
      s"word=${state.word.word}\n"

  private def parse(str: String): IO[RepositoryError, State] =
    for {
      entries <- {
        val entries = str
          .split(",")
          .map(_.trim)
          .toList
          .map(_.split("=").map(_.trim).toList)

        if (entries.length == 3) entries.succeed
        else RepositoryError(s"Could not retrieve state from: '$str'").fail
      }

      table <- {
        for {
          pairs <- entries.map {
                    case key :: value :: Nil            => Right(key, value)
                    case key :: Nil if key == "guesses" => Right(key, "")
                    case xs                             => Left(s"No key/value detected on: '${xs.mkString}'")
                  }.succeed
          (lefts, rights) = pairs.partition(_.isLeft)
          table <- {
            if (lefts.nonEmpty) {
              val messages = lefts.collect { case Left(message) => message }
              RepositoryError(messages.mkString(", ")).fail
            } else {
              val pairs = rights.collect { case Right(pair) => pair }
              pairs.toMap.succeed
            }

          }
        } yield table
      }

      state <- {
        for {
          nameRaw <- table.get("name").map(_.succeed).getOrElse(RepositoryError(s"Key 'name' not found").fail)
          name    <- Name.make(nameRaw).map(_.succeed).getOrElse(RepositoryError(s"Invalid name: '$nameRaw'").fail)

          guessesRaw                       <- table.get("guesses").map(_.succeed).getOrElse(RepositoryError(s"Key 'guesses' not found").fail)
          maybeGuesses: Set[Option[Guess]] = guessesRaw.toSet.map((char: Char) => Guess.make(char.toString))
          guesses <- {
            val guesses: Set[Guess] = maybeGuesses.flatMap(_.toSet)
            if (maybeGuesses.size == guesses.size) guesses.succeed
            else RepositoryError(s"Invalid guesses: '$guessesRaw'").fail
          }

          wordRaw <- table.get("word").map(_.succeed).getOrElse(RepositoryError(s"Key 'word' not found").fail)
          word    <- Word.make(wordRaw).map(_.succeed).getOrElse(RepositoryError(s"Invalid word: '$wordRaw'").fail)

        } yield State.make(name, guesses, word)
      }

    } yield state

  private def openFileToAppend(filePath: Path): IO[RepositoryError, BufferedWriter] =
    attempt(new BufferedWriter(new FileWriter(filePath.toFile, true))) { t =>
      RepositoryError(t, s"Fail into open to append file: '$filePath")
    }

  private def openFileToRead(filePath: Path): IO[RepositoryError, BufferedSource] =
    attempt(Source.fromFile(filePath.toFile))(t => RepositoryError(t, s"Fail into open to read file: '$filePath"))

  private def close(closeable: Closeable): UIO[Unit] = closeable.close().succeed

  private def existsPath(filePath: Path): IO[RepositoryError, Boolean] =
    attempt(Files.exists(filePath))(t => RepositoryError(t, s"Fail to verify if path exists: '$filePath'"))

  private def resolvePath(basePath: Path, stateId: StateId): IO[RepositoryError, Path] =
    attempt(basePath.resolve(stateId.value)) { t =>
      RepositoryError(t, s"Fail to resolve path '$basePath' and '$stateId'")
    }
}
