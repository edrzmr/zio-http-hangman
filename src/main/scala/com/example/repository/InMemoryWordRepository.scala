package com.example.repository
import com.example.extension.model._
import com.example.model.Word
import com.example.model.Word.WordId
import com.example.model.error.{ NotFoundError, RepositoryError }

import scala.io.{ BufferedSource, Source }
import zio._

class InMemoryWordRepository private (words: List[String]) extends WordRepository {

  override def get(id: WordId): IO[NotFoundError, Word] = {
    val maybeWord = for {
      str  <- words.lift(id.value.toInt)
      word <- Word.make(str)
    } yield word

    maybeWord
      .map(word => word.succeed)
      .getOrElse(NotFoundError(s"Word with id '${id.value}' not found").fail)
  }

  override def count: Long = words.length
}

object InMemoryWordRepository {

  val DEFAULT_RESOURCE_FILENAME: String = "words.txt"

  def live(resourceFilename: String): Layer[RepositoryError, InMemoryWordRepository] =
    ZLayer {

      def acquire(resourceName: String): IO[RepositoryError, BufferedSource] =
        attempt(Source.fromResource(resourceName)) { t =>
          RepositoryError(s"Fail into acquire resource: '$resourceName'", t)
        }

      def readLines(bufferedSource: BufferedSource): IO[RepositoryError, List[String]] =
        attempt(bufferedSource.getLines.toList) { t =>
          RepositoryError(s"Fail into read lines from: $resourceFilename", t)
        }

      def release(bufferedSource: BufferedSource): UIO[Unit] = bufferedSource.close.succeed

      ZIO
        .acquireReleaseWith(acquire(resourceFilename))(release)(readLines)
        .map(words => new InMemoryWordRepository(words))

    }
}
