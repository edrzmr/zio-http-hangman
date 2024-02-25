package com.example.repository
import com.example.model.Word
import com.example.model.Word.WordId
import com.example.model.error.NotFoundError

import scala.io.{ BufferedSource, Source }
import zio._

class InMemoryWordRepository private (words: List[String]) extends WordRepository {

  override def get(id: WordId): IO[NotFoundError, Word] = {
    val maybeWord = for {
      str  <- words.lift(id.value.toInt)
      word <- Word.make(str)
    } yield word

    maybeWord
      .map(ZIO.succeed(_))
      .getOrElse(ZIO.fail(NotFoundError(s"Word with id '${id.value}' not found")))
  }

  override def count: Long = words.length
}

object InMemoryWordRepository {

  val DEFAULT_RESOURCE_FILENAME: String = "words.txt"

  def live(resourceFilename: String): ZLayer[Any, Nothing, InMemoryWordRepository] =
    ZLayer {
      def acquire(resourceName: String): Task[BufferedSource]           = ZIO.attempt(Source.fromResource(resourceName))
      def readLines(bufferedSource: BufferedSource): Task[List[String]] = ZIO.attempt(bufferedSource.getLines.toList)
      def release(bufferedSource: BufferedSource): UIO[Unit]            = ZIO.succeed(bufferedSource.close())

      ZIO
        .acquireReleaseWith(acquire(resourceFilename))(release)(readLines)
        .map(words => new InMemoryWordRepository(words))
        .orDieWith(t => new Error(s"Could not load words from resource file: '$resourceFilename'", t))
    }
}
