package com.example.repository

import com.example.model.Word
import com.example.model.Word.WordId
import com.example.model.error.NotFoundError
import zio.IO

trait WordRepository {

  def get(id: WordId): IO[NotFoundError, Word]

  def count: Long
}
