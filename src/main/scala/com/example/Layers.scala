package com.example

import com.example.model.error.RepositoryError
import com.example.repository.{ FileStateRepository, InMemoryWordRepository, WordRepository }
import zio._

object Layers {

  val wordRepositoryLayer: Layer[RepositoryError, InMemoryWordRepository] =
    InMemoryWordRepository.live(InMemoryWordRepository.DEFAULT_RESOURCE_FILENAME)

  val stareRepositoryLayer: Layer[RepositoryError, FileStateRepository] =
    FileStateRepository.live(FileStateRepository.DEFAULT_STATE_PATH)
}
