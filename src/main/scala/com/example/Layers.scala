package com.example

import com.example.repository.{InMemoryWordRepository, WordRepository}
import zio._

object Layers {

  val wordRepositoryLayer: ULayer[WordRepository] = InMemoryWordRepository.live(InMemoryWordRepository.DEFAULT_RESOURCE_FILENAME)
}
