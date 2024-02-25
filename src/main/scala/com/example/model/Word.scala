package com.example.model

sealed abstract case class Word private (word: String) {
  def contains(char: Char): Boolean = word.contains(char)
  val length: Int                   = word.length
  def toList: List[Char]            = word.toList
  def toSet: Set[Char]              = word.toSet
}

object Word {
  def make(word: String): Option[Word] =
    if (word.nonEmpty && word.forall(_.isLetter)) Some(new Word(word.toLowerCase) {})
    else None

  final case class WordId private (value: Long) extends AnyVal
  object WordId {
    def make(id: Long): WordId = WordId(id)
  }
}
