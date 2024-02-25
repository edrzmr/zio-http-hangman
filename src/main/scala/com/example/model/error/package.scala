package com.example.model

package object error {

  sealed trait DomainError extends Product with Serializable {
    def message: String
  }

  final case class NotFoundError(message: String) extends DomainError

}
