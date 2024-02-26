package com.example.model

package object error {

  sealed trait DomainError extends Product with Serializable {
    def message: String
    def throwable: Option[Throwable]

    def asThrowable: Throwable = throwable match {
      case None    => new Throwable(message)
      case Some(t) => new Throwable(message, t)
    }
  }

  final case class NotFoundError private (message: String, throwable: Option[Throwable]) extends DomainError
  object NotFoundError {
    def apply(message: String): NotFoundError                       = NotFoundError(message, None)
    def apply(message: String, throwable: Throwable): NotFoundError = NotFoundError(message, Some(throwable))
  }

  final case class RepositoryError private (message: String, throwable: Option[Throwable]) extends DomainError
  object RepositoryError {
    def apply(message: String): RepositoryError                   = RepositoryError(message, None)
    def apply(message: String, cause: Throwable): RepositoryError = RepositoryError(message, Some(cause))
    def apply(cause: Throwable, message: String): RepositoryError = RepositoryError(message, Some(cause))
  }

}
