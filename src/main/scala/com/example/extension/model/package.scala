package com.example.extension

import com.example.model.State
import com.example.model.error.DomainError
import zio.{IO, UIO, ZIO}

package object model {

  implicit class DomainErrorOps[T <: DomainError](val value: T) extends AnyVal {
    def fail: IO[T, Nothing] = ZIO.fail(value)
  }

  implicit class Ops[T](val value: T) extends AnyVal {
    def succeed: UIO[T] = ZIO.succeed(value)
  }

  def attempt[E <: DomainError, C](code: => C)(f: Throwable => E): IO[E, C] = ZIO.attempt(code).mapError(f)

  implicit class StateOps(val state: State) extends AnyVal {
    def withName(name: String): State = state.copy()
  }

}
