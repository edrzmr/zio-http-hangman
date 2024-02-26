package com.example.repository

import com.example.model.State
import com.example.model.State.StateId
import com.example.model.error.RepositoryError
import zio.IO

trait StateRepository {

  def create(initial: State): IO[RepositoryError, StateId]

  def write(stateId: StateId)(state: State): IO[RepositoryError, Unit]

  def read(stateId: StateId): IO[RepositoryError, State]

}
