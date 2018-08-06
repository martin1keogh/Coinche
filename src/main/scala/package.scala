package mk

import cats.data.EitherT
import cats.effect.IO

package object coinche {
  type UserResponse[A] = EitherT[IO, ui.UserInputError, A]
}
