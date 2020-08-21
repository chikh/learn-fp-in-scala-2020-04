package state.examples

import state.State
import state.State._

sealed trait Input
case object Insert extends Input
case object Turn extends Input

case class Candies(value: Int) extends AnyVal
case class Coins(value: Int) extends AnyVal
case class Locked(value: Boolean) extends AnyVal

case class Machine(locked: Locked, candies: Candies, coins: Coins)

object Machine {
  def simulate(inputs: List[Input]): State[Machine, (Candies, Coins)] = {
    for {
      _ <- sequence(inputs.map(modify[Machine] _ compose update))
      s <- get
    } yield s match {
      case Machine(_, candies, coins) => (candies, coins)
    }
  }

  def update: Input => Machine => Machine = input => machine =>
    (input, machine) match {
    case (Insert, m @ Machine(Locked(true), Candies(candies), _)) if candies > 0 =>
      m.copy(locked = Locked(false))
    case (Turn, m @ Machine(Locked(false), Candies(candies), Coins(coins))) =>
      m.copy(locked = Locked(true), candies = Candies(candies - 1), coins = Coins(coins + 1))
    case _ => machine
  }
}
