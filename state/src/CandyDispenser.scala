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
    val modifiers: List[State[Machine, ()]] = inputs.map {
      case Insert => modify(unlockIfCandies)
      case Turn => modify(dispenseIfUnlocked)
    }

    sequence(modifiers).flatMap(_ => get).map {
      case Machine(_, candies, coins) => (candies, coins)
    }
  }

  def unlockIfCandies: Machine => Machine = machine =>
    if (machine.candies.value > 0) machine.copy(locked = Locked(false))
    else machine

  def dispenseIfUnlocked: Machine => Machine = machine =>
    if (!machine.locked.value) machine.copy(candies = Candies(machine.candies.value - 1), coins = Coins(machine.coins.value + 1), locked = Locked(true))
    else machine
}
