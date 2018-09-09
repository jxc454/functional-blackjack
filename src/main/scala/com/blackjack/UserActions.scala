package com.blackjack

object UserActions {
  val actions: Seq[String] = Seq("h", "s", "d", "r", "p")

  val actionsMap: Map[String, PlayerMove] = Map(
    "h" -> Hit,
    "s" -> Stay,
    "d" -> DoubleDown,
    "r" -> Surrender,
    "p" -> Split
  )
}