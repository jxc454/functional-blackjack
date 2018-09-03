package com.blackjack

case class Player(
  bet: Option[Double] = None,
  hand: BjHand = null,
  balance: Double = 0.0)
