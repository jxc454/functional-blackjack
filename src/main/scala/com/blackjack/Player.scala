package com.blackjack

case class Player(
  bet: Option[Double] = None,
  hand: Option[Hand[BjCard]] = None,
  balance: Double = 0.0)
