package com.blackjack

case class Dealer(
                 rules: DealerRules.type = DealerRules,
                 shoe: Shoe = BjCard.createShoe(1),
                 hand: Option[Hand[BjCard]] = None,
                 balance: Double = 0.0)
