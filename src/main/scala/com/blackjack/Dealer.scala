package com.blackjack

case class Dealer(
                 rules: DealerRules.type = DealerRules,
                 shoe: Shoe = BjCard.createShoe(1),
                 hand: BjHand = null,
                 balance: Double = 0.0)
