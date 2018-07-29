package com.blackjack

object Shoe {

}

class Shoe(cardsIn: List[Card])(shuffle: Boolean=false) {

  val cards: List[Card] = if (shuffle) shuffleCards(cardsIn) else cardsIn

  // state transition
  def deal(n: Int): State[Shoe, Option[List[Card]]] = {
    if (cards.lengthCompare(n) >= 0) {
      State(shoe => {
        val (handCards, shoeCards) = shoe.cards.splitAt(n)
        (Some(handCards), new Shoe(shoeCards))
      })
    } else {
      State(shoe => {
        (None, shoe)
      })
    }
  }

  def shuffleCards(cards: List[Card]): List[Card] = {
    scala.util.Random.shuffle(cards)
  }
}
