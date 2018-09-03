package com.blackjack

object Shoe {
  def dealMultipleHands(shoe: Shoe, n: Int): (Option[Seq[Seq[BjCard]]], Shoe) = {
    val (hands: Seq[Option[Seq[BjCard]]], newShoe: Shoe) = State.sequence(Seq.fill(n)(shoe.dealCards(2))).run(shoe)

    // convert Seq[Option[Seq]] to Option[Seq[Seq]]
    val invertedHands: Option[Seq[Seq[BjCard]]] = hands.foldLeft(Some(Nil): Option[Seq[Seq[BjCard]]])((acc, hand) => hand match {
      case Some(cardsOpt) => acc.map(cardsOpt +: _)
      case None => None
    })

    (invertedHands, newShoe)
  }
}

class Shoe(cardsIn: Seq[BjCard])(shuffle: Boolean=false) {

  val cards: Seq[BjCard] = if (shuffle) shuffleCards(cardsIn) else cardsIn

  val size: Int = cards.size

  def dealCards(n: Int): State[Shoe, Option[Seq[BjCard]]] = {
    State(shoe => {
      val (cardsOp: Option[Seq[BjCard]], newShoe: Shoe) = deal(n).run(shoe)

      (cardsOp, newShoe)

    })
  }

  def deal(n: Int): State[Shoe, Option[Seq[BjCard]]] = {
    if (size >= n) {
      State(shoe => {
        val (handCards, shoeCards) = shoe.cards.splitAt(n)
        (Some(handCards), new Shoe(shoeCards)(false))
      })
    } else {
      State(shoe => {
        (None, shoe)
      })
    }
  }

  def shuffleCards(cards: Seq[BjCard]): Seq[BjCard] = {
    scala.util.Random.shuffle(cards)
  }
}
