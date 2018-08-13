package com.blackjack

object Shoe {
  def dealMultipleHands(shoe: Shoe, n: Int): (Option[Seq[BjHand]], Shoe) = {
    val (hands, newShoe) = State.sequence(Seq.fill(n)(shoe.dealHand(2))).run(shoe)

    // convert Seq[Option] to Option[Seq]
    val invertedHands: Option[Seq[BjHand]] = hands.foldLeft(Some(Nil): Option[Seq[BjHand]])((acc, hand) => hand match {
      case Some(handOpt) => acc.map(handOpt +: _)
      case None => None
    })

    (invertedHands, newShoe)
  }
}

class Shoe(cardsIn: Seq[BjCard])(shuffle: Boolean=false) {

  val cards: Seq[BjCard] = if (shuffle) shuffleCards(cardsIn) else cardsIn

  val size: Int = cards.size

  def dealHand(n: Int): State[Shoe, Option[BjHand]] = {
    State(shoe => {
      val (cardsOp: Option[Seq[BjCard]], newShoe: Shoe) = deal(n).run(shoe)

      (cardsOp.map(cards => new BjHand(cards)), newShoe)

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
