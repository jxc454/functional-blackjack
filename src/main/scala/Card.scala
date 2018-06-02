package card

sealed trait Card {
  val pip: Int
  val pipName: String
  val group: String
}

object BjCard {
  private val validPipNames = List("2", "3", "4", "5", "6", "7", "8", "9", "T", "J", "Q", "K", "A")
  private val validGroups = List("h", "s", "d", "c")

  def createShoe(decks: Int): List[BjCard] = {
    validPipNames.foldLeft(Nil:List[BjCard])((l: List[BjCard], p: String) => l ++
      validGroups.foldLeft(Nil:List[BjCard])((l2: List[BjCard], g: String) =>
        List.fill(decks)(new BjCard(p, g)) ++ l2))
  }
}

class BjCard(val pipName: String, val group: String) extends Card {

  if (!BjCard.validGroups.contains(group)) {throw new IllegalArgumentException}
  if (!BjCard.validPipNames.contains(pipName)) {throw new IllegalArgumentException}

  private def getPipValue(pipValue: String): Int = pipValue match {
    case "T" | "J" | "Q" | "K" => 10
    case "A" => 11
    case x => x.toInt
  }

  def to_string: String = this.pipName.concat(this.group)

  val pip: Int = getPipValue(pipName)
}




