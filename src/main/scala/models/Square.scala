package models

case class Square(x: Char, y: Int, state: State.Value){
//  override def equals(obj: Any): Boolean = obj match{
//      case obj: models.Square => obj.x == this.x && obj.y == this.y
//      case _ => false
//    }
}

object State extends Enumeration {
  val WATER, HIT, SHOT, OCCUPIED, SINK = Value
}