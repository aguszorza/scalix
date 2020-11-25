package partie5

class Actor(val id: BigInt, val name: String) {
  override def equals(that: Any): Boolean = {
    that match {
      case that: Actor => that.id == id
      case _ => false
    }
  }

  override def hashCode(): Int = id.toInt

  override def toString = f"Name: $name, Id: $id"
}
