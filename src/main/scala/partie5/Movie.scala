package partie5

class Movie(val id: BigInt, val title: String) {
  override def equals(that: Any): Boolean = {
    that match {
      case that: Movie => that.id == id
      case _ => false
    }
  }

  override def hashCode(): Int = id.toInt

  override def toString = f"Title: $title, Id: $id"
}
