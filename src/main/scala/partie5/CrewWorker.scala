package partie5

class CrewWorker(val id: BigInt, val name: String, val job: String) {
  override def equals(that: Any): Boolean = {
    that match {
      case that: CrewWorker => that.id == id
      case _ => false
    }
  }

  override def hashCode(): Int = id.toInt

  override def toString = f"Name: $name, Id: $id"
}
