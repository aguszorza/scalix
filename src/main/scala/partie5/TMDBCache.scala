package partie5

import java.io.PrintWriter
import java.nio.file.{Files, Paths}

import scala.io.Source


class TMDBCache {
  val dir = "data"
  var cacheActor: Map[(String, String), Some[Actor]] = Map[(String, String), Some[Actor]]()
  var cacheMovie:  Map[BigInt, Set[Movie]] = Map[BigInt, Set[Movie]]()
  var cacheDirector:  Map[BigInt, Some[CrewWorker]] = Map[BigInt, Some[CrewWorker]]()
  var cacheRequest: Map[(String, String), Set[(CrewWorker, Movie)]] = Map[(String, String), Set[(CrewWorker, Movie)]]()

  def containsActor(name: String, surname: String): Boolean = {
    cacheActor.contains(name,surname) || Files.exists(Paths.get(s"$dir/actor${name.toLowerCase}_${surname.toLowerCase}.txt"))
  }

  def getActor(name: String, surname: String): Some[Actor] = {
    if (cacheActor.contains(name, surname)) {
      return cacheActor.getOrElse((name, surname), null)
    } else if (Files.exists (Paths.get(s"$dir/actor${name.toLowerCase}_${surname.toLowerCase}.txt"))) {
      val data = Source.fromFile(s"$dir/actor${name.toLowerCase}_${surname.toLowerCase}.txt")
      return ActorSearch.parseActor(data.mkString, name + " " + surname)
    }
    null
  }

  def saveActor(data: String, name: String, surname: String, actor: Some[Actor]): Unit = {
    cacheActor += (name, surname) -> actor
    val actorName = s"${name}_$surname".toLowerCase
    val out = new PrintWriter(s"$dir/actor$actorName.txt")
    out.print(data)
    out.close()
  }

  def containsMovie(actor: Actor): Boolean = {
    cacheMovie.contains(actor.id) || Files.exists(Paths.get(s"$dir/movie${actor.id}.txt"))
  }

  def getMovie(actor: Actor): Set[Movie] = {
    if (cacheMovie.contains(actor.id)) {
      return cacheMovie.getOrElse(actor.id, Set[Movie]())
    } else if (Files.exists(Paths.get(s"$dir/movie${actor.id}.txt"))) {
      val data = Source.fromFile(s"$dir/movie${actor.id}.txt")
      return MovieSearch.parseMovie(data.mkString)
    }
    Set[Movie]()
  }

  def saveMovie(data: String, actor: Actor, movies: Set[Movie]): Unit = {
    cacheMovie += actor.id -> movies
    val out = new PrintWriter(s"$dir/movie${actor.id}.txt")
    out.print(data)
    out.close()
  }

  def containsDirector(movie: Movie): Boolean = {
    cacheDirector.contains(movie.id)
  }

  def getDirector(movie: Movie): Some[CrewWorker] = {
    if (cacheDirector.contains(movie.id)) {
      return cacheDirector.getOrElse(movie.id, null)
    }
    null
  }

  def saveDirector(director: Some[CrewWorker], movie: Movie): Unit = {
    cacheDirector += movie.id -> director
  }

  def getCacheRequest: Map[(String, String), Set[(CrewWorker, Movie)]] = {
    cacheRequest
  }

  def saveRequest(actor1: String, actor2: String, data: Set[(CrewWorker, Movie)]): Unit = {
    cacheRequest += (actor1, actor2) -> data
  }
}
