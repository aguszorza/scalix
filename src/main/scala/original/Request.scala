package original

import java.io.PrintWriter
import java.nio.file.{Files, Paths}

import org.json4s.DefaultFormats
import org.json4s.native.JsonMethods.parse

import scala.collection.immutable
import scala.io.Source

object Request extends App {
  val key = "1a059ea9a7af7fba1577e326ae77a692"
  val dir = "data"
  implicit val formats: DefaultFormats.type = DefaultFormats

  var cacheActorId = Map[(String, String), BigInt]()
  var cacheMovieId = Map[BigInt, Set[(BigInt, String)]]()
  var cacheDirectorId = Map[BigInt, (BigInt, String)]()
  var cacheRequest = Map[(String, String), Set[(String, String)]]()

  def get_actor_information(name: String, surname: String) = {
    val searchName = name + "%20" + surname
    if (Files.exists(Paths.get(s"$dir/actor${name.toLowerCase}_${surname.toLowerCase}.txt"))) {
      val data = Source.fromFile(s"$dir/actor${name.toLowerCase}_${surname.toLowerCase}.txt")
      data.mkString
    } else {
      val data = Source.fromURL(s"https://api.themoviedb.org/3/search/person?api_key=$key&language=en-US&query=$searchName&page=1&include_adult=false")
      val jsonString = data.mkString
      val actorName = s"${name}_$surname".toLowerCase
      val out = new PrintWriter(s"$dir/actor$actorName.txt")
      out.print(jsonString)
      out.close()
      jsonString
    }
  }

  def get_movie_information(actorId: BigInt) = {
    if (Files.exists(Paths.get(s"$dir/movie$actorId.txt"))) {
      val data = Source.fromFile(s"$dir/movie$actorId.txt")
      data.mkString
    } else {
      val data = Source.fromURL(s"https://api.themoviedb.org/3/person/$actorId/movie_credits?api_key=$key&language=en-US")
      val jsonString = data.mkString
      val out = new PrintWriter(s"$dir/movie$actorId.txt")
      out.print(jsonString)
      out.close()
      jsonString
    }
  }

  def findActorId(name: String, surname: String): Option[BigInt] = {
    val actorFullName = name + " " + surname
    if (cacheActorId.contains((name, surname))) {
      return cacheActorId.get((name, surname))
    }
    val jsonString = get_actor_information(name, surname)
    (parse(jsonString) \\ "results").values.asInstanceOf[List[immutable.HashMap[String, Any]]].foreach(map => {
      if (map.getOrElse("name", "").asInstanceOf[String].toLowerCase == actorFullName.toLowerCase) {
        cacheActorId += ((name, surname) -> map.getOrElse("id", 0).asInstanceOf[BigInt])
        return map.get("id").asInstanceOf[Some[BigInt]]
      }
    })
    null
  }

  def findActorMovies(actorId: BigInt): Set[(BigInt, String)] = {
    if (cacheMovieId.contains(actorId)) {
      return cacheMovieId(actorId)
    }
    val movies = get_movie_information(actorId) //Source.fromURL(s"https://api.themoviedb.org/3/person/$actorId/movie_credits?api_key=$key&language=en-US");
    val result = (parse(movies.mkString) \\ "cast").values.asInstanceOf[List[immutable.HashMap[String, Any]]]
      .toSet[immutable.HashMap[String, Any]].map(map => {
      (map.getOrElse("id", 0).asInstanceOf[BigInt], map.getOrElse("title", "").asInstanceOf[String])
    })
    cacheMovieId += (actorId -> result)
    result
  }

  def findMovieDirector(movieId: BigInt): Option[(BigInt, String)] = {
    if (cacheDirectorId.contains(movieId)) {
      return Option(cacheDirectorId(movieId))
    }
    val movieCredits = Source.fromURL(s"https://api.themoviedb.org/3/movie/$movieId/credits?api_key=$key")
//    val crew = (parse(movieCredits.mkString) \\ "crew").extract[List[CrewWorker]]
    (parse(movieCredits.mkString) \\ "crew").values.asInstanceOf[List[immutable.HashMap[String, Any]]].foreach(map => {
      if (map.getOrElse("job", "").asInstanceOf[String] == "Director") {
        cacheDirectorId += (movieId -> (map.getOrElse("id", 0).asInstanceOf[BigInt], map.getOrElse("name", "").asInstanceOf[String]))
        return cacheDirectorId.get(movieId)
      }
    })
    null
  }

  def request(actor1: String, actor2: String): Set[(String, String)] = {
    if (cacheRequest.contains((actor1, actor2))) {
      return cacheRequest((actor1, actor2))
    }
    val firstActorName = actor1.split(" ")
    val secondActorName = actor2.split(" ")
    val firstActorMovies = findActorMovies(findActorId(firstActorName(0), firstActorName(1)).get)
    val secondActorMovies = findActorMovies(findActorId(secondActorName(0), secondActorName(1)).get)
    val commonMovies = firstActorMovies.intersect(secondActorMovies)
    val result = commonMovies.map((movie: (BigInt, String)) => {
      val director = findMovieDirector(movie._1)
      (director.get._2, movie._2)
    })
    cacheRequest += ((actor1, actor2) -> result)
    result
  }

  def frequentDuo(): (String, String) = {
    // This operation can be parallelized as the order in which the operation is applied
    // to the elements does not matter to the final solution. The two pairs of values can
    // be in any order and the final result would be the same
    val x = cacheRequest.reduce((a,b) => {
      if (a._2.size > b._2.size)
        a
      else
        b
    })
    x._1
  }

  def frequentDuoWithFor(): (String, String) = {
    // This operation can not be parallelized as it may have race conditions when different threads
    // try to update the variables max_size and duo
    var max_size = 0
    var duo: (String, String) = ("", "")
    cacheRequest.foreachEntry((key: (String, String) , value: Set[(String, String)]) => {
      if (value.size > max_size) {
        max_size = value.size
        duo = key
      }
    })
    duo
  }

  val michael: Option[BigInt] = findActorId("michael", "caine")
  println(michael)
  val michaelMovies: Set[(BigInt, String)] = findActorMovies(michael.get)
  println(michaelMovies)
  val director = findMovieDirector(michaelMovies.head._1)
  println(director)
  val commonMovies = request("michael caine", "christian bale")
  println(commonMovies)
  println(commonMovies.size)
  val commonMovies2 = request("meryl streep", "nicole kidman")
  println(commonMovies2)
  println(commonMovies2.size)
  println(request("michael caine", "nicole kidman").size)
  println(request("meryl streep", "michael caine").size)
  println(request("meryl streep", "christian bale").size)
  println(request("christian bale", "nicole kidman").size)

  println(frequentDuo())
  println(frequentDuoWithFor())

}
