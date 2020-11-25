package partie5

import org.json4s.DefaultFormats
import org.json4s.native.JsonMethods.parse

import scala.io.Source

class MovieSearch(cache: TMDBCache) extends Searcher(cache) {
  def search(actor: Actor): Set[Movie] = {
    if (cache.containsMovie(actor)) {
      return cache.getMovie(actor)
    }
    val rawMovies = Source.fromURL(s"https://api.themoviedb.org/3/person/${actor.id}]/movie_credits?api_key=$key&language=en-US")
    val movieData = rawMovies.mkString
    val movies = MovieSearch.parseMovie(movieData)
    cache.saveMovie(movieData, actor, movies)
    movies
  }

  def getCommonMovies(actor1: Actor, actor2: Actor): Set[Movie] = {
    val movies1 = search(actor1)
    val movies2 = search(actor2)
    movies1.intersect(movies2)
  }
}

object MovieSearch {
  implicit val formats: DefaultFormats.type = DefaultFormats

  def parseMovie(data: String): Set[Movie] = {
    (parse(data) \\ "cast").extract[Set[Movie]]
  }
}
