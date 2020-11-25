package partie5

import org.json4s.DefaultFormats
import org.json4s.native.JsonMethods.parse

import scala.io.Source


class DirectorSearch(cache: TMDBCache) extends Searcher(cache) {
  def search(movie: Movie): Some[CrewWorker] = {
    if (cache.containsDirector(movie)) {
      return cache.getDirector(movie)
    }
    val rawMovies = Source.fromURL(s"https://api.themoviedb.org/3/movie/${movie.id}/credits?api_key=$key")
    val data = rawMovies.mkString
    val director = DirectorSearch.parseDirector(data)
    cache.saveDirector(director, movie)
    director
  }
}

object DirectorSearch {
  implicit val formats: DefaultFormats.type = DefaultFormats

  def parseDirector(data: String): Some[CrewWorker] = {
    val crew: List[CrewWorker] = (parse(data) \\ "crew").extract[List[CrewWorker]]
    for (crewWorker <- crew) {
      if (crewWorker.job == "Director") {
        return Some(crewWorker)
      }
    }
    null
  }
}
