package partie5

import org.json4s.DefaultFormats
import org.json4s.native.JsonMethods.parse

import scala.io.Source

class ActorSearch (cache: TMDBCache) extends Searcher(cache) {
  def search(name: String, surname: String): Some[Actor] = {
    if (cache.containsActor(name, surname)) {
      return cache.getActor(name, surname)
    }
    val searchName = name + "%20" + surname
    val rawActor = Source.fromURL(s"https://api.themoviedb.org/3/search/person?api_key=$key&language=en-US&query=$searchName&page=1&include_adult=false")
    val actorData = rawActor.mkString
    val actor = ActorSearch.parseActor(actorData, name + " " + surname)
    cache.saveActor(actorData, name, surname, actor)
    actor
  }
}

object ActorSearch {
  implicit val formats: DefaultFormats.type = DefaultFormats

  def parseActor(data: String, name: String): Some[Actor] = {
    val actors:List[Actor] = (parse(data) \\ "results").extract[List[Actor]]
    for (actor <- actors) {
      if (actor.name.toLowerCase == name.toLowerCase) {
        return Some(actor)
      }
    }
    Some(null)
  }
}