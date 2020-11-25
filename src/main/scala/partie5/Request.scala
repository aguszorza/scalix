package partie5

object Request {
  val cache = new TMDBCache
  val actorSearcher = new ActorSearch(cache)
  val movieSearcher = new MovieSearch(cache)
  val directorSearcher = new DirectorSearch(cache)

  def request(actor1: String, actor2: String): Set[(CrewWorker, Movie)] = {

    val firstActorName = actor1.split(" ")
    val secondActorName = actor2.split(" ")
    val firstActorMovies = actorSearcher.search(firstActorName(0), firstActorName(1))//findActorMovies(findActorId().get)
    val secondActorMovies = actorSearcher.search(secondActorName(0), secondActorName(1))//findActorMovies(findActorId().get)
    val commonMovies = movieSearcher.getCommonMovies(firstActorMovies.value, secondActorMovies.value)//firstActorMovies.intersect(secondActorMovies)
    val result = commonMovies.map((movie: Movie) => {
      val director = directorSearcher.search(movie)//findMovieDirector(movie._1)
      (director.value, movie)
    })
    cache.saveRequest(actor1, actor2, result)
    result
  }

  def frequentDuo(): (String, String) = {
    // This operation can be parallelized as the order in which the operation is applied
    // to the elements does not matter to the final solution. The two pairs of values can
    // be in any order and the final result would be the same
    if (cache.getCacheRequest.isEmpty) {
      return null
    }
    val x = cache.getCacheRequest.reduce((a: ((String, String), Set[(CrewWorker, Movie)]), b: ((String, String), Set[(CrewWorker, Movie)])) => {
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
    var duo: (String, String) = null
    cache.getCacheRequest.foreachEntry((key: (String, String) , value: Set[(CrewWorker, Movie)]) => {
      if (value.size > max_size) {
        max_size = value.size
        duo = key
      }
    })
    duo
  }

  def main(args: Array[String]): Unit = {
    // Y a-t-il des avantages a cette nouvelle organisation, des inconvenients ?
    // Les avantages : le code est plus modulaire et propre, les differentes classes sont plus independantes.
    // Cela facilite leur reutilisation  et modification plus tard.
    // Les inconvenients : le code est plus complexe et plus lourd.
    val michael: Some[Actor] = actorSearcher.search("michael", "caine")//Cache.getActor("michael", "caine")
    println(michael)
    val michaelMovies: Set[Movie] = movieSearcher.search(michael.value)//Cache.getMovie(michael.value)
    println(michaelMovies)
    val director = directorSearcher.search(michaelMovies.head)//Cache.getDirector(michaelMovies.head)
    println(director)
    val christian: Some[Actor] = actorSearcher.search("christian", "bale")
    println(christian)
    val commonMovies = movieSearcher.getCommonMovies(michael.value, christian.value)
    println(commonMovies)
    println(commonMovies.size)

    val meryl: Some[Actor] = actorSearcher.search("meryl", "streep")
    val nicole: Some[Actor] = actorSearcher.search("nicole", "kidman")
    val commonMovies2 = movieSearcher.getCommonMovies(meryl.value, nicole.value)
    println(commonMovies2)
    println(commonMovies2.size)

    println(request("michael caine", "christian bale").size)
    println(request("meryl streep", "nicole kidman").size)
    println(request("michael caine", "nicole kidman").size)
    println(request("meryl streep", "michael caine").size)
    println(request("meryl streep", "christian bale").size)
    println(request("christian bale", "nicole kidman").size)



    println(frequentDuo())
    println(frequentDuoWithFor())
  }
}
