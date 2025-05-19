package prob


// TODO can be much faster
class Dist [A](val dist: Map[A, Double]) {
  val impl: List[(Double, A)] = {

    // least likely things near 0.0 for better floating point resolutoin
    val relative = dist.toList.sortBy(_._2)
    var temp = 0.0

    var out = List[(Double, A)]()

    for (i <- Range(0, dist.size)) {
      val (a, d) = relative(i)
      out ++= List((d + temp, a))
      temp += d

    }
    out
  }

  def sample(d: Double): A = {

    for ((s, a) <- impl) {
      if (d < s) {
        return a
      }
    }
    return impl.last._2
  }
}
