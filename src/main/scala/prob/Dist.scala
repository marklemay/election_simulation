package prob


object Dist {
  def Uniform[A](as: Set[A]): Dist[A] = Dist(as.map(a => (a, 1.0/as.size)).toMap)

  def FromCount[A](as: Map[A, Int]): Dist[A] = {
    val total = as.values.sum.toDouble
    Dist(as.map((a,c) => (a, c.toDouble/total)))
  }

  def Normalize[A](as: Map[A, Double]): Dist[A]  = {
    val total = as.values.sum
    Dist(as.map((a,c) => (a, c/total)))
  }
}


// TODO can be much faster
class Dist [A](val dist: Map[A, Double]) {

  override def toString:String = dist.mkString(",")

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
  
  def apply(a:A): Double = dist.getOrElse(a,0.0)
  
  
  
}
