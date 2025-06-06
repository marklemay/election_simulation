package prob


object Dist {
  def Exactly[A](a: A): Dist[A] = Dist(Map(a -> 1))

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


  // B needs decidable Eq
  def flatMap[B](f: A => Dist[B]): Dist[B] = {
    Dist(
      dist.toSeq.flatMap((a,p) =>f(a).dist.toSeq.map((b,q) => (b,q*p))).groupBy(_._1).map((c, s) => (c, s.map(_._2).sum)).toMap
    )
  }

  // B needs decidable Eq
  //override
  def map[B](f: A => B): Dist[B] = {
    Dist(dist.toSeq.map((a,p) => (f(a), p)).groupBy(_._1).map((c, s) => (c, s.map(_._2).sum)).toMap)
  }



//  @inline final override def foreach[U](f: A => U): Unit = {
//    var these = this
//    while (!these.isEmpty) {
//      f(these.head)
//      these = these.tail
//    }
  
}
