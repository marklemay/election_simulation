
import scala.collection.mutable.{ArrayBuffer, ListBuffer}
import scala.util.Random

// This was from before I though through the asomtotics


//TOOD could be made extreamely faster!
// coulc sample form int and precompute efficient bit maskins, or at least a tree
sealed class Sampler[A](dist: Map[A, Double]) {
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


def sampleTest(): Unit = {


  println("???")
  val numVotesr = 3
  val numOptions = 5
  //  val election = Plurality(numVotesr,numOptions)
  val election = InstantRunOff(numVotesr, numOptions)


  val s = Sampler(Map(.01 -> 'a',.1 -> 'b',.89 -> 'c'))

  println(s.sample(0.0))
  println(s.sample(0.001))
  println(s.sample(0.1))
  println(s.sample(0.12))
  println(s.sample(0.3))
  println(s.sample(1.0))

}


@main
def scratch2(): Unit = {


  println("???")
  val numVotesr = 16
  val numOptions = 6
  val samplesPerStep = 1000

  //  val election = Plurality(numVotesr,numOptions)
  val election = InstantRunOff(numVotesr, numOptions)


  val voters = Seq.fill(numVotesr)(Seq.fill(numOptions)(Random.nextDouble()))

  println(voters)
  for (voter <- voters) {
    println(voter)
  }


  import election.{Ballot, Candidate}

  val samplers = Seq.fill(election.voters)(Sampler(election.allBallots.map(b => (b, 1.0 / election.allBallots.size)).toMap))

  var nextVotes = scala.collection.mutable.Map[Int, Ballot]()

  for (voter <- Range(0, numVotesr)) {

    // TODO unlcear exactly what is best to cache, winners (for everybody?, EVs, ...)
    val resultsCache = scala.collection.mutable.Map[election.Tally, Double]()

    val EVs = scala.collection.mutable.Map[Ballot, Double]().withDefault(_ => 0)
    val otherSamplers = samplers.take(voter) ++ samplers.drop(voter + 1)

    for (_ <- Range(0, samplesPerStep)) {

      val otherVotes = otherSamplers.map(_.sample(r.nextDouble()))

      // possibly could be faster with inlined loops
      val subTally = otherVotes.groupBy(x=>x).map((b,ls) => (b, ls.size)).withDefault(_ => 0)

      for (myBallot <- election.allBallots) {
        val tally = subTally + (myBallot -> (subTally(myBallot) + 1))

        val EV = resultsCache.getOrElseUpdate(tally,{
          val winners =election.winner(tally)
          winners.map(voters(voter)).sum / winners.size
        }) // TODO: speed up

        EVs(myBallot) += EV
      }
    }

    println(EVs)
    nextVotes(voter) = EVs.maxBy(_._2)._1
    println(nextVotes(voter))
  }


}
