
import scala.collection.mutable.{ArrayBuffer, ListBuffer}
import scala.util.Random

// This was from before I though through the asomtotics


//TOOD could be made extreamely faster!
// coulc sample form int and precompute efficient bit maskins, or at least a tree
sealed class Sampler[A](val dist: Map[A, Double]) {
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
  val numVotesr = 8
  val numOptions = 5
  val samplesPerStep = 5000

  //  val election = Plurality(numVotesr,numOptions)
  val election = Approval(numVotesr, numOptions)


  val voters = Seq.fill(numVotesr)(Seq.fill(numOptions)(Random.nextDouble())
//    .sortBy(x => x) //TODO remove
  )

  println(voters)
  for (voter <- voters) {
    println(voter)
  }


  import election.{Ballot, Candidate}

  val samplers : Seq[Sampler[Ballot]] = Seq.fill(election.voters)(Sampler(election.allBallots.map(b => (b, 1.0 / election.allBallots.size)).toMap))

  var nextVotes = scala.collection.mutable.Map[Int, Ballot]()


  for (voter <- Range(0, numVotesr)) {

    val EVs = scala.collection.mutable.Map[Ballot, Double]().withDefault(_ => 0)

    for (_ <- Range(0, samplesPerStep)) {

      val votes : Array[Ballot] = samplers.map(_.sample(r.nextDouble())).toArray

      for (myBallot <- election.allBallots) {
        votes(voter) = myBallot
        val winners = election.winner(votes.toList) // TODO: speed up

        val EV = winners.map(voters(voter)).sum / winners.size
        EVs(myBallot) += EV
      }
    }

    println()
    println(EVs)
    nextVotes(voter) = EVs.maxBy(_._2)._1
    println(nextVotes(voter))

    val n = voters(voter).zipWithIndex.map(_.swap).toMap
    val correctAns = election.nieve(n)
    if(correctAns != nextVotes(voter)){
      println(correctAns)
      println("!!!!")
    }


  }


}

def scratch21(): Unit = {


  println("???")
  val numVotesr = 8
  val numOptions = 5
  val samplesPerStep = 2000

  //  val election = Plurality(numVotesr,numOptions)
  val election = Approval(numVotesr, numOptions)



    val n = List(0.9963499552159691, 0.30602026639544544, 0.8175255738424995, 0.5032050573305196, 0.3403952909287561).zipWithIndex.map(_.swap).toMap
    val correctAns = election.nieve(n)
      println(correctAns)

}
