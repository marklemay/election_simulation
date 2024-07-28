import java.util.Dictionary
import scala.annotation.tailrec
import scala.util.Random


val runs = 1000
val numVotesr = 10
val numOptions = 5

val r = new scala.util.Random

@tailrec
def instantRunOff(votes: Seq[Seq[Int]]): Int = {
  val firstChoice = votes.groupBy(_(0))
  val outrightwinner = firstChoice.find((_, fistChoiceVotes) => fistChoiceVotes.size.toDouble / numVotesr.toDouble > 0.5)

  println(outrightwinner)
  outrightwinner match {
    case Some(w, _) => return w
    case None => {
      val leastPopular = votes.groupBy(_.last).maxBy((_, votes) => votes.size)._1
      println("drop")
      println(leastPopular)

      val runnOff = votes.map(_.filter(_ != leastPopular))
      println(runnOff)

      return instantRunOff(runnOff)
    }
  }
}

// IDEAL plurality voter strategy
// project a vote distribution based on underlieng preferences (but self)
// use the distribution to estimate giving tyemaking/breaking votes
// calculate teh EV of tyebreaking with preference difference

//can probably simplify some of these assumptions

def StrategicWithPoll(voters: Seq[Seq[Double]], i:Int) : Int = {
  // poll many pairs of voters to estimate prob of each tye break

  val OtherVoters = voters.zipWithIndex.filter(_._2 != i).map(_._1)
  val me = voters(i)

  // ideally bump https://github.com/nicolasstucki/multisets to scala 3.
  var dictionary = List[(Int,Int)]()

  for( _ <- Range(0, 100)) {

    val c1 = TopChoice(OtherVoters(r.between(0, OtherVoters.size)))
    val c2 = TopChoice(OtherVoters(r.between(0, OtherVoters.size)))

    dictionary = (c1, c2) :: dictionary
  }
  dictionary = dictionary.filterNot(_ == _)
  val res = dictionary.groupBy((x,y) => if(x < y) {(x,y)} else {(y,x)}).map((xx,l) => (xx,l.size))
  println(res)

  val myProportanateEVParts = res.map( ( xxx , c) => (xxx, c.toDouble * (Math.abs(me(xxx._1)-me(xxx._2)))))
  println(myProportanateEVParts)

  ???
}


//def strategic()


def TopChoice: Seq[Double] => Int = {
  _.zipWithIndex.maxBy(_._1)._2
}

@main
def main(): Unit = {
  println("Hello world!")


  // also interested in the distribution of preference drop and worst case
  var aggregatePrefDrop = 0.0
  var aggregateNieveRunOffPrefDrop = 0.0


  for (i <- Range(0, runs)) {


    val voters = Seq.fill(numVotesr)(Seq.fill(numOptions)(Random.nextDouble()))
    //
    //  var best
    //  for (option <- Range(0,numOptions)) {
    //
    //  }
    println(voters)

    // least square diff ok?
    var aggregatePreference = Range(0, numOptions).map(i => voters.map(voter => Math.pow(voter(i) - 1.0, 2.0)).sum) //.minBy(i => voters.map( voter => Math.pow( voter(i) - 1.0, 2.0)).sum)

    println(aggregatePreference)
    var (prefBest, best) = aggregatePreference.zipWithIndex.minBy(_._1)
    println((prefBest, best))


//    // nieve plurality
//    {
//      val votes = voters.map(TopChoice)
//
//      println(votes)
//      val winner = votes.groupBy(identity).view.mapValues(_.size).maxBy(_._2)._1
//      println(winner)
//      val preferenceDrop = Math.abs(prefBest - aggregatePreference(winner))
//      println(preferenceDrop)
//      aggregatePrefDrop = aggregatePrefDrop + preferenceDrop
//    }
//
//
//    //    // nieve instant run off
//    {
//      val votes = voters.map(_.zipWithIndex.sortBy(_._1).reverse.map(_._2))
//      println(votes)
//      val winner = instantRunOff(votes)
//      val preferenceDrop = Math.abs(prefBest - aggregatePreference(winner))
//      println(preferenceDrop)
//      aggregateNieveRunOffPrefDrop = aggregateNieveRunOffPrefDrop + preferenceDrop
//
//    }

    // sort of stretegic plurality
      {
        StrategicWithPoll(voters,0)

      }


  }
  println("aggregatePrefDrop/runs.toDouble")
  println(aggregatePrefDrop / runs.toDouble)

  println("aggregateNieveRunOffPrefDrop/runs.toDouble")
  println(aggregateNieveRunOffPrefDrop / runs.toDouble)

}


// todo drinking age situations
// todo different rank choice eleimination criteria (instant run off, least populer, sum place in list)
// todo slow run off