package simulate

import VoteSystem.{VoteSystem, VoteSystemFancy, VoteSystemWithClarity}
import prob.Dist

import java.io.{File, FileWriter}
import scala.collection.immutable.Seq
import scala.util.Random


trait QualityMeasure {


}


// consider each voter
class Exhaustive(val election: VoteSystemWithClarity,
                 numVoters: Int
                ) {

  import election._

  private def allElections(v: Int): LazyList[Election] = {
    if (v == 0) {
      LazyList(List())
    } else {
      val last = allElections(v - 1)
      for {b <- allBallots.to(LazyList)
           rest <- last
           } yield b :: rest
    }
  }

  lazy val elections: LazyList[Election] = allElections(numVoters)


  type Tally = Map[Ballot, Int]

  def winner(e: Tally): Dist[Candidate] = election.winner(e.toList.flatMap((b, v) => List.fill(v)(b)))
  // intelijj bad type infrenece on winner

  private val winnerCache2 = scala.collection.mutable.Map[Tally, Dist[Candidate]]()

  def winnerFast(e: Tally): Dist[Candidate] = {
    winnerCache2.getOrElseUpdate(e, winner(e))
  }

  private val winnerCache = scala.collection.mutable.Map[Election, Dist[Candidate]]()

  def winnerFast(e: Election): Dist[Candidate] = {
    winnerCache.getOrElseUpdate(e, election.winner(e))
  }


  val datas = scala.collection.mutable.ListBuffer.empty[Seq[Double]]

  class Run(voters: Seq[Seq[Double]]) {


    // calculate the next votes and probablities afeter checking every permutation
    def NextProb(publicProbs: Seq[Dist[Ballot]], util: Int => Candidate => Double, probFallOff: Double): (List[Ballot], Seq[Dist[Ballot]]) = {
      //println(publicProbs)
      var nextVotes = scala.collection.mutable.Map[Int, Ballot]()
      for (voter <- Range(0, voters.size)) {

        val EVs = scala.collection.mutable.Map[Ballot, Double]().withDefault(_ => 0)
        for (e <- elections) {
          val myBallot = e(voter)
          //        val winners = winnerFast(e)
          val winners = winnerFast(e.groupBy(identity).map((b, l) => (b, l.size)))

          var prob = 1.0

          for ((ballot, otherVoter) <- e.zipWithIndex) {
            if (otherVoter != voter) {
              prob *= publicProbs(otherVoter)(ballot)
            }
          }
          //println(winners)
          val EV = prob * winners.dist.map((c, p) => util(voter)(c) * p).sum
          EVs(myBallot) += EV
        }
        //println(EVs)
        nextVotes(voter) = EVs.maxBy(_._2)._1
        //println(nextVotes)
      }
      //println(Range(0, voters).map(nextVotes))

      return (
        nextVotes.toList.sortBy(_._1).map(_._2)
        ,
        publicProbs.zipWithIndex.map((ballotDist, voter) => Dist(ballotDist.dist.map((ballot, prob) => if (nextVotes(voter) == ballot) {
          (ballot, (1.0 - probFallOff) + probFallOff * prob)
        } else {
          (ballot, probFallOff * prob)
        }))))

    }


    def runsystem(): Unit = {


      import election.Ballot
      import election.Candidate


      var publicProbs = Seq.fill(voters.size)(Dist.Uniform(election.allBallots))


      val aggregatePreference = election.candidates.map(i => voters.map(voter => voter(i)).sum)

      println(aggregatePreference)
      var (prefBest, best) = aggregatePreference.zipWithIndex.maxBy(_._1)


      val (honestVote, publicProbs1) = NextProb(publicProbs, voters, .5)


      println(s"##  ${election.getClass.getName}")
      println(s"best $best ")

      var finalVote = honestVote


      def data(): Seq[Double] = {
        val w = election.winner(finalVote)

        val regretPerVoter = (aggregatePreference(best) - w.dist.map((c, p) => aggregatePreference(c) * p).sum) / voters.size.toDouble

        val curtanty = publicProbs.map(_.dist.maxBy(_._2)._2).fold(1.0)(_ * _)
        // could uies this to determine the ramp up

        val estimatedPreference = finalVote.map(b => election.estimatedNievePrefference(b))
        val estimateAggregatedPreference = election.candidates.map(c => estimatedPreference.map(_(c)).sum)
        val clarity = (election.candidates.map(c => scala.math.pow((aggregatePreference(c) - estimateAggregatedPreference(c)) / voters.size.toDouble, 2) / candidates.size.toDouble).sum) // div num candidates?

        Seq(curtanty, regretPerVoter, clarity)
      }

      def summery(): String = {
        val w = election.winner(finalVote)

        val agg = election match
          case fancy: VoteSystemFancy =>
            fancy.aggregate(finalVote.asInstanceOf).toString
          case _ =>
            ""

        val Seq(curtanty, regretPerVoter, clarity) = data()

        //.runtimeClass.isInstance(


        f"${w}%-100s ${agg}%-100s \t${finalVote.map(_.toString.padTo(30, ' ')).mkString("\t")} ${curtanty}%-40s ${regretPerVoter}%-40s ${clarity}%-40s"
      }

      println(summery() ++ f"\t?")

      publicProbs = publicProbs1

      //TODO also return next probs and vites

      for (i <- Range(2, 7)) {
        var (finalVote1, publicProbs1) = NextProb(publicProbs, voters, (1.0 - 1.0 / i.toDouble))

        //      println(f"winner \t${election.winner(finalVote1)}%-20s \t${finalVote1.map(_.toString.padTo(30, ' ')).mkString("\t")}")

        finalVote = finalVote1
        println(summery())
        publicProbs = publicProbs1


        //      println()
      }


      //  val mildstretigicWinner = election.winner(finalVote)
      //  val mildstretigicValuePerVoter = mildstretigicWinner.map(i => voters.map(voter => voter(i)).sum / mildstretigicWinner.size.toDouble).sum / voters.size.toDouble
      //  val milddiffBallots = honestVote.zip(finalVote).map((honest, strategic) => if (honest == strategic) {
      //    0
      //  } else {
      //    1
      //  }).sum.toDouble / voters.size.toDouble


      // exelerate
      for (i <- Range(7, 20)) {
        var (finalVote1, publicProbs1) = NextProb(publicProbs, voters, (1.0 - 1.0 / 7.0))

        //      println(f"${election.winner(finalVote1)}%-100s \t${finalVote1.map(_.toString.padTo(30, ' ')).mkString("\t")} +")

        finalVote = finalVote1
        println(summery() ++ f"\t+")
        publicProbs = publicProbs1
        //      println()
      }

      datas += data()
    }
  }

  def average(d: Iterable[Double]): Double = d.sum / d.size.toDouble

  def summery(): String = {
    f"${average(datas.map(_(0)))}%-40s ${average(datas.map(_(1)))}%-40s ${average(datas.map(_(2)))}%-40s"
  }
}

@main
def run(): Unit = {
  import _root_.VoteSystem.RankChoice.OptimalGameTheory
  import _root_.VoteSystem.RankChoice.BordaCount
  import _root_.VoteSystem.OneChoice.Plurality
  import _root_.VoteSystem.RankChoice.InstantRunoffVoting
  import _root_.VoteSystem.RankChoice.Silly.ProportionallyRandom
  import _root_.VoteSystem.RankChoice.Silly.Parity
  val r = new Random

  val numVoters = 7
  type numOptions = 3

  val runs = 1000000


  val e = OptimalGameTheory[3]()
  //    val e = BordaCount(numOptions)
  //    val e = Plurality(numOptions)

  val systems = List(
    Exhaustive(OptimalGameTheory[numOptions](), numVoters),
    Exhaustive(InstantRunoffVoting(valueOf[numOptions]), numVoters),
    Exhaustive(BordaCount(valueOf[numOptions]), numVoters),

    //silly
    //Exhaustive(ProportionallyRandom(valueOf[numOptions]),numVoters),
    //    Exhaustive(Parity(valueOf[numOptions]),numVoters),

  )

  for (i <- Range(0, runs)) {

    val voters = Seq.fill(numVoters)(Seq.fill(valueOf[numOptions])(Random.nextDouble()))
    //val voters = List(List(0.8448046022556366, 0.35468166475319185, 0.6227897425995516), List(0.20983807345379102, 0.6405377223702815, 0.6494095953406748), List(0.0015109866494222857, 0.7970935133931731, 0.4001702165080565), List(0.24621471321625188, 0.25779627370523306, 0.7811329970698041), List(0.6385039043271907, 0.9032141404489262, 0.6705143353752466), List(0.9659238381880431, 0.1249433817210781, 0.6819527652117311))

    println(voters)
    //val voters = List(List(0.7155237932298478, 0.9428369677018246, 0.08898937689290554, 0.8456904365593594), List(0.24828963026960715, 0.5283198600998977, 0.8353706206462521, 0.9707476532135482), List(0.2749787467645757, 0.3496408704246007, 0.5120396282998364, 0.09611124094347578), List(0.10163955132484959, 0.23355449919013538, 0.9472414104635279, 0.35386847365191765), List(0.16203065946356054, 0.32913220594685766, 0.2779247637589507, 0.6400427195766016), List(0.957142024648165, 0.6190048364369548, 0.5982902326289554, 0.6822937050104824))
    //List(List(0.7998139911563363, 0.9598790204037694, 0.7196849436076475, 0.8395395520553166), List(0.3731212406517461, 0.7600402313526347, 0.6967361000439918, 0.7251618413781908), List(0.7938171573565171, 0.416444545785444, 0.4470620972177066, 0.8898349187957365), List(0.5479007196890486, 0.56824061708372, 0.4640401073079, 0.693134700258453), List(0.6173016871988352, 0.7971275188267796, 0.28454446586209137, 0.7954702695918969), List(0.694592941977278, 0.5234305999674427, 0.7183346762458745, 0.1646795724906459))

    for (voter <- voters) {
      println(voter)
    }

    for (system <- systems) {
      system.Run(voters).runsystem()
    }


    val fileWriter = new FileWriter(new File("results.txt"))
    fileWriter.write(s"from $i runs\n")
    for (system <- systems) {
      val s = f"${system.election.getClass.getName}%-100s ${system.summery()}\n"
      println(s)
      fileWriter.write(s)
    }
    fileWriter.close();

    //    val s = Exhaustive(e, voters)
    //
    //    s.runsystem()

    //      val fileWriter = new FileWriter(new File("results.txt"))
    //      println(i)
    //      //    e.map(x => println(x.info()))
    //
    //      fileWriter.write(s"from $i runs\n")
    //TODO what is the averege unneeded compromise between candidate 2nd best cadidate, 3rd best,...

    //    e.map(x => fileWriter.write(x.info() ++ "\n"))
    //    fileWriter.close();


    //  println(Plurality(4,3).winner(List(1,1,2)))
    //  println(Plurality(4,3).allBallots)
    //  println(election.allElections)
    //println(election.allElections().size)

    //runsystem(election, voters)
  }
}