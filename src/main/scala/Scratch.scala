
import scala.util.Random


@main
def main12(): Unit = {

  println("???")
  val numVotesr = 3
  val numOptions = 5
//  val election = Plurality(numVotesr,numOptions)
  val election = InstantRunOff(numVotesr,numOptions)

  println(election.allPivotalSubTally.size)

  val voters = Seq.fill(numVotesr)(Seq.fill(numOptions)(Random.nextDouble()))
  //val voters = List(List(0.9598846282508307, 0.0442994812994405, 0.8173809006531977), List(0.2593282713077234, 0.995328335687889, 0.9040670911685706), List(0.3921522689485478, 0.8019699578489333, 0.7751359979241987), List(0.09358012355234258, 0.03794660888318724, 0.9242094400600395))


  println(voters)
  //val voters = List(List(0.7155237932298478, 0.9428369677018246, 0.08898937689290554, 0.8456904365593594), List(0.24828963026960715, 0.5283198600998977, 0.8353706206462521, 0.9707476532135482), List(0.2749787467645757, 0.3496408704246007, 0.5120396282998364, 0.09611124094347578), List(0.10163955132484959, 0.23355449919013538, 0.9472414104635279, 0.35386847365191765), List(0.16203065946356054, 0.32913220594685766, 0.2779247637589507, 0.6400427195766016), List(0.957142024648165, 0.6190048364369548, 0.5982902326289554, 0.6822937050104824))
  //List(List(0.7998139911563363, 0.9598790204037694, 0.7196849436076475, 0.8395395520553166), List(0.3731212406517461, 0.7600402313526347, 0.6967361000439918, 0.7251618413781908), List(0.7938171573565171, 0.416444545785444, 0.4470620972177066, 0.8898349187957365), List(0.5479007196890486, 0.56824061708372, 0.4640401073079, 0.693134700258453), List(0.6173016871988352, 0.7971275188267796, 0.28454446586209137, 0.7954702695918969), List(0.694592941977278, 0.5234305999674427, 0.7183346762458745, 0.1646795724906459))

  for (voter <- voters){
    println(voter)
  }

  println("???")

  // TODO  // TODO  // TODO  // TODO  // TODO  // TODO  // TODO
  // bug here need to not coulnt own prior votes or will be too conservative
//  var seen = election.allBallots
  var prev : scala.collection.mutable.Map[Int,List[election.Ballot]] = scala.collection.mutable.Map[Int,List[election.Ballot]]()

  for{v<- Range(0,numVotesr)}{
    prev(v)= election.allBallots.toList
  }


  var nextVotes = scala.collection.mutable.Map[Int, election.Ballot]()

  for (step <- Range(0,100)){
  //println(publicProbs)

//    // problematically less acurite
//    val seen = (prev.toMap).toList.flatMap((_, l) => l).groupBy(x => x).map((b, l) => (b, l.size)) //.collect()//.flatten.groupBy((v,b) => ???)
//    val seencount = seen.values.sum
//    val probBallot = election.allBallots.map(b => (b, seen.getOrElse(b,0).toDouble / seencount.toDouble)).toMap

  for ((voter,i) <- voters.zipWithIndex){
val seenbutMe = (prev.toMap - i).toList.flatMap((_,l) => l).groupBy(x => x).map((b,l) => (b,l.size))//.collect()//.flatten.groupBy((v,b) => ???)
    val seenbutMecount = seenbutMe.values.sum

    val probBallot = election.allBallots.map(b => (b, seenbutMe.getOrElse(b,0).toDouble / seenbutMecount.toDouble)).toMap

    //println(i)
    val PivitalEVs = scala.collection.mutable.Map[election.Ballot, Double]().withDefault(_ => 0)
    for (sistuation <- election.allPivotalSubTally) {
      // TODO can cache?
      val prob = sistuation.map((b,v) => scala.math.pow(probBallot(b),v)).product

      for (b <- election.allBallots) {

        val winners = election.winnerFast(sistuation+ (b -> (sistuation(b)+1)))

        PivitalEVs(b) += prob * winners.map(voter).sum/winners.size
        //voter(b)
      }
    }
    //Wprintln(election.allBallots.map(PivitalEVs))

    nextVotes(i) = PivitalEVs.maxBy(_._2)._1

  }
    for((v,b) <- nextVotes){
      prev(v) = prev(v) ++ List(b)
    }


  println(Range(0, voters.size).map(nextVotes))
    //println(s"                   $seen")
  }
}
