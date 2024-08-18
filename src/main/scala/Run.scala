import scala.util.Random




class SystemCOunter(election: VotingSys){

  var worstHonest :Double = 0.0
  var averageHonest :Double = 0.0


  var worstStrat :Double = 0.0
  var averageStrat :Double = 0.0

  var counter = 0


  def runsystem( voters: Seq[Seq[Double]]): Unit = {
    var publicProbs: Seq[Map[election.Ballot, Double]] = Seq.fill(election.voters)(election.allBallots.map(b => (b, 1.0 / election.allBallots.size)).toMap)


    var aggregatePreference = Range(0, election.outcome).map(i => voters.map(voter => Math.abs(voter(i))).sum)

    println(aggregatePreference)
    var (prefBest, best) = aggregatePreference.zipWithIndex.minBy(_._1)


    val (honestVote, publicProbs1) = election.NextProb(publicProbs, voters, .5)

    println(honestVote)

    println(election.winner(honestVote))




    publicProbs = publicProbs1
    //TODO also return next probs and vites

//    for (i <- Range(2, 100)) {
//      publicProbs = election.NextProb(publicProbs, voters, (1.0 - 1.0 / i.toDouble))
//    }


  }


}





@main
def run(): Unit = {
  val r = new Random

  val numVotesr = 6
  val numOptions = 3


  val election = Plurality(numVotesr,numOptions)



  val voters = Seq.fill(numVotesr)(Seq.fill(numOptions)(Random.nextDouble()))
  //val voters = List(List(0.8448046022556366, 0.35468166475319185, 0.6227897425995516), List(0.20983807345379102, 0.6405377223702815, 0.6494095953406748), List(0.0015109866494222857, 0.7970935133931731, 0.4001702165080565), List(0.24621471321625188, 0.25779627370523306, 0.7811329970698041), List(0.6385039043271907, 0.9032141404489262, 0.6705143353752466), List(0.9659238381880431, 0.1249433817210781, 0.6819527652117311))

  println(voters)
  //val voters = List(List(0.7155237932298478, 0.9428369677018246, 0.08898937689290554, 0.8456904365593594), List(0.24828963026960715, 0.5283198600998977, 0.8353706206462521, 0.9707476532135482), List(0.2749787467645757, 0.3496408704246007, 0.5120396282998364, 0.09611124094347578), List(0.10163955132484959, 0.23355449919013538, 0.9472414104635279, 0.35386847365191765), List(0.16203065946356054, 0.32913220594685766, 0.2779247637589507, 0.6400427195766016), List(0.957142024648165, 0.6190048364369548, 0.5982902326289554, 0.6822937050104824))
  //List(List(0.7998139911563363, 0.9598790204037694, 0.7196849436076475, 0.8395395520553166), List(0.3731212406517461, 0.7600402313526347, 0.6967361000439918, 0.7251618413781908), List(0.7938171573565171, 0.416444545785444, 0.4470620972177066, 0.8898349187957365), List(0.5479007196890486, 0.56824061708372, 0.4640401073079, 0.693134700258453), List(0.6173016871988352, 0.7971275188267796, 0.28454446586209137, 0.7954702695918969), List(0.694592941977278, 0.5234305999674427, 0.7183346762458745, 0.1646795724906459))

  for (voter <- voters){
    println(voter)
  }
  val xxx = SystemCOunter(election)


  xxx.runsystem(voters)
  //  println(Plurality(4,3).winner(List(1,1,2)))
  //  println(Plurality(4,3).allBallots)
  //  println(election.allElections)
  //println(election.allElections().size)

  //runsystem(election, voters)
}