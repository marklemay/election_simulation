import java.io.{File, FileWriter}
import scala.util.Random




class SystemCOunter(election: VotingSys){



  var bestValuePerVoter :Double = 0.0

  var honesrValuePerVoter :Double = 0.0

  var mildStretigicValuePerVoter :Double = 0.0
  var mildDiffBallotsPerVoter :Double = 0.0

  var stretigicValuePerVoter :Double = 0.0
  var diffBallotsPerVoter :Double = 0.0


  var nieveprefdiff :Double = 0.0


  var counter = 0


  def info() = f"${election.getClass.getName}%-30s ave. unneeded compromise per voter(honest)=${(bestValuePerVoter-honesrValuePerVoter)/counter.toDouble}%1.5f, dif in estimatedptref= ${nieveprefdiff/counter.toDouble}%1.5f," ++
    f"ave. unneeded compromise per voter(mild strategic)=${(bestValuePerVoter-mildStretigicValuePerVoter)/counter.toDouble}%1.5f, ave. strat votes=${mildDiffBallotsPerVoter/counter.toDouble}%1.5f, " ++
    f"ave. unneeded compromise per voter(hard strategic)=${(bestValuePerVoter-stretigicValuePerVoter)/counter.toDouble}%1.5f, ave. strat votes=${diffBallotsPerVoter/counter.toDouble}%1.5f"

  def runsystem( voters: Seq[Seq[Double]]): Unit = {


    var publicProbs = Seq.fill(election.voters)(Sampler(election.allBallots.map(b => (b, 1.0 / election.allBallots.size)).toMap))


    var aggregatePreference = Range(0, election.outcome).map(i => voters.map(voter => voter(i)).sum)

    println(aggregatePreference)
    var (prefBest, best) = aggregatePreference.zipWithIndex.maxBy(_._1)


    val (honestVote, publicProbs1) = election.NextProbBySmaple(publicProbs, voters, .5)


    println(s"##  ${election.getClass.getName}")
    println(s"best $best ")
    println(f"winner \t${election.winner(honestVote)}%-20s \t${honestVote.map(_.toString.padTo(30,' ')).mkString("\t")} ?")


    var finalVote = honestVote


    publicProbs = publicProbs1
    //TODO also return next probs and vites

    for (i <- Range(2, 7)) {
      var (finalVote1, publicProbs1) = election.NextProbBySmaple(publicProbs, voters, (1.0 - 1.0 / i.toDouble))

      println(f"winner \t${election.winner(finalVote1)}%-20s \t${finalVote1.map(_.toString.padTo(30,' ')).mkString("\t")}")

      finalVote= finalVote1
      publicProbs = publicProbs1
    }


    val mildstretigicWinner =  election.winner(finalVote)
    val mildstretigicValuePerVoter = mildstretigicWinner.map(i => voters.map(voter => voter(i)).sum/mildstretigicWinner.size.toDouble).sum/ voters.size.toDouble
    val milddiffBallots = honestVote.zip(finalVote).map((honest, strategic) => if(honest == strategic) {0}else {1}).sum.toDouble/ voters.size.toDouble


    // exelerate
    for (i <- Range(7, 20)) {
      var (finalVote1, publicProbs1) = election.NextProbBySmaple(publicProbs, voters, (1.0 - 1.0 / 7.0))

      println(f"winner \t${election.winner(finalVote1)}%-20s \t${finalVote1.map(_.toString.padTo(30,' ')).mkString("\t")} +")

      finalVote = finalVote1
      publicProbs = publicProbs1
    }


    val honestWinner =  election.winner(honestVote)
    val honesrValuePerVoter = honestWinner.map(i => voters.map(voter => voter(i)).sum/honestWinner.size.toDouble).sum/ voters.size.toDouble


    val stretigicWinner =  election.winner(finalVote)
    val stretigicValuePerVoter = stretigicWinner.map(i => voters.map(voter => voter(i)).sum/stretigicWinner.size.toDouble).sum/ voters.size.toDouble


    val diffBallots = honestVote.zip(finalVote).map((honest, strategic) => if(honest == strategic) {0}else {1}).sum.toDouble/ voters.size.toDouble

    println(s"bestValuePerVoter${prefBest / voters.size.toDouble}, honesrValuePerVoter $honesrValuePerVoter, stretigicValuePerVoter $stretigicValuePerVoter, diffBallots $diffBallots" )


    this.bestValuePerVoter+= prefBest / voters.size.toDouble
    this.honesrValuePerVoter += honesrValuePerVoter

    this.mildStretigicValuePerVoter += mildstretigicValuePerVoter
    this.mildDiffBallotsPerVoter += milddiffBallots

    this.stretigicValuePerVoter += stretigicValuePerVoter
    this.diffBallotsPerVoter += diffBallots


//    val estimatedHonestPref = Range(0,election.option).map(v => honestVote.map(election.estimatedNievePrefference))
//    val estimatedHonestPrefDiff = Range(0, election.outcome).map(i => Math.abs(voters.map(voter => voter(i)).sum - estimatedHonestPref(i)) ).sum

    this.nieveprefdiff += 0 //estimatedHonestPrefDiff / voters.size.toDouble
//    estimatedHonestPref






    counter += 1
  }


}





@main
def runOld(): Unit = {
  val r = new Random

  val numVotesr = 8
  val numOptions = 5

  val runs = 10000



  val xxx = List(SystemCOunter(Plurality(numVotesr,numOptions)),
    SystemCOunter(InstantRunOff(numVotesr,numOptions)),
      SystemCOunter(RankBySum(numVotesr,numOptions)),
    SystemCOunter(InstantRunOffReomveLeastSum(numVotesr,numOptions)),
    SystemCOunter(Approval(numVotesr,numOptions))
  )

  for(i <- Range(0,runs)){

    val voters = Seq.fill(numVotesr)(Seq.fill(numOptions)(Random.nextDouble()))
    //val voters = List(List(0.8448046022556366, 0.35468166475319185, 0.6227897425995516), List(0.20983807345379102, 0.6405377223702815, 0.6494095953406748), List(0.0015109866494222857, 0.7970935133931731, 0.4001702165080565), List(0.24621471321625188, 0.25779627370523306, 0.7811329970698041), List(0.6385039043271907, 0.9032141404489262, 0.6705143353752466), List(0.9659238381880431, 0.1249433817210781, 0.6819527652117311))

    println(voters)
    //val voters = List(List(0.7155237932298478, 0.9428369677018246, 0.08898937689290554, 0.8456904365593594), List(0.24828963026960715, 0.5283198600998977, 0.8353706206462521, 0.9707476532135482), List(0.2749787467645757, 0.3496408704246007, 0.5120396282998364, 0.09611124094347578), List(0.10163955132484959, 0.23355449919013538, 0.9472414104635279, 0.35386847365191765), List(0.16203065946356054, 0.32913220594685766, 0.2779247637589507, 0.6400427195766016), List(0.957142024648165, 0.6190048364369548, 0.5982902326289554, 0.6822937050104824))
    //List(List(0.7998139911563363, 0.9598790204037694, 0.7196849436076475, 0.8395395520553166), List(0.3731212406517461, 0.7600402313526347, 0.6967361000439918, 0.7251618413781908), List(0.7938171573565171, 0.416444545785444, 0.4470620972177066, 0.8898349187957365), List(0.5479007196890486, 0.56824061708372, 0.4640401073079, 0.693134700258453), List(0.6173016871988352, 0.7971275188267796, 0.28454446586209137, 0.7954702695918969), List(0.694592941977278, 0.5234305999674427, 0.7183346762458745, 0.1646795724906459))

    for (voter <- voters){
      println(voter)
    }

    xxx.map(_.runsystem(voters))

    val fileWriter = new FileWriter(new File("results.txt"))
    println(i)
    xxx.map(x => println(x.info()))

    fileWriter.write(s"from $i runs\n")
    //TODO what is the averege unneeded compromise between candidate 2nd best cadidate, 3rd best,...

    xxx.map(x => fileWriter.write(x.info() ++ "\n"))
    fileWriter.close();


    //  println(Plurality(4,3).winner(List(1,1,2)))
    //  println(Plurality(4,3).allBallots)
    //  println(election.allElections)
    //println(election.allElections().size)

    //runsystem(election, voters)
  }
}