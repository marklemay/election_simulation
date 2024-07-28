import scala.util.Random


trait VotingSys(voters: Int, outcome: Int) {

  type Candidate
  type Ballot

  def winner(e:Election) : Set[Candidate]

  lazy val allBallots : List[Ballot]

  type Election = List[Ballot]

  lazy val allElections: List[Election]

  // should live elsewhere
  def NextProb(publicProbs: Seq[Map[Ballot, Double]], util: Int => Candidate => Double, probFallOff: Double): Seq[Map[Ballot, Double]] = {
    //println(publicProbs)
    var nextVotes = scala.collection.mutable.Map[Int, Ballot]()
    for (voter <- Range(0, voters)) {

      val EVs = scala.collection.mutable.Map[Ballot, Double]().withDefault(_ => 0)
      for (e <- allElections) {
        val myBallot = e(voter)
        val winners = winner(e)

        var prob = 1.0

        for ((ballot, otherVoter) <- e.zipWithIndex) {
          if (otherVoter != voter) {
            prob *= publicProbs(otherVoter)(ballot)
          }
        }
        val EV = prob * winners.map(util(voter)).sum / winners.size
        EVs(myBallot) += EV
      }
      //println(EVs)
      nextVotes(voter) = EVs.maxBy(_._2)._1
      //println(nextVotes)
    }
    println(Range(0, voters).map(nextVotes))

    return publicProbs.zipWithIndex.map((ballotDist, voter) => ballotDist.map((ballot, prob) => if (nextVotes(voter) == ballot) {
      (ballot, (1.0 - probFallOff) + probFallOff * prob)
    } else {
      (ballot, probFallOff * prob)
    }))
  }

}


class Plurality(voters: Int, options :Int) extends VotingSys(voters,options){

  // TOOD constain as needed
  type Candidate = Int
  type Ballot = Candidate

  def winner(e:Election) : Set[Candidate] ={

    val counts = e.groupBy(i=>i).map((x,y) => (x,y.size))
    val mostvotes = counts.maxBy(_._2)._2

    counts.filter(_._2 == mostvotes).keySet
  }

  lazy val allBallots : List[Ballot] = {
    Range(0,options).toList
  }

  // Refactor up to VotingSys
  lazy val allElections : List[Election] = {
    if(voters==0){
      List(List())
    }else{
      var out = List()
      for(b <- allBallots){
        Plurality(voters-1, options).allElections

      }
      val last = Plurality(voters - 1, options)
      for {b <- allBallots
           rest <- last.allElections
           } yield b :: rest
    }
  }

}


class InstantRunOff(voters: Int, options :Int) extends VotingSys(voters,options){

  // TOOD constain as needed
  type Candidate = Int
  type Ballot = List[Candidate]


  def winnerHelper(e: List[List[Candidate]]): Set[Candidate] = {
    if(e(0).size==0){
      return Set()
    }

    val firstVotes = e.groupBy(x => x(0)).mapValues(v => v.size)
//    println("firstvotes")
//    println(firstVotes.toList)
    val win = firstVotes.filter(_._2 > voters.toDouble / 2.0)
//    println(win.toList)

    if (win.size > 0) {
      return win.keySet.toSet
    } else {
//      println("runn off")
      val numLeastFirstVotes = firstVotes.minBy(_._2)._2
//      println(numLeastFirstVotes)
      val candidatesToRemovw = firstVotes.filter(_._2 == numLeastFirstVotes).keys.toList
//      println(candidatesToRemovw)

      val runnoff = e.map(_.filter(c => candidatesToRemovw.find(c == _).size == 0))
//      println("new ballot")
//      println(runnoff)

      val res = winnerHelper(runnoff)
      if(res.size>0){
        return res
      }else{
        // in the case of a tye runoff, jsut use the first vote getters

        val mostvotes = firstVotes.maxBy(_._2)._2

        return firstVotes.filter(_._2 == mostvotes).keySet.toSet
      }


    }
  }


  override def winner(e:Election) : Set[Candidate] ={
    winnerHelper(e)
  }

  lazy val allBallots : List[Ballot] = {
    Range(0,options).toList.permutations.toList
  }

  // Refactor up to VotingSys
  lazy val allElections : List[Election] = {
    if(voters==0){
      List(List())
    }else{
      var out = List()
      for(b <- allBallots){
        InstantRunOff(voters-1, options).allElections

      }
      val last = InstantRunOff(voters - 1, options)
      for {b <- allBallots
           rest <- last.allElections
           } yield b :: rest
    }
  }

}

// TODO there are many resolution criterai that could be studied: win in 1 round by by least sum of vote positions
// run off with least first place removed, run off with most last place removed, run off with most position count removed
// requier more then 50% support...



@main
def main3(): Unit = {

  val numVotesr = 6
  val numOptions = 4

  val election = InstantRunOff(numVotesr,numOptions)

  //println(election.winner(List(List(0,1,2),List(0,1,2),List(1,0,2),List(1,0,2),List(2,0,1))))

  //println(election.allBallots)
  //println(election.allElections.size)


  val voters = Seq.fill(numVotesr)(Seq.fill(numOptions)(Random.nextDouble()))
  println(voters)
  //val voters = List(List(0.7155237932298478, 0.9428369677018246, 0.08898937689290554, 0.8456904365593594), List(0.24828963026960715, 0.5283198600998977, 0.8353706206462521, 0.9707476532135482), List(0.2749787467645757, 0.3496408704246007, 0.5120396282998364, 0.09611124094347578), List(0.10163955132484959, 0.23355449919013538, 0.9472414104635279, 0.35386847365191765), List(0.16203065946356054, 0.32913220594685766, 0.2779247637589507, 0.6400427195766016), List(0.957142024648165, 0.6190048364369548, 0.5982902326289554, 0.6822937050104824))
  //List(List(0.7998139911563363, 0.9598790204037694, 0.7196849436076475, 0.8395395520553166), List(0.3731212406517461, 0.7600402313526347, 0.6967361000439918, 0.7251618413781908), List(0.7938171573565171, 0.416444545785444, 0.4470620972177066, 0.8898349187957365), List(0.5479007196890486, 0.56824061708372, 0.4640401073079, 0.693134700258453), List(0.6173016871988352, 0.7971275188267796, 0.28454446586209137, 0.7954702695918969), List(0.694592941977278, 0.5234305999674427, 0.7183346762458745, 0.1646795724906459))

  for (voter <- voters){
    println(voter)
  }


  //  println(Plurality(4,3).winner(List(1,1,2)))
  //  println(Plurality(4,3).allBallots)
  //  println(election.allElections)
  println(election.allElections.size)

  val probFallOff = 0.5
  var publicProbs : Seq[Map[election.Ballot, Double]] =  Seq.fill(numVotesr)(election.allBallots.map(b => (b,1.0/election.allBallots.size)).toMap)

  println(publicProbs)

  for(_ <- Range(0,10)){
    publicProbs = election.NextProb(publicProbs,voters,probFallOff)
  }


}



def main4(): Unit = {
  val r = new scala.util.Random

  val numVotesr = 8
  val numOptions = 5

  val election = Plurality(numVotesr,numOptions)


  val voters = Seq.fill(numVotesr)(Seq.fill(numOptions)(Random.nextDouble()))
  //println(voters)
  //val voters = List(List(0.7155237932298478, 0.9428369677018246, 0.08898937689290554, 0.8456904365593594), List(0.24828963026960715, 0.5283198600998977, 0.8353706206462521, 0.9707476532135482), List(0.2749787467645757, 0.3496408704246007, 0.5120396282998364, 0.09611124094347578), List(0.10163955132484959, 0.23355449919013538, 0.9472414104635279, 0.35386847365191765), List(0.16203065946356054, 0.32913220594685766, 0.2779247637589507, 0.6400427195766016), List(0.957142024648165, 0.6190048364369548, 0.5982902326289554, 0.6822937050104824))
  //List(List(0.7998139911563363, 0.9598790204037694, 0.7196849436076475, 0.8395395520553166), List(0.3731212406517461, 0.7600402313526347, 0.6967361000439918, 0.7251618413781908), List(0.7938171573565171, 0.416444545785444, 0.4470620972177066, 0.8898349187957365), List(0.5479007196890486, 0.56824061708372, 0.4640401073079, 0.693134700258453), List(0.6173016871988352, 0.7971275188267796, 0.28454446586209137, 0.7954702695918969), List(0.694592941977278, 0.5234305999674427, 0.7183346762458745, 0.1646795724906459))

  for (voter <- voters){
    println(voter)
  }


//  println(Plurality(4,3).winner(List(1,1,2)))
//  println(Plurality(4,3).allBallots)
//  println(election.allElections)
  println(election.allElections.size)

  val probFallOff = 0.5
  var publicProbs  =  Seq.fill(numVotesr)(Seq.fill(numOptions)(1.0/numOptions).zipWithIndex.map((x,y) => (y,x)).toMap)

  println(publicProbs)

  for(_ <- Range(0,10)){
    publicProbs = election.NextProb(publicProbs,voters,probFallOff)
  }

}




//todo investigate rank choice
//todo: make much more efficeint with pivots