import scala.util.Random


trait VotingSys(val voters: Int, val outcome: Int) {

  type Candidate = Int
  type Ballot

  def winner(e: Election): Set[Candidate]

  // TODO something cleaner?
  private val winnerCache = scala.collection.mutable.Map[Election, Set[Candidate]]()
  def winnerFast(e: Election): Set[Candidate] = {
    winnerCache.getOrElseUpdate(e,winner(e))
  }

  lazy val allBallots: List[Ballot]


  // TODO remove type ailias?
  type Election = List[Ballot]



  def NextProb(publicProbs: Seq[Map[Ballot, Double]], util: Int => Candidate => Double, probFallOff: Double): (List[Ballot], Seq[Map[Ballot, Double]]) = {
    //println(publicProbs)
    var nextVotes = scala.collection.mutable.Map[Int, Ballot]()
    for (voter <- Range(0, voters)) {

      val EVs = scala.collection.mutable.Map[Ballot, Double]().withDefault(_ => 0)
      for (e <- allElections()) {
        val myBallot = e(voter)
        val winners = winnerFast(e)

        var prob = 1.0

        for ((ballot, otherVoter) <- e.zipWithIndex) {
          if (otherVoter != voter) {
            prob *= publicProbs(otherVoter)(ballot)
          }
        }
        //println(winners)
        val EV = prob * winners.map(util(voter)).sum / winners.size
        EVs(myBallot) += EV
      }
      //println(EVs)
      nextVotes(voter) = EVs.maxBy(_._2)._1
      //println(nextVotes)
    }
    //println(Range(0, voters).map(nextVotes))

    return (
      nextVotes.toList.sortBy(_._1).map(_._2)
      , publicProbs.zipWithIndex.map((ballotDist, voter) => ballotDist.map((ballot, prob) => if (nextVotes(voter) == ballot) {
      (ballot, (1.0 - probFallOff) + probFallOff * prob)
    } else {
      (ballot, probFallOff * prob)
    })))
  }


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

  def allElections(): LazyList[Election] = allElections(voters)


  type Tally = Map[Ballot, Int]

  def winner(e: Tally): Set[Candidate] = winner(e.toList.flatMap((b, v) => List.fill(v)(b)))
  // intelijj bad type infrenece on winner

  // TODO something cleaner?
  private val winnerCache2 = scala.collection.mutable.Map[Tally, Set[Candidate]]()
  def winnerFast(e: Tally): Set[Candidate] = {
    winnerCache2.getOrElseUpdate(e,winner(e))
  }





  // TODO faster memize
  private def allTally(cs: List[Ballot], sum: Int): IndexedSeq[Tally] = {
    cs match {
      case List(b) => IndexedSeq(Map(cs.head -> sum))
      case b :: otherBallots => for {votes <- Range(0, sum + 1)
                                     rest <- allTally(otherBallots, sum - votes)
                                     } yield rest + (b -> votes)
    }
  }

  lazy val allTally: IndexedSeq[Tally] = allTally(allBallots, voters)

  /// all tallies (voters-1), where the next voter can change the outcome
  lazy val allPivotalSubTally: IndexedSeq[Tally] = allTally(allBallots, voters - 1).filter(t => allBallots.map(b => winner(t + (b -> (t(b) + 1)))).toSet.size > 1)


  // elections whos outcome will change based on the first vote
  //  lazy val pivitalElections: LazyList[List[Ballot]] = {
  //    val possibleRests = allElections(voters - 1)
  //
  //    var out = List[List[Ballot]]()
  //
  //    for {rest <- possibleRests}{
  //
  //
  //      val winners = for {b <- allBallots.to(LazyList)} yield winner(b :: rest)
  //        if(!winners.forall(_ == winners.head)){
  //          out = rest :: out
  //        }
  //    }
  //
  //    out.to(LazyList)
  //  }

}


class Plurality(voters: Int, options: Int) extends VotingSys(voters, options) {

  // TOOD constain as needed
  type Candidate = Int
  type Ballot = Candidate

  def winner(e: Election): Set[Candidate] = {

    val counts = e.groupBy(i => i).map((x, y) => (x, y.size))
    val mostvotes = counts.maxBy(_._2)._2

    counts.filter(_._2 == mostvotes).keySet
  }

  lazy val allBallots: List[Ballot] = {
    Range(0, options).toList
  }

}


class InstantRunOff(voters: Int, options: Int) extends VotingSys(voters, options) {

  // TOOD constain as needed
  type Candidate = Int
  type Ballot = List[Candidate]

  // when the number of candidates is nearly eaqual yto the number of voters, pidgeon hole effects happen.  this will keep trying to break ties.
  def removeCandidate(e: List[List[Candidate]], candidaates: Set[Candidate], depth: Int): Set[Candidate] = {

    if (e(0).size <= depth) {
      candidaates
    } else {
      val counts = e.map(_(depth)).groupBy(c => c).map((c, l) => (c, l.size))

      val fullCounts = candidaates.map(c => (c, counts.getOrElse(c, 0))).toMap

      val minCount = fullCounts.minBy(_._2)._2

      val next = fullCounts.filter((c, counts) => counts == minCount).keys.toSet

      if (next.size == 1) {
        next
      } else {
        removeCandidate(e, next, depth + 1)
      }
    }
  }

  def winnerHelper(e: List[List[Candidate]], candidaates: Set[Candidate]): Set[Candidate] = {
    if (e(0).size == 0) {
      return Set()
    }

    val firstVotes1 = e.groupBy(x => x(0)).mapValues(v => v.size)

    //and 0 to the others
    val firstVotes = candidaates.map(c => (c, firstVotes1.getOrElse(c, 0))).toMap

    //    println("firstvotes")
    //    println(firstVotes.toList)
    val win = firstVotes.filter(_._2 > voters.toDouble / 2.0)
    //    println(win.toList)

    if (win.size > 0) {
      return win.keySet.toSet
    } else {
      //      println("runn off")
      val candidatesToRemovw = removeCandidate(e, candidaates, 0)

      val runnoff = e.map(_.filter(c => candidatesToRemovw.find(c == _).size == 0))
      //      println("new ballot")
      //      println(runnoff)

      val res = winnerHelper(runnoff, candidaates -- candidatesToRemovw)
      if (res.size > 0) {
        return res
      } else {
        // in the case of a tye runoff, jsut use the first vote getters

        val mostvotes = firstVotes.maxBy(_._2)._2

        return firstVotes.filter(_._2 == mostvotes).keySet.toSet
      }
    }
  }


  override def winner(e: Election): Set[Candidate] = {
    winnerHelper(e, Range(0, options).toSet)
  }




  lazy val allBallots: List[Ballot] = {
    Range(0, options).toList.permutations.toList
  }

}


class RankBySum(voters: Int, options: Int) extends VotingSys(voters, options) {

  // TOOD constain as needed
  type Candidate = Int
  type Ballot = List[Candidate]


  override def winner(e: Election): Set[Candidate] = {
    val dd = e.flatMap(b => b.zipWithIndex.map((candidate, position) => (candidate, options - position - 1))).groupBy((c, _) => c).map((c, v) => (c, v.map(_._2).sum))
    val mostVotes = dd.maxBy(_._2)._2

    dd.filter(_._2 == mostVotes).map(_._1).to(Set)
  }

  lazy val allBallots: List[Ballot] = {
    Range(0, options).toList.permutations.toList
  }

}

//TODO test manually
class InstantRunOffReomveLeastSum(voters: Int, options: Int) extends VotingSys(voters, options) {

  // TOOD constain as needed
  type Candidate = Int
  type Ballot = List[Candidate]


  def winnerHelper(e: List[List[Candidate]], candidaates: Set[Candidate], sum: Map[Candidate, Int]): Set[Candidate] = {
    if (e(0).size == 0) {
      return Set()
    }

    val firstVotes1 = e.groupBy(x => x(0)).mapValues(v => v.size)

    //and 0 to the others
    val firstVotes = candidaates.map(c => (c, firstVotes1.getOrElse(c, 0))).toMap

    //    println("firstvotes")
    //    println(firstVotes.toList)
    val win = firstVotes.filter(_._2 > voters.toDouble / 2.0)
    //    println(win.toList)

    if (win.size > 0) {
      return win.keySet.toSet
    } else {

      //      println("runn off")
      val numLeastSumm = sum.minBy(_._2)._2
      //      println(numLeastFirstVotes)
      val candidatesToRemovw = sum.filter(_._2 == numLeastSumm).keys.toList
      //      println(candidatesToRemovw)

      val runnoff = e.map(_.filter(c => candidatesToRemovw.find(c == _).size == 0))
      //      println("new ballot")
      //      println(runnoff)

      val res = winnerHelper(runnoff, candidaates -- candidatesToRemovw, sum -- candidatesToRemovw)
      if (res.size > 0) {
        return res
      } else {
        // in the case of a tye runoff, jsut use the first vote getters

        val mostvotes = firstVotes.maxBy(_._2)._2

        return firstVotes.filter(_._2 == mostvotes).keySet.toSet
      }
    }
  }


  override def winner(e: Election): Set[Candidate] = {
    winnerHelper(e, Range(0, options).toSet,
      e.flatMap(b => b.zipWithIndex.map((candidate, position) => (candidate, options - position - 1))).groupBy((c, _) => c).map((c, v) => (c, v.map(_._2).sum)))
  }

  lazy val allBallots: List[Ballot] = {
    Range(0, options).toList.permutations.toList
  }

}


class Approval(voters: Int, options: Int) extends VotingSys(voters, options) {

  // TOOD constain as needed
  type Candidate = Int
  type Ballot = List[Boolean]


  override def winner(e: Election): Set[Candidate] = {
    val dd = e.map(_.zipWithIndex.filter((v, c) => v)).flatten.groupBy(_._2).map((c, l) => (c, l.size))

    if (dd.size == 0) {
      Range(0, options).toSet
    } else {

      val mostVotes = dd.maxBy(_._2)._2

      dd.filter(_._2 == mostVotes).map(_._1).to(Set)
    }
  }

  def allBallotsf(v: Int): List[Ballot] = {
    if (v == 0) {
      List(List())
    } else {
      val rests = allBallotsf(v - 1)
      for {b <- List(true, false)
           rest <- rests
           } yield b :: rest
    }
  }

  lazy val allBallots: List[Ballot] = {
    allBallotsf(options)
  }

}

// TODO there are many resolution criterai that could be studied: win in 1 round by by least sum of vote positions
// run off with least first place removed, run off with most last place removed, run off with most position count removed
// requier more then 50% support...


def main3(): Unit = {

  val numVotesr = 4
  val numOptions = 4

  val election = InstantRunOff(numVotesr, numOptions)

  //println(election.winner(List(List(0,1,2),List(0,1,2),List(1,0,2),List(1,0,2),List(2,0,1))))

  //println(election.allBallots)
  //println(election.allElections.size)


  val voters = Seq.fill(numVotesr)(Seq.fill(numOptions)(Random.nextDouble()))
  //val voters = List(List(0.8448046022556366, 0.35468166475319185, 0.6227897425995516), List(0.20983807345379102, 0.6405377223702815, 0.6494095953406748), List(0.0015109866494222857, 0.7970935133931731, 0.4001702165080565), List(0.24621471321625188, 0.25779627370523306, 0.7811329970698041), List(0.6385039043271907, 0.9032141404489262, 0.6705143353752466), List(0.9659238381880431, 0.1249433817210781, 0.6819527652117311))

  println(voters)
  //val voters = List(List(0.7155237932298478, 0.9428369677018246, 0.08898937689290554, 0.8456904365593594), List(0.24828963026960715, 0.5283198600998977, 0.8353706206462521, 0.9707476532135482), List(0.2749787467645757, 0.3496408704246007, 0.5120396282998364, 0.09611124094347578), List(0.10163955132484959, 0.23355449919013538, 0.9472414104635279, 0.35386847365191765), List(0.16203065946356054, 0.32913220594685766, 0.2779247637589507, 0.6400427195766016), List(0.957142024648165, 0.6190048364369548, 0.5982902326289554, 0.6822937050104824))
  //List(List(0.7998139911563363, 0.9598790204037694, 0.7196849436076475, 0.8395395520553166), List(0.3731212406517461, 0.7600402313526347, 0.6967361000439918, 0.7251618413781908), List(0.7938171573565171, 0.416444545785444, 0.4470620972177066, 0.8898349187957365), List(0.5479007196890486, 0.56824061708372, 0.4640401073079, 0.693134700258453), List(0.6173016871988352, 0.7971275188267796, 0.28454446586209137, 0.7954702695918969), List(0.694592941977278, 0.5234305999674427, 0.7183346762458745, 0.1646795724906459))

  for (voter <- voters) {
    println(voter)
  }


  //  println(Plurality(4,3).winner(List(1,1,2)))
  //  println(Plurality(4,3).allBallots)
  //  println(election.allElections)
  //println(election.allElections().size)

  val probFallOff = 0.9
  var publicProbs: Seq[Map[election.Ballot, Double]] = Seq.fill(numVotesr)(election.allBallots.map(b => (b, 1.0 / election.allBallots.size)).toMap)

  println(publicProbs)

  for (i <- Range(1, 100)) {
    publicProbs = election.NextProb(publicProbs, voters, (1.0 - 1.0 / i.toDouble))._2
  }


}


def main6(): Unit = {
  val r = new scala.util.Random

  val numVotesr = 3
  val numOptions = 3

  val election = RankBySum(numVotesr, numOptions)

  election.winner(List(List(0, 1, 2), List(0, 2, 1)))
}


def main7(): Unit = {
  val r = new scala.util.Random

  val numVotesr = 5
  val numOptions = 3

  val election = RankBySum(numVotesr, numOptions)

  //println(election.winner(List(List(0,1,2),List(0,1,2),List(1,0,2),List(1,0,2),List(2,0,1))))

  println(election.allBallots.size)
  //println(election.allElections.size)


  val voters = Seq.fill(numVotesr)(Seq.fill(numOptions)(Random.nextDouble()))
  //val voters = List(List(0.8448046022556366, 0.35468166475319185, 0.6227897425995516), List(0.20983807345379102, 0.6405377223702815, 0.6494095953406748), List(0.0015109866494222857, 0.7970935133931731, 0.4001702165080565), List(0.24621471321625188, 0.25779627370523306, 0.7811329970698041), List(0.6385039043271907, 0.9032141404489262, 0.6705143353752466), List(0.9659238381880431, 0.1249433817210781, 0.6819527652117311))

  println(voters)
  //val voters = List(List(0.7155237932298478, 0.9428369677018246, 0.08898937689290554, 0.8456904365593594), List(0.24828963026960715, 0.5283198600998977, 0.8353706206462521, 0.9707476532135482), List(0.2749787467645757, 0.3496408704246007, 0.5120396282998364, 0.09611124094347578), List(0.10163955132484959, 0.23355449919013538, 0.9472414104635279, 0.35386847365191765), List(0.16203065946356054, 0.32913220594685766, 0.2779247637589507, 0.6400427195766016), List(0.957142024648165, 0.6190048364369548, 0.5982902326289554, 0.6822937050104824))
  //List(List(0.7998139911563363, 0.9598790204037694, 0.7196849436076475, 0.8395395520553166), List(0.3731212406517461, 0.7600402313526347, 0.6967361000439918, 0.7251618413781908), List(0.7938171573565171, 0.416444545785444, 0.4470620972177066, 0.8898349187957365), List(0.5479007196890486, 0.56824061708372, 0.4640401073079, 0.693134700258453), List(0.6173016871988352, 0.7971275188267796, 0.28454446586209137, 0.7954702695918969), List(0.694592941977278, 0.5234305999674427, 0.7183346762458745, 0.1646795724906459))

  for (voter <- voters) {
    println(voter)
  }


  //  println(Plurality(4,3).winner(List(1,1,2)))
  //  println(Plurality(4,3).allBallots)
  //  println(election.allElections)
  //println(election.allElections().size)

  val probFallOff = 0.9
  var publicProbs: Seq[Map[election.Ballot, Double]] = Seq.fill(numVotesr)(election.allBallots.map(b => (b, 1.0 / election.allBallots.size)).toMap)

  println(publicProbs)

  for (i <- Range(1, 100)) {
    publicProbs = election.NextProb(publicProbs, voters, (1.0 - 1.0 / i.toDouble))._2
  }
}


def main8(): Unit = {
  val r = new scala.util.Random

  val numVotesr = 3
  val numOptions = 3

  val election = Approval(numVotesr, numOptions)

  println(election.allBallots)

  println(election.winner(List(List(true, true, false), List(true, false, false))))
}

def main9(): Unit = {
  val r = new scala.util.Random

  val numVotesr = 4
  val numOptions = 4

  val election = Approval(numVotesr, numOptions)

  //println(election.winner(List(List(0,1,2),List(0,1,2),List(1,0,2),List(1,0,2),List(2,0,1))))

  println(election.allBallots.size)
  //println(election.allElections.size)


  val voters = Seq.fill(numVotesr)(Seq.fill(numOptions)(Random.nextDouble()))
  //val voters = List(List(0.8448046022556366, 0.35468166475319185, 0.6227897425995516), List(0.20983807345379102, 0.6405377223702815, 0.6494095953406748), List(0.0015109866494222857, 0.7970935133931731, 0.4001702165080565), List(0.24621471321625188, 0.25779627370523306, 0.7811329970698041), List(0.6385039043271907, 0.9032141404489262, 0.6705143353752466), List(0.9659238381880431, 0.1249433817210781, 0.6819527652117311))

  println(voters)
  //val voters = List(List(0.7155237932298478, 0.9428369677018246, 0.08898937689290554, 0.8456904365593594), List(0.24828963026960715, 0.5283198600998977, 0.8353706206462521, 0.9707476532135482), List(0.2749787467645757, 0.3496408704246007, 0.5120396282998364, 0.09611124094347578), List(0.10163955132484959, 0.23355449919013538, 0.9472414104635279, 0.35386847365191765), List(0.16203065946356054, 0.32913220594685766, 0.2779247637589507, 0.6400427195766016), List(0.957142024648165, 0.6190048364369548, 0.5982902326289554, 0.6822937050104824))
  //List(List(0.7998139911563363, 0.9598790204037694, 0.7196849436076475, 0.8395395520553166), List(0.3731212406517461, 0.7600402313526347, 0.6967361000439918, 0.7251618413781908), List(0.7938171573565171, 0.416444545785444, 0.4470620972177066, 0.8898349187957365), List(0.5479007196890486, 0.56824061708372, 0.4640401073079, 0.693134700258453), List(0.6173016871988352, 0.7971275188267796, 0.28454446586209137, 0.7954702695918969), List(0.694592941977278, 0.5234305999674427, 0.7183346762458745, 0.1646795724906459))

  for (voter <- voters) {
    println(voter)
  }


  //  println(Plurality(4,3).winner(List(1,1,2)))
  //  println(Plurality(4,3).allBallots)
  //  println(election.allElections)
  //println(election.allElections().size)

  val probFallOff = 0.9
  var publicProbs: Seq[Map[election.Ballot, Double]] = Seq.fill(numVotesr)(election.allBallots.map(b => (b, 1.0 / election.allBallots.size)).toMap)

  //println(publicProbs)

  for (i <- Range(1, 100)) {
    publicProbs = election.NextProb(publicProbs, voters, (1.0 - 1.0 / i.toDouble))._2
  }
}

//val numVotesr = 6 //val numOptions = 3
//val numVotesr = 4 //val numOptions = 4


def main10(): Unit = {

  val numVotesr = 6
  val numOptions = 3
  val election = InstantRunOff(numVotesr, numOptions)

  println(election.winner(List(List(0, 1, 2), List(2, 0, 1), List(0, 1, 2), List(0, 1, 2), List(2, 1, 0), List(2, 1, 0))))
}


def main11(): Unit = {

  val numVoter = 20
  val numOptions = 3
  val election = Plurality(numVotesr, numOptions)

  println(election.allTally)
  println(election.allPivotalSubTally)
}


@main
def main54545(): Unit = {

  val numVoter = 4
  val numOptions = 4
  val election = InstantRunOff(numVotesr, numOptions)

  val votesr = List(
    List(0, 1, 2, 3),
    List(1, 2, 0, 3),
    List(2, 3, 0, 1),
    List(3, 0, 2, 1),
  )

  println(election.removeCandidate(votesr, Set(0, 1, 2, 3), 0))

}






//todo investigate rank choice
//todo: make much more efficeint with pivots? probably need to do sumething like the sum of ballots to be scalable