
import scala.collection.mutable.{ArrayBuffer, ListBuffer}
import scala.util.Random


@main
def scratch3(): Unit = {

  println("???")
  val numVotesr = 8
  val numOptions = 5
  val samples = 1000000

  val election = Approval(numVotesr, numOptions)

  import election.{Ballot, Candidate}

  // estimate prefferences from an honest ballot


//TODO could be radically faster if took advantage of candidate symetry

var record  = Map[Ballot,List[Seq[Double]]]().withDefault(_ => List())

  for (_ <- Range(0, samples)) {
    val voter = Seq.fill(numOptions)(Random.nextDouble())
      .sortBy(x => x) //TODO remove

    val res = election.nieve(voter.zipWithIndex.map(_.swap).toMap)

    record += (res -> (record(res)  ++ List(voter) ))

  }

  val out = record.map((b,vs) => (b,Range(0,numOptions).map(i => vs.map(_(i)).sum/vs.size.toDouble )))


//  election.allBallots.map(b => println(s"$b => ${out(b)} \n     ${election.estimatedNievePrefference(b)}"))



  election.allBallots.map(b => println(s"$b => ${record(b)} "))
  println()
election.allBallots.map(b => println(s"$b => ${out.withDefault(_ => List())(b)} "))

  //Plurality
// BEST =  1 - 1 / 6
//  REST = (1 / 6 + 2 / 6 + 3 / 6 + 4 / 6) / 4 = (1+2+3+4) /6  /4
// =  (1+2+3...numOptions-2)/ ((1+numOptions)(numOptions-1)


  //(numOptions+1)
}
