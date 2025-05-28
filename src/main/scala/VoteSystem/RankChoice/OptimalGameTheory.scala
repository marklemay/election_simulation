package VoteSystem.RankChoice

import VoteSystem.{VoteSystem, VoteSystemFancy}
import matr.{Matrix, MatrixFactory}
import matr.rows
import prob.Dist
import matr.MatrBundle.defaultMatrixFactory
import matr.min
import optimus.optimization._
import optimus.optimization.MPModel
import optimus.optimization.enums.SolverLib
import optimus.optimization.model.MPFloatVar
import optimus.algebra.Int2Const


/// An Optimal Single-Winner Preferential Voting System Based on Game Theory
/// https://www.stat.uchicago.edu/~lekheng/meetings/mathofranking/ref/rivest.pdf
/// https://github.com/ron-rivest/game-theory-voting-system
class OptimalGameTheory[NunCandidates <: Int]
                                             (using
                                              Matrix.Requirements.NonNegativeDimensions[NunCandidates, NunCandidates], // TODO this should be refactored hard...
                                              // NunCandidates > 0,
                                              ValueOf[NunCandidates])
  extends RankChoice, VoteSystemFancy, VoteSystem(valueOf[NunCandidates])  {
  
  
  type Aggregate = Matrix[NunCandidates, NunCandidates, Int]
  // TODO could be even more restrictive

  def aggregate(e: Seq[Ballot]): Aggregate = {
    MatrixFactory[NunCandidates, NunCandidates, Int].tabulate((x, y) => e.map( ballot =>
      val c = ballot.indexOf(x).compare(ballot.indexOf(y))
      if (c > 0) { -1 } else if (c < 0) { 1} else 0
    ).sum
    )
  }


  def winner(a: Aggregate): Dist[Candidate] = {

    import ch.qos.logback.classic.{Level, Logger}
    import org.slf4j.LoggerFactory

    LoggerFactory
      .getLogger(org.slf4j.Logger.ROOT_LOGGER_NAME)
      .asInstanceOf[Logger]
      .setLevel(Level.OFF)


    implicit val model: MPModel = MPModel(SolverLib.oJSolver)

    val w = Math.max(1, -2*a.min)

    val m = a.map(i => -i + w)

    val vars = Range(0, valueOf[NunCandidates]).map(i => MPFloatVar(i.toString))

    // basic
    //minimize(vars.fold(Int2Const(0))(_ + _))

    // full GT
    minimize(vars.map(x => x * x).fold(Int2Const(0))(_ + _))

    add(w * vars.fold(Int2Const(0))(_ + _):= 1)

    // common constraints
    for (x <- vars) {
      add(x >:= 0)
    }

    // just a matrix multiply
    for (row <- m.rows){
      add(row.zipWithIndex.map((payoff, i) => vars(i) * payoff).fold(Int2Const(0))(_ + _) >:= 1)
    }


    start()

//    println(s"objective: $objectiveValue")
//
//    for (x <- vars) {
//      println(s"$x -> ${x.value.get * w} ")
//    }

    val out = Dist.Normalize(vars.zipWithIndex.map((x, c) => (c, x.value.get )).toMap)
    release() // can reales earlier
    out
  }


  //TODO property test against condorcet winners and losers
//  def winner(a: Aggregate): Dist[Candidate] = {
//
//    // "When there is a Condorcet winner, then it
//    //is easy to see that it is optimal to pick the Condorcet
//    //winner as the election winner."
//
//    // find condorcet winner if exists
//
//     // for each!!
//    for{
//      r <- Range(0,a.rowDim)
//      }{
//      var b = true
//      for {
//        c <- Range(0, a.colDim)
//      } {
//      if (r!=c && a(r,c) <= 0){
//        b= false
//      }
//      }
//      if(b){
//        return Dist.Uniform(Set(r))
//      }
//    }
//
//    //"When there is no Condorcet winner, there is a generalized tie, and the optimal strategy is to play according to an optimal mixed strategy. ...
//
//
//    ???
//
//  }

  //TODO test against examples in the paper

}



@main
def mainxvxcvxc(): Unit = {
  println("Hello world!")
  val e = OptimalGameTheory[5]
  println( e.aggregate(List(
    List(1,2,3,4,0),
    List(1,4,3,2,0),

    List(0,1,2,3,4),
  )).mkString)
  println()
  println( e.aggregate(List(
    List(0,2,3,4,1),
    List(1,2,3,4,0),
    List(3,2,4,0,1),

  )).mkString)


  println()
  println(e.winner(e.aggregate(List(
    List(1,2,3,4,0),
    List(1,4,3,2,0),

    List(0,1,2,3,4),

  ))))


  println()
  println(e.winner(e.aggregate(List(
    List(0,2,3,4,1),
    List(1,2,3,4,0),
    List(3,2,4,0,1),

  ))))
  println()
  println(e.aggregate(List(

    List(0,1,2,3,4),
    List(0, 2, 3, 4, 1),
    List(1, 2, 3, 4, 0),
    List(3, 2, 4, 0, 1),

  )).mkString)

  println(e.winner(e.aggregate(List(

    List(0,1,2,3,4),
    List(0, 2, 3, 4, 1),
    List(1, 2, 3, 4, 0),
    List(3, 2, 4, 0, 1),

  ))))

//  //ExaMPLE 1.1
//  val (a,b,c,d) = (0,1,2,3)
//  val e = OptimalGameTheory[4]
//  val ballots =
//    Range(0,40).map(_ => List(a,b,c,d))
//    ++ Range(0,30).map(_ => List(b,c,a,d))
//      ++ Range(0,20).map(_ => List(c,a,b,d))
//      ++ Range(0,10).map(_ => List(c,b,a,d))
//
//
//  val ag = e.aggregate(ballots)
//    println(ag.mkString)
//
//  println(e.winner(ag))

}