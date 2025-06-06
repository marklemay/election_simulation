package VoteSystem

import matr.{Matrix, MatrixFactory}
import matr.rows
import prob.Dist
import matr.MatrBundle.defaultMatrixFactory
import matr.min


// https://en.wikipedia.org/wiki/STAR_voting
// 0 to 5
class Star[NunCandidates <: Int]
(using
 Matrix.Requirements.NonNegativeDimensions[NunCandidates, NunCandidates], // TODO this should be refactored hard...
 // NunCandidates > 0,
 ValueOf[NunCandidates])
  extends VoteSystemFancy,
    VoteSystem(valueOf[NunCandidates]) {
  type Ballot = List[Int]

  val StarBpund = 5 + 1

  private def allBallotsHelper(c: Int): Set[List[Int]] = {
    if (c < 0) {
      Set(List())
    } else {
      val others = allBallotsHelper(c - 1)
      for {other <- others
           thisStar <- Range(0, StarBpund)} yield {
        other.updated(c, thisStar)
      }
    }

  }

  override lazy val allBallots: Set[List[Int]] = allBallotsHelper(valueOf[NunCandidates] - 1)

  type Aggregate = (List[Int], Matrix[NunCandidates, NunCandidates, Int])

  def aggregate(e: Seq[Ballot]): Aggregate = {

    val totals = candidates.map(c => e.map(b => b(c)).sum).toList
    val runOffs =
      MatrixFactory[NunCandidates, NunCandidates, Int].tabulate((x, y) => e.map(ballot =>
        if (ballot(x) > ballot(y)) {
          -1
        } else if (ballot(x) < ballot(y)) {
          1
        } else 0
      ).sum
      )

    (totals, runOffs)
  }

  // TODO clean up
  // https://www.starvoting.org/ties is a bit under specified

  def CountMatchUpWins(v: List[Int], cs: Set[Candidate]): Int = v.zipWithIndex.map((v,c) => if (cs.contains(c) && v > 0) {
    1
  } else {
    0
  }).sum

  def scoringTie(cs: Set[Candidate], m: Matrix[NunCandidates, NunCandidates, Int], target: Int): Set[Candidate] = {
    val zzz = cs.map(c => (c, CountMatchUpWins(m.rows(c), cs))).toMap

    val worstMatchup = zzz.maxBy(_._2)._2

    val worst = cs.filter(c => zzz(c) == worstMatchup)

    val potentialNext = cs -- worst

    if (potentialNext.size == target) {
      return potentialNext
    } else if (potentialNext.size < target) {
      // TODO maybe you do stp 3 here, it is unclear
      return cs // no further tie resolution
    } else {
      return scoringTie(potentialNext, m, target)
    }
  }


//need to be a dist since in some instance ties are broke randomly
  def scoring(a: Aggregate): Dist[Set[Candidate]] = {
    val (totals, runOffs) = a

    val m = totals.max

    val tops = totals.zipWithIndex.filter((s, c) => s == m).map((s, c) => c)

    tops.toList match {
      case List(top) => {

        val m2 = (totals.toSet - m).max
        val tops2 = totals.zipWithIndex.filter((s, c) => s == m2).map((s, c) => c)

        if (tops2.size == 1) {
          return Dist.Exactly(tops.toSet ++ tops2)
        } else if (tops2.size > 1) {
          val breaks = scoringTie(tops2.toSet, runOffs, 1)
          if (breaks.size == 1) {
            return Dist.Exactly(tops.toSet ++ breaks)
          } else {
            // a tie can not be resolved as above, the tie will be broken randomly
            val xxx = for {break <- breaks} yield {
              Set(top, break)
            }
            return Dist.Uniform(xxx)
          }
        }
        ???
      }
      case List(top, top2) => return Dist.Exactly(Set(top, top2))

      case _
      => {
        val breaks = scoringTie(tops.toSet, runOffs, 2)
        if (breaks.size == 2) {
          return Dist.Exactly(breaks)
        } else {
          // every equally combo eq weighted
          val xxx = for {a <- breaks
                         b <- breaks
                         if a != b} yield {
            Set(a, b)
          }
          return Dist.Uniform(xxx)
        }
      }
    }

  }


  def winner(a: Aggregate): Dist[Candidate] = {

    val (totals, runOffs) = a
    val tops = scoring(a)


    // meh
    object SetExtractor {
      def unapplySeq[T](s: Set[T]): Seq[T] = s.toSeq
    }

    tops.flatMap(s =>
      val SetExtractor(top1,top2) = s

      val headtoHead = runOffs(top2, top1)
      if (headtoHead > 0) {
        Dist.Exactly(top1)
      }
      else if (headtoHead < 0) {
        Dist.Exactly(top2)
      } else {
        // TODO skipping step 3 for now
        if(totals(top1)> totals(top2)) {
          Dist.Exactly(top1)
        }else
          if(totals(top1) < totals(top2)) {
            Dist.Exactly(top2)
          } else {
            Dist.Uniform(Set(top1,top2))
          }
      }

      )

  }

}

@main
def main(): Unit = {
  println("Hello world!")
  val e = Star[3]
  //  println(e.allBallots.toList.sorted.mkString("\n"))
  //
  //  val e2 = Star[6]
  //  println(e2.allBallots.size)

  val a @ (t, m) = e.aggregate(List(

    List(3, 2, 2),
    List(1, 3, 2)
  ))
  println(t)
  println(m.mkString)

  println(e.scoring(a))
  println(e.winner(a))

  //  println( e.winner(List(
  //    List(1,2,3,4,0),
  //    List(1,4,3,2,0),
  //
  //    List(0,1,2,3,4),
  //  )))
  //  println( e.winner(List(
  //    List(0,2,3,4,1),
  //    List(1,2,3,4,0),
  //    List(3,2,4,0,1),
  //
  //  )))

}