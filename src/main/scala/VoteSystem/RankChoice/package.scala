package VoteSystem

import VoteSystem.{VoteSystem, VoteSystemFancy}
import prob.Dist

package object RankChoice {

  trait RankChoice extends VoteSystem {

    type Ballot = List[Candidate]

    lazy val allBallots: List[Ballot] =
      Range(0, NunCandidates).toList.permutations.toList

  }

}
