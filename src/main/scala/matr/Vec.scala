package matr

import matr.{Matrix, MatrixFactory}

import scala.annotation.targetName
import scala.compiletime.ops.boolean.&&
import scala.compiletime.ops.int.*
import scala.math.*
// Bring bundled implementations in scope
import matr.MatrBundle.given



type Vec[X <: Int, P] = Matrix[X, 1, P]

extension [X <: Int, P](a: Vec[X, P])
  def apply(i:Int): P = a(i,0)


extension[X <: Int, P] (a: Vec[X, P] )
  def forAll(p: P => Boolean): P = {
???
  }


object Vec {
//  def fromTuple[P](p: P)(using Numeric[P]): Vec[1, P] = MatrixFactory[1, 1, P].fromTuple(Tuple1(Tuple1(p)))
//
//  def fromTuple[P](x: P, y: P)(using Numeric[P]): Vec[2, P] = MatrixFactory[2, 1, P].fromTuple((Tuple1(x), Tuple1(y)))
//
//  def fromTuple[P](x: P, y: P, z: P)(using Numeric[P]): Vec[3, P] = MatrixFactory[3, 1, P].fromTuple((Tuple1(x), Tuple1(y), Tuple1(z)))
//  //  fromTuple[X <: Int, P]() //TODO generalize


//  def repeat[Y <: Int, X <: Int, P](v: Vec[X, P])
//                                   (using Numeric[P])
//                                   (using Matrix.Requirements.NonNegativeDimensions[X, Y])
//                                   (using ValueOf[X])(using ValueOf[Y]): Matrix[X, Y, P] =
//    MatrixFactory[X, Y, P].tabulate((x, _) => v(x, 0))

//  def flatten[X <: Int, Y <: Int, A](m: Matrix[X, Y, A])(using vX:ValueOf[X])(using Numeric[A],Matrix.Requirements.NonNegativeDimensions[X * Y, 1],ValueOf[X * Y]): Vec[X * Y, A] = {
//    val ls = matrixToList(m)
//    MatrixFactory[X * Y, 1, A].tabulate((x, y) => ls(y * vX.value + x))
//  }
}