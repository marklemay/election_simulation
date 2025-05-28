package matr

import matr.{Matrix, MatrixFactory}

import scala.annotation.targetName
import scala.compiletime.ops.boolean.&&
import scala.compiletime.ops.int.>
import scala.math.{Numeric, *}
// Bring bundled implementations in scope
import matr.MatrBundle.given


extension [X <: Int, Y <: Int](a: Matrix[X,  Y, Double])
  def ham(b: Matrix[X, Y, Double])(using Matrix.Requirements.NonNegativeDimensions[X, Y], ValueOf[X], ValueOf[Y]): Matrix[X, Y, Double] =
    MatrixFactory[X, Y, Double].tabulate((x, y) => a(x, y) * b(x, y))

  @targetName("Hammpond product")
  infix def ⊙(b: Matrix[X, Y, Double])(using Matrix.Requirements.NonNegativeDimensions[X, Y], ValueOf[X], ValueOf[Y]): Matrix[X, Y, Double] = a.ham(b)

  @targetName("dot product")
  infix def ⋅[Z<: Int](b: Matrix[Y, Z, Double])(using Matrix.Requirements.NonNegativeDimensions[X, Z],ValueOf[X], ValueOf[Y], ValueOf[Z]) = a.dot(b)

  def unary_-(using Matrix.Requirements.NonNegativeDimensions[X, Y], ValueOf[X], ValueOf[Y]) = a.map(x => -x)

  def sum():Double = {
    var out = 0.0
    a.iterate((x,y) => {out += a(x,y)})
    out
  }




// TODO matrix should be for eachable!

extension (a: Double)
  def *[X <: Int, Y <: Int](b: Matrix[X, Y, Double])(using Matrix.Requirements.NonNegativeDimensions[X, Y], ValueOf[X], ValueOf[Y]): Matrix[X, Y, Double] = b.map(a*_)

extension[X <: Int, Y <: Int] (a: Matrix[X, Y, Int] )
  def min: Int = {

    var out = Int.MaxValue
    a.iterate((x,y) => {
      if(out >a(x,y)){out = a(x,y)} })
    out
  }



extension[X <: Int, Y <: Int, T] (a: Matrix[X, Y, T] )
  def rows(using Matrix.Requirements.NonNegativeDimensions[X, Y], ValueOf[X], ValueOf[Y]): Seq[List[T]] = 
    Range(0, a.rowDim).map(i => Range(0, a.colDim).map(j => a(i,j) ).toList)
    
//extension[X <: Int, Y <: Int, T] (a: Matrix[X, Y, T] )
//  def rows(using Matrix.Requirements.NonNegativeDimensions[X, Y], ValueOf[X], ValueOf[Y]): Seq[Vec[Y, T]] = {
//    for {r <- Range(0,a.rowDim)}{
//
//    } yields {  MatrixFactory[1,a.colDim, T].tabulate( (_, c) => a(r,c))}
//    ???
//  }


//  coersion of 1x1 mats to underling value
//given unbox[P]: Conversion[Matrix[1, 1, P], P] with
//  def apply(a: Matrix[1, 1, P]): P = a(0,0)

//given box[P](using Numeric[P]): Conversion[P, Matrix[1, 1, P]] with
//  def apply(a: P): Matrix[1, 1, P] = fromTuple[P](a)




//  def circumference: Double = c.radius * math.Pi * 2

//def zip[X <: Int, Y <: Int, A, B](a : Matrix[X, Y, A])(b : Matrix[X, Y, B]): Matrix[X, Y, (A, B)] =
//  MatrixFactory[X, Y, (A, B)].tabulate((x, y) => (a(x)(y), b(x)(y)))

// Hammpond product



def matrixToList[X <: Int, Y <: Int, A](m: Matrix[X, Y, A]): List[A] = {
  var out = List[A]()
  var c: Int = 0
  while c < m.colDim do
    var r: Int = 0
    while r < m.rowDim do
      out = out ++ List(m(r, c))
      r = r + 1
    c = c + 1
  out
}

//def matrixFromList[X <: Int, Y <: Int, A](w: List[A])(using Matrix.Requirements.NonNegativeDimensions[X, Y], Numeric[A])(using vX : ValueOf[X], vY: ValueOf[Y]): (Matrix[X, Y, A], List[A]) = {
//  (MatrixFactory[X, Y, A].tabulate((x, y) => w(y * vX.value + x)), w.drop(vX.value * vY.value))
//}



// TODO: delete for above
def matrixToDoubles[X <: Int, Y <: Int](m: Matrix[X, Y, Double]): List[Double] = {
  var out = List[Double]()
  var c: Int = 0
  while c < m.colDim do
    var r: Int = 0
    while r < m.rowDim do
      out = out ++ List(m(r, c))
      r = r + 1
    c = c + 1
  out
}

def matrixFromDoubles[X <: Int, Y <: Int](w: List[Double])(using Matrix.Requirements.NonNegativeDimensions[X, Y])(using vX : ValueOf[X], vY: ValueOf[Y]): (Matrix[X, Y, Double], List[Double]) = {

  (MatrixFactory[X, Y, Double].tabulate((x, y) => w(y * vX.value + x)), w.drop(vX.value * vY.value))
}



//  val expected




//
//  val ex: Layer[3, 2] = Layer.rand(3, 2)
//  val in = Vec.fromTuple(-100.0, -100.0, -100.0)
//
//  val out = ex(in)
//
//  println(out.mkString)
//
//  val a: Matrix[2, 4, Double] =
//    MatrixFactory[2, 4, Int].fromTuple(
//      (0, 8, 15, 0),
//      (4, 7, 1, 1)
//    ).map(_.toDouble)
//
//
//  println(a.mkString)
//
//  val ls = matrixToDoubles(a)
//  println(ls.toString())
//  val (m,ls2) = matrixFromDoubles[2, 4](ls++List(11.0,12.0))
//
//  println(m.mkString)
//  println(ls2.toString)
//
//  println(Vec.repeat[4, 3, Double](Vec.fromTuple(1.0, 2.0, 3.0)).mkString)

//
//  println(s"${sig(-100)},${sig(0)},${sig(100)}")
//
//  val a: Matrix[2, 4, Double] =
//    MatrixFactory[2, 4, Double].tabulate((_, _) => scala.util.Random.nextDouble())
//
//  val b: Vec[4, Double] =
//    MatrixFactory[4, 1, Double].tabulate((_, _) => scala.util.Random.nextDouble())
//
//  println(a.mkString)
//  println((a dot b).mkString)
//
//  //  println(s"Hello world! ${a.det}")
//  //  println((a dot a).mkString)
//
//
//  //  println(a.inv.mkString)
//
//  //  println(s"Hello world! ${3.0 * a}")
//  //  println(s"Hello world! ${a * a}")
//
//  //  println(s"Hello world! ${a times 3.0}")
//
//  val x = 3
//  var y = 4
//  y += 1
//  println(s"Hello world! ${x + y}")