package arbitrary

import shapeless._
import shapeless.ops.{coproduct, hlist, nat}
import org.scalacheck._
import shapeless.ops.nat.ToInt


object Run extends App {

  // Length of types
  println(Nat.toInt[Nat._4])
  val hlistLen = hlist.Length[String :: Int :: Boolean :: Int :: HNil]
  val coProdLen = coproduct.Length[Double :+: Char :+: CNil]
  println(Nat.toInt[hlistLen.Out])

  println(Nat.toInt[coProdLen.Out])


  //sizeof
  sealed trait SizeOf[A] {
    def value: Int
  }

  implicit def genericSizeOf[A, L <: HList, N <: Nat](implicit generic: Generic.Aux[A, L],
                                                      size: hlist.Length.Aux[L, N],
                                                      sizeToInt: nat.ToInt[N]
                                                     ): SizeOf[A] = new SizeOf[A] {
    val value = sizeToInt.apply()
  }

  def sizeOf[A](implicit size: SizeOf[A]): Int = size.value

  case class IceCream(name: String, numCherries: Int, inCone: Boolean)

  println(sizeOf[IceCream])

  //random values

  for (_ <- 1 to 3) println(Arbitrary.arbitrary[Int].sample)


  //basic definitions
  sealed trait Random[A] {
    def get: A
  }

  def random[A](implicit r: Random[A]): A = r.get

  def createRandom[A](func: () => A): Random[A] = new Random[A] {
    def get = func()
  }

  //simple instances
  implicit val intRandom: Random[Int] = createRandom(() => scala.util.Random.nextInt(10))
  implicit val charRandom: Random[Char] = createRandom(() => ('A'.toInt + scala.util.Random.nextInt(26)).toChar)
  implicit val boolRandom: Random[Boolean] = createRandom(() => scala.util.Random.nextBoolean())

  for (_ <- 1 to 3) println(random[Int])

  //random products
  implicit def genericRandom[A, R](implicit gen: Generic.Aux[A, R],
                                   random: Lazy[Random[R]]): Random[A] = createRandom(() => gen.from(random.value.get))

  implicit val hnilRandom: Random[HNil] = createRandom(() => HNil)

  implicit def hlistRandom[H, T <: HList](implicit hRandom: Lazy[Random[H]], tRandom: Random[T]): Random[H :: T] =
    createRandom(() => hRandom.value.get :: tRandom.get)

  case class Cell(col: Char, row: Int)

  for (_ <- 1 to 3) println(random[Cell])

  //random coproducts

  implicit val cnilRandom: Random[CNil] = createRandom(() => throw new Exception("Should not happen"))

  implicit def coProdRandom[H, T <: Coproduct, L <: Nat](implicit
                                                         hRandom: Lazy[Random[H]],
                                                         tRandom: Random[T],
                                                         tLength: coproduct.Length.Aux[T, L],
                                                         tLengthAsInt: ToInt[L]): Random[H :+: T] =
    createRandom { () =>
      val length = 1 + tLengthAsInt()
      val chooseH = scala.util.Random.nextDouble < (1.0 / length)
      if (chooseH) Inl(hRandom.value.get) else Inr(tRandom.get)
    }

  sealed trait Light
  case object Red extends Light
  case object Green extends Light
  case object Blue extends Light

  for(_ <- 1 to 10) println(random[Light])

  case class Error(msg: String)
//  implicit val strRandom: Random[String] = createRandom(() => java.util.UUID.randomUUID().toString)

  for(_ <- 1 to 2) println(random[Error])
}
