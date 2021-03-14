package webapp.core

import org.scalacheck._

import scala.annotation.tailrec

object Generator {
  var varsCount = 2
  val vars: Seq[Var[_]] = ('a' to 'z').map(_.toString).map(c => Var(c)).take(varsCount)

  val genVar: Gen[Var[_]] = Gen.oneOf(vars)

  def genApp(minLevel: Int, maxLevel: Int, minLevelAppl: Int, maxLevelAppl: Int, exactly: Boolean): Gen[Application[_]] = for {
    t1 <- genTerm(minLevel, maxLevel, minLevelAppl, maxLevelAppl, exactly)
    t2 <- genTerm(minLevel, maxLevel, maxLevelAppl, maxLevelAppl, exactly)
  } yield Application(t1, t2)

  def genAbs(minLevel: Int, maxLevel: Int, minLevelAppl: Int, maxLevelAppl: Int, exactly: Boolean): Gen[Abstraction[_]] = for {
    v <- genVar
    b <- genTerm(minLevel, maxLevel, minLevelAppl, maxLevelAppl, exactly)
  } yield Abstraction(v, b)

  def genTerm(minLevelAbs: Int, maxLevelAbs: Int, minLevelAppl: Int, maxLevelAppl: Int, exactly: Boolean): Gen[Term[_]] = {
    if (exactly)
      if (minLevelAbs >= maxLevelAbs && minLevelAppl >= maxLevelAppl) genVar
      else {
        if (minLevelAppl == maxLevelAppl)
          genAbs(minLevelAbs+1, maxLevelAbs, minLevelAppl, maxLevelAppl, exactly)
        else if (minLevelAbs == minLevelAbs)
          genApp(minLevelAbs, maxLevelAbs, minLevelAppl+1, maxLevelAppl, exactly)
        else Gen.lzy(Gen.oneOf(
          genAbs(minLevelAbs+1, maxLevelAbs, minLevelAppl, maxLevelAppl, exactly),
          genApp(minLevelAbs, maxLevelAbs, minLevelAppl+1, maxLevelAppl, exactly))
        )
      }
    else
      if (minLevelAbs >= maxLevelAbs || minLevelAppl >= maxLevelAppl) genVar
      else Gen.lzy(Gen.oneOf(
        genAbs(minLevelAbs+1, maxLevelAbs, minLevelAppl, maxLevelAppl, exactly),
        genApp(minLevelAbs, maxLevelAbs, minLevelAppl+1, maxLevelAppl, exactly))
      )

  }

  def getGeneratedTerm(LevelAbs: Int, LevelAppl: Int, exactly: Boolean): Term[Nothing] =
    genTerm(0,LevelAbs,0,LevelAppl-1, exactly).sample.get


  @tailrec
  def genByCountOfSteps(LevelAbs: Int, LevelAppl: Int, exactly: Boolean, countOfSteps: Int): Term[Nothing] = {
    val t = getGeneratedTerm(LevelAbs, LevelAppl, exactly)
    Semantics.evaluate[Nothing](t, Semantics.normReduction)
    if (Semantics.C >= countOfSteps)
      t
    else
      genByCountOfSteps(LevelAbs, LevelAppl, exactly, countOfSteps: Int)
  }


//  def main(args: Array[String]): Unit = {
//    println(genByCountOfSteps(4,5,false,3))
//  }
}

