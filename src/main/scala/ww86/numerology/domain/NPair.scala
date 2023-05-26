package ww86.numerology.domain

import ww86.numerology.Dictionary.{Subnumbers, _}
import ww86.numerology._

case class NPair(from: Set[SingleWord], vowelsSum: Int, consonantsSum: Int) {
  import NPair._
  val bothSum = vowelsSum + consonantsSum

  def bothDigit = toBasicNDigit(vowelsDigit + consonantsDigit)

  def vowelsDigit = toBasicNDigit(vowelsSum)

  def consonantsDigit = toBasicNDigit(consonantsSum)

  val subNumbersCandidates = Set(bothSum, vowelsSum, consonantsSum, toNDigitOneStep(bothSum), toNDigitOneStep(vowelsSum), toNDigitOneStep(consonantsSum))

  def jestKarmiczna = Subnumbers.karmic.intersect(subNumbersCandidates).nonEmpty

  def jestOchronna = Subnumbers.protective.intersect(subNumbersCandidates).nonEmpty

  val jestMistrzowska = Subnumbers.masterly.intersect(subNumbersCandidates).nonEmpty

  def jestMocy = Subnumbers.powerful.intersect(subNumbersCandidates).nonEmpty

  def subnumbersRootOnly(target: NPair): Set[(Int, String)] =
    Subnumbers.allSpecial.intersect(target.subNumbersCandidates).map(_ -> target.from.mkString(" "))

  def subnumbersRecursively: Set[(Int, String)] =
    (subnumbersRootOnly(this) ++ from.map(NPair.apply).flatMap(subnumbersRootOnly))
}

object NPair {
  def apply(str: String): NPair = {
    val samo = str.filter(el => vowels.contains(el))
    val spół = str.filterNot(el => vowels.contains(el))
    NPair(Set(str), samo.map(digitsOfLetters).sum, spół.map(digitsOfLetters).sum)
  }

  def apply(słowa: Seq[SingleWord]): NPair = {
    val pary = słowa.map(apply)
    NPair(słowa.toSet, pary.map(_.vowelsSum).sum, pary.map(_.consonantsSum).sum)
  }

  def prettyProfile(value: NPair): String = {
    val podliczby = value.subnumbersRecursively

    import value._
    s"""Pełny profil numerologiczny:
  samogłosekSuma \t= $vowelsSum
  spółgłosekSuma \t= $consonantsSum
  sumaObu \t\t\t= $bothSum
  cyfra samogłosek \t= $vowelsDigit ; aka wewnętrzna
  cyfra spółgłosek \t= $consonantsDigit ; aka zewnętrzna
  cyfra obu \t\t= $bothDigit ; aka cel
  podliczby: \t\t${podliczby.toList.sortBy(_._1).mkString("\n\t\t\t\t\t")}
"""
  }

  def prettyShortProfile(value: NPair): String = {
    import value._
    s"$vowelsSum/$consonantsSum/$bothSum"
  }

  def toBasicNDigit(liczba: Int): Digit =
    if (liczba < 10 ) liczba
    else
      toBasicNDigit(toNDigitOneStep(liczba))

  def toNDigitOneStep(liczba: Int): Digit =
    liczba.toString.toCharArray.map(c => Integer.parseInt(c.toString)).sum

}