package ww86.numerology.domain

import ww86.numerology._
import ww86.numerology.Dictionary._

case class NPair(from: String, vowelsSum: Int, consonantsSum: Int) {
  import NPair.toNDigit
  def sumaObu = vowelsSum + consonantsSum

  def obu = toNDigit(samogłosek + spółgłosek)

  def samogłosek = toNDigit(vowelsSum)

  def spółgłosek = toNDigit(consonantsSum)

  def jestKarmiczna = Subnumbers.karmic.contains(vowelsSum) || Subnumbers.karmic.contains(consonantsSum) || Subnumbers.karmic.contains(sumaObu)

  def jestOchronna = Subnumbers.protective.contains(vowelsSum) || Subnumbers.protective.contains(consonantsSum) || Subnumbers.protective.contains(sumaObu)

  def jestMistrzowska = Subnumbers.masterly.contains(vowelsSum) || Subnumbers.masterly.contains(consonantsSum) || Subnumbers.masterly.contains(sumaObu)

  def jestMocy = 27 == vowelsSum || 27 == consonantsSum || 27 == (sumaObu)
}

object NPair {
  def prettyProfile(value: NPair): String = {
    import value._
    s"""Pełny profil numerologiczny:
  samogłosekSuma \t= $vowelsSum
  spółgłosekSuma \t= $consonantsSum
  sumaObu \t\t\t= $sumaObu
  samogłosek \t\t= $samogłosek ; aka wewnętrzna
  spółgłosek \t\t= $spółgłosek ; aka zewnętrzna
  obu \t\t\t\t= $obu ; aka cel
  jestKarmiczna \t= $jestKarmiczna
  jestOchronna \t\t= $jestOchronna
  jestMistrzowska \t= $jestMistrzowska
  jestMocy \t\t\t= $jestMocy
"""
  }

  def prettyShortProfile(value: NPair): String = {
    import value._
    s"$vowelsSum/$consonantsSum/$sumaObu"
  }

  def toNDigit(liczba: Int): Digit =
    if (liczba < 10) liczba
    else
      toNDigit(liczba.toString.toCharArray.map(c => Integer.parseInt(c.toString)).sum)
}