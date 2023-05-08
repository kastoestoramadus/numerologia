package ww86.numerology.domain

import ww86.numerology.Dictionary._

case class NPair(samogłosekSuma: Int, spółgłosekSuma: Int) {
  def sumaObu = samogłosekSuma + spółgłosekSuma

  def obu = toNCyfra(samogłosek + spółgłosek)

  def samogłosek = toNCyfra(samogłosekSuma)

  def spółgłosek = toNCyfra(spółgłosekSuma)

  def jestKarmiczna = Subnumbers.karmic.contains(samogłosekSuma) || Subnumbers.karmic.contains(spółgłosekSuma) || Subnumbers.karmic.contains(sumaObu)

  def jestOchronna = Subnumbers.protective.contains(samogłosekSuma) || Subnumbers.protective.contains(spółgłosekSuma) || Subnumbers.protective.contains(sumaObu)

  def jestMistrzowska = Subnumbers.masterly.contains(samogłosekSuma) || Subnumbers.masterly.contains(spółgłosekSuma) || Subnumbers.masterly.contains(sumaObu)

  def jestMocy = 27 == samogłosekSuma || 27 == spółgłosekSuma || 27 == (sumaObu)

  def mkString =
    s"""Słowa o numerologii:
  samogłosekSuma = $samogłosekSuma
  spółgłosekSuma = $spółgłosekSuma
  sumaObu = $sumaObu
  samogłosek = $samogłosek ; aka wewnętrzna
  spółgłosek = $spółgłosek ; aka zewnętrzna
  obu = $obu ; aka cel
  jestKarmiczna = $jestKarmiczna
  jestOchronna = $jestOchronna
  jestMistrzowska = $jestMistrzowska
  jestMocy = $jestMocy
"""

  def mkStringSumy = s"$samogłosekSuma/$spółgłosekSuma/$sumaObu"

  def toNCyfra(liczba: Int): Digit =
    if (liczba < 10) liczba
    else
      toNCyfra(liczba.toString.toCharArray.map(c => Integer.parseInt(c.toString)).sum)
}
