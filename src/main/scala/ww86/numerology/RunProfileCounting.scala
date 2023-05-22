package ww86.numerology

import ww86.numerology.domain.{NPair, NProfile}

object RunProfileCounting extends App {
  import Dictionary._

  def makeNPair(str: String): NPair = {
    val samo = str.filter(el => vowels.contains(el))
    val spół = str.filterNot(el => vowels.contains(el))
    NPair(str, samo.map(digitsOfLetters).sum, spół.map(digitsOfLetters).sum)
  }

  def makeNPairs(słowa: MultiWordName): NPair = {
    val pary = słowa.map(makeNPair)
    NPair(słowa.mkString(" "), pary.map(_.vowelsSum).sum, pary.map(_.consonantsSum).sum)
  }

  def sortingAttribute(tr: NProfile): String = tr.wnętrzna.toString + tr.zewnętrzna.toString + tr.cele.toString

  def countProfile(słowa: MultiWordName): NProfile = {
    val paraCałości = makeNPairs(słowa)
    NProfile(paraCałości.samogłosek, paraCałości.spółgłosek, paraCałości.obu)
  }

  def countDigitsPrintable(słowa: MultiWordName): String = {
    val from = słowa.flatten.map(digitsOfLetters).groupBy(el => el).map(pair => pair._1 -> pair._2.size)
    val sequenced = (1 to 9).map(d => s"$d -> ${from(d)}").mkString("\n\t")
    s"""Liczba cyfr w nazwie: \n"${słowa.mkString(" ")}": \n\t""" + sequenced
  }

  def transformingConfigurations(słowa: Vector[MultiWordName]) = słowa.map(el => countProfile(el) -> el).groupBy(el => el._1).map(el => el._1 -> el._2.map(_._2.head))

  def beneficialNames(słowa: MultiWordName, baza: WordsBase) = {
    val nParaCałości = makeNPairs(słowa)
    baza.filter(imię => {
      val npara = makeNPair(imię)
      (!npara.jestKarmiczna) && (npara.jestMistrzowska || npara.jestOchronna || npara.jestMocy || nParaCałości.jestMistrzowska || nParaCałości.jestOchronna || nParaCałości.jestMocy)
    }
    )
  }

  def showOptionsFor(obszarDobierany: Vector[CountedName], słowa: MultiWordName, ograniczDo: Set[NProfile] = Set()): Unit = {
    println(
      countDigitsPrintable(słowa)
    )

    println(NPair.prettyProfile(makeNPairs(słowa)))

    println(s"Nowe konfiguracje dla $słowa")

    val bazaImion = beneficialNames(słowa, obszarDobierany.map(_.name))

    transformingConfigurations(
      bazaImion
      .filter{imię => imię.count(l => digitsOfLetters(l) == 9) == 0 }// && imię.count(l => cyfryLiter(l) == 3) > 0}
        .filter{imię => imię.count(l => digitsOfLetters(l) == 5) == 0 }
        .filter{imię => imię.count(l => digitsOfLetters(l) == 2) > 0 }
        .filter{imię => imię.count(l => digitsOfLetters(l) == 4) > 0 }
      .map(el => el :: słowa)).toVector
      .filter(układ => ograniczDo.contains(układ._1))
      .map{słowa =>
        val całości = makeNPairs(słowa._2.toList)
        if(całości.jestMistrzowska || całości.jestOchronna)
          println(s"! ${NPair.prettyShortProfile(całości)} - to słowo ${słowa._2.head} w sumie dodaje mistrzowską.")
        słowa
      }
      .map{case (trójka, imionaProponowane) => trójka -> imionaProponowane.sortBy(imię => obszarDobierany.find(k => k.name == imię).map(_.count).get).reverse}
      .sortBy(t => sortingAttribute(t._1))
      .foreach(println)
  }

  val ewy = List("EWA", "IZABELA", "WERONIKA", "BEDNAREK", "KIEŁBASA")

  val ireny = List("IRENA", "MAGDALENA", "MAKAREWICZ")

  val bazoweSłowaWW: MultiWordName = List("WALDEMAR", "GRZEGORZ", "MELCHIOR", "WOSIŃSKI", "HEKTOR")

  val bazoweSłowaMagdzik: MultiWordName = List("MAGDALENA", "MARIA", "BARBARA", "WIELGOŁASKA")

  val bazoweDaniela: MultiWordName = List("DANIEL", "KONRAD", "KALIŃSKI")

  val bazoweMŚledź = List("MARZENA", "ANNA", "MARIA", "MAGDALENA", "ŚLEDŹ") //KALINKA BAJKA

  val bazoweKStańczuk = List("KATARZYNA", "ANNA", "STAŃCZUK")

  val bazoweHilda = List("Olga", "Hildegarda", "Graboś").map(_.toUpperCase)

  showOptionsFor(maleNames, bazoweSłowaWW, bestForSixs)
}

