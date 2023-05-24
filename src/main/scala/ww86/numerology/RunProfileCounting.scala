package ww86.numerology

import ww86.numerology.domain.{NPair, NShortProfile}

object RunProfileCounting extends App {
  import Dictionary._

  def sortingAttribute(tr: NShortProfile): String = tr.wnętrzna.toString + tr.zewnętrzna.toString + tr.cele.toString

  def countProfile(słowa: List[SingleWord]): NShortProfile = {
    val paraCałości = NPair(słowa)
    NShortProfile(paraCałości.vowelsDigit, paraCałości.consonantsDigit, paraCałości.bothDigit)
  }

  def countDigitsPrintable(słowa: List[SingleWord]): String = {
    val from = słowa.flatten.map(digitsOfLetters).groupBy(el => el).map(pair => pair._1 -> pair._2.size).withDefaultValue(0)
    val sequenced = (1 to 9).map(d => s"$d -> ${from(d)}").mkString("\n\t")
    s"""Liczba cyfr w nazwie: \n"${słowa.mkString(" ")}": \n\t""" + sequenced
  }

  def transformingConfigurations(słowa: Vector[List[SingleWord]]) = słowa.map(el => countProfile(el) -> el).groupBy(el => el._1).map(el => el._1 -> el._2.map(_._2.head))

  def beneficialNames(słowa: List[SingleWord], baza: WordsBase) = {
    val nParaCałości = NPair(słowa)
    baza.filter(imię => {
      val npara = NPair(imię)
      (!npara.jestKarmiczna) && (npara.jestMistrzowska || npara.jestOchronna || npara.jestMocy || nParaCałości.jestMistrzowska || nParaCałości.jestOchronna || nParaCałości.jestMocy)
    }
    )
  }

  def showOptionsFor(obszarDobierany: Vector[CountedName], słowa: List[SingleWord], ograniczDo: Set[NShortProfile] = Set()): Unit = {
    println(
      countDigitsPrintable(słowa)
    )

    println(NPair.prettyProfile(NPair(słowa)))

    println(s"Nowe konfiguracje dla $słowa")

    val bazaImion = beneficialNames(słowa, obszarDobierany.map(_.name))

    transformingConfigurations(
      bazaImion
//      .filter{imię => imię.count(l => digitsOfLetters(l) == 9) == 0 }// && imię.count(l => cyfryLiter(l) == 3) > 0}
//        .filter{imię => imię.count(l => digitsOfLetters(l) == 5) == 0 }
//        .filter{imię => imię.count(l => digitsOfLetters(l) == 2) > 0 }
//        .filter{imię => imię.count(l => digitsOfLetters(l) == 4) > 0 }
      .map(el => el :: słowa)).toVector
      .filter(układ => ograniczDo.contains(układ._1))
      .map{słowa =>
        val całości = NPair(słowa._2.toList)
        if(całości.jestMistrzowska || całości.jestOchronna)
          println(s"! ${NPair.prettyShortProfile(całości)} - to słowo ${słowa._2.head} w sumie dodaje mistrzowską.")
        słowa
      }
      .map{case (trójka, imionaProponowane) => trójka -> imionaProponowane.sortBy(imię => obszarDobierany.find(k => k.name == imię).map(_.count).get).reverse}
      .sortBy(t => sortingAttribute(t._1))
      .foreach(println)
  }

  val ewy = List("EWA", "IZABELA", "WERONIKA", "BEDNAREK", "KIEŁBASA")

  val irenyM = List("IRENA", "MAGDALENA", "MAKAREWICZ")

  val irenyW = List("IRENA", "WERONIKA", "WOSIŃSKA")

  val mamaUli = List("ANNA", "BIEŃKA","ELŻBIETA")

  val bazoweSłowaWW: List[SingleWord] = List("WALDEMAR", "GRZEGORZ", "MELCHIOR", "WOSIŃSKI")

  val bazoweSłowaMagdzik: List[SingleWord] = List("MAGDALENA", "MARIA", "BARBARA", "WIELGOŁASKA")

  val bazoweDaniela: List[SingleWord] = List("DANIEL", "KONRAD", "KALIŃSKI")

  val bazoweMŚledź = List("MARZENA", "ANNA", "MARIA", "MAGDALENA", "ŚLEDŹ") //KALINKA BAJKA

  val bazoweKStańczuk = List("KATARZYNA", "ANNA", "STAŃCZUK")

  val bazoweHilda = List("Olga", "Hildegarda", "Graboś").map(_.toUpperCase)

  showOptionsFor(femaleNames, bazoweSłowaMagdzik, bestForSixs)
}

