package ww86.numerology

import ww86.numerology.domain.{CountedName, NPair, NShortProfile}

object RunProfileCounting extends App {
  import Dictionary._

  def sortingAttribute(tr: NShortProfile): String = tr.wnętrzna.toString + tr.zewnętrzna.toString + tr.cele.toString

  def countDigits(słowa: List[SingleWord]) =
    słowa.flatten.map(digitsOfLetters).groupBy(el => el).map(pair => pair._1 -> pair._2.size).withDefaultValue(0)

  def printableCountedDigits(słowa: List[SingleWord]): String = {
    val digitsCounted = countDigits(słowa)
    val sequenced = (1 to 9).map(d => s"$d -> ${digitsCounted(d)}").mkString("\n\t")
    s"""Liczba cyfr w nazwie: \n"${słowa.mkString(" ")}": \n\t""" + sequenced
  }

  def groupNamesBySameNProfile(słowa: Vector[List[SingleWord]]) =
    słowa.map(el => NShortProfile(el) -> el).groupBy(el => el._1).map(el => el._1 -> el._2.map(_._2.head))

  def beneficialNames(słowa: List[SingleWord], baza: WordsBase): Vector[SingleWord] = {
    val nParaCałości = NPair(słowa)
    baza.filter(imię => {
      val npara = NPair(imię)
      (!npara.jestKarmiczna) //&& (npara.jestMistrzowska || npara.jestOchronna || npara.jestMocy || nParaCałości.jestMistrzowska || nParaCałości.jestOchronna || nParaCałości.jestMocy)
    }
    )
  }

  def countOptionsFor(namesToConsider: Vector[CountedName], customerName: List[SingleWord], profilesFilter: Set[NShortProfile]) = {
    val candidates = {
      val bazaImion = beneficialNames(customerName, namesToConsider.map(_.name))
      bazaImion
        //      .filter{imię => imię.count(l => digitsOfLetters(l) == 9) == 0 }// && imię.count(l => cyfryLiter(l) == 3) > 0}
        //        .filter{imię => imię.count(l => digitsOfLetters(l) == 5) == 0 }
        //        .filter{imię => imię.count(l => digitsOfLetters(l) == 2) > 0 }
        //        .filter{imię => imię.count(l => digitsOfLetters(l) == 4) > 0 }
        .filterNot(el => customerName.contains(el))
        .map(el => el :: customerName) // add each name to customerName
    }

    groupNamesBySameNProfile(candidates).toVector
      .filter(układ => profilesFilter.contains(układ._1))
  }

  def showOptionsFor(namesToConsider: Vector[CountedName], customerName: List[SingleWord], profilesFilter: Set[NShortProfile] = Set()): Unit = {
    println(
      printableCountedDigits(customerName)
    )

    println(NPair.prettyProfile(NPair(customerName)))

    println(s"Nowe konfiguracje dla $customerName")

    val filteredSolutions = countOptionsFor(namesToConsider, customerName, profilesFilter)

    filteredSolutions.foreach { słowa =>
      val całości = NPair(słowa._2.toList)
      if (całości.jestMistrzowska || całości.jestOchronna)
        println(s"! ${NPair.prettyShortProfile(całości)} - to słowo ${słowa._2.head} w sumie dodaje mistrzowską.")
    }
    filteredSolutions
      .map{case (shortProfile, proposedNewName) =>
        shortProfile ->
          proposedNewName
            .sortBy(name =>
              namesToConsider.find(k => k.name == name).map(_.count).get // by usage frequency in nameBase
            ).reverse
      }
      .sortBy(t => sortingAttribute(t._1))
      .foreach(println)
  }

  val ewy = List("EWA", "IZABELA", "WERONIKA", "BEDNAREK", "KIEŁBASA")

  val irenyM = List("IRENA", "MAGDALENA", "MAKAREWICZ")

  val irenyW = List("IRENA", "WERONIKA", "WOSIŃSKA")

  val mamaUli = List("ANNA", "BIEŃKA","ELŻBIETA")

  val bazoweSłowaWW: List[SingleWord] = List("WALDEMAR", "GRZEGORZ", "MELCHIOR", "WOSIŃSKI", "HEKTOR")

  val bazoweSłowaMagdzik: List[SingleWord] = List("MAGDALENA", "MARIA", "BARBARA", "WIELGOŁASKA")

  val bazoweDaniela: List[SingleWord] = List("DANIEL", "KONRAD", "KALIŃSKI")

  val bazoweMŚledź = List("MARZENA", "ANNA", "MARIA", "MAGDALENA", "ŚLEDŹ") //KALINKA BAJKA

  val bazoweKStańczuk = List("KATARZYNA", "ANNA", "STAŃCZUK")

  val bazoweHilda = List("Olga", "Hildegarda", "Graboś").map(_.toUpperCase)

  showOptionsFor(maleNames, bazoweSłowaWW, bestForSixs)
}

