package ww86.numerology

object Numerologia extends App {
  import Dictionary._


  case class NPara(samogłosekSuma: Int, spółgłosekSuma: Int) {
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
  }

  def toNCyfra(liczba: Int): Digit =
    if(liczba<10) liczba
    else
      toNCyfra(liczba.toString.toCharArray.map(c => Integer.parseInt(c.toString)).sum)

  def wyliczNParę(str: String): NPara = {
    val samo = str.filter(el => vowels.contains(el))
    val spół = str.filterNot(el => vowels.contains(el))
    NPara(samo.map(digitsOfLetters).sum, spół.map(digitsOfLetters).sum)
  }

  def wyliczNParę(słowa: MultiWordName): NPara = {
    val pary = słowa.map(wyliczNParę)
    NPara(pary.map(_.samogłosekSuma).sum, pary.map(_.spółgłosekSuma).sum)
  }


  case class NTrójca(wnętrzna: Digit, zewnętrzna: Digit, cele: Digit)

  def sortujWedług(tr: NTrójca): String = tr.wnętrzna.toString + tr.zewnętrzna.toString + tr.cele.toString

  def wylicz(słowa: MultiWordName): NTrójca = {
    val paraCałości = wyliczNParę(słowa)
    NTrójca(paraCałości.samogłosek, paraCałości.spółgłosek, paraCałości.obu)
  }

  def ilośćCyfr(słowa: MultiWordName): Map[Digit, Int] = słowa.flatten.map(digitsOfLetters).groupBy(el => el).map(pair => pair._1 -> pair._2.size)

  def konfiguracja(słowa: Vector[MultiWordName]) = słowa.map(el => wylicz(el) -> el).groupBy(el => el._1).map(el => el._1 -> el._2.map(_._2.head))

  val ewy = List("EWA", "IZABELA","WERONIKA", "BEDNAREK", "KIEŁBASA")

  val ireny = List("IRENA", "MAGDALENA","MAKAREWICZ")

  val bazoweSłowaWW: MultiWordName = List("WALDEMAR", "GRZEGORZ", "MELCHIOR", "WOSIŃSKI", "HEKTOR" )

  val bazoweSłowaMagdzik: MultiWordName = List("MAGDALENA", "MARIA", "BARBARA", "WIELGOŁASKA")

  val bazoweDaniela: MultiWordName = List("DANIEL", "KONRAD", "KALIŃSKI")

  val bazoweMŚledź = List ("MARZENA", "ANNA", "MARIA", "MAGDALENA", "ŚLEDŹ") //KALINKA BAJKA

  val bazoweKStańczuk = List("KATARZYNA", "ANNA", "STAŃCZUK")

  val bazoweHilda = List("Olga","Hildegarda","Graboś").map(_.toUpperCase)

  def wyróżnioneSłowa(słowa: MultiWordName, baza: WordsBase) = {
    val nParaCałości = wyliczNParę(słowa)
    baza.filter(imię => {
      val npara = wyliczNParę(imię)
      (!npara.jestKarmiczna) //&& (npara.jestMistrzowska || npara.jestOchronna || npara.jestMocy || nParaCałości.jestMistrzowska || nParaCałości.jestOchronna || nParaCałości.jestMocy)
    }
    )
  }

  def pokażOpcjeDla(obszarDobierany: Vector[CountedName], słowa: MultiWordName, ograniczDo: Set[NTrójca] = Set()): Unit = {
    println(
      ilośćCyfr(słowa)
    )

    println(wyliczNParę(słowa).mkString)

    println(s"Nowe konfiguracje dla $słowa")

    val bazaImion = wyróżnioneSłowa(słowa, obszarDobierany.map(_.name))

    konfiguracja(
      bazaImion
      .filter{imię => imię.count(l => digitsOfLetters(l) == 9) == 0 }// && imię.count(l => cyfryLiter(l) == 3) > 0}
        .filter{imię => imię.count(l => digitsOfLetters(l) == 5) == 0 }
        .filter{imię => imię.count(l => digitsOfLetters(l) == 2) > 0 }
        .filter{imię => imię.count(l => digitsOfLetters(l) == 4) > 0 }
      .map(el => el :: słowa)).toVector
      .filter(układ => ograniczDo.contains(układ._1))
      .map{słowa =>
        val całości = wyliczNParę(słowa._2.toList)
        if(całości.jestMistrzowska || całości.jestOchronna)
          println(s"! ${całości.mkStringSumy} - to słowo ${słowa._2.head} w sumie dodaje mistrzowską.")
        słowa
      }
      .map{case (trójka, imionaProponowane) => trójka -> imionaProponowane.sortBy(imię => obszarDobierany.find(k => k.name == imię).map(_.count).get).reverse}
      .sortBy(t => sortujWedług(t._1))
      .foreach(println)
  }

  pokażOpcjeDla(maleNames, bazoweSłowaWW, bestForSixs)
}

