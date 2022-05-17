import Numerologia.{Cyfra, NTrójca}

object Numerologia extends App {
  import Słownik._
  type Cyfra = Int

  val karmiczne = Set(13, 14, 16, 19, 26)
  val mistrzowskie = Set(11, 22, 33, 44, 55, 66, 77, 88, 99)
  val opiekaBoska = Set(17,41)

  case class NPara(samogłosekSuma: Int, spółgłosekSuma: Int) {
    def samogłosek = toNCyfra(samogłosekSuma)
    def spółgłosek = toNCyfra(spółgłosekSuma)
    def jestKarmiczna = karmiczne.contains(samogłosekSuma) || karmiczne.contains(spółgłosekSuma) || karmiczne.contains(samogłosekSuma + spółgłosekSuma)
    def jestOchronna = opiekaBoska.contains(samogłosekSuma) || opiekaBoska.contains(spółgłosekSuma) || opiekaBoska.contains(samogłosekSuma + spółgłosekSuma)
    def jestMistrzowska = mistrzowskie.contains(samogłosekSuma) || mistrzowskie.contains(spółgłosekSuma) || mistrzowskie.contains(samogłosekSuma + spółgłosekSuma)
    def jestMocy = 27 == samogłosekSuma || 27 == spółgłosekSuma || 27 == (samogłosekSuma + spółgłosekSuma)
  }

  def toNCyfra(liczba: Int): Cyfra =
    if(liczba<10) liczba
    else
      toNCyfra(liczba.toString.toCharArray.map(c => Integer.parseInt(c.toString)).sum)

  def wylicz(str: String): NPara = {
    val samo = str.filter(el => samogłoski.contains(el))
    val spół = str.filterNot(el => samogłoski.contains(el))
    NPara(samo.map(cyfryLiter).sum, spół.map(cyfryLiter).sum)
  }

  case class NTrójca(wnętrzna: Cyfra, zewnętrzna: Cyfra, cele: Cyfra)

  def sortujWedług(tr: NTrójca): String = tr.wnętrzna.toString + tr.zewnętrzna.toString + tr.cele.toString

  def wylicz(słowa: Seq[String]): NTrójca = {
    val pary = słowa.map(wylicz)
    val wew = toNCyfra(pary.map(_.samogłosek).sum)
    val zew = toNCyfra(pary.map(_.spółgłosek).sum)
    val cel = toNCyfra(wew + zew)
    NTrójca(wew, zew, cel)
  }

  def ilośćCyfr(słowa: List[String]): Map[Cyfra, Int] = słowa.flatten.map(cyfryLiter).groupBy(el => el).map(pair => pair._1 -> pair._2.size)

  def konfiguracja(słowa: Seq[Seq[String]]) = słowa.map(el => wylicz(el) -> el).groupBy(el => el._1).map(el => el._1 -> el._2.map(_._2.head))

  val ewy = List("EWA", "IZABELA","WERONIKA", "BEDNAREK", "KIEŁBASA")

  val ireny = List("IRENA", "MAGDALENA","MAKAREWICZ")

  val bazoweSłowaWW: List[String] = List("WALDEMAR", "GRZEGORZ", "MELCHIOR", "WOSIŃSKI")

  val bazoweSłowaMagdzik: List[String] = List("MAGDALENA", "MARIA", "BARBARA", "WIELGOŁASKA", "DOBROGNIEWA")

  val lepszeŻeńśkie =
    imionaŻeńskie.filter(imię => {
          val npara = wylicz(imię)
          (!npara.jestKarmiczna) && (npara.jestMistrzowska || npara.jestOchronna || npara.jestMocy)
        }
    )


  val lepszeMęskie =
    imionaMęskie.filter(imię => {
        val npara = wylicz(imię)
        (!npara.jestKarmiczna) && (npara.jestMistrzowska || npara.jestOchronna || npara.jestMocy)
      }
    )

  def pokażŻeńśkieOpcjeDla(słowa: List[String], ograniczDo: Set[NTrójca] = Set()): Unit = {
    println(
      ilośćCyfr(słowa)
    )

    println(wylicz(słowa))

    println(s"Nowe konfiguracje dla $słowa")

    konfiguracja((lepszeŻeńśkie.filter{imię => imię.count(l => "BKT".contains(l)) > 0 && imię.count(l => "FOX".contains(l)) > 0})
      .map(el => el :: słowa)).toVector.filter(układ => ograniczDo.contains(układ._1)).sortBy(t => sortujWedług(t._1)).foreach(println)
  }

  def pokażMęskieOpcjeDla(słowa: List[String], ograniczDo: Set[NTrójca] = Set()): Unit = {
    println(
      ilośćCyfr(słowa)
    )

    println(wylicz(słowa))

    println(s"Nowe konfiguracje dla $słowa")

    konfiguracja((lepszeMęskie.filter(imię => imię.count(l => "BKT".contains(l)) > 1))
      .map(el => el :: słowa)).toVector.filter(układ => ograniczDo.contains(układ._1)).sortBy(t => sortujWedług(t._1)).foreach(println)
  }

  pokażŻeńśkieOpcjeDla(bazoweSłowaMagdzik, wyróżnioneDlaJedynki)
  //pokażMęskieOpcjeDla(bazoweSłowaWW)
}

object Słownik {
  val wyróżnioneDlaJedynki = Set(
    NTrójca(7,3,1),
    NTrójca(2,9,2),
    NTrójca(8,3,2),
    NTrójca(2,1,3),
    NTrójca(4,3,7),
    NTrójca(7,9,7),
    NTrójca(7,1,8),
    NTrójca(8,1,9),
    NTrójca(7,2,9),
    NTrójca(6,3,9)
  )
  val wyróżnioneDlaDwójki = Set(
    NTrójca(1,9,1),
    NTrójca(9,1,1),
    NTrójca(1,2,3),
    NTrójca(1,3,4)
  )
  val wyróżnioneDlaTrójki = Set(
    NTrójca(7,3,1),
    NTrójca(2,9,2),
    NTrójca(8,3,2),
    NTrójca(2,1,3),
    NTrójca(7,1,8),
    NTrójca(8,1,9),
    NTrójca(7,2,9)
  )
  val wyróżnioneDlaCzwórki = Set(
    NTrójca(1,9,1),
    NTrójca(1,3,4),
    NTrójca(1,2,3),
    NTrójca(6,3,9)
  )
  val wyróżnioneDlaPiątki = Set(
    NTrójca(7,3,1),
    NTrójca(2,9,2),
    NTrójca(2,1,3),
    NTrójca(4,3,7),
    NTrójca(7,9,7),
    NTrójca(7,1,8),
    NTrójca(7,2,9)
  )
  val wyróżnioneDlaSzóstki = Set(
    NTrójca(1,9,1),
    NTrójca(1,2,3),
  )
  val wyróżnioneDlaSiódemki = Set(
    NTrójca(1,9,1),
    NTrójca(1,2,3),
    NTrójca(9,3,3),
    NTrójca(1,3,4)
  )
  val wyróżnioneDlaÓsmeki = Set(
    NTrójca(1,9,1),
    NTrójca(1,3,4),
    NTrójca(1,8,9)
  )
  val wyróżnioneDlaDziewiątki = Set(
    NTrójca(7,3,1),
    NTrójca(2,9,2),
    NTrójca(8,3,2),
    NTrójca(2,1,3),
    NTrójca(7,1,8),
    NTrójca(7,2,9)
  )

  final val cyfryLiter: Map[Char, Cyfra] = Map(
    'A' -> 1,
    'Ą' -> 1,
    'B' -> 2,
    'C' -> 3,
    'Ć' -> 3,
    'D' -> 4,
    'E' -> 5,
    'Ę' -> 5,
    'F' -> 6,
    'G' -> 7,
    'H' -> 8,
    'I' -> 9,
    'J' -> 1,
    'K' -> 2,
    'L' -> 3,
    'Ł' -> 3,
    'M' -> 4,
    'N' -> 5,
    'Ń' -> 5,
    'O' -> 6,
    'Ó' -> 6,
    'P' -> 7,
    'Q' -> 8,
    'R' -> 9,
    'S' -> 1,
    'Ś' -> 1,
    'T' -> 2,
    'U' -> 3,
    'V' -> 4,
    'W' -> 5,
    'X' -> 6,
    'Y' -> 7,
    'Z' -> 8,
    'Ż' -> 8,
    'Ź' -> 8
  )
  final val samogłoski: String = "AĄEĘIOÓU"

  lazy val imionaMęskie: Vector[String] = {
    println("Wczytywanie imion męskich.")
    val bufferedSource = io.Source.fromFile("/Users/waldemar.wosinski/IdeaProjects/Numerologia/src/main/resources/WYKAZ_IMION_MĘSKICH.csv")
    val lines = bufferedSource.getLines.drop(1)
    val r =
      (for {line <- lines} yield line.split(",").map(_.trim).head).toVector

    println("Wczytano imona męskie.")
    bufferedSource.close
    r.filter(_.forall(cyfryLiter.contains))
  }

  lazy val imionaŻeńskie: Vector[String] = {
    println("Wczytywanie imion żeńskich.")
    val bufferedSource = io.Source.fromFile("/Users/waldemar.wosinski/IdeaProjects/Numerologia/src/main/resources/WYKAZ_IMION_ŻEŃSKICH.csv")
    val lines = bufferedSource.getLines.drop(1)
    val r =
      (for {line <- lines} yield line.split(",").map(_.trim).head).toVector

    println("Wczytano imona żeńskich.")
    bufferedSource.close
    r.filter(_.forall(cyfryLiter.contains))
  }

  lazy val imionaZDwójkami = Set(
    "ALBERT",
    "ALBRECHT",
    "BALTAZAR",
    "BARTŁOMIEJ",
    "BARNABA",
    "BARTOSZ",
    "BENEDYKT",
    "DOBROGOST",
    "GILBERT",
    "HEKTOR",
    "HUBERT",
    "JAKUB",
    "KAJETAN",
    "KEMOK",
    "KONSTANTY",
    "KRYSTIAN",
    "KRYSTYN",
    "KRZYSZTOF",
    "LAMBERT",
    "NORBERT",
    "OKTAWIAN",
    "PATRYK",
    "ROBERT",
    "SEBASTIAN",
    "ŚWIĘTOBÓR",
    "TOBIASZ",
    "TYMOTEUSZ",
    "TYTUS",
    "WIKTOR",
    "ARLETTA",
    "BARBARA",
    "BEATA",
    "BENITA",
    "BERENIKA",
    "BERNADETTA",
    "BODŻANTA",
    "ELŻBIETA",
    "FORTUNATA",
    "HENRIETTA",
    "JULIETTA",
    "JUTTA",
    "KATARZYNA",
    "KLEOPATRA",
    "KRISTINA",
    "KRYSTINA",
    "KRYSTYNA",
    "REBEKA",
    "ROBERTA",
    "SCHOLASTYKA",
    "TATIANA",
    "WIKTORIA",
    "WIOLETTA"
  )
}