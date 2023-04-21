package ww86.numerology

import ww86.numerology.Numerologia.{Cyfra, NTrójca}

object Dictionary {
  val wyróżnioneDlaJedynki = Set(
    NTrójca(7, 3, 1),
    NTrójca(2, 9, 2),
    NTrójca(8, 3, 2),
    NTrójca(2, 1, 3),
    NTrójca(4, 3, 7),
    NTrójca(7, 9, 7),
    NTrójca(7, 1, 8),
    NTrójca(8, 1, 9),
    NTrójca(7, 2, 9),
    NTrójca(6, 3, 9)
  )
  val wyróżnioneDlaDwójki = Set(
    NTrójca(1, 9, 1),
    NTrójca(9, 1, 1),
    NTrójca(1, 2, 3),
    NTrójca(1, 3, 4)
  )
  val wyróżnioneDlaTrójki = Set(
    NTrójca(7, 3, 1),
    NTrójca(2, 9, 2),
    NTrójca(8, 3, 2),
    NTrójca(2, 1, 3),
    NTrójca(7, 1, 8),
    NTrójca(8, 1, 9),
    NTrójca(7, 2, 9)
  )
  val wyróżnioneDlaCzwórki = Set(
    NTrójca(1, 9, 1),
    NTrójca(1, 3, 4),
    NTrójca(1, 2, 3),
    NTrójca(6, 3, 9)
  )
  val wyróżnioneDlaPiątki = Set(
    NTrójca(7, 3, 1),
    NTrójca(2, 9, 2),
    NTrójca(2, 1, 3),
    NTrójca(4, 3, 7),
    NTrójca(7, 9, 7),
    NTrójca(7, 1, 8),
    NTrójca(7, 2, 9)
  )
  val wyróżnioneDlaSzóstki = Set(
    NTrójca(1, 9, 1),
    NTrójca(1, 2, 3),
  )
  val wyróżnioneDlaSiódemki = Set(
    NTrójca(1, 9, 1),
    NTrójca(1, 2, 3),
    NTrójca(9, 3, 3),
    NTrójca(1, 3, 4)
  )
  val akceptowalneDlaSiódemki = Set(
    NTrójca(9, 1, 1),
    NTrójca(3, 7, 1),
    NTrójca(9, 2, 2),
    NTrójca(3, 9, 3),
    NTrójca(3, 1, 4),
    NTrójca(9, 7, 7),
    NTrójca(1, 7, 8),
    NTrójca(1, 8, 9),
    NTrójca(7, 2, 9),
    NTrójca(9, 9, 9),
  )
  val wyróżnioneDlaÓsmeki = Set(
    NTrójca(1, 9, 1),
    NTrójca(1, 3, 4),
    NTrójca(1, 8, 9)
  )
  val wyróżnioneDlaDziewiątki = Set(
    NTrójca(7, 3, 1),
    NTrójca(2, 9, 2),
    NTrójca(8, 3, 2),
    NTrójca(2, 1, 3),
    NTrójca(7, 1, 8),
    NTrójca(7, 2, 9)
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

  case class CzęśtotliwośćImienia(imię: String, ilość: Int)

  lazy val imionaMęskie: Vector[CzęśtotliwośćImienia] = {
    println("Wczytywanie imion męskich.")
    val bufferedSource = io.Source.fromResource("male_names.csv")
    val lines = bufferedSource.getLines.drop(1)
    val r =
      (for {line <- lines} yield {
        val both = line.split(",").map(_.trim)
        CzęśtotliwośćImienia(both.head, both.tail.tail.head.toInt)
      }).toVector

    println("Wczytano imona męskie.")
    bufferedSource.close
    r.filter(_.imię.forall(cyfryLiter.contains))
  }

  lazy val imionaŻeńskie: Vector[CzęśtotliwośćImienia] = {
    println("Wczytywanie imion żeńskich.")
    val bufferedSource = io.Source.fromResource("female_names.csv")
    val lines = bufferedSource.getLines.drop(1)
    val r =
      (for {line <- lines} yield {
        val both = line.split(",").map(_.trim)
        CzęśtotliwośćImienia(both.head, both.tail.tail.head.toInt)
      }).toVector

    println("Wczytano imona żeńskich.")
    bufferedSource.close
    r.filter(_.imię.forall(cyfryLiter.contains))
  }

  object Podliczby {
    val karmiczne = Set(13, 14, 16, 19, 26)
    val mistrzowskie = Set(11, 22, 33, 44, 55, 66, 77, 88, 99, 111, 222)
    val opiekaBoska = Set(17, 41)
  }
}
