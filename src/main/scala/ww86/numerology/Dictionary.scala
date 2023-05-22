package ww86.numerology

import ww86.numerology.domain.NProfile

object Dictionary {
  val bestForOnes = Set(
    NProfile(7, 3, 1),
    NProfile(2, 9, 2),
    NProfile(8, 3, 2),
    NProfile(2, 1, 3),
    NProfile(4, 3, 7),
    NProfile(7, 9, 7),
    NProfile(7, 1, 8),
    NProfile(8, 1, 9),
    NProfile(7, 2, 9),
    NProfile(6, 3, 9)
  )
  val bestForTwos = Set(
    NProfile(1, 9, 1),
    NProfile(9, 1, 1),
    NProfile(1, 2, 3),
    NProfile(1, 3, 4)
  )
  val bestForThrees = Set(
    NProfile(7, 3, 1),
    NProfile(2, 9, 2),
    NProfile(8, 3, 2),
    NProfile(2, 1, 3),
    NProfile(7, 1, 8),
    NProfile(8, 1, 9),
    NProfile(7, 2, 9)
  )
  val bestForFours = Set(
    NProfile(1, 9, 1),
    NProfile(1, 3, 4),
    NProfile(1, 2, 3),
    NProfile(6, 3, 9)
  )
  val bestForFives = Set(
    NProfile(7, 3, 1),
    NProfile(2, 9, 2),
    NProfile(2, 1, 3),
    NProfile(4, 3, 7),
    NProfile(7, 9, 7),
    NProfile(7, 1, 8),
    NProfile(7, 2, 9)
  )
  val bestForSixs = Set(
    NProfile(1, 9, 1),
    NProfile(1, 2, 3),
  )
  val bestForSevens = Set(
    NProfile(1, 9, 1),
    NProfile(1, 2, 3),
    NProfile(9, 3, 3),
    NProfile(1, 3, 4)
  )
  val acceptableForSevens = Set(
    NProfile(9, 1, 1),
    NProfile(3, 7, 1),
    NProfile(9, 2, 2),
    NProfile(3, 9, 3),
    NProfile(3, 1, 4),
    NProfile(9, 7, 7),
    NProfile(1, 7, 8),
    NProfile(1, 8, 9),
    NProfile(7, 2, 9),
    NProfile(9, 9, 9),
  )
  val bestForEights = Set(
    NProfile(1, 9, 1),
    NProfile(1, 3, 4),
    NProfile(1, 8, 9)
  )
  val bestForNines = Set(
    NProfile(7, 3, 1),
    NProfile(2, 9, 2),
    NProfile(8, 3, 2),
    NProfile(2, 1, 3),
    NProfile(7, 1, 8),
    NProfile(7, 2, 9)
  )

  final val digitsOfLetters: Map[Char, Digit] = Map(
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
  final val vowels: String = "AĄEĘIOÓU"

  case class CountedName(name: String, count: Int)

  lazy val maleNames: Vector[CountedName] = {
    println("Wczytywanie imion męskich.")
    val bufferedSource = io.Source.fromResource("male_names.csv")
    val lines = bufferedSource.getLines.drop(1)
    val r =
      (for {line <- lines} yield {
        val both = line.split(",").map(_.trim)
        CountedName(both.head, both.tail.tail.head.toInt)
      }).toVector

    println("Wczytano imona męskie.")
    bufferedSource.close
    r.filter(_.name.forall(digitsOfLetters.contains))
  }

  lazy val femaleNames: Vector[CountedName] = {
    println("Wczytywanie imion żeńskich.")
    val bufferedSource = io.Source.fromResource("female_names.csv")
    val lines = bufferedSource.getLines.drop(1)
    val r =
      (for {line <- lines} yield {
        val both = line.split(",").map(_.trim)
        CountedName(both.head, both.tail.tail.head.toInt)
      }).toVector

    println("Wczytano imona żeńskie.")
    bufferedSource.close
    r.filter(_.name.forall(digitsOfLetters.contains))
  }

  object Subnumbers {
    val karmic = Set(13, 14, 16, 19, 26)
    val masterly = Set(11, 22, 33, 44, 55, 66, 77, 88, 99, 111, 222)
    val protective = Set(17, 41)
  }
}
