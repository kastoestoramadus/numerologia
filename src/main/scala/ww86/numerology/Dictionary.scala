package ww86.numerology

import ww86.numerology.domain.{CountedName, NShortProfile}

object Dictionary {
  val bestForOnes = Set(
    NShortProfile(7, 3, 1),
    NShortProfile(2, 9, 2),
    NShortProfile(8, 3, 2),
    NShortProfile(2, 1, 3),
    NShortProfile(4, 3, 7),
    NShortProfile(7, 9, 7),
    NShortProfile(7, 1, 8),
    NShortProfile(8, 1, 9),
    NShortProfile(7, 2, 9),
    NShortProfile(6, 3, 9)
  )
  val bestForTwos = Set(
    NShortProfile(1, 9, 1),
    NShortProfile(9, 1, 1),
    NShortProfile(1, 2, 3),
    NShortProfile(1, 3, 4)
  )
  val bestForThrees = Set(
    NShortProfile(7, 3, 1),
    NShortProfile(2, 9, 2),
    NShortProfile(8, 3, 2),
    NShortProfile(2, 1, 3),
    NShortProfile(7, 1, 8),
    NShortProfile(8, 1, 9),
    NShortProfile(7, 2, 9)
  )
  val bestForFours = Set(
    NShortProfile(1, 9, 1),
    NShortProfile(1, 3, 4),
    NShortProfile(1, 2, 3),
    NShortProfile(6, 3, 9)
  )
  val bestForFives = Set(
    NShortProfile(7, 3, 1),
    NShortProfile(2, 9, 2),
    NShortProfile(2, 1, 3),
    NShortProfile(4, 3, 7),
    NShortProfile(7, 9, 7),
    NShortProfile(7, 1, 8),
    NShortProfile(7, 2, 9)
  )
  val bestForSixs = Set(
    NShortProfile(1, 9, 1),
    NShortProfile(1, 2, 3),
  )
  val bestForSevens = Set(
    NShortProfile(1, 9, 1),
    NShortProfile(1, 2, 3),
    NShortProfile(9, 3, 3),
    NShortProfile(1, 3, 4)
  )
  val acceptableForSevens = Set(
    NShortProfile(9, 1, 1),
    NShortProfile(3, 7, 1),
    NShortProfile(9, 2, 2),
    NShortProfile(3, 9, 3),
    NShortProfile(3, 1, 4),
    NShortProfile(9, 7, 7),
    NShortProfile(1, 7, 8),
    NShortProfile(1, 8, 9),
    NShortProfile(7, 2, 9),
    NShortProfile(9, 9, 9),
  )
  val bestForEights = Set(
    NShortProfile(1, 9, 1),
    NShortProfile(1, 3, 4),
    NShortProfile(1, 8, 9)
  )
  val bestForNines = Set(
    NShortProfile(7, 3, 1),
    NShortProfile(2, 9, 2),
    NShortProfile(8, 3, 2),
    NShortProfile(2, 1, 3),
    NShortProfile(7, 1, 8),
    NShortProfile(7, 2, 9)
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
    val powerful = Set(27)
    val allSpecial = karmic ++ masterly ++ protective ++ powerful
  }
}
