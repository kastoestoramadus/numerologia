package ww86.numerology.domain

import ww86.numerology.{Digit, SingleWord}

case class NShortProfile(wnętrzna: Digit, zewnętrzna: Digit, cele: Digit)

object NShortProfile {
  def apply(słowa: List[SingleWord]): NShortProfile = {
    val fullProfile = NPair(słowa)
    NShortProfile(fullProfile.vowelsDigit, fullProfile.consonantsDigit, fullProfile.bothDigit)
  }
}