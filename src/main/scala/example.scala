package object example {
  implicit class compareEithers[A,B](a: Either[A,B]) {
    def mustEqual(b: Either[A,B]) {
      if(a == b)
        println("OK")
      else
        println(s"KO!!! $a != $b")
    }
  }
}
