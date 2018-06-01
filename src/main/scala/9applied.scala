object applied {

    // def println(s: String): Unit = () //scala.Predef.println(s)

    // def readLine(): String = "" //scala.io.StdIn.readLine()

    /**
      An `IO[A]` is a description of an effectful program that will produce an `A`.
     */
    final case class IO[A](unsafePerformIO: Function[Unit, A]) { self =>
      def map[B](f: A => B): IO[B] = IO(f.compose(unsafePerformIO))
      def flatMap[B](f: A => IO[B]): IO[B] = 
        // IO((_ : Unit) => f(unsafePerformIO()).unsafePerformIO())
       IO(f(unsafePerformIO()).unsafePerformIO) //claimed to not work, if not working, use line above
       

    }
    object IO {
        def point[A](a: => A): IO[A] = IO((_ : Unit) => a)


    }

    def println(s: String): IO[Unit] = IO.point(scala.Predef.println(s))

    val readLine: IO[String] = IO.point(scala.io.StdIn.readLine())


    case class Player(name: String)
    case class State(player: Player)

    sealed trait Command
    case object Exit extends Command
    case object Look extends Command
    case class Pickup(what: String) extends Command


    def parseInput(input: String): Either[String, Command] = {
      val tokens: List[String] = input.split("\\s+").toList.filter(_ != "").map(_.toLowerCase)

      tokens match {
        case Nil => Left("No command buddy ji")
        case "look" :: _ => Right(Look)
        case "exit" :: _ | "quit" :: _ => Right(Exit)
        case "pickup" :: what :: _ => Right(Pickup(what))
        case _ => Left(s"What is this command buddy ji \n $input" )
      }
    }

    def update(command: Command, state: State): (String, Option[State]) = 
      command match {
        case Exit =>  ("Goodbye, " + state.player.name, None)
        case Look => ("Looking", Some(state))
        case Pickup(what) => (s"Pickup $what ? No!", Some(state))
      }

    def mainLoop(state: State): IO[Unit] = {

      val maybeContinue: Command => IO[Unit] = { (command: Command) => 
        val (msg, next) = update(command, state)

        for {
          _ <- println(msg)
          _ <- next.fold(IO.point(()))(mainLoop)
        } yield ()
      }

      def printContinue(line: String): IO[Unit] =
        for {
          _ <- println(line)
          _ <- mainLoop(state) //continue or exit game
        } yield ()

      for {
        // input  <- readLine
        // either = parseInput(input)
        // above lines or write in one line
        either <- readLine.map(parseInput)
        _      <- either.fold(printContinue, maybeContinue)
      } yield ()

    }


    val program: IO[Unit] = 
      for {
        _    <- println("What is your name?")
        name <- readLine
        _    <- println(s"I am $name")
        player = Player(name)
        state = State(player)
        _     <- mainLoop(state)
      } yield ()

   def main(args: Array[String]): Unit = program.unsafePerformIO() // only non-fp

}