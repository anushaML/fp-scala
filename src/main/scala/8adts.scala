object adts {
  
  case class Email private (value: String)

  object Email {
    def apply(value: String): Option[Email] = ??? // validation here
  }

  sealed trait PaymentMethod // sum of products
  final case class CreditCard(number: String, code: String, name: String) extends PaymentMethod
  final case object Cash extends PaymentMethod
  final case class Bank(routingNumber: String, swiftCode: String, accountNumber: String) extends PaymentMethod
  final case class Bitcount(sig: String, address: String) extends PaymentMethod

  final case class CreditCardNumber private ()

  object CreditCardNumber {
    def apply(number: String): Option[CreditCardNumber] = ??? // validation here
  }

  final case class Employee(
    name: String, 
    startDate: java.time.Instant,
    position: Position
  )

  sealed trait Position
  final case object CEO extends Position
  final case object CTO extends Position
  final case class Engineer(level: Int) extends Position
  
}