package com.yazanin.xmless.example

import com.yazanin.xmless.encoder.XmlEncoder

case class Seat(number: Int)

case class Seats(seat: Seq[Seat])

case class Room(name: String, seats: Seats)


case class PaymentMethod(paymentType: String)

case class PaymentMethods(paymentMethod: Seq[PaymentMethod])

case class OrderLine(primeLineNo: Int)

case class OrderLines(orderLine: Seq[OrderLine])

case class Order(
                  orderNo: String,
                  orderLines: OrderLines,
                  paymentMethods: PaymentMethods,
                  pippo: Option[Room],
                  levelOfService: Option[String] = Some("1234"))


object Example extends App {

  val room = Room("Cassandra", seats = Seats(Seq(Seat(number = 1), Seat(2))))
  val encoder = XmlEncoder[Room]


  val order = Order(
    orderNo = "12345",
    orderLines = OrderLines(
      orderLine = Seq(
        OrderLine(primeLineNo = 1),
        OrderLine(primeLineNo = 2)
      )
    ),
    paymentMethods = PaymentMethods(
      paymentMethod = Seq(PaymentMethod(paymentType = "CREDIT_CARD"))
    ),
    pippo = Some(room)
  )

  val orderLines2 = OrderLines(
    orderLine = Seq(
      OrderLine(primeLineNo = 1),
      OrderLine(primeLineNo = 2)
    )
  )

  val orderEncoder = XmlEncoder[Order]
  val orderLinesEncoder = XmlEncoder[OrderLines]

  println(encoder.encode(room).toNodeSeq("Room"))
  println(orderEncoder.encode(order).toNodeSeq("Order"))
  println(orderLinesEncoder.encode(orderLines2).toNodeSeq("OrderLines"))


}
