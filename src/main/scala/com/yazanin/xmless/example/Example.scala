package com.yazanin.xmless.example

import com.yazanin.xmless.encoder.{XmlEncoder, XmlEncoderInstances, XmlValueEncoderInstances}


case class Seat(number: Int)

case class Seats(seat: Seq[Seat])

case class Room(name: String, seats: Seats)

object Example extends App with XmlEncoderInstances with XmlValueEncoderInstances {

  val room = Room("Cassandra", seats = Seats(Seq(Seat(number = 1), Seat(2))))
  val encoder = XmlEncoder[Room]

  println(encoder.encode(room))
}
