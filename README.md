# xmless
Provides generic xml encoder using shapeless. This works for a subset of the xml grammar.

# Example

```scala
  val room = Room("Cassandra", seats = Seats(Seq(Seat(number = 1), Seat(2))))
  val encoder = XmlEncoder[Room]

  println(encoder.encode(room).toNodeSeq("Room"))
```
