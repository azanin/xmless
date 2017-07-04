package com.yazanin.xmless.encoder

import java.time.LocalDateTime
import java.time.format.DateTimeFormatter

import com.yazanin.xmless.model.{Attribute, Elems, XmlValue, None}
import shapeless.{HList, LabelledGeneric, Lazy}

trait XmlValueEncoder[T] {
  def encode(value: T): XmlValue
}

trait XmlValueEncoderInstances {

  private def instance[T](f: T => XmlValue): XmlValueEncoder[T] = (value: T) => f(value)

  implicit val stringEncoder: XmlValueEncoder[String] = instance[String](s => Attribute(s))

  implicit val bigDecimalEncoder: XmlValueEncoder[BigDecimal] = instance[BigDecimal](s => Attribute(s.toString))

  implicit val intEncoder: XmlValueEncoder[Int] = instance[Int](s => Attribute(s.toString))

  implicit val booleanEncoder: XmlValueEncoder[Boolean] = instance[Boolean](b => if(b) Attribute("Y") else Attribute("N"))

  implicit val localDateTimeEncoder: XmlValueEncoder[LocalDateTime] = instance[LocalDateTime](
    date => Attribute(date.format(DateTimeFormatter.ofPattern("yyyy-MM-dd")))
  )

  implicit def productEncoder[A, H <: HList](implicit
                                             generic: LabelledGeneric.Aux[A, H],
                                             encoder: Lazy[ElemEncoder[H]]
                                            ): XmlValueEncoder[A] =
    instance[A] { product => encoder.value.encode(generic.to(product)) }

  implicit def seqEncoder[A](implicit productEncoder: XmlValueEncoder[A]): XmlValueEncoder[Seq[A]] = instance[Seq[A]] {
    seq: Seq[A] => seq.foldLeft(Elems(Nil): XmlValue)((acc: XmlValue, actual: A) =>
      acc match {
        case els: Elems => els.copy(elems =  els.elems :+ productEncoder.encode(actual) )
        case _ => throw new Exception
      }
    )
  }

  implicit def optionEncode[A](implicit encoder: XmlValueEncoder[A]): XmlValueEncoder[Option[A]] = instance[Option[A]] {
    opt: Option[A] => opt.fold[XmlValue](None)(value => encoder.encode(value))
  }
}

object XmlValueEncoder {
  def apply[T](implicit summoner: XmlValueEncoder[T]): XmlValueEncoder[T] = summoner
}
