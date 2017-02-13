package com.yazanin.xmless.encoder

import com.yazanin.xmless.model.{Attribute, Elems, XmlValue}
import shapeless.{HList, LabelledGeneric, Lazy}

trait XmlEncoder[T] { self =>
  def encode(value: T): XmlValue
  def contramap[A](f:A => T):XmlEncoder[A] = new XmlEncoder[A] {
    override def encode(value: A): XmlValue = {
      val t = f(value)
      self.encode(t)
    }

  }
}

object XmlEncoder {


  def apply[T](implicit summoner: XmlEncoder[T]): XmlEncoder[T] = summoner

  private def instance[T](f: T => XmlValue): XmlEncoder[T] = new XmlEncoder[T] {
    override def encode(value: T): XmlValue = f(value)
  }

  implicit val stringEncoder = instance[String](s => Attribute(s))

  implicit val bigDecimalEncoder = instance[BigDecimal](s => Attribute(s.toString))

  implicit val booleanEncoder = instance[Boolean](b => if(b) Attribute("Y") else Attribute("N"))

  implicit def productEncoder[A, H <: HList](implicit
                                             generic: LabelledGeneric.Aux[A, H],
                                             encoder: Lazy[ElemEncoder[H]]
                                            ): XmlEncoder[A] =
    instance[A] { product => encoder.value.encode(generic.to(product)) }

  implicit def seqEncoder[A](implicit productEncoder: XmlEncoder[A]): XmlEncoder[Seq[A]] = instance[Seq[A]] {
    seq: Seq[A] => seq.foldLeft(Elems(Nil): XmlValue)((acc: XmlValue, actual: A) =>
      acc match {
        case a: Elems => a.copy(elems = Seq(productEncoder.encode(actual)) ++ a.elems)
        case _ => throw new Exception
      }
    )
  }
}