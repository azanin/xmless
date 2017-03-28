package com.yazanin.xmless.encoder

import com.yazanin.xmless.model.{Attribute, Elems, XNone, XmlValue}
import shapeless.{HList, LabelledGeneric, Lazy}

trait XmlEncoder[T] {
  def encode(value: T): XmlValue
}

object XmlEncoder {


  def apply[T](implicit summoner: XmlEncoder[T]): XmlEncoder[T] = summoner

  private def instance[T](f: T => XmlValue): XmlEncoder[T] = new XmlEncoder[T] {
    override def encode(value: T): XmlValue = f(value)
  }

  implicit val stringEncoder = instance[String](s => Attribute(s))

  implicit val bigDecimalEncoder = instance[BigDecimal](s => Attribute(s.toString))

  implicit val intEncoder = instance[Int](s => Attribute(s.toString))

  implicit val booleanEncoder = instance[Boolean](b => if(b) Attribute("Y") else Attribute("N"))

  implicit def productEncoder[A, H <: HList](implicit
                                             generic: LabelledGeneric.Aux[A, H],
                                             encoder: Lazy[ElemEncoder[H]]
                                            ): XmlEncoder[A] =
    instance[A] { product => encoder.value.encode(generic.to(product)) }


  implicit def optionEncode[A](implicit encoder: XmlEncoder[A]): XmlEncoder[Option[A]] = instance[Option[A]] {
    opt: Option[A] => opt.fold[XmlValue](XNone)(value => encoder.encode(value))
  }

  implicit def seqEncoder[A](implicit productEncoder: XmlEncoder[A]): XmlEncoder[Seq[A]] = instance[Seq[A]] {
    seq: Seq[A] => seq.foldLeft(Elems(Nil): XmlValue)((acc: XmlValue, actual: A) =>
      acc match {
        case els: Elems => els.copy(elems =  els.elems :+ productEncoder.encode(actual) )
        case _ => throw new Exception
      }
    )
  }
}
