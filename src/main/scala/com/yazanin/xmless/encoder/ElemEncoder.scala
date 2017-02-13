package com.yazanin.xmless.encoder

import com.yazanin.xmless.model.Elem
import shapeless.labelled._
import shapeless.{::, HList, HNil, Lazy, Witness}

trait ElemEncoder[T] extends XmlEncoder[T] {
  def encode(value: T): Elem
}

object ElemEncoder {

  def apply[T](implicit summoner: ElemEncoder[T]): ElemEncoder[T] = summoner

  private def instance[T](f: T => Elem): ElemEncoder[T] = new ElemEncoder[T] {
    override def encode(value: T): Elem = f(value)
  }

  implicit val hNilEncoder = ElemEncoder.instance[HNil](hNil => Elem(Nil))

  implicit def hListEncoder[K <: Symbol, H, T <: HList](
                                                         implicit
                                                         witness: Witness.Aux[K],
                                                         hEncoder: Lazy[XmlEncoder[H]],
                                                         tEncoder: ElemEncoder[T]
                                                       ): ElemEncoder[FieldType[K, H] :: T] = {
    val tagName = witness.value.name
    ElemEncoder.instance { (hlist: FieldType[K, H] :: T) =>
      val head = hEncoder.value.encode(hlist.head)
      val tail = tEncoder.encode(hlist.tail)
      Elem(Seq((tagName, head)) ++ tail.xmlValues)
    }
  }
}
