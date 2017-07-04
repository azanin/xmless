package com.yazanin.xmless.encoder

import scala.xml.Elem

trait XmlEncoder[I] {
  def encode(input: I): Elem
}

object XmlEncoder {
  def apply[T](implicit summoner: XmlEncoder[T]): XmlEncoder[T] = summoner
}

trait XmlEncoderInstances {

  implicit def xmlEncoderInstances[I](implicit xmlValueEncoder: XmlValueEncoder[I]): XmlEncoder[I] =
    (input: I) => xmlValueEncoder.encode(input).toElem(input.getClass.getSimpleName)

}
