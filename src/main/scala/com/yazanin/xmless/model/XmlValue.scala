package com.yazanin.xmless.model

import scala.xml.{Null, Text, TopScope}

sealed trait XmlValue

case class Attribute(value: String) extends XmlValue

case class Elem(xmlValues: Seq[(String, XmlValue)]) extends XmlValue

case class Elems(elems: Seq[XmlValue]) extends XmlValue

case object None extends XmlValue


object XmlValue {

  implicit class ToElem(xml: XmlValue) {
    def toElem(rootLabel: String) = {
      xml match {
        case xml: Elem => elemValueToElem(rootLabel, xml)
        case _ => throw new Exception("shit")
      }
    }

    private def elemValueToElem(label: String, elemValue: Elem): scala.xml.Elem = {
      elemValue.xmlValues
        .foldLeft(
          scala.xml.Elem(
            prefix = null,
            label = label.capitalize,
            attributes = Null,
            scope = TopScope,
            minimizeEmpty = true
          )
        )((elem: scala.xml.Elem, actual: (String, XmlValue)) => {
          actual._2 match {
            case None => elem
            case Attribute(v) => elem % scala.xml.Attribute(key = actual._1.capitalize, value = Text(v), next = Null)
            case e: Elem => elem.copy(child = elem.child ++ elemValueToElem(actual._1, e))
            case es: Elems => es.elems.foldLeft(
              scala.xml.Elem(
                prefix = null,
                label = label.capitalize,
                attributes = Null,
                scope = TopScope,
                minimizeEmpty = true
              )
            )((acc: scala.xml.Elem, value: XmlValue) => value match {
                case v: Elem => elem.copy(child = acc.child ++ elemValueToElem(actual._1, v))
                case _ => throw new Exception
              }
            )
          }
        })
    }

  }

}