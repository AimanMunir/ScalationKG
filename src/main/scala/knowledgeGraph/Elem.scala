package knowledgeGraph

import scala.reflect._
import scalation.math.{Complex, Rational, Real}
import scalation.math.StrO
import scalation.math.StrO.StrNum
import scalation.math.TimeO
import scala.collection.mutable.HashMap

abstract class Elem(id : String){
    val _id = id
    val properties = new HashMap[String,PrimitiveType]()
}