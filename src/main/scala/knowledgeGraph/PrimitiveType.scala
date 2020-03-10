package knowledgeGraph

import scala.reflect._
import scalation.math.{Complex, Rational, Real}
import scalation.math.StrO
import scalation.math.StrO.StrNum
import scalation.math.TimeO

type PrimitiveType = Real.type | Rational.type | StrO.type | TimeO.type | Complex.type | Int.type | Double.type | Long.type 