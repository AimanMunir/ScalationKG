import scala.reflect._
import scalation.math.{Complex, Rational, Real}
import scalation.math.StrO
import scalation.math.TimeO

// MetaGraph for Properties 
// Provides function for storing, deleting and updating property and its types

import scala.collection.mutable.HashMap
//import java.util.HashMap
case class Property()
{
	type PrimitiveType = Real.type | Rational.type | StrO.type | TimeO.type | Complex.type | Int.type | Double.type | Long.type
	//type PrimitiveType = Int.type | String.type | Double.type | Long.type | Boolean.type
	

	val Prop = new HashMap[String,PrimitiveType]()
	
	def addProperty(name:String, value:PrimitiveType): Unit = 
	{
		Prop.put(name, value)
	} // addProperty
	
	def removeProperty(name:String): Unit = 
	{
		Prop.remove(name)
	} // removeProperty
	
	def getPropertiesList(): HashMap[String,PrimitiveType]=
	{
		Prop
	}
	
	/*def updatePrimitiveType(name:String, newPrType:PrimitiveType): Unit =
	{
		Prop.put(name,newPrType)
	} // updatePrimitiveTYpe}*/
}

class Person{
 
     type PrimitiveType = Real.type | Rational.type | StrO.type | TimeO.type | Complex.type | Int.type | Double.type | Long.type	
     
     type Primitive = Real | Rational | StrO.StrNum | TimeO.TimeNum | Complex | Int | Double | Long
     
     var personPropValues = new HashMap[String, Primitive]
     		
     var personProp = new HashMap[String,PrimitiveType]
     
     def setProperties(Properties:HashMap[String,PrimitiveType]) :Unit=
     {
	personProp = Properties
	}
  
     def getProperties():HashMap[String,PrimitiveType] =
     {
      	  personProp
     }

     def setPropertyValues(PropertyValue:Primitive*):Unit =
     {
	var propNames = personProp.keys.toList
	println("propertyNames = "+propNames)
	println("PropertyValues = "+PropertyValue)
	}
  
}


class Road{
      
      type PrimitiveType = Real.type | Rational.type | StrO.type | TimeO.type | Complex.type | Int.type | Double.type | Long.type
      
      type Primitive = Real | Rational | StrO.StrNum | TimeO.TimeNum | Complex | Int | Double | Long

      var RoadPropValues = new HashMap[String, Primitive]

      var RoadProp = new HashMap[String,PrimitiveType]
      
      def setProperties(Properties:HashMap[String,PrimitiveType]) : Unit=
      {  	  
      RoadProp = Properties
      }
      
      def getProperties():HashMap[String,PrimitiveType] =
      {   
      RoadProp  
      }

      def setPropertyValues(PropertyValue:Primitive*):Unit =
      {
      var propNames = RoadProp.keys.toList
      println("propertyNames = "+propNames)
      println("PropertyValues = "+PropertyValue)
      }
      

}

class Sensor{

      type PrimitiveType = Real.type | Rational.type | StrO.type | TimeO.type | Complex.type | Int.type | Double.type | Long.type

      type Primitive = Real | Rational | StrO.StrNum | TimeO.TimeNum | Complex | Int | Double | Long

      var SensorPropValues = new HashMap[String, Primitive]

      var SensorProp = new HashMap[String,PrimitiveType]

      def setProperties(Properties:HashMap[String,PrimitiveType]) : Unit=
      {
      SensorProp = Properties
      }

      def getProperties():HashMap[String,PrimitiveType] =
      {
      SensorProp
      }

      def setPropertyValues(PropertyValue:Primitive*):Unit =
      {
      var propNames = SensorProp.keys.toList
      propNames
      }

}



//case class for Element
abstract class Element(properties : Property){
var pro = properties.Prop
}

//case class for Node
case class Node(propertiess : Property) extends Element(propertiess){
var id = 0
var name = ""

    def setProperties(NodeName:String) : Unit=
    {
    name = NodeName
    }

    def getProperties():String =	
    {
    name
    }



}

//case class for NodeType
case class NodeType(node : Node){

}

//case class for EdgeType
case class EdgeType(edge : Edge){

}

//case class for Edge
case class Edge(source:Node,target:Node,propertiess:Property)extends Element(propertiess){
var to = target
var from = source

    def setNodeForEdges(fromNode:Node , toNode:Node) : Unit=
    {
    from = fromNode
    to = toNode
    }

    def getNodes()=
    {
    (from,to)
    }



}

//case class TimeStamp
case class TimeStamp(edge : Edge){

}

object PropertyTest extends App
{
	var pro = new Property()
	pro.addProperty("name",StrO)
	println(pro.Prop)
	pro.addProperty("age",StrO)
	println(pro.Prop)
	//pro.removeProperty("p1")
	//pro.updatePrimitiveType("p2",StrO)
	var p = pro.getPropertiesList()
	println(p.keys)
	
	println("PersonTest")
	var person = new Person()
	person.setProperties(p)
	println("Person prop")
	
	println(person.getProperties())
	person.setPropertyValues("abc","123")

}

object ElementTest extends App
{
	println("For Node 1")
	var pro1 = new Property()
	println("Adding p1")
	pro1.addProperty("name",StrO)
	println("Adding p2")
	pro1.addProperty("age",StrO)
	var node1 = new Node(pro1)
	var name1 = node1.pro
	println(s"node1 == $name1")


	println("\n\nFor node 2")
	var pro2 = new Property()
	println("Adding p3")
	pro2.addProperty("p3",StrO)
	println("Adding p4")
	pro2.addProperty("p4",StrO)
	val node2 = new Node(pro2)
	val name2 = node2.pro
	println(s"mynode2 == $name2")
	

	println("\n\n\n")
	println("EdgeTest")
	var edgeproperty = new Property()
	edgeproperty.addProperty("EdgeProperty1",StrO)
	var edge1 = new Edge(node1,node2,edgeproperty)
	var node1ID =node1.id
	var node2ID = node2.id
	println(s"From node: $node1ID")
	println(s"To node: $node2ID")
	val edgePro = edge1.pro
	println(s"Edge properties: $edgePro")
	println("\n\n\n")
}

object RoadSensorTest extends App
{
	var RoadProp = new Property()
	println("Road Test")
	println("Adding property 1")
	RoadProp.addProperty("RoadName",StrO)
	println("Adding property 2")
	RoadProp.addProperty("pm",Double)
	var Road = new Node(RoadProp)
	
	println(s"Road1 prop == $RoadProp")

	// Create Road object
	var Road1 = new Road()
	Road1.setProperties(RoadProp.Prop)
	Road1.setPropertyValues("Highway", 35.5)

	
	// Create Sensor Properties
	println("\n\n For Sensor")
	println("SENSOR Test")
	var SensorProp = new Property()
	println("Adding property 1")
	SensorProp.addProperty("SensorName",StrO)
	println("Adding property 2")
	SensorProp.addProperty("longitude",Double)
	println("Adding property 3")
	SensorProp.addProperty("latitude", Double)
	var Sensor = new Node(SensorProp)
	
	println(s"Sensor Prop == $SensorProp.Prop")

	// Create Sensor Object
	var Sensor1 = new Sensor()
	Sensor1.setProperties(SensorProp.Prop)
	Sensor1.setPropertyValues("Sensor",32.1,-2.2)
}

