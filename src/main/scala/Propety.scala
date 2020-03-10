import scala.reflect._
import scalation.math.{Complex, Rational, Real}
import scalation.math.StrO
import scalation.math.StrO.StrNum
import scalation.math.TimeO

// MetaGraph for Properties 
// Provides function for storing, deleting and updating property and its types
import scala.collection.mutable._
import scala.collection.mutable.HashMap
//import java.util.HashMap

class PropertyGraph(name: String)
{
var GraphName = name 
var GraphElements = List[Element]()

def addElements(element: Element): Unit =
          GraphElements = GraphElements.::(element)
end addElements      // addElements

def getElements(): List[Element] =
          GraphElements
end getElements      // getElements


}

abstract class Property
{	 
	  
	
}


//abstract class for Element
abstract class Element{
	 
	 //addProperty 
	 //set and getProperty
	 type PrimitiveType = Real.type | Rational.type | StrO.type | TimeO.type | Complex.type | Int.type | Double.type | Long.type
	 
	 type Primitive = Real | Rational | StrO.StrNum | TimeO.TimeNum | Complex | Int | Double | Long          // To provide values for property e.g., name is "Node1"
	
	 var ElementProp = new LinkedHashMap[String,PrimitiveType]()
	 
	 var comparisonResult = false
	 
	 var ElementPropValues = List[Primitive]()                        // A list to store values of node
	 
	 protected def addProperty(name: String, value: PrimitiveType): Unit =
	 	   ElementProp.put(name, value)
	 end addProperty           // addProperty

	 protected def removeProperty(name: String): Unit =
	 	   ElementProp.remove(name)
	 end removeProperty        // removeProperty
	 
	 /*def updatePrimitiveType(name: String, newPrType: PrimitiveType): Unit =
	      Prop.put(name,newPrType)
	 end updatePrimitiveType   // updatePrimitiveTYpe}*/

	 def getEProperties(): LinkedHashMap[String,PrimitiveType] =
	     ElementProp
	 end getEProperties	   //end getEProperties
	 
	 def setEPropertyValues(PropertyValues: Primitive*):Unit ={

	 if(ElementProp.size != PropertyValues.size )
	 {
	 println(s"${ElementProp.size} values Required")
	 println("Couldn't set properties\n")
	 } //end main if
	 
	 if(ElementProp.size == PropertyValues.size )
	 {
	 var MismatchCount = 0
	 
	 MismatchCount = CompareValuesWithType(ElementProp, PropertyValues.toList)   //A variable to store number of mismatches between PropertyTypes and ValueTypes
	 
	 if(MismatchCount == 0)
	 {
	 ElementPropValues = PropertyValues.toList
	 println("Properties successfully set")
	 } //end 2nd if
	 else
	 {
	 println("Couldn't set properties\n")
	 } //end 2nd else

	 } //end main else
	 }//end setEPropertyValues		// end setEPropertyValues

	 // Function to get values of Element's Properties
	 def getEPropertyValues(): List[Primitive]=
	     ElementPropValues
	 end getEPropertyValues		//end getEPropertyValues


	 // FUnction to Compare TypeOfProperties with given ValueList
	 def CompareValuesWithType(PropertyList: LinkedHashMap[String,PrimitiveType], PropertyValues:List[Primitive]): Int={
	 
	 var PropNames = PropertyList.keys.toList
	 var PropTypes = PropertyList.values.toList 
	 var MismatchCount = 0
	 for(i <- 0 until PropTypes.size)
	 {
	 var PropType=(PropTypes(i).getClass).toString.replace("$","")   //replacing extra characters so comparison can be generalized
	 var PropValueType = (PropertyValues(i).getClass).toString.replace("class java.lang.","")
	 PropValueType = PropValueType.replace("Integer","Int") 
	 if( PropType.equals(PropValueType) || PropValueType.contains(PropType) || PropType.contains(PropValueType) )   
	 {
		//do nothing because properties match
	 }//end if
	  
	 else{
	 println(s"Type mismatch for ${PropNames(i)} (Required:${PropType.replace("class ","")} Found:${PropValueType})")
	 MismatchCount	= MismatchCount+1
	 } // end else

	 }//end for loop

	 return MismatchCount
	 
	 } //end CompareValuesWithTypes
	 
} //end Element

//class for Node
class Node(g: PropertyGraph) extends Element{

private var id=NodeId.currentCount()
var SubjectEdges = List[Edge]()
var ObjectEdges = List[Edge]()

def getId():Int=  //Function to get Node Id
    id
end getId

def addSubjectEdge(edge: Edge): Unit =
	  SubjectEdges = SubjectEdges.::(edge)
end addSubjectEdge	// addSubjectEdge

def addObjectEdge(edge: Edge): Unit =
	  ObjectEdges = ObjectEdges.::(edge)
end addObjectEdge	// addObjectEdge

def getSubjectEdges(): List[Edge] =
          SubjectEdges
end getSubjectEdges      // getSubjectEdges

def getObjectEdges(): List[Edge] =
          ObjectEdges
end getObjectEdges      // getObjectEdges


} //Node

//object for counter of id
object NodeId {
  var count = 0

  def currentCount(): Int = 
      count += 1
      count
  end currentCount

} //NodeId



//case class for Edge
class Edge(g: PropertyGraph,source: Node,target: Node)extends Element(){
var to = target
var from = source

var SubjectNodes = List[Node]()
var ObjectNodes = List[Node]()

def addSubjectNode(node:Node): Unit =
          SubjectNodes = SubjectNodes.::(node)
end addSubjectNode      // addSubjectNode

def addObjectNode(node:Node): Unit =
          ObjectNodes = ObjectNodes.::(node)
end addObjectNode       // addObjectNode

def setNodesForEdge(fromNode:Node , toNode:Node) : Unit=
    from = fromNode
    to = toNode
end setNodesForEdge    //end setNodesForEdge

def getNodesofEdge()=
    (from,to)
end getNodesofEdge    //end getNodesofEdge

}   //EdgeType


//case class TimeStamp
case class TimeStamp(edge : Edge){
} //TimeStamp



class Road(g: PropertyGraph) extends Node(g: PropertyGraph){

addProperty("RoadName",StrO)
addProperty("Speed_limit",Int)
addProperty("Lanes",Int)

private val RoadProp = getEProperties()

def getRoadProperties(): LinkedHashMap[String,PrimitiveType] =       // Function to get Road Propertiess
    RoadProp
end getRoadProperties		// end getRoadProperties

} //Road



class Sensor(g: PropertyGraph) extends Node(g: PropertyGraph){

addProperty("SensorName",StrO)
addProperty("Latitude",Double)
addProperty("Longitude",Double)

private val SensorProp= getEProperties()

def getSensorProperties(): LinkedHashMap[String,PrimitiveType]=	// Function to set values of Road's Properties
    SensorProp
end getSensorProperties 
    
} //Sensor


case class Intersection (g: PropertyGraph, r1: Road, r2: Road)
     extends Edge (g, r1, r2)
{
addProperty("Control",StrO)

} 

case class On (g: PropertyGraph, r1: Road, s1: Sensor)
     extends Edge (g, r1, s1)
{


} 


object RoadIntersectionTest extends App
{
val g = PropertyGraph("Athens_Road")

val main = Road (g) 
main.setEPropertyValues("main",35,2)


val oak  = Road (g)
oak.setEPropertyValues("oak",45,1)

val main2oak = Intersection (g, main, oak)
main2oak.setEPropertyValues("stop_sign")




}




object RoadTest extends App
{
val g = PropertyGraph("Athens_Road")

val main = Road (g)
main.setEPropertyValues("main",35,2)

var sensor1 = new Sensor(g)
sensor1.setEPropertyValues("Sensor1",33.0,34.0)

//println(road1.ElementProp)
//println(sensor1.ElementProp)

var sensorOnMain = new On(g,main,sensor1)

var ToNode = sensorOnMain.to
var FromNode = sensorOnMain.from

println(s"Nodes in edges:")
println(s"First Node: ${FromNode.getEPropertyValues()}")
println(s"ID for firstNode:${FromNode.getId()}")

println(s"Sencond Node: ${ToNode.getEPropertyValues()}")
println(s"ID for secondNode:${ToNode.getId()}")
} //RoadTest
