import scala.reflect._
import scalation.math.{Complex, Rational, Real}
import scalation.math.StrO
import scalation.math.StrO.StrNum
import scalation.math.TimeO

// MetaGraph for Properties 
// Provides function for storing, deleting and updating property and its types

import scala.collection.mutable.HashMap
//import java.util.HashMap
abstract class Property()
{
	type PrimitiveType = Real.type | Rational.type | StrO.type | TimeO.type | Complex.type | Int.type | Double.type | Long.type

	val Prop = new HashMap[String,PrimitiveType]()
	
	protected def addProperty(name:String, value:PrimitiveType): Unit = 
		Prop.put(name, value)
	end addProperty           // addProperty

	protected def removeProperty(name:String): Unit = 
		Prop.remove(name)
	end removeProperty        // removeProperty
	
	protected def getPropertiesList(): HashMap[String,PrimitiveType]=
		Prop
	end getPropertiesList
	
	/*def updatePrimitiveType(name:String, newPrType:PrimitiveType): Unit =
		Prop.put(name,newPrType)
	end updatePrimitiveType   // updatePrimitiveTYpe}*/
}


//abstract class for Element
abstract class Element extends Property{
	 
	 var comparisonResult = false
	 
	 var ElementProp = new HashMap[String,PrimitiveType]()				// A HashMap to store names of properties and their PrimitiveType
	 
	 type Primitive = Real | Rational | StrO.StrNum | TimeO.TimeNum | Complex | Int | Double | Long		 // To provide values for property e.g., name is "Node1"

	 var ElementPropValues = List[Primitive]()	  		  // A list to store values of node
	 
	 def setEProperties(Properties:HashMap[String,PrimitiveType]) : Unit=	// Function to set Element's MetaProperties
	     ElementProp = Properties
	 end setEProperties
	 
	 def getEProperties():HashMap[String,PrimitiveType] =
	     ElementProp
	 end getEProperties		//end getEProperties
	 
	 def setEPropertyValues(PropertyValues:Primitive*):Unit =

	 if(ElementProp.size != PropertyValues.size )
	 {
	 println(s"${ElementProp.size} values Required")
	 } //end main if

	 else if(ElementProp.size == PropertyValues.size )
	 {
	 var MismatchCount = CompareValuesWithType(ElementProp, PropertyValues.toList)   //A variable to store number of mismatches between PropertyTypes and ValueTypes
	 
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
	 end setEPropertyValues		// end setEPropertyValues

	 // Function to get values of Element's Properties
	 def getEPropertyValues(): List[Primitive]=
	     ElementPropValues
	 end getEPropertyValues		//end getEPropertyValues


	 // FUnction to Compare TypeOfProperties with given ValueList
	 def CompareValuesWithType(PropertyList: HashMap[String,PrimitiveType], PropertyValues:List[Primitive]): Int={
	 
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
	 
	 
	
}

//case class for Node
class Node extends Element{

private var id=NodeId.currentCount()

def getId():Int=	//Function to get Node Id
    id
end getId

} //Node

//object for counter of id
object NodeId {
  var count = 0

  def currentCount(): Int = 
      count += 1
      count
  end currentCount

} //NodeId




//case class for NodeType
case class NodeType(node : Node){

} //NodeType

//case class for EdgeType
case class EdgeType(edge : Edge){

} //EdgeType

//case class for Edge
case class Edge(source:Node,target:Node)extends Element(){
var to = target
var from = source

def setNodesForEdge(fromNode:Node , toNode:Node) : Unit=
    from = fromNode
    to = toNode
end setNodesForEdge    //end setNodesForEdge

def getNodesofEdge()=
    (from,to)
end getNodesofEdge    //end getNodesofEdge

}   //Edge


//case class TimeStamp
case class TimeStamp(edge : Edge){
} //TimeStamp



class Road extends Node{

addProperty("RoadName",StrO)
addProperty("PostMile",Double)

private val RoadProp = getPropertiesList()
ElementProp = RoadProp

def getRoadProperties(): HashMap[String,PrimitiveType] =       // Function to get Road Propertiess
    RoadProp
end getRoadProperties		// end getRoadProperties

} //Road



class Sensor extends Node{

addProperty("SesorName",StrO)
addProperty("Latitude",Double)
addProperty("Longitude",Double)

private val SensorProp= getPropertiesList()
ElementProp = SensorProp

def getSensorProperties():HashMap[String,PrimitiveType]=	// Function to set values of Road's Properties
    SensorProp
end getSensorProperties 
    
} //Sensor


object RoadTest extends App
{
var road1 = new Road()
var sensor1 = new Sensor()

road1.setEPropertyValues("Highway1",34.0)
sensor1.setEPropertyValues("Sensor1",33.0,34.0)

var edge1= new Edge(road1,sensor1)

var ToNode = edge1.to
var FromNode = edge1.from

println(s"Nodes in edges:")
println(s"First Node: ${FromNode.getEPropertyValues()}")
println(s"ID for firstNode:${FromNode.getId()}")

println(s"Sencond Node: ${ToNode.getEPropertyValues()}")
println(s"ID for firstNode:${ToNode.getId()}")
} //RoadTest
