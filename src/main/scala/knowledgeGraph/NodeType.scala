package knowledgeGraph

import scala.collection.immutable.Vector
import scala.collection.mutable.HashMap
import scala.reflect._
import scalation.math.{Complex, Rational, Real}
import scalation.math.StrO
import scalation.math.StrO.StrNum
import scalation.math.TimeO

/*
 * The NodeType object holds a representation of all of the Nodes of this
 * NodeType as well as the meta information around this NodeType including
 * the properties that a Node of this NodeType can have, the domains for
 * those properties, and the EdgeTypes for which this NodeType can be
 * either a subject node or object node. 
 * 
 */

class NodeType(name : String){
      
    //The number of properties for this NodeType
    var nProp = 0

    //The number of nodes of this NodeType
    var nNodes = 0 

    //The map for getting node system id numbers from node names
    private val idMap = new HashMap[String,Int]()

    //The map for getting the index of a property in the meta info vector from the property names
    private val propertyNameMap = new HashMap[String, Int]()

    //The row representation of the nodes of this type
    private val rows = Vector[PrimitiveType]()

    //The columnar representation of thennodes of this NodeType
    private val cols = Vector[PrimitiveType]()

    /*
     * The map for getting the property values of a node from the system id for the node
     * Note that you'll need the system id for the node which you'll probably need to get
     * from the idMap 
     */
    private val nodes = new HashMap[Int,Vector[PrimitiveType]]()

    //The schema (with domains) for nodes of this NodeType
    private var meta = Vector[String]()

    //The EdgeTypes for which this node can be either a subject or object
    private var subjectEdges = Vector[EdgeType]()
    private var objectEdges  = Vector[EdgeType]()

    /*
     * The getters for the types of edges that a node of this type can
     * be either a subject or object for
     */
    def isSubjectFor(et : EdgeType) = {subjectEdges.contains(et)}
    def isObjectFor (et : EdgeType) = {objectEdges.contains(et)}

    /*
     * The setters for the types of edges that a node of this type can
     * be either a subject or object for      
     *
     */
    def makeSubjectFor(et : EdgeType) = {subjectEdges = subjectEdges :+ et}
    def makeObjectFor (et : EdgeType) = {objectEdges  = objectEdges :+ et}

    /*
     * Add the property and it's domain to this NodeType
     *
     */
    def addProperty(propertyName : String, domain : String) =
    {
    	nProp += 1
    	propertyNameMap.update(propertyName, nProp)
	    meta = meta :+ domain
    }

    /*
     * Get the meta information for this NodeType
     * TODO : Fix this to return a copy of the array
     */
    def getMeta() = meta

    /*
     * Get the number of nodes of this NodeType
     */
    def numNodes() = nNodes

    /*
     * Get the number of properties for this NodeType
     */
    def numProperties() = nProp
    
} // NodeType

object NodeTypeTester extends App{
    val pass = "Pass"
    val fail = "Fail"
    val nt1 = new NodeType("nt1")

    val edgeTypes = Array(new EdgeType("et1"),new EdgeType("et2"))

    println("Testing object / subject EdgeType compatability funcationality for NodeType")
    for ( et <- edgeTypes ) {
        print(s"\tempty node type contains no subject EdgeTypes: ")
        println(s"${if (!nt1.isSubjectFor(et)) pass else fail}")
    }
    
    for ( et <- edgeTypes ) {
        print(s"\tempty node type contains no object EdgeTypes: ")
        println(s"${if (!nt1.isObjectFor(et)) pass else fail}")
    }
    
    nt1.makeSubjectFor(edgeTypes(0))
    nt1.makeObjectFor(edgeTypes(1))

    for ( et <- 0 until edgeTypes.length ) {
        val expected = {et==0}
        val found = nt1.isSubjectFor(edgeTypes(0))
        println(s"\tnodeType isSubjectFor test ${et}: ${if (expected == found) pass else fail}")
    }
    
    for ( et <- 0 until edgeTypes.length ) {
        val expected = {et==1}
        val found = nt1.isObjectFor(edgeTypes(0))
        println(s"\tnodeType isObjectFor test ${et}: ${if (expected == found) pass else fail}")
    }
    
    println("Testing add properties.")
    println(s"\tMeta information before adding properties: \n\t${nt1.getMeta()}")

    val props = Array("p1","p2","p3")
    val domns = Array("d1","d2","d3")

    for ( kv <- props zip domns ) {
        nt1.addProperty(kv(0),kv(1))
        println("\tAfter adding property, meta: \n\t${nt1.getMeta()}\n\tnProp: ${nt1.numProperties()}")
    }
    
    
}//NodeTypeTester	