package knowledgeGraph

import scala.collection.immutable.Vector
    
class EdgeType(name : String){

      private var subjectNodes = Vector[NodeType]()
      private var objectNodes  = Vector[NodeType]()

      def canLeadFrom(nt : NodeType) = subjectNodes.contains(nt)
      def canLeadsTo  (nt : NodeType) = objectNodes.contains(nt)

      def letLeadFrom(nt : NodeType) = {subjectNodes = subjectNodes :+ nt}
      def letLeadTo  (nt : NodeType) = {objectNodes  = objectNodes :+ nt}
} // EdgeType