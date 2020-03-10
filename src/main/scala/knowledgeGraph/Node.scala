package knowledgeGraph

import scala.collection.mutable.HashMap

class Node(id : String)
      extends Elem(id){

      def apply(key : String) = properties.getOrElse(key,null)
      
} 