DTNode2 <- function(edges, ichildren, maxind, leafstart){
  #""" Represent a Dirichlet Tree node """
  
  self <- list(
    edges = edges,
    ichildren = ichildren,
    maxind = maxind,
    leafstart = leafstart
  )
  
  self$numLeaves <- function(){
    #Get the number of leaves below this node
    n = sum(unlist(lapply(self$ichildren, function(ichild) ichild$numLeaves())))
    
    return (n + length(self$edges) - length(self$ichildren))
  }
  
  self$tupleConvert <- function() {
    #Given a Dirichlet Tree in node=List form,
    #recursively convert to node=Tuple form
    
    tchildren = lapply(self$ichildren, function(ichild) ichild$tupleConvert())
    
    return(DTNode2(self$edges, tchildren, self$maxind, self$leafstart))
  }
  
  self <- list2env(self)
  class(self) <- "DTNode2"
  
  return(self)
  
}

MLNode2 <- function(edges, ichildren, maxind, leafstart, words){
  #""" Represent a MustLink-node """
  
  self <- DTNode2(edges, ichildren, maxind, leafstart)
  self$words = words
  
  
  class(self) <- append(class(self),"MLNode2")
  
  return(self)

}

MultiNode2 <- function(edges, ichildren, maxind, leafstart, words, variants){
  #Represent a MultiNode
  # (can take on different subtree structures)
   
  self <- DTNode2(edges, ichildren, maxind, leafstart)
  self$words = words
  self$variants = variants

  self$tupleConvert <- function(){
    #For MultiNode, will need to also convert variants """
    tchildren = lapply(self$ichidlren, function(ichild) ichild$tupleConvert())
    tvars = lapply(self$invariants, function(invar) list(fakeroot = invar$fakeroot$tupleConvert(), 
                                                         fake_leafmap = invar$fake_leafmap))
  
    #FIXME ichildren or tchildren
    return(MultiNode(c(), tchildren, self$maxind, 
                self$leafstart, self$words, tvars))
  }
  
  self$numLeaves <- function(){

    #Can't take len(edges) b/c edges==None by convention
    n=sum(unlist(lapply(self$ichildren, function(ichild) ifelse(ichild == NULL,NULL,ichild$numLeaves()))))
    return (n + length(self$words))
  }


  class(self) <- append(class(self),"MultiNode2")
  
  return(self)
}