
#propagate must links to induce new links
#Input: a list of vectors: each denoting a user-defined set of words to appear together
propagate.mustlinks <- function(mlinks) {
  require(igraph)  
  #Get must-link connected components from pairwise mustlink constraints
  
  #Check if mustlinks is empty    
  if(length(mlinks) == 0)
    return(NULL)
  
  edges = c()     
  
  for (mlink in mlinks){
    if (length(mlink) < 2)
      break
    
    for (j in 2:length(mlink))
      edges <- rbind(edges, c(mlink[j-1], mlink[j]))
    
  }
  
  gr <- graph.data.frame(edges, directed=FALSE)
  
  dgr <- decompose.graph(gr)
  
  # Get must-link connected components
  return (lapply(dgr, function(x) V(x)$name))
  
}


#Pre-condition: Assume cannot links are pairwise constraints
propagate.cannotlinks <- function(clinks) {
  require(igraph)  
  #Get must-link connected components from pairwise mustlink constraints
  
  #Check if cannotlinks is empty    
  if(length(clinks) == 0)
    return(NULL)
  
  edges = c()     
  
  for (i in 1:length(clinks)){
    l <- clinks[[i]]
    
    if (length(l) < 2)
      break
    
    for (j in 2:length(l))
      edges <- rbind(edges, c(l[j-1], l[j]))
    
  }

  
  gr <- graph.data.frame(edges, directed=FALSE)
  
#   print(gr)
  
  dgr <- decompose.graph(gr)
  
#   print(lapply(dgr, function(c) V(c)))
  
  # For each connected component, enumerate all maximal independent sets
  # (equivalent to all maximal cliques in the complement graph)
#   allowable = [((cl.subgraph(cc)).complement()).cliques()
#                for cc in clcc]

  #Not sure about max= 2
#    allowables <- lapply(dgr, function(c) maximal.cliques(graph.complementer(gr), subset=V(c)))
# 
# 
# print(allowables)

# allowables <- lapply(dgr, function(c) independent.vertex.sets(subgraph.edges(gr, E(c), delete.vertices = FALSE), min=2) )


# allowables <- lapply(dgr, function(c) maximal.independent.vertex.sets(subgraph.edges(gr, E(c), delete.vertices = FALSE)) )

allowables <- lapply(dgr, function(c) maximal.cliques(graph.complementer(subgraph.edges(gr, E(c), delete.vertices = FALSE), loops=FALSE)))

# print(allowables)
# stop("allowables")#print(allowables)

allowables <- lapply(allowables, function(s) lapply(s, function(c)  V(subgraph(gr, c))$name))

print(allowables)

                                                        #min=2)), max=2))
  #allowables <- lapply(dgr, function(c) maximal.cliques(gr, subset=V(c)))
  

  
  
  return(list(clcc =lapply(dgr, function(x) V(x)$name), allowable = allowables))
  
}


propagate.isolatelinks <- function(ilinks, W, vocab) {
  
  constraints = list(mlinks = NULL, clinks = NULL)
  
  for (ilink in ilinks) {
    
    complement.vocab <- vocab[!(vocab %in% ilink)]
    
    mlink <- ilink
    
    print(mlink)
    
    constraints$mlinks <- append(constraints$mlinks, list(mlink))
    
    clinks = list() 
    for (l in ilink)
      clinks <- append(clinks, lapply(complement.vocab, function(v) c(l, v)))
    
#     print(clinks)
#     stop(":as")
    
    constraints$clinks <- append(constraints$clinks, clinks)
    
  }
  
  return(constraints)
  
}

#Pre-condition: constraint words is a subst of the vocabulary
process.pairwise <- function(constr,W,vocab){
  require(igraph)
  
  # Unpack pairwise to construct graphs
  #constr = constr$mlinks, constr$clinks
  
  #construct a edge-less graph
  gr <- graph.empty(directed=FALSE) + vertices(vocab)
  #set of vertices of gr
  vertices <- V(gr)$name
  
  # Get ML connected components
  mlcc <- propagate.mustlinks(constr$mlinks)
  
  mlcc <- lapply(mlcc, function(cc) match(cc, vertices))
  #   # Invert this mapping
  #   if(length(mlcc) > 0):
  #     invmlcc = dict(reduce(OP.concat,[[(wi,mli+W) for wi in mlcc[mli]]
  #                                      for mli in range(len(mlcc))],[]))
  #   else:
  #     invmlcc = {}
  
  #   # Invert this mapping
#   invmlcc = list()
#   if (length(mlcc) > 0)
#     for (mli in seq(length(mlcc)))
#       for (wi in mlcc[[mli]])
#         invmlcc[[wi]] <- mli+W
#       
#     # Translate cannot-links between words to cannot-links
#     # between ML components (when applicable)
#   #   newcannotlinks = map(lambda x: tuple(map(lambda y: invmlcc.get(y,y),x)),
#   #                        constr$clinks)
#   
#   print(mlcc)
#   
#   newcannotlinks <- lapply(constr$clinks, 
#                            function(cl) unlist(lapply(cl, 
#                                       function(y){
#                                         y <- match(y, vertices)
#                                         if (!is.null(invmlcc[[y]]))
#                                           invmlcc[[y]]
#                                         else
#                                           y
#                                       }))
#                            )
#   
#   # Check for conflicting constraints
#   #
#   # if 2 words are connected (transitively or directly) by must-links,
#   # they will be in the same ML connected component
#   #
#   # if 2 words also have a pairwise cannot-link btwn them, then after
#   # the mapping we will have a a pairwise cannot-link between an ML
#   # connected component and *itself*
#   
#  #conflicts <- [cl[0] for cl in newcannotlinks if cl[0]==v[1]]
#  
#  conflicts <- unlist(lapply(newcannotlinks, function(cl) if (cl[1] == cl[2]) cl[1]))
#  
#  conflict.constraints =list()
#  
#  
# #  print(conflicts)
# #  stop("")
#  
#   if(length(conflicts) > 0){
#     # Identify the conflicting constraints
#     print('!!! CONFLICTING CONSTRAINTS !!!')
#     for(conflict in conflicts){
#       print('Following pairs must *and* cannot-link:')
#       mli <- conflict - W
#       mlw <- mlcc[[mli]]
#       vmlw <- vocab[mlw]
#       for(cl in constr$clinks)
#         if((cl[1] %in% vmlw) && (cl[2] %in% vmlw)){
#           # Print words if available, else word indices
# #           if(length(vocab) > 0){
#             conflict <- c(cl[1],cl[2])
#             print(conflict)
#             conflict.constraints[[length(conflict.constraints) + 1]] <- conflict
# #           }
# #           else{
# #             conflict <- c(cl[1],cl[2])
# #             print(conflict)
# #             conflict.constraints[[length(conflict.constraints) + 1]] <- conflict
# #           }
#           }
#     }
#     #raise RuntimeError('Conflicting constraints, see err msg')
#     #stop('Conflicting constraints, see err msg')
#     return(list(conflicts = conflict.constraints))
#   }
  
  # Get CL graph connected components and allowable sets
  #clinks = propagate.cannotlinks(newcannotlinks, gr)
  
  clinks = propagate.cannotlinks(constr$clinks)

#   clinks <- propagate.cannotlinks(newcannotlinks)
  
  clcc <- lapply(clinks$clcc, function(cc) match(cc, vertices))
  
  allowable <- lapply(clinks$allowable, 
                      function(aset) lapply(aset, 
                                    function(as) match(as, vertices)))
  
  #FIXME to allow for a list called clinks = list(clcc, allowable) 
 # return(list(mlcc = mlcc, clcc = clcc, allowable=allowable, vertices = vertices))
 return(list(mlcc = mlcc, clcc = clcc, allowable=allowable, conflicts = NULL))
}

#buildTree <- function(mlcc,clinks,allowable,W,beta,eta){
buildTree <- function(mlcc,clcc, allowables,W, beta,eta, dt){
  #"""  
  #Given:
  #mlcc = must-link clusters of words (List of Vectors)
  #clcc = cannot-link clusters of must-link clusters and words (List of Tuples)
  #allowable = within each clcc, the allowable combinations of mlcc/words
  #W = total number of words (connect extra words directly to root)
  
  #Return Dirichlet Forest data structure
  #(Dirichlet Tree plus special multinodes for structure variants)
  
  #PROCEDURE
  #1) construct tree structure
  #2) populate indices
  #3) tuple-ify tree for pass to intLDA
  #"""
  #Load libraries
  require(Rcpp)
  require(ITM)
  require(inline)
  
  
  
  DTNode = dt$DTNode
  MLNode = dt$MLNode
  MultiNode = dt$MultiNode  
  
  #return(MLNode)
  
  # Build M-nodes for each mlcc (will have only leaf children)
    #MLNode(edges, ichildren, maxind, leafstart, words)

  # leaftstart=-1
  
  ml_nodes = lapply(mlcc, function(mi) 
    new (MLNode, eta*beta*rep(1, length(mi)), list(),integer(0), -1,mi))
#   print("length(mlcc)")
#   print(length(mlcc))
# Build multinodes for each clcc
  multinodes = list()
  idx = 0  


allvars = list()
  for (cc in clcc){
    idx <- idx + 1
    # these are the true output connections of the multinode
    # ensure that ML-nodes always appear 1st
    #FIXED the indices
    #icids = sapply(clink$clcc, function(ci) ifelse(ci >= W, ci, NULL))
    icids = unlist(lapply(cc, function(ci) if(ci > W) ci))
    #lcids = sapply(clink$clcc, function(ci) if (ci < W) return(ci) else c())
    lcids = unlist(lapply(cc, function(ci) if (ci <= W) ci))
    #fake_words = cicids + lcids
    fake_words = c(icids, lcids)
    print(fake_words)
    
    #return(list(cc=cc,icids=icids, lcids=lcids, fake_words=fake_words))
    
    # construct a variation for each allowable set
    variations = list()

    for (aset in allowables[[idx]]) 
      if (length(aset) > 0){
      
      # split between good/bad (allowable/not)
      good = aset
      
      #bad = lapply(fake_words, function(fl) if (!(fl %in% good)) return fl else c())
      bad = unlist(lapply(fake_words, function(fl) if (!(fl %in% good)) fl))
      # scale the edges coming out of the likely internal node

      print(list(good=good, bad=bad))

      aedges = c()
      
#       print(ml_nodes)
      
      for (a in good)
        if(a > W)
        # M-node? --> beta * num leaves
          aedges <- c(aedges, beta * ml_nodes[[a-W]]$numLeaves())
      else
        # Leaf? --> beta
        aedges <- c(aedges, beta)
#       print(list(aset=aset, aedges=aedges))
      

      print(aedges)
      
    # scale the edges coming out of the fakeroot
      fedges = c(eta * sum(aedges))
    
      for (b in bad)
        if(b > W) 
        # M-node? --> beta * num leaves 
        fedges <- c(fedges, beta * ml_nodes[[b-W]]$numLeaves())
      else
        # Leaf? --> beta
        fedges <- c(fedges, beta)

    
    
print(list(aedges=aedges, fedges=fedges))



# stop("no reason")

      # variant subtree
      #likely_internal = DTNode(aedges,list(),list(),0)
      likely_internal = new (DTNode, aedges,list(),integer(0),1)
      #fakeroot = DTNode(fedges, likely_internal, length(good)-1, length(good))
      fakeroot = new (DTNode, fedges, list(likely_internal), 
                #       length(good)-1, length(good))
                length(good), length(good)+1)

      # fake leafmap permutation for this variant
      # fake_wordmap = good + bad
      # fake_wordmap is a vector
      fake_wordmap = c(good, bad)

      #the index of first word index - corresponding to index function to python
      fake_wordmap <- unique(fake_wordmap)
      fake_leafmap = sapply(fake_wordmap, function(wi) which(fake_wordmap==wi)[1])
    
#       fake_leafmap = unlist(lapply(fake_wordmap, function(wi) match(wi, fake_wordmap)))
    
      # save it
      #FIXME An expensive operation
      variations[[length(variations)+1]] = list(fakeroot = fakeroot,fake_leafmap = fake_leafmap)
    }

  allvars[[length(allvars) + 1]] <- variations

#     
#     return(variations)
#     
    # build the multinode
#     ichildren = [ml_nodes[ci-W] for ci in cc if ci >= W]
#     lchildren = [ci for ci in cc if ci < W]
    #FIXED the indices for ci and W are fixed
    ichildren = list()

  for (i in 1:length(cc)){
      print(cc[i])
      print(W)
      if(cc[i] > W) 
        ichildren[[length(ichildren)+1]] <- ml_nodes[[cc[i]-W]]
      }
    lchildren = unlist(lapply(cc, function(ci) if (ci <= W) ci))

  print(list(ichildren=ichildren, lchildren=lchildren))
    multinodes <- append(multinodes, 
                         new (MultiNode, numeric(0),ichildren,integer(0),0,lchildren,variations))
  
  }

# return(allvars)


  print(length(multinodes))
  
# Create empty root node
  #root = DTNode(c(),list(),list(),0)
  root <- new(DTNode, numeric(0), list(), integer(0) , 0)

  # Start connecting things and populating indices
  #
  cur_ind = 1 # current leaf index
  wordmap = c() # maps leaf index --> word index

  # MultiNodes connected to root
  #
  for (multinode in multinodes) {
    # Map ML-node children
    for (ml_child in multinode$ichildren){
      # Get word indices under this M-node
      #wordmap = wordmap + ml_child$words,
      # is it c, cbind
      wordmap <- c(wordmap, ml_child$words)
      # Save leafstart index for this M-node
      ml_child$leafstart = cur_ind
      # Record maxind in multinode
      cur_ind <- cur_ind + length(ml_child$words)
#       print("printing current index")
#       print(cur_ind)
      multinode$maxind <- c(multinode$maxind, cur_ind-1)
#       print("printing multinode maxind")
#       print(multinode$maxind)
      
    }

    # Does this multinode connect directly to any leaves?
    if(length(multinode$words) > 0){
      # Record leaf start index
      multinode$leafstart = cur_ind
      cur_ind <- cur_ind + length(multinode$words)
      # Record word indices
      # wordmap = wordmap + multinode.words
      wordmap = c(wordmap, multinode$words)
    }
  
    # Put this C-node and maxindex in root
    root$edges <- c(root$edges, beta*multinode$numLeaves())
    root$ichildren <- c(root$ichildren, multinode)
    root$maxind <- c(root$maxind, cur_ind-1)

  } 

# return(multinodes)
    
  # ML-nodes connected directly to root
  #

print(length(ml_nodes))
  for (mln in ml_nodes)
    # Leaf start index == None --> has not been assigned yet
    if(mln$leafstart == -1){
      
      #stop("reached here")
      # Set leaf start index for ML-node
      mln$leafstart = cur_ind
      cur_ind <- cur_ind + mln$numLeaves()
      # Put ML-node and its maxind in root
#       print("mln$numLeaves()")
#       print(mln$numLeaves())
      root$edges <- c(root$edges, beta*mln$numLeaves())
      root$ichildren <- c(root$ichildren, mln)
      root$maxind <- c(root$maxind, cur_ind-1)
      # Record word indices under this M-node
      #wordmap = wordmap + mln.words
      wordmap = c(wordmap, mln$words)
  }

# print(allvars)
# print("wordmap")
# print(wordmap)
# stop("nor eason")
  # Finally, rest of words must be connected directly to root
  #FIXME Not sure how to interpret this:
  #root.edges = array(root.edges + [beta for i in range(W - cur_ind)])
#   print("cur_ind")
#   print(cur_ind)
#   print(rep(beta, W - (cur_ind - 1)))

print(W)
print(beta)
print(cur_ind)


  root$edges = append(root$edges, rep(beta, W - (cur_ind - 1)))

#   print("printing root edges")
#   print(root$edges)

  root$leafstart = cur_ind
  #wordmap = wordmap + [wi for wi in seq(W) if wi not in wordmap]
#   print("wordmap")
 print(wordmap)
  wordmap = c(unique(wordmap), unlist(sapply(seq(W), function(wi) if (!(wi %in% wordmap)) wi )))
print(length(wordmap))

  # Convert everything to tuples
  #root = root$tupleConvert()
  
# print(wordmap)

  # Recover leafmap by inverting wordmap permutation
  #leafmap = [wordmap.index(wi) for wi in range(W)]
#the index of first word index - corresponding to index function to python
  leafmap = unlist(sapply(seq(W), function(wi) which(wordmap==wi)[1]))

# print(leafmap)

  return(list(root= root,leafmap = leafmap))
}


test.constraints <- function() {
  require(Rcpp)
  require(inline)
  
  dt = Module("dt", getDynLib("ITM"))
  voc = c("a", "b", "c", "d", "e", "f")
#   mlinks = list(c("a", "b", "c"), c("d", "e", "f"))
  #clinks = list(c("a", "f"))

clinks = list()
  W = length(voc)
  
  l = process.pairwise(list(clinks=clinks, mlinks = mlinks), W, voc)
  



  t = buildTree(l$mlcc, l$clcc, l$allowable, W, 0.1, 10000, dt)
  
  return(t)
}