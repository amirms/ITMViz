library(inline)
library(Rcpp)
library(inline)
library(ITM)

ldaState <- function(alpha, beta, eta, K=1, W=0, D=0, vocab=NULL, doc.names=NULL, session=NULL) {
  state = list (
    dt = Module("dt", getDynLib("ITM")),
    
    #Current Project in benchmark folder
    prname= NULL, 
    
    min.tfidf= 3, #the minimum tf-idf score - default value = 3
    K = K, #User-selected number of topics
    docs = NULL, #User-selected bag-of-words
    labels = NULL, #User-selected labels for topics
    vocab = vocab,
    
    labels = paste0("Topic", 1:K), #instantiated with arbitrary topic names
    doc.names = doc.names,
    W = W, #Number of words
    D = D,
    N=0, # total number of tokens in the data
    
    #User-provided parameters of LDA
    alpha = alpha,
    beta = beta,
    eta = eta,
    
    #Create the corresponding alpha and beta for performing LDA
    #     ldaalpha = matrix(alpha, nrow = 1, ncol=K),
    #     
    #     #ldabeta = self.beta * ones((self.T,self.W))
    #     ldabeta = matrix(beta, nrow=K, ncol=W),
    
    numsamp = 50,
    randseed = 821945,
    
    #TODO figure out what this is for
    f=NULL,
    
    #State of Interactive LDA
    curIterations = 0, #LDA iterations run so far (gets reset when 'Reset Topics' is clicked)
    nextIterations = 0, #LDA iterations to run in the next LDA command
    append=FALSE, #True if we're updating an existing LDA run, false if we're starting over.
    randomSeed = -1, #a negative random seed means that we should use the default
    dirty = FALSE, #Dirty flag: true if we've changed the constraints in any way
    #without running LDA to refine the topics.
    session = session, #Current Session
    
    #Constraints
    constraints = list(  #User-selected constraints
      mlinks = list(), #Must-Link Constraints
      clinks = list(), #Cannot-Link Constraints
      ilinks = list(), #Isolate Constraints
      conflicts = NULL
    ),
    
    #Visualization information
    doc.term = NULL,
    term.frequency = NULL,
    topic.proportion = NULL,
    doc.proportion = NULL,
    
    #Results of constraints compilation
    root = NULL,
    leafmap = NULL,
    
    # Outputs of inference    
    zsamp = NULL,
    qsamp = NULL,
    #phi = matrix(0, nrow=T, ncol=W) #a T x W matrix
    phi = NULL,
    #theta = matrix(0, nrow=W, ncol=D) #a W x D matrix
    theta = NULL,
    phi.freq = 0, #the token-topic occurrence table
    theta.freq = 0, #the topic-document occurrence table
    rel.freq = 0 #the doc-topic frequency
  )
  
  
  #Reset Constraints
  state$resetConstraints <- function(){
    state$constraints = NULL
  }
  
  #Add a constraint
  state$addConstraint <- function(words, isCannotLink)
  {   
    constr = list(words = words, isCannotLink = isCannotLink)
    state$constraints <-append(state$constraints, constr)
    state$dirty = TRUE
  }
  
  
  #Replace a constraint with a new set of words 
  state$replaceConstraint <- function(index, words, isCannotLink)
  {
    constr = list(words = words, isCannotLink = isCannotLink)
    state$constraints[[index]] <- constr
    state$dirty = TRUE
  }
  
  #Delete a constraint
  state$deleteConstraint <- function(index)
  {
    if (index <= length(state$constraints))
      state$constraints <- stateconstraints[-index]
    
    state$dirty = TRUE
  }  
  
  state <- list2env(state)
  class(state) <- "LDAState"
  return(state)
}


fit.LDA <- function(mystate) {
  
  require(Rcpp)
  require(inline)
  require(ITM)
  
  if (is.null(mystate$prname))
    return()
  
  if(mystate$K <= 1)
    return()
  
  #if this is the first iteration
  if (mystate$curIterations == 0) {
    #the min.tfidf must have been set in the constructor of mystate
    
    r <- read.dt(mystate$prname, mystate$min.tfidf)
    
    #   list(doclist=doclist, vocab = colnames(bow), docs=rownames(bow), 
    #        term.frequency = colSums(bow), doc.term = rowSums(bow)))
    
    mystate$docs = r$docs
    mystate$vocab = r$vocab
    mystate$labels <- unlist(lapply(seq(mystate$K), function(c) paste("Topic", c, sep="")))
    mystate$term.frequency = r$term.frequency
    mystate$doc.term = r$doc.term
    mystate$doc.names = r$doc.names
    
    mystate$N <- sum(mystate$term.frequency) 
    mystate$rel.freq <- mystate$term.frequency/mystate$N
    
    mystate$W = length(r$vocab)
    mystate$D = length(r$doc.names)
    
  }
  #check if the dt is already set
  if (is.null(mystate$dt))
    mystate$dt = Module("dt", getDynLib("ITM"))
  # Compile preferences, if we haven't already
  if(is.null(mystate$root) || isTRUE(mystate$dirty)) {
    mystate$f = rep(0, mystate$D)
    
    mystate$qsamp = NULL
    mystate$zsamp = NULL
    
    # print("working")
    
    isolate.constraints <- propagate.isolatelinks(mystate$constraints$ilinks, mystate$W, mystate$vocab)
    
    
    
    expanded.constraints <- list(mlinks = append(isolate.constraints$mlinks, mystate$constraints$mlinks),
                                 clinks = append(isolate.constraints$clinks, mystate$constraints$clinks))
    
#     print(mystate$constraints$mlinks)
#     if (length(mystate$constraints$mlinks) > 0) {
#       print(expanded.constraints)
#       
#       stop("aasas")
#     }
#     
    #     return(expanded.constraints)
    
    # Compile constraints into Dirichlet Forest data structure
    pc = process.pairwise(expanded.constraints, mystate$W, mystate$vocab)
    
    
    if (!is.null(pc$conflicts)){
      mystate$constraints$conflicts <- pc$conflicts
      
      return()
    }
    
    
    
    # tree = list(root,leafmap)
    tree = buildTree(pc$mlcc, pc$clcc, pc$allowable,
                     mystate$W, mystate$beta, mystate$eta, mystate$dt)
    
    #     return(tree)
    
    mystate$root = tree$root
    mystate$leafmap = tree$leafmap
    
    mystate$dirty = FALSE
  }
  
  ldaalpha = matrix(mystate$alpha, nrow = 1, ncol=mystate$K)
  
  lda = intLDA(mystate$docs, ldaalpha, mystate$root, mystate$leafmap,
               mystate$numsamp, mystate$randseed,
               mystate$zsamp, mystate$qsamp, mystate$f)
  
  
  #Update global state with the results
  mystate$phi = t(lda$phi)
  #Name rows and cols of phi
  rownames(mystate$phi) <- mystate$vocab
  colnames(mystate$phi) <- mystate$labels
  
  #Name rows and cols of theta  
  mystate$theta = lda$theta
  rownames(mystate$theta) <- mystate$doc.names
  colnames(mystate$theta) <- mystate$labels
  
  mystate$zsamp = lda$zsamp
  mystate$qsamp = lda$qsamp
  
  #Compute topic proportion
  total.topic <- sum(colSums(mystate$theta))
  mystate$topic.proportion = colSums(mystate$theta) / total.topic
  
  #Compute document proportion
  #total.terms <- sum(mystate$doc.term)
  #add doc.proportion to ldastate
  mystate$doc.proportion <- mystate$doc.term / mystate$N
  
  # This is necessary for subsetting data upon selection in topicz.js
  
  # compute the token-topic occurrence table:
  mystate$phi.freq <- (mystate$phi / rowSums(mystate$phi)) * mystate$term.frequency
  #mystate$theta.freq <- t(mystate$theta * mystate$doc.proportion * mystate$K)
  mystate$theta.freq <- t(mystate$theta / mystate$D) * 100
  
  return(mystate)
}


read.dt <- function(prname , t=10) {

  require(lsa)
  
#   setwd("~/workspace")
  
#   prname <- "jedit-5.1.0"
  
  print(getwd())
  
#   bow <- read.table(paste("data", prname , "mydata-BoW-matrix.csv", sep="/"), sep=",", row.names = 1, header = TRUE, check.names = FALSE)
  bow <- read.table(paste("data", prname , "mydata-BoW-matrix.csv", sep="/"), sep=",", row.names = 1, header = TRUE, check.names = FALSE)
  
  bow <- as.matrix(bow)
  
#    bow <- bow[1:30,]
  
  print(dim(bow))

  names <- colnames(bow)

  bow <- bow[,which(nchar(names) > 3)]
  
  nbow <- idf.weight(bow)

  y = apply(nbow, 2, function(x) (length(x[which(x >= t)]) > 0) )
  
  bow <- bow[,which(y)]
  
#   return(bow)
  
  doclist <- convertbow2list(bow)
  
  return(list(docs=doclist, vocab = colnames(bow), doc.names=rownames(bow), 
              term.frequency = colSums(bow), doc.term = rowSums(bow)))
}

# Compute inverse document frequency weights and rescale a data frame
# Input: data frame
# Calls: scale.cols
# Output: scaled data-frame
idf.weight <- function(x) {
  # IDF weighting
  doc.freq <- colSums(x>0)
  doc.freq[doc.freq == 0] <- 1
  w <- log(nrow(x)/doc.freq)
  return(scale.cols(x,w))
}

# Rescale the columns of a data frame or array by a given weight vector
# Input: arrray, weight vector
# Output: scaled array
scale.cols <- function(x,s) {
  return(t(apply(x,1,function(x){x*s})))
}

# Rescale rows of an array or data frame by a given weight vector
# Input: array, weight vector
# Output: scaled array
scale.rows <- function(x,s) {
  return(apply(x,2,function(x){x*s}))
}

#Input: a document-term matrix(bow)
convertbow2list <- function(bow) {
    
  doclist = list()
  
  for(i in 1:dim(bow)[1]) {
    doc = c()
    for (j in 1:dim(bow)[2])
      doc <- c(doc, rep(j, bow[i,j]))  
  
    doclist[[length(doclist)+1]] <- doc
  }
  
  return(doclist)
  
}


#Input: 
#bow: An unnormalized bag-of-words
#K: no of topics

fit.classic.LDA <- function(bow, K) {
  require(topicmodels)
  require(tm)
  
  results = list(phi=NULL, term.frequency=NULL, vocab=NULL, topic.proportion=NULL, doc.names=NULL, theta=NULL)
  
  results$term.frequency = colSums(bow)
  
  results$doc.term = rowSums(bow)
  
  results$vocab = colnames(bow)
  
  results$doc.names = rownames(bow) 
  
  dtm <- as.DocumentTermMatrix(bow,  weighting = function(x) weightTf(x))
  
  lda <- LDA(dtm, control = list(alpha = 0.1), k = K)
  lda_inf <- posterior(lda)
  
  results$phi = t(lda_inf$terms)
  
  total.topic = sum(colSums(lda_inf$topics))
  
  results$topic.proportion = colSums(lda_inf$topics) / total.topic
      
  results$theta = lda_inf$topics
  
  return(results)
}

prepare.global <- function(prname, K) {
  
  
  alpha = 0.1
  beta = .1
  eta = 10000 
  mystate <- ldaState(alpha = alpha, beta = beta, eta = eta)
  
  mystate$prname= "jedit-5.1.0"
  mystate$K= 20
  
  bow <- read.dt(mystate$prname, 15)
  
  results = fit.classic.LDA(bow, mystate$K)
  
  mystate$phi = results$phi
  mystate$term.frequency = results$term.frequency
  mystate$topic.proportion = results$topic.proportion
  mystate$vocab = results$vocab
  mystate$docs = results$docs
  mystate$theta = results$theta
  mystate$doc.term = results$doc.term
  
  colnames(mystate$phi) <- paste0("Topic", 1:mystate$K)
  
  #Compute topic proportion
  total.topic <- sum(colSums(mystate$theta))
  mystate$topic.proportion = colSums(mystate$theta) / total.topic
  
  mystate$N <- sum(mystate$term.frequency) 
  mystate$rel.freq <- mystate$term.frequency/mystate$N
  mystate$phi.freq <- t(t(mystate$phi) * mystate$topic.proportion * mystate$N)
  
}


# compute.MoJoSim <- function(prname, theta, k) {
#   
#   d <- proxy::dist(theta)
#     
#   c <- kmeans(d, centers = k, nstart = 300, iter.max = 200)
#   
#   compare.MoJo(prname, c$cluster)
# }

# compare.MoJo <- function(prname, clusters) {
#   require(gelato)
#   
# #   setwd("~/workspace")
#   
#   #Load the priori decomposition
#   decomposition <- read.csv(paste("benchmark", prname ,"decomposition.csv", sep="/"), sep=",",  header = TRUE)
#   priori.decomp <- decomposition$x
#   names(priori.decomp) <- decomposition$X
#   
# 
# #   priori.decomp <- normalizeVector(priori.decomp) 
#   priori.decomp <- find.intersection(priori.decomp, clusters)
#   clusters <- find.intersection(clusters, priori.decomp)
# 
# 
#   priori.decomp <- normalizeVector(priori.decomp)
#   
#   N <- length(clusters)
#   
#   mojo <- compute.MoJo(clusters, priori.decomp)
#   
#   mojosim <- sapply(mojo, function(m) 1 - (m/N))
#   
#   nmis <- compute.NMI(clusters, priori.decomp)
#   
#   purities <- compute.cluster.purity(clusters, priori.decomp)
#   
#   
#   return(list(mojosim=mojosim, nmis=nmis, purities=purities))
#   
# }
