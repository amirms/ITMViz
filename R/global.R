library(gelato)
library(tm)


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
  
#   nbow <- read.table(paste("benchmark", prname , "mydata-idf-BoW-matrix.csv", sep="/"), sep=",", row.names = 1, header = TRUE, check.names = FALSE)
  
nbow <- idf.weight(bow)

  y = apply(nbow, 2, function(x) (length(x[which(x >= t)]) > 0) )
  
  bow <- bow[,which(y)]
  
#   return(bow)
  
  doclist <- convertbow2list(bow)
  
  return(list(docs=doclist, vocab = colnames(bow), doc.names=rownames(bow), 
              term.frequency = colSums(bow), doc.term = rowSums(bow)))
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


compute.MoJoSim <- function(prname, theta, k) {
  
  d <- proxy::dist(theta)
    
  c <- kmeans(d, centers = k, nstart = 300, iter.max = 200)
  
  compare.MoJo(prname, c$cluster)
}

compare.MoJo <- function(prname, clusters) {
  require(gelato)
  
  setwd("~/workspace")
  
  #Load the priori decomposition
  decomposition <- read.csv(paste("benchmark", prname ,"decomposition.csv", sep="/"), sep=",",  header = TRUE)
  priori.decomp <- decomposition$x
  names(priori.decomp) <- decomposition$X
  

#   priori.decomp <- normalizeVector(priori.decomp) 
  priori.decomp <- find.intersection(priori.decomp, clusters)
  clusters <- find.intersection(clusters, priori.decomp)


  priori.decomp <- normalizeVector(priori.decomp)
  
  N <- length(clusters)
  
  mojo <- compute.MoJo(clusters, priori.decomp)
  
  mojosim <- sapply(mojo, function(m) 1 - (m/N))
  
  nmis <- compute.NMI(clusters, priori.decomp)
  
  purities <- compute.cluster.purity(clusters, priori.decomp)
  
  
  return(list(mojosim=mojosim, nmis=nmis, purities=purities))
  
}
