#Input: docs = document-term matrix of class DocumentTermMatrix
inference <- function(docs, numsamp, randseed){#,**kwargs){
#   require(gelatoViz)
  require(Rcpp)
  require(inline)
  require(ITM)
  
  # Do inference via collapsed Gibbs sampling
  #,estimate phi/theta from final sample

  # Compile preferences, if we haven't already
  if(mystate$root == NULL || isTrue(mystate$dirty) ) {
    #FIXME there is probably a way out of using this

    if (mystate$dt==NULL)
      dt = Module("dt", getDynLib("ITM"))
    
    #Remving the previous state qinit and zinit
    mystate$qsamp = NULL
    mystate$zsamp = NULL

    # Compile constraints into Dirichlet Forest data structure
    pc = process.pairwise(mystate$constraints, mystate$W, mystate$vocab)
    
   # tree = list(root,leafmap)
    tree = buildTree(pc$mlcc, pc$clcc, pc$allowable,
                     mystate$W, mystate$beta, mystate$eta, mystate$dt)
    
    mystate$root = tree$root
    mystate$leafmap = tree$leafmap
   
   
  }
  # Update numsamp and randseed arguments
  #Just integer numbers
  mystate$numsamp = numsamp
  mystate$randseed = randseed
  
  mystate$f = rep(0, mystate$D)
  
  # Do inference, initializing sampler if applicable
#   if(mystate$zsamp != NULL && mystate$qsamp != NULL)
#     lda = intLDA(docs, mystate$alpha, tree$root,
#                 tree$leafmap, mystate$numsamp, mystate$randseed,
#                 init=mystate$zsamp, qinit=mystate$qsamp)
#                 #, **kwargs)
#   else if(mystate$zsamp != NULL)
#     lda = intLDA(docs, mystate$alpha, tree$root,
#                 tree$leafmap, mystate$numsamp, mystate$randseed,
#                 init=mystate$zsamp) #, **kwargs)
#   else if(mystate$qsamp != NULL)
#     lda = intLDA(docs, mystate$alpha, tree$root,
#                 tree$leafmap, mystate$numsamp, mystate$randseed,
#                 qinit=mystate$qsamp)#, **kwargs)
#   else
# init = mystate$zsamp, qinit = mystate$qsamp
  ldaalpha = matrix(mystate$alpha, nrow = 1, ncol=mystate$K)
  lda = intLDA(mystate$docs, mystate$ldaalpha, mystate$root, mystate$leafmap,
                  mystate$numsamp, mystate$randseed,
                  mystate$zsamp, mystate$qsamp, mystate$f)
  
  print(dim(lda$phi))

  #Update global state with the results
  mystate$phi = t(lda$phi)
  mystate$theta = lda$theta
  mystate$zsamp = lda$zsamp
  mystate$qsamp = lda$qsamp
   
}

lda.test <- function(){
  require(ITM)
  require(Rcpp)
  require(inline)

  dt = Module("dt", getDynLib("ITM"))
  vocab = make.random.string(20,3)
    
  # """ Set up base data/parameter values """
  K = 5
  alpha = 0.5
  beta = 0.1
  eta = 10000
  W = length(vocab)
  D = 10
  
  mystate <- ldaState(alpha = alpha, beta = beta, eta = eta, 
                     K = K, W=W, D=D,vocab=vocab, session=NULL)
  
  doclens <- sample(1:100, D)
  
  doclens <- c(37, 54, 16, 90, 63, 64, 61, 80, 13 , 7)
  
  #a document term matrix
  mystate$docs <- lapply(doclens, function(d) sample(1:W, d, replace=TRUE))
  
  mystate$term.frequency = colSums(convert.list2DTMatrix(mystate$docs, W))
  
  #mystate$init = matrix(0, nrow=dim(state$docs)[1], ncol=dim(state$docs)[2])
  mystate$f = rep(0, D)
  
  mystate$numsamp = 50
  mystate$randseed = 821945
  
  # Build DF object
#   self.df = DF.DirichletForest(self.alpha,self.beta,self.eta,
#                                self.T,self.W)
#   
  # Default constraint set will create a 'full' tree
  # (ie, exhibits all variety of substructures)
#   clinks = list(c('b','c'), c('a','c'))

 clinks <- lapply(seq(1), function(x) vocab[sample(1:length(vocab), 2)])

#   clinks <- list(c(vocab[1], vocab[10]), c(vocab[1], vocab[5]))

#    clinks = list()

#   mlinks = list(c('e','f'))        

  #generate ranfom must links
#    mlinks <- lapply(seq(2), function(x) vocab[sample(1:length(vocab), 2)])

  mlinks = list()
  
  mystate$constraints = list(mlinks=mlinks, clinks=clinks)
  


  print(mystate$constraints)

  # equality tolerance for testing
  #self.tol = 1e-6
  l = process.pairwise(mystate$constraints, W, vocab)
# return(l)

print("Printing Constraints")
print(l)

  t = buildTree(l$mlcc, l$clcc, l$allowable, W, 0.1, 10000, dt)

#  return(t)


  ldaalpha = matrix(mystate$alpha, nrow = 1, ncol=mystate$K)

  print(t$root)

  print(dim(ldaalpha))

  print(mystate$numsamp)

return("sasas")

  #qsamp = NULL?!?
  lda = intLDA(mystate$docs, ldaalpha, t$root, t$leafmap,
             mystate$numsamp, mystate$randseed, #mystate$init, mystate$docs, mystate$f)
             NULL, NULL, mystate$f)

  print("reached")

  mystate$phi = t(lda$phi)
  colnames(mystate$phi) <- paste0("Topic", 1:mystate$K)
  
  mystate$theta = lda$theta
  # rownames(mystate$theta) <- mystate$doc.names
  
  mystate$zsamp = lda$zsamp
  mystate$qsamp = lda$qsamp
  
  #Compute topic proportion
  total.topic <- sum(colSums(lda$theta))
  mystate$topic.proportion = colSums(lda$theta) / total.topic
  
  # This is necessary for subsetting data upon selection in topicz.js
  
  # compute the token-topic occurrence table:
  #mystate$phi.freq <- t(t(mystate$phi) * mystate$topic.proportion * mystate$N)
  mystate$phi.freq <- (mystate$phi / rowSums(mystate$phi)) * mystate$term.frequency

return(mystate)

}

convert.list2DTMatrix <- function(docs, W){
  m = matrix(0, length(docs), W)
  
  for (d in 1:length(docs))
    for (w in 1: length(docs[[d]]))
    m[d,docs[[d]][w]] <- m[d,docs[[d]][w]] + 1
  
  return(m)
}

x <- function() {
  K = 15
#   alpha = 50 / K
  alpha = 0.5
  beta = .1
  eta = 10000 
# eta = 1000
  mystate <- ldaState(alpha = alpha, beta = beta, eta = eta, K = K)
  mystate$prname= "jedit-5.1.0"
# mystate$prname= "jdom-2.0.5"
  mystate$min.tfidf = 25
  mystate$numsamp =200
  mystate$randseed = 821945
 # mystate$constraints$mlinks = list(c("close", "open" ), c("min", "minimum"), c("max", "maximum"))
  #   mystate$constraints$clinks = list(c("parser", "textarea"))
#   mystate$constraints$mlinks = list(#c("menu", "menubar", "jmenu", "submenu"), 
#                                     c("textarea", "gutter", "painter"),
#                                     c("pars", "parser", "lexic", "regexp", "rule"),
# #                                     c("plugin", "instal"),
# #                                     #c("get", "set"),
# #                                     c("bufferset", "bufferlength", "buffer"),
# #                                     c("edit", "editor", "editpan"),
# #                                     c("search", "hypersearch", "searchbar"),
#                                     c("indent", "whitespac", "bracket", "deep", "align"),
# #                                     c("help", "helpview", "histori", "forward" ),
#                                     c("macro", "bean", "shell", "eval", "interpret"))
#                                     #c("io", "iobuff", "iobufs", "ioerror", "iorequest"))#,
#                                     #c("data", "transfer", "flavor"))


mystate$constraints$mlinks = list(c("textarea", "paint", "gutter", "widget", "caret", "statusbar"),
c("plugin", "instal", "download", "unload", "mirror", "depend", "urlconnect", "obsolet", "pluginset", "text", "painter"),
c("regex", "regexp", "parser", "ruleset", "rule", "lexic"),
c("filesystem", "path", "directori", "ioerror", "browser"),
c("eval", "callstack", "interpret", "bean", "shell", "beanshel"),
c("search", "hypersearch", "forward", "searchbar"),
c("toggl", "comment"),
c("highlight", "syntax" ),
c("syntax", "bracket", "match"),
c("indent", "fold", "whitespac"),
c("parser", "pattern"),
c("search", "replac"),
c("menu", "jmenu", "toolbar"),
c("buffer", "view"),
c("menu", "textarea")
)


# mystate$constraints$clinks = list(c("textarea", "plugin"), #c("eval", "textarea"), c("eval", "rule"),
#                                 c("plugin", "rule"), c("rule", "path"), c("search", "plugin"))


  mystate$constraints$clinks = 
                                list(c("menu", "textarea"),
                                    c("menu", "parser"),
                                   c("menu", "plugin"),
                                    c("textarea", "parser"),
                                     c("textarea", "plugin"),
                                    c("parser", "plugin"),
                                    c("edit", "search"))

 # mystate$constraints$ilinks = list(c("get", "set"))
# mystate$constraints$clinks = list(c("parser", "textarea"), c("get", "set"))
}


anwb <- function() {
    K = 15
#     alpha = 50 / K
      alpha = 0.5
    beta = .1
    eta = 10000 
    mystate <- ldaState(alpha = alpha, beta = beta, eta = eta, K = K)
    mystate$prname= "bg01_prod_arox_srclib"
    mystate$min.tfidf = 25
    mystate$numsamp =70
    mystate$randseed = 821945
  
  
}
