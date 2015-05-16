ldaState <- function(alpha, beta, eta, K=1, W=0, D=0, vocab=NULL, doc.names=NULL, session=NULL) {
  require(ITMViz)
  require(Rcpp)
  require(inline)
  require(gelato)
  
  state = list (
    dt = Module("dt", getDynLib("ITMViz")),
    
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