
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
# 
# http://www.rstudio.com/shiny/
#

# load some libraries:
library(shiny)
library(MASS)
library(proxy)
library(plyr)
library(reshape2)
library(ggvis)
library(dplyr)
library(googleCharts)
# library(shinyIncubator)
library(wordcloud)
library(memoise)
library(googleVis)
# library(shinydashboard)
library(ITM)

palette(c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3",
          "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999"))






shinyServer(function(input, output, session) {
  
  #Init
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
  #mystate$numsamp =200
  mystate$numsamp =50
  mystate$randseed = 821945

#   
#   mystate <- fit.LDA(mystate)
# #   
#   print(dim(mystate$theta))
#   
#   #Get all the constraints
#   mlinks <- unlist(lapply(mystate$constraints$mlinks, function(l) paste(l,collapse=" ") ))
#   clinks <- unlist(lapply(mystate$constraints$clinks, function(l) paste(l,collapse=" ") ))
#   ilinks <- unlist(lapply(mystate$constraints$ilinks, function(l) paste(l,collapse=" ") ))
  
  #Initialization (probably shouldn't be here)
#   alpha = 0.1
#   beta = 0.1
#   eta = 10000  
#   mystate <- ldaState(alpha = alpha, beta = beta, eta = eta, 
#                       numTopics = K, W=W, D=D,vocab=vocab, session=session)
  
  # Set the values of a few parameters related to the size of the data set:
#   K <- length(mystate$topic.proportion) # number of topics
#    K <- mystate$numTopics
#   W <- length(vocab) # size of vocabulary
#   N <- sum(mystate$term.frequency) # total number of tokens in the data
#   D <- length(docs) #Number of documents
  
#   # This is necessary for subsetting data upon selection in topicz.js
#   colnames(mystate$phi) <- paste0("Topic", 1:mystate$K)
#   rel.freq <- mystate$term.frequency/mystate$N
#   
#   # compute the token-topic occurrence table:
#   phi.freq <- t(t(mystate$phi) * mystate$topic.proportion * N)
  
 
  
  
  
  # Compute distance matrix between topics
  # We wrap this in its own reactive function so that it isn't recomputed if say the value of lambda changes
  computeDist <- reactive({
    #d <- proxy::dist(t(phi), method = distance(measure = input$itm.distance))
    d <- proxy::dist(t(mystate$phi))
    # Multidimensional scaling to project the distance matrix onto two dimensions for the vis:
    # Maybe we should explore including options for different scaling algorithms???
    switch(input$itm.scaling,
           PCA = fit <- stats::cmdscale(d, k = 2),
           kruskal = fit <- MASS::isoMDS(d, k = 2)$points,
           sammon = fit <- MASS::sammon(d, k = 2)$points
    )
    x <- fit[, 1]
    y <- fit[, 2]
    # collect the (x, y) locations of the topics and their overall proportions in a data.frame:
    mds.df <- data.frame(topics=1:mystate$K, x=fit[, 1], y=fit[, 2], Freq=mystate$topic.proportion*100)
    list(mds.df = mds.df, x = x, y = y)
  })
  
  
  fitCluster <- reactive({
    # Bring in the necessary clustering data
    distDat <- computeDist()
    # workaround errors if no clustering is done (ie, input$itm.kmeans == 1)
    distDat$mds.df$cluster <- 1
    centers <- data.frame(x = 0, y = 0)
    if (input$itm.kmeans > 1) { # and clustering info (if relevant)
      cl <- stats::kmeans(cbind(x=distDat$x, y=distDat$y), input$itm.kmeans)
      distDat$mds.df$cluster <- factor(cl$cluster)
      centers <- data.frame(cl$centers)
    }
    list(mds.df = distDat$mds.df, x = distDat$x, y = distDat$y, centers=centers)
  })
  
  #Output for Topic Visualization
  output$mdsDat <- reactive({   
    compute_state()

    ##############################################################################
    ### Create a df with the info neccessary to make the default OR new bar chart when selecting a topic or cluster.
    ### This functionality requires that we keep track of the top input$itm.nTerms within each cluster and topic (as well as overall).
    ### The ranking of words under a topic is done via a weighted average of the lift and probability of the word given the topic.
    ### The ranking of words under a cluster is done via a similar weighted average (summing over the relevant topics)
    dat <- fitCluster()
    mds.df <- dat$mds.df
    print(dim(mds.df))
    x <- dat$x
    y <- dat$y
    centers <- dat$centers
    #get the top terms for each topic
    nTermseq <- 1:input$itm.nTerms
    weight <- input$itm.lambda*log(mystate$phi) + (1 - input$itm.lambda)*log(mystate$phi/mystate$rel.freq)
    top.terms <- NULL
    for (i in 1:mystate$K) {
      weights <- weight[, i]
      o <- order(weights, decreasing=TRUE)
      terms <- mystate$vocab[o][nTermseq]
      top.terms <- c(top.terms, terms)
    }
    # We need this labeling or else topicz.js will not subset correctly...
    term.labs <- rep(paste0("Topic", 1:mystate$K), each = input$itm.nTerms)
    topic.df <- data.frame(Term = top.terms, Category = term.labs, stringsAsFactors = FALSE)
    # get the top terms for each cluster
    clust.terms <- NULL
    if (input$itm.kmeans == 1) {
      #if no clustering is done, we don't want to change the 'most informative words' upon hover
      clust.terms <- terms
    } else {
      for (i in 1:input$itm.kmeans) {
        #grab topics that belong to the current cluster
        topicz <- mds.df$cluster %in% i
        sub.phi <- mystate$phi[, topicz]
        #only sum if multiple columns exist
        if (!is.null(dim(sub.phi))) {
          sub.phi <- apply(t(sub.phi)*mystate$topic.proportion[topicz], 2, sum) # weighted by topic term frequency
        }
        weight <- input$itm.lambda*log(sub.phi) + (1 - input$itm.lambda)*log(sub.phi/mystate$rel.freq)
        o <- order(weight, decreasing=TRUE)
        terms <- mystate$vocab[o][nTermseq]
        clust.terms <- c(clust.terms, terms)
      }
    }
    term.labs <- rep(paste0("Cluster", 1:input$itm.kmeans), each = input$itm.nTerms)
    clust.df <- data.frame(Term = clust.terms, Category = term.labs, stringsAsFactors = FALSE)
    # compute the distinctiveness and saliency of the tokens:
    #t.w <- t(topic.proportion * t(phi))/rel.freq # P(T|w) = P(T)*P(w|T)/P(w)
    t.w <- mystate$phi/apply(mystate$phi, 1, sum)
    t.w.t <- t(t.w) # Change dimensions from W x K to K x W
    kernel <- t.w.t * log(t.w.t/mystate$topic.proportion)
    saliency <- mystate$term.frequency * colSums(kernel)
    # By default, order the terms by saliency:
    salient <- mystate$vocab[order(saliency, decreasing = TRUE)][nTermseq]
    top.df <- data.frame(Term = salient, Category = "Default")
    # put together the most salient words with the top words for each topic/cluster
    all.df <- rbind(topic.df, clust.df, top.df)
    # Overall frequency for each possible word
    all.df$Total <- mystate$term.frequency[match(all.df$Term, mystate$vocab)]
#     print("mds all/df$Total")
#     print(all.df$Total)
    
    # Initiate topic/cluster specific frequencies with 0 since that is used for the 'default' category
    all.df$Freq <- 0
    # Collect P(w|T) for each possible word
    all.words <- unique(all.df$Term)
    keep <- mystate$vocab %in% all.words
    phi2 <- data.frame(mystate$phi.freq[keep, ], Term = mystate$vocab[keep])
    t.phi <- reshape2::melt(phi2, id.vars = "Term", variable.name = "Category", value.name = "Freq2")
    all.df <- plyr::join(all.df, t.phi)
    # Collect P(w|Cluster) for each possible word
    c.phi <- plyr::join(t.phi, data.frame(Category = paste0("Topic", mds.df$topics), cluster = paste0("Cluster", mds.df$cluster)))
    c.phi <- plyr::ddply(c.phi, c("Term", "cluster") , plyr::summarise, Freq3 = sum(Freq2))
    names(c.phi) <- sub("cluster", "Category", names(c.phi))
    all.df <- plyr::join(all.df, c.phi)
    all.df$Freq[!is.na(all.df$Freq2)] <- all.df$Freq2[!is.na(all.df$Freq2)]
    all.df$Freq[!is.na(all.df$Freq3)] <- all.df$Freq3[!is.na(all.df$Freq3)]
    all.df <- all.df[, -grep("Freq[0-9]", names(all.df))]
    # Infer the occurences within topic/cluster
    #all.df$Freq <- all.df$Total * all.df$Freq
    # P(T|w) -- as a percentage -- for each possible term
    t.w2 <- data.frame(100*t.w[keep, ], Term = mystate$vocab[keep])
    topic.table <- reshape2::melt(t.w2, id.vars = "Term", variable.name = "Topic", value.name = "Freq")
    kmeans <- input$itm.kmeans
#     print("printing dim all.df")
#     print(dim(all.df))
# print("topic.table")
# print(topic.table)
    return(list(mdsDat = mds.df, mdsDat2 = topic.table, barDat = all.df,
                centers = centers, nClust = kmeans))
  })
  
  
  
  computeDocDist <- reactive({
    #d <- proxy::dist(t(phi), method = distance(measure = input$itm.distance))
    d <- proxy::dist(mystate$theta)
    # Multidimensional scaling to project the distance matrix onto two dimensions for the vis:
    # Maybe we should explore including options for different scaling algorithms???
    switch(input$cls.scaling,
           PCA = fit <- stats::cmdscale(d, k = 2),
           kruskal = fit <- MASS::isoMDS(d, k = 2)$points,
           sammon = fit <- MASS::sammon(d, k = 2)$points
    )
    x <- fit[, 1]
    y <- fit[, 2]
    # collect the (x, y) locations of the topics and their overall proportions in a data.frame:
    #mds.df <- data.frame(topics=1:mystate$K, x=fit[, 1], y=fit[, 2], Freq=mystate$topic.proportion*100)
    cls.df <- data.frame(docs=1:mystate$D, docnames = mystate$doc.names ,x=fit[, 1], y=fit[, 2], 
                         Freq=mystate$doc.proportion * 100)
    list(cls.df = cls.df, x = x, y = y)   
  })
  
  
  fitDocCluster <- reactive({
    # Bring in the necessary clustering data
    distDat <- computeDocDist()
    # workaround errors if no clustering is done (ie, input$itm.kmeans == 1)
    distDat$cls.df$cluster <- 1
    centers <- data.frame(x = 0, y = 0)
    if (input$cls.kmeans > 1) { # and clustering info (if relevant)
      cl <- stats::kmeans(cbind(x=distDat$x, y=distDat$y), input$cls.kmeans)
      distDat$cls.df$cluster <- factor(cl$cluster)
      centers <- data.frame(cl$centers)
    }
    list(cls.df = distDat$cls.df, x = distDat$x, y = distDat$y, centers=centers)
  })
  
  
  #Clustering output
  output$clsDat <- reactive({ 
    compute_state()
    ##############################################################################
    ### Create a df with the info neccessary to make the default OR new bar chart when selecting a topic or cluster.
    ### This functionality requires that we keep track of the top input$itm.nTerms within each cluster and topic (as well as overall).
    ### The ranking of words under a topic is done via a weighted average of the lift and probability of the word given the topic.
    ### The ranking of words under a cluster is done via a similar weighted average (summing over the relevant topics)
    dat <- fitDocCluster()
    cls.df <- dat$cls.df
    print(dim(cls.df))
    x <- dat$x
    y <- dat$y
    centers <- dat$centers
    #get the top topics for each document
	#ensure that cls.nTopics is at most number of topics
    nTopicseq <- 1:input$cls.nTopics
    #weight <- input$cls.lambda*log(mystate$theta) + (1 - input$cls.lambda)*log(mystate$theta/mystate$rel.freq)
    #FIXME is this necessary?
    weight <- log(mystate$theta)
  
    top.topics <- NULL
	#For each document, find the top nTopicseq topics
    for (i in 1:mystate$D) {
      weights <- weight[i, ]
      o <- order(weights, decreasing=TRUE)
      topics <- mystate$labels[o][nTopicseq]
      top.topics <- c(top.topics, topics)
    }
    # We need this labeling or else topicz.js will not subset correctly...
    #term.labs <- rep(paste0("Topic", 1:mystate$K), each = input$itm.nTerms)
	  #topic.df <- data.frame(Term = top.terms, Category = term.labs, stringsAsFactors = FALSE)
	  topic.labs <- rep(paste0("Document", 1:mystate$D), each = input$cls.nTopics)
    document.df <- data.frame(Topic = top.topics, Category = topic.labs, stringsAsFactors = FALSE)
    # get the top topics for each cluster
    clust.topics <- NULL
    if (input$cls.kmeans == 1) {
      #if no clustering is done, we don't want to change the 'most informative words' upon hover
      clust.topics <- topics
    } else {
      for (i in 1:input$cls.kmeans) {
        #grab documents that belong to the current cluster
        docz <- cls.df$cluster %in% i
        sub.theta <- mystate$theta[docz, ]
        #only sum if multiple columns exist
		#TODO figure out how to replace topic.proportion FIXME
        if (!is.null(dim(sub.theta))) {
          #sub.theta <- apply(t(sub.theta)*mystate$doc.proportion[docz], 2, sum) # weighted by doc topic frequency
          sub.theta <- apply(t(sub.theta), 2, sum) # weighted by doc topic frequency
        }
        #weight <- input$itm.lambda*log(sub.phi) + (1 - input$itm.lambda)*log(sub.phi/mystate$rel.freq)
        weight <- sub.theta
		    o <- order(weight, decreasing=TRUE)
        topics <- mystate$labels[o][nTopicseq]
        clust.topics <- c(clust.topics, topics)
      }
    }
  
    topic.labs <- rep(paste0("Cluster", 1:input$cls.kmeans), each = input$cls.nTopics)
    clust.df <- data.frame(Topic = clust.topics, Category = topic.labs, stringsAsFactors = FALSE)
	
    # compute the distinctiveness and saliency of the tokens:
    #t.w <- t(topic.proportion * t(phi))/rel.freq # P(T|w) = P(T)*P(w|T)/P(w)
    #d.t <- mystate$theta/apply(mystate$theta, 2, sum)
    d.t <- 100*t(mystate$theta.freq)
    #d.t.t <- t(d.t) # Change dimensions from D x K to K x D
    #kernel <- t.w.t * log(t.w.t/mystate$topic.proportion)
	  kernel <- d.t
    #saliency <- mystate$term.frequency * colSums(kernel)
	  saliency <- colSums(kernel)
    # By default, order the topics by saliency:
    salient <- mystate$labels[order(saliency, decreasing = TRUE)][nTopicseq]
    top.df <- data.frame(Topic = salient, Category = "Default")
    # put together the most salient topics with the top topics for each document/cluster
    all.df <- rbind(document.df, clust.df, top.df)
    # Overall frequency for each possible topic
  #all.df$Total <- mystate$term.frequency[match(all.df$Term, mystate$vocab)]

	# doc.term <- term frequency per document
  
	#FIXME all.df$Total is it really necessary? If so, what should I replace it with?
    #all.df$Total <- mystate$doc.term[match(all.df$Topic, mystate$labels)]
	  all.df$Total <- 100*mystate$topic.proportion[match(all.df$Topic, mystate$labels)]
  
	print("printing all.df$Total")
	print(match(all.df$Topic, mystate$labels) )
  
    # Initiate topic/cluster specific frequencies with 0 since that is used for the 'default' category
    all.df$Freq <- 0
    # Collect P(t|D) for each possible word
    all.topics <- unique(all.df$Topic)
    keep <- mystate$labels %in% all.topics
  print("printing keep")
  print(keep)
  
    #change names to Document + no
    #Better to keep a dictionary of document names to id's
    colnames(mystate$theta.freq) <- paste0("Document", cls.df$docs)
    #theta2 <- data.frame(mystate$theta.freq[keep, ], Topic = mystate$labels[keep])
	  theta2 <- data.frame(mystate$theta.freq[keep, ], Topic = mystate$labels[keep])
  
    t.theta <- reshape2::melt(theta2, id.vars = "Topic", variable.name = "Category", value.name = "Freq2")
    all.df <- plyr::join(all.df, t.theta)
    # Collect P(t|Cluster) for each possible topic
    c.theta <- plyr::join(t.theta, data.frame(Category = paste0("Document", cls.df$docs), cluster = paste0("Cluster", cls.df$cluster)))
    c.theta <- plyr::ddply(c.theta, c("Topic", "cluster") , plyr::summarise, Freq3 = sum(Freq2))
    names(c.theta) <- sub("cluster", "Category", names(c.theta))
    all.df <- plyr::join(all.df, c.theta)
    all.df$Freq[!is.na(all.df$Freq2)] <- all.df$Freq2[!is.na(all.df$Freq2)]
    all.df$Freq[!is.na(all.df$Freq3)] <- all.df$Freq3[!is.na(all.df$Freq3)]
    all.df <- all.df[, -grep("Freq[0-9]", names(all.df))]
    # Infer the occurences within document/cluster
    #all.df$Freq <- all.df$Total * all.df$Freq
    # P(D|t) -- as a percentage -- for each possible term
	#rownames(d.t) <- paste0("Document", cls.df$docs)
  
	
    #d.t2 <- data.frame(100*t(d.t)[keep, ], Topic = mystate$labels[keep])
	d.t2 <- data.frame(t(d.t)[keep, ], Topic = mystate$labels[keep])
    doc.table <- reshape2::melt(d.t2, id.vars = "Topic", variable.name = "Document", value.name = "Freq")
    kmeans <- input$cls.kmeans
	 # rownames(cls.df) <- paste0("Document", cls.df$docs) 
  print(doc.table)
    return(list(clsDat = cls.df, clsDat2 = doc.table, topDat = all.df,
                centers = centers, nClust = kmeans))
  })
  
  
  #output$cls.plot <- renderPlot({
  #  dat <- fitDocCluster()
    
  #  par(mar = c(5.1, 4.1, 0, 1))
  #  plot(cbind(dat$mds.df$x, dat$mds.df$y),
  #    col = dat$mds.df$cluster, xlab="", ylab="",
  #    pch = 20, cex = 3)
  #  points(dat$centers, pch = 4, cex = 4, lwd = 4)
  #})
  
  
  # Function for generating module text
#   module_tooltip <- function(x) {
#     if (is.null(x)) return(NULL)
#     if (is.null(x$docs)) return(NULL)
#     
#     docname = mystate$doc.names[x$docs]
#     #paste0("<b>", movie$Title, "</b><br>",
#     #       movie$Year, "<br>",
#     #       "$", format(movie$BoxOffice, big.mark = ",", scientific = FALSE)
#     #)
#     paste0("<b>", docname, "</b>")
#   }
  
  
#   reactive({
#     dat <- fitDocCluster()
#     
#     dat$mds.df %>%
#       ggvis(~x, ~y, fill=~cluster, key := ~docs) %>% 
#       layer_points(size := 50, size.hover := 200,
#                    fillOpacity := 0.2, fillOpacity.hover := 0.5) %>%
#       add_tooltip(module_tooltip, "hover") %>%
# #      add_axis("x", title = "") %>%
#  #     add_axis("y", title = "yvar_name") %>%
#       set_options(width = 1000, height = 500)
#         
#   }) %>% bind_shiny("plot1")

defaultColors <- c("#3366cc", "#dc3912", "#ff9900", "#109618", "#990099", "#0099c6", "#dd4477")

# series <- structure(
#   lapply(defaultColors, function(color) { list(color=color) })
# )

# myData <- reactive({
#   # Filter to the desired year, and put the columns
#   # in the order that Google's Bubble Chart expects
#   # them (name, x, y, color, size). Also sort by region
#   # so that Google Charts orders and colors the regions
#   # consistently.
#   dat <- fitDocCluster()
#   
#   #ss <- data.frame(modules = rownames(dat$mds.df), dat$mds.df, size=rep(1, dim(dat$mds.df)[1]))
#   ss <- data.frame(modules = rownames(dat$mds.df), dat$mds.df, size=mystate$doc.term)
#   
#   df <- ss %.%
#     select(modules, x, y, cluster, size)
# })

#   output$chart <- reactive({
#     dat <- fitDocCluster()
#     # Return the data and options
#     list(
#        data = googleDataTable(myData())
# #        options = list(
# # #         title = sprintf(
# # #           "Health expenditure vs. life expectancy, %s",
# # #           input$year),
# #          series = series
# #        )
#     )
#   })


  output$upl.contents <- renderDataTable({
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, it will be a data frame with 'name',
    # 'size', 'type', and 'datapath' columns. The 'datapath'
    # column will contain the local filenames where the data can
    # be found.
    
    inFile <- input$upl.file
    
    if (is.null(inFile))
      return(NULL)
    
    read.csv(inFile$datapath, header = input$upl.header,
             sep = input$upl.sep, quote = input$upl.quote)
  })

addConstraint <- function(words, type) {
    
  if (type == 1)
    mystate$constraints$mlinks[[length(mystate$constraints$mlinks) + 1]] <- words
  else if (type == 2)
    mystate$constraints$clinks[[length(mystate$constraints$clinks) + 1]] <- words
  
}

observe({
  if (input$addConstr == 0) 
    return()
  
  addConstraint(isolate(input$selectWords), isolate(input$constr.type))
  
  mlinks <- unlist(lapply(mystate$constraints$mlinks, function(l) paste(l,collapse=" ") ))
  clinks <- unlist(lapply(mystate$constraints$clinks, function(l) paste(l,collapse=" ") ) )               
  
  #update the 
  updateSelectInput(session, "selected.mlinks", choices = mlinks, selected =  mlinks)
  updateSelectInput(session, "selected.clinks", choices = clinks, selected =  clinks)
  
  #clear the select words input
  updateTextInput(session, "selectWords", value = " ")   
  
  #Set the dirty bit
  mystate$dirty <- TRUE
})

updateMustLinkConstraints <- function() {
  mystate$constraints$mlinks <- lapply(isolate(input$selected.mlinks), function(l) unlist(strsplit(l, " ")))
}

updateCannotLinkConstraints <- function() {
  mystate$constraints$clinks <- lapply(isolate(input$selected.clinks), function(l) unlist(strsplit(l, " ")))
}

observe({
  if (input$refitLDA ==0)
    return()
  mystate$dirty <- TRUE
  
  updateMustLinkConstraints()  
  updateCannotLinkConstraints()

#   print(mystate$constraints)
     
  compute_state(msg='Recomputing LDA')
  
})



observe({
  if (input$fitLDA ==0)
    return()

  mystate$alpha <- input$alpha
  mystate$beta = input$beta
  mystate$eta = input$eta 
  mystate$K <- input$tc
  mystate$numsamp = input$ns
  mystate$randseed = 821945
  
  mystate$dirty <- TRUE
       
  compute_state()
})


compute_state <- function(msg = 'Computing LDA'){
  if (is.null(mystate$phi) | isTRUE(mystate$dirty)) {
#     withProgress(message = msg,
#                  detail = 'This may take a while...', value = 0, {     
    
    progress <- Progress$new(session)
    progress$set(message = msg, detail = 'This may take a while...', value = 0.5)
    
    
                   fit.LDA(mystate)
                   
                   if (is.null(mystate$constraints$conflicts)){
                     print(mystate$labels)
                     updateSelectInput(session, "selectWords",  choices = as.list(mystate$vocab))
                     updateSelectInput(session, "topic.select",  choices = as.list(mystate$labels), selected = mystate$labels[1])
                   }
                   else {#there conflicts  
                     updateTextInput(session, inputId= "constrConflicts", value=mystate$constraints$conflicts) 
                     mystate$constraints$conflicts <- NULL
                   }
                   #                  })
    progress$close()

  }
}

observe({
  if (input$topic.updateLabel==0)
    return()
  
  new_label <- isolate(input$topic.label)
  old_label <- isolate(input$topic.select)
  
  
  mystate$labels[which(mystate$labels==old_label)] <- new_label
  
  updateSelectInput(session, "topic.select",  choices = mystate$labels, selected = input$topic.label)

  
})

wordcloud_rep <- repeatable(wordcloud)

output$topic.count.plot <- renderPlot({
     
  compute_state()

  #create a dependency on the update button
#   input$topic_update
  
  top <- input$topic.select
  
  index <- which(mystate$labels == top)

  wordcloud_rep(isolate(mystate$vocab), isolate(mystate$phi[,index]), scale=c(4,0.5),
                max.words=input$topic.freq.max,
                colors=brewer.pal(8, "Dark2"))
})


output$topicDistPlot <- renderGvis({
  #create a dependency on the update
  top <- input$topic.select
  
  index <- which(mystate$labels == top)
  
  df <- data.frame(t(isolate(mystate$phi[,index])*100))
  
  
  
  gvisColumnChart(df)
})


})
