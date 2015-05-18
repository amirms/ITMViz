
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
# 
# http://www.rstudio.com/shiny/
#
library(ITM)
library(shiny)
library(ggvis)
library(googleCharts)
library(shinysky)
# library(shinyIncubator)

#  addResourcePath('assets', system.file('assets', package='ITMViz'))
#   addResourcePath('data', system.file('data', package='ITMViz'))

# addResourcePath('www', system.file('www', package='ITMViz'))
#addResourcePath('assets', normalizePath('ITMViz'))





hasConflicts = FALSE

#source("chooser.R")

#scatterDiv <- function (outputId) {
#  HTML(paste0("<div id=\"", outputId, "\" class=\"shiny-scatter-output\"><svg /></div>"))
#}
# Thanks Winston! https://github.com/wch/testapp/blob/master/custom-style/ui.R
# widget_style <-
#   "display: inline-block;
# vertical-align: text-top;
# padding: 7px;
# border: solid;
# border-width: 1px;
# border-radius: 4px;
# border-color: #CCC;"
# side_margins <-
#   "margin-right:50px;
# margin-left:50px;"
# #ugly hack to line up the documents below the scatterplot
# top_margin <-
#   "margin-top:-550px;"



K = 15
#   alpha = 50 / K
alpha = 0.5
beta = .1
eta = 10000
vocab=list()
labels = list()
# eta = 1000

# mystate <- ldaState(alpha = alpha, beta = beta, eta = eta, K = K)

prname= "jedit-5.1.0"
min.tfidf = 25
numsamp =50
randseed = 821945


#Get all the constraints
# mlinks <- unlist(lapply( constraints$mlinks, function(l) paste(l,collapse=" ") ))
# clinks <- unlist(lapply( constraints$clinks, function(l) paste(l,collapse=" ") ))
# ilinks <- unlist(lapply( constraints$ilinks, function(l) paste(l,collapse=" ") ))
mlinks <- c()
clinks <- c()
ilinks <- c()

# headerTag <- "No project is selected! Please open an existing project from the toolbar."
# if (!is.null(mystate))
  headerTag <- paste("The selected project is:",  prname, sep= " ")

#Constraint Conflicts
hasConflicts = FALSE


ui = shinyUI( navbarPage("ITMViz", id="itmviz", header=headerTag,

                        tabPanel("Existing Projects",
                                 tags$head(
                                   tags$link(rel = 'stylesheet', type = 'text/css', href = 'assets/styles.css')
                                 ),
                           fluidPage(
#                                 fluidRow(
#                                  column(6,
                             verticalLayout(
                                    inputPanel(
                                       numericInput('tc', 'Topic Count',  K, min = 1),
                                       numericInput('alpha', 'Alpha',  alpha, min = 0),
                                       numericInput('beta', 'Beta',  beta, min = 0),
                                       numericInput('eta', 'Eta',  eta, min = 1)
                                   )
                                 ),
#                                  column(6, 
verticalLayout(
                                   inputPanel(
                                       numericInput('burnin', 'Number of Burn-ins', 0, min = 0),
                                       numericInput('ns', 'Number of Samples',  numsamp, min = 1),
                                       numericInput('sinterval', 'Sample Interval', 1, min = 1)
                                   )
                                 
                             ),
                             fluidRow(
                              
                               actionButton("fitLDA","Compute LDA")
                             )
                           )
                        ),
                  tabPanel("Topic Visualization",    
                              tags$head(
                                tags$script(src = "assets/d3.v3.js"),
                                tags$script(src ="assets/topicz.js"),
                                tags$link(rel = 'stylesheet', type = 'text/css', href = 'assets/topicz.css')
                              ),
                           busyIndicator(wait = 1000),
                           
                              # absolutePanel(fixed = TRUE, draggable = TRUE,
                              #              top = 60, left = "auto", right = 20, bottom = "auto",
                              #              width = 330, height = "auto",
                              inputPanel(
#                                 selectInput("itm.distance", "Topical Distance Calculation", choices = c("Jensen-Shannon" = "JS",
#                                                                                                         "Symmetric Kullback-Leibler" = "KL")),
                                selectInput("itm.scaling", "Multidimensional Scaling Method", choices = c("Classical (PCA)" = "PCA",
                                                                                                          "Kruskal's Non-Metric" = "kruskal",
                                                                                                          "Sammon's Non-Linear Mapping" = "sammon")),
                                sliderInput("itm.kmeans", "Number of clusters", min = 1, max = 10, value = 1, width = "125px"),
                                sliderInput("itm.nTerms", "Number of terms", min = 1, max = 50, value = 30, width = "125px"),
                                sliderInput("itm.lambda", "Value of lambda", min = 0, max = 1, value = 0.6, width = "200px")
                              ),
                              #the el parameter in the js code selects the outputIds
                              mainPanel(HTML("<div id=\"mdsDat\" class=\"shiny-scatter-output\"><svg /></div>"))             
                              
                           
                              # )
                     ),
                  tabPanel("Topic Analysis", value=4,
#                            progressInit(),
                           
                           # Application title
#                            headerPanel("Word Cloud"),
#                           sidebarLayout(
  
                           # Sidebar with a slider and selection inputs
                           sidebarPanel(width = 3,
                                        selectInput("topic.select", "Choose a topic:", 
                                                    choices =  labels),
#                                         actionButton("topic_update", "Change"),
                                        hr(),
                                        sliderInput("topic.freq.max", 
                                                    "Maximum Number of Words:", 
                                                    min = 1,  max = 300,  value = 50),

                                        hr(),
                                        checkboxInput("setLabel", "Set Label", value = FALSE),
                                        conditionalPanel(
                                          condition = "input.setLabel == true",
                                          wellPanel(
                                            textInput("topic.label", "Label:", ""),
                                            actionButton("topic.updateLabel", "Update")
                                          )
                                        )
                           ),
                           
                           # Show Word Cloud
                           mainPanel(
                             plotOutput("topic.count.plot")
#                              htmlOutput("topicDistPlot")  
                           )
#                            )
                    ),
                  tabPanel("Constraints",
#                            fluidPage(
                             inputPanel(
#                              fluidRow(
#                                column(
#                                   select2Input("selectWords","Input Words", choices= vocab,
#                                  type="select",multiple=TRUE),
#                                 ),
                                selectInput("selectWords", "Input Words", choices= vocab, multiple=TRUE),
#                                column(
                                 radioButtons("constr.type", label = "Type of Constraint",
                                              choices = list("Must Link" = 1, "Cannot Link" = 2),
                                              selected = 1),
#                                ),
#                                column(
                                  actionButton("addConstr","Add")                                                      
#                                )
#                             )
#                              )
                            ),
                          
#                           verbatimTextOutput("constr.selection"),
#                          mainPanel(
                            conditionalPanel(condition="hasConflicts == true",
#                                              h4("Conflicting Constraints"),
                                             textInput(inputId="constrConflicts", label="Conflicting Constraints", value = "")
#                                              verbatimTextOutput(outputId = "constrConflicts")
                            ),
                           inputPanel(
                            select2Input("selected.mlinks","Must Link Constraints",
                                 choices=mlinks,selected=mlinks, 
                                 type="select",multiple=TRUE),
                            select2Input("selected.clinks","Cannot Link Constraints",
                                         choices=clinks,selected=clinks,
                                         type="select",multiple=TRUE)
#                             select2Input("selected.ilinks","Isolate Constraints",
#                                          choices=ilinks,selected=ilinks,
#                                          type="select",multiple=TRUE)
                            ),
                            busyIndicator("Calculation In progress",wait = 0),
                            actionButton("refitLDA","Re-Compute LDA") 
#                           )
                           
                  ),
                  
                     tabPanel("Cluster Documents",
                              tags$head(
                                tags$script(src = "assets/d3.v3.js"),
                                #tags$script(type="text/javascript", src = "https://www.google.com/jsapi"),
                                tags$script(src="assets/d3.tip.v0.6.3.js"),
                                tags$script(src ="assets/docz.js"),
                                tags$link(rel = 'stylesheet', type = 'text/css', href = 'assets/docz.css')
                              ),
                              busyIndicator(wait = 1000),
                              
                              inputPanel(
#                                 selectInput("cls.distance", "Topical Distance Calculation", choices = c("Jensen-Shannon" = "JS",
#                                                                                                         "Symmetric Kullback-Leibler" = "KL")),
                                selectInput("cls.scaling", "Multidimensional Scaling Method", choices = c("Classical (PCA)" = "PCA",
                                                                                                          "Kruskal's Non-Metric" = "kruskal",
                                                                                                          "Sammon's Non-Linear Mapping" = "sammon")),
                                sliderInput("cls.kmeans", "Number of clusters", min = 1, max = 10, value = 1, width = "125px"),
                                sliderInput("cls.nTopics", "Number of topics", min = 1, max =  K, value = 1, width = "125px")
                              ),
                              #the el parameter in the js code selects the outputIds
                              mainPanel(HTML("<div id=\"clsDat\" class=\"shiny-document-output\"><svg /></div>"))      
                                
                     )
  )
  
)
