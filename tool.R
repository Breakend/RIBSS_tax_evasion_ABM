## app.R ##

library(shinydashboard)
library(flexdashboard)
library(shinythemes)
library(rhandsontable)
library(shinyjs)

if(!require(visNetwork)) 
{
  install.packages("visNetwork")
  library(visNetwork)
} 


#############################################################
###                                                       ###
###        USER INTERFACE           
###                                                       ###
#############################################################

ui <- dashboardPage(
  dashboardHeader(title = "Tax Evasion Model"),
  ## Sidebar content
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Policy Levers", tabName = "policyinputs", icon = icon("th")),
      menuItem("Behavioral Parameters", tabName = "inputs", icon = icon("th")),
      menuItem("Chart: Macro",  tabName = "charts1", icon = icon("line-chart")),
      menuItem("Chart: Micro",  tabName = "charts2", icon = icon("chart")),
      menuItem("Chart: Histograms",  tabName = "hist", icon = icon("bar-chart")),
      menuItem("Source code", icon = icon("file-code-o"), 
               href = "https://github.com/rstudio/shinydashboard/"),
      actionButton("run.model", "Run the Model", icon = icon("play"), 
                   style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
      br(strong("Authors:"), br(tags$small("Raffaele Vardavas & Pavan Katkar")))
    )
  ),
  ## Body content
  dashboardBody(
    tabItems(
      # Dashboard tab content
      tabItem(tabName = "dashboard",
              fluidRow(
                tabBox(
                  side = "left", height = "500px",
                  selected = "Tab3",
                  tabPanel(title="Agg", background = "maroon", solidHeader = TRUE,
                           plotOutput("aggregated.dyn.plot", height = 300)),
                  tabPanel("Tab2", "Tab content 2"),
                  tabPanel("Tab3", "Note that when side=right, the tab order is reversed.")
                ),
                
                
                tabBox(
                  title = "First tabBox",
                  # The id lets us use input$tabset1 on the server to find the current tab
                  id = "tabset1", height = "500px",
                  tabPanel(
                    title="Histogram", background = "maroon", solidHeader = TRUE,
                    plotOutput("plot1", height = 300)
                  ),
                  tabPanel(
                    title="Inputs", status = "warning", solidHeader = TRUE,
                    "Box content here", br(), "More box content",
                    sliderInput("slider", "Slider input:", 1, 100, 50),
                    textInput("text", "Text input:")
                  )
                )
              ),

              
              ##  Dashboard Indicators
              
              # gauge(16, min = 0, max = 100, symbol = '%', gaugeSectors(
              #   success = c(80, 100), warning = c(40, 79), danger = c(0, 39)
              # )),
              
              # infoBoxes with fill=FALSE
              fluidRow(
                # A static infoBox
                infoBox("New Orders", 10 * 2, icon = icon("credit-card")),
                # Dynamic infoBoxes
                infoBoxOutput("progressBox"),
                infoBoxOutput("approvalBox")
              )
              
              # fluidRow(
              #   # Clicking this will increment the progress amount
              #   box(width = 4, actionButton("count", "Increment progress"))
              # )
      ),
      
      # Inputs tab content
      tabItem(tabName = "inputs",
              h2("Inputs tab content"),
              box(
                title = "Time Scale", height = "500px", width = 4,
                solidHeader = TRUE, #background = "black",
                box(width="100%",
                    sliderInput("morale.half.life",
                                ("morale.half.life"),
                                min = 1,
                                max = 8,
                                value = 2, step = 1),
                    sliderInput("v.PP",
                                ("Evasion Relapse"),
                                min = 0.5,
                                max = 1.0,
                                value = 0.9, step = 0.01),
                    sliderInput("full.tendency.factor",
                                ("full.tendency.factor"),
                                min = 1.0,
                                max = 2.0,
                                value = 1.6, step = 0.1),
                    sliderInput("rate.refund.movement",
                                ("rate.refund.movement"),
                                min = 0.05,
                                max = 0.2,
                                value = 0.1, step = 0.01)
                )
                ),
              box(
                title = "Weights", height = "500px",width = 4,
                solidHeader = TRUE, #background = "black",
                box(width="100%",
                    sliderInput("beta.personal",
                                ("beta.personal"),
                                min = 0.45,
                                max = 1.00,
                                value = 0.55, step = 0.01),
                    sliderInput("c1.dist.weight",
                                ("c1.dist.weight"),
                                min = 0,
                                max = 1.0,
                                value = 1.0, step = 0.1),
                    sliderInput("c2",
                                ("c2"),
                                min = 0.65,
                                max = 1.0,
                                value = 0.7, step = 0.01),
                    sliderInput("return.weight",
                                "return.weight",#tags$small()
                                min = 0.0,
                                max = 0.37,
                                value = 0.08, step = 0.01)
                )
              ),
  
              box(
                title = "Responses", height = "600px",width = 4,
                solidHeader = TRUE, #background = "black",
                box(width="100%",
                    sliderInput("m.qP",
                                ("m.qP"),
                                min = 0.02,
                                max = 0.11,
                                value = 0.06, step = 0.01),
                    sliderInput("bomb.crater.factor",
                                "bomb.crater.factor",#tags$small()
                                min = 0.01,
                                max = 2.2,
                                value = 0.58, step = 0.01),
                    sliderInput("gamblers.fallacy.grad",
                                ("gamblers.fallacy.grad"),
                                min = 0.00,
                                max = 0.20,
                                value = 0.08, step = 0.01),
                    sliderInput("media.mid.effect",
                                "media.mid.effect",#tags$small()
                                min = 0.30,
                                max = 0.47,
                                value = 0.38, step = 0.01)
                    # sliderInput("tax.gap.reporting.media.threshold",
                    #             ("tax.gap.reporting.media.threshold"),
                    #             min = 0.01,
                    #             max = 0.15,
                    #             value = 0.10, step = 0.01)
                )
              )
                
              
              # {
              # num.individuals = 5  # determine the number of individuals here
              # 
              # # concatenate the select input and the other inputs
              # inputs =  lapply(1:num.individuals, function(i) {
              #   sliderInput(inputId = paste0("ind", i), label = paste("Individual", i), 
              #     min = 0, max = 20000, value = c(0, 2500), step = 250)
              # })
              # 
              # sidebar.panel = do.call(sidebarPanel, inputs)}
      ),
      
      # Inputs tab content
      tabItem(tabName = "policyinputs", 
              h2("Policy Inputs"),
              box(
                 title = "General Levers", height = "500px",
                 solidHeader = TRUE, #background = "black",
                 box(width="100%",
                sliderInput("tax.rate",
                            "Tax Rate",#tags$small()
                            min = -0.2,
                            max = 0.2,
                            value = 0.0, step = 0.01),
                sliderInput("audit.rate",
                            ("Audit Rate"),
                            min = 0.002,
                            max = 0.05,
                            value = 0.01),
                sliderInput("detection.eff",
                            ("Detection Efficiency"),
                            min = 0.7,
                            max = 0.9,
                            value = 0.8),
                sliderInput("penalty.rate",
                            ("Penalty Rate"),
                            min = 0.1,
                            max = 0.5,
                            value = 0.15)
                 )
              ),
              box(
                #http://rmarkdown.rstudio.com/flexdashboard/using.html
                title = "Specific Levers",height = "500px",
                #background = "black", ### can't add color issue with rhandsontable
                solidHeader = TRUE,
              tabBox(
                side = "left", width="100%",
                selected = "Tax Rate Schedule",
                tabPanel("Tax Rate Schedule", 
                         tabBox(
                         side = "left", width="100%",
                         selected = "S",
                         tabPanel("S", 
                                  "Filing Status: Single",
                                  rHandsontableOutput("FP_Single")
                         )
                         ,
                         tabPanel("HH",
                                  "Filing Status: Head of Household",
                                 rHandsontableOutput("FP_Head.of.Household")
                         ),
                         tabPanel("MFJ",
                                  "Filing Status: Married filing Jointly",
                                    rHandsontableOutput("FP_Married.Filing.Jointly")
                         ),
                         tabPanel("MFS",
                                  "Filing Status: Married filing Seperatly",
                                  rHandsontableOutput("FP_Married.Filing.Separately")
                         )
                         )
                         
                         ),
                tabPanel("Deterrence Strategy",
                         rHandsontableOutput("DS"))
              ))
      ),
      
      # Charts tab content
      tabItem(tabName = "charts",
              h2("Charts tab content")
      )
    )
  )
)


#############################################################
###                                                       ###
###        TAX MODEL        
###                                                       ###
#############################################################

source("TaxModel.R")
run.model <- function(){
  #print(paste(Sys.time(), "Control in run model"))
  #print(paste("Before", "Sum of per.audit.rate:", sum(population.data[, "per.audit.rate"]), sum(population.data[, "per.penalty.rate"])))
  if(is.null(initial.state))
  {  
    population.data <- initialize.risk.perceptions(population.data, config)
  } else 
  {
    population.data <- initialize.risk.perceptions(population.data, config, initial.state[, c("per.audit.rate", "per.penalty.rate")])
  }
  
  all.data <- run.dynamics(population.data, initial.state = initial.state, config, g.info, 
                           table.audit.rates, table.refund, final.year = NULL, cl=cl)
  
  track.id <- as.data.frame(bind_rows(all.data$track.dyn))
  aggregated.dyn <- as.data.frame(all.data[['aggregated.dyn']])  
  final.state <- all.data[['final.state']]
  #Save parts of the final state as initial state for future run
  initial.state <<- final.state
  
  equilibrium.reached <- all.data[['equilibrium.reached']]
  
  #Get all the history data from final state. 
  #We initialize it only to create the required data frame structure with appropriate names
  hist <- initialize.history(N)
  compliant.history <- final.state[, names(hist[['compliant.history']])]
  penalty.history <- final.state[, names(hist[['penalty.history']])]
  audit.history <- final.state[, names(hist[['audit.history']])]
  amount.under.reporting.history <- final.state[, names(hist[['amount.under.reporting.history']])]
  perc.reporting.history <- final.state[, names(hist[['perc.reporting.history']])]
  perc.hideable.reporting.history <- final.state[, names(hist[['perc.hideable.reporting.history']])]
  
  tmpr <- sapply(names(perc.reporting.history), function(col.name) {
    perc.reporting.history[, col.name] <<- ifelse(perc.reporting.history[, col.name] > 100, 100, perc.reporting.history[, col.name])
  })
  rm(tmpr)
  final.state[, names(perc.reporting.history)] <- perc.reporting.history
  
  sim.data <- with(final.state, data.frame(tax.ids=population.data$tax.ids, 
                                           self.employed=population.data$self.employed,
                                           c1=round(100*population.data$c1,0),
                                           c1.tilde=round(100*c1.tilde,0),
                                           per.audit.rate = per.audit.rate*100,
                                           per.penalty.rate = per.penalty.rate*100,
                                           perc.hideable.income = population.data$prop.hideable.income*100,
                                           Delta.Morale = round(100*Delta.Morale ,0),
                                           w = round(100*w,0),
                                           freq.audits=freq.audits,
                                           freq.penalty=freq.penalty,
                                           years.since.last.compliant =years.since.last.compliant,
                                           years.since.last.audit=years.since.last.audit,
                                           years.since.last.penalty=years.since.last.penalty,
                                           amount.under.reporting.history,
                                           perc.reporting.history,
                                           perc.hideable.reporting.history,
                                           compliant.history,
                                           audit.history,
                                           penalty.history))
  
  plots <- create.plots(sim.data, track.id, aggregated.dyn, config, g)
  return(list(plots=plots, track.dyn=track.id))
}


#############################################################
###                                                       ###
###        SERVER        
###                                                       ###
#############################################################

server <- function(input, output) {


  ### Fiscal Policy . 
  tax.schedule <- read.csv("Inputs/US_Income_Tax_Rates_2016.csv",
                           stringsAsFactors = F)
  colnames(tax.schedule)[c(2,3)]<-c("from","to")
  tax.schedule.split <-split(tax.schedule,tax.schedule$filing.status) 
  output$FP_Single <- renderRHandsontable({
    if (is.null(input$FP_Single)) {
      DF.s <- tax.schedule.split$Single
      DF.s <- DF.s[,c(2:4)]
    } else {
      DF.s <- hot_to_r(input$FP_Single)
    }
    DF.s$from <- c(0,DF.s$to)[1:nrow(DF.s)]
    rhandsontable(DF.s, height = "100%" ,useTypes = TRUE, rowHeaders = NULL) %>%
      hot_col(col = 1, format = "0$", readOnly = TRUE) %>%
      hot_col(col = 2, format = "0$")%>%
      hot_col(col = 3, format = "0.0%") 
  })
  output$FP_Head.of.Household <- renderRHandsontable({
    if (is.null(input$FP_Head.of.Household)) {
      DF.hh <- tax.schedule.split$Head.of.Household
      DF.hh <- DF.hh[,c(2:4)]
    } else {
      DF.hh <- hot_to_r(input$FP_Head.of.Household)
    }
    DF.hh$from <- c(0,DF.hh$to)[1:nrow(DF.hh)]
    rhandsontable(DF.hh, height = "100%" ,useTypes = TRUE, rowHeaders = NULL) %>%
      hot_col(col = 1, format = "0$", readOnly = TRUE) %>%
      hot_col(col = 2, format = "0$")%>%
      hot_col(col = 3, format = "0.0%") 
  })
  output$FP_Married.Filing.Jointly <- renderRHandsontable({
    if (is.null(input$FP_Married.Filing.Jointly)) {
      DF.mfj <- tax.schedule.split$Married.Filing.Jointly
      DF.mfj <- DF.mfj[,c(2:4)]
    } else {
      DF.mfj <- hot_to_r(input$FP_Married.Filing.Jointly)
    }
    DF.mfj$from <- c(0,DF.mfj$to)[1:nrow(DF.mfj)]
    rhandsontable(DF.mfj, height = "100%" ,useTypes = TRUE, rowHeaders = NULL) %>%
      hot_col(col = 1, format = "0$", readOnly = TRUE) %>%
      hot_col(col = 2, format = "0$")%>%
      hot_col(col = 3, format = "0.0%") 
  })
  output$FP_Married.Filing.Separately <- renderRHandsontable({
    if (is.null(input$FP_Married.Filing.Separately)) {
      DF.mfs <- tax.schedule.split$Married.Filing.Separately
      DF.mfs <- DF.mfs[,c(2:4)]
    } else {
      DF.mfs <- hot_to_r(input$FP_Married.Filing.Separately)
    }
    DF.mfs$from <- c(0,DF.mfs$to)[1:nrow(DF.mfs)]
    rhandsontable(DF.mfs, height = "100%" ,useTypes = TRUE, rowHeaders = NULL) %>%
      hot_col(col = 1, format = "0$", readOnly = TRUE) %>%
      hot_col(col = 2, format = "0$")%>%
      hot_col(col = 3, format = "0.0%") 
  })
  
  #tax.schedule <-rbind(DF.s,DF.hh)
  
  
  ### Deterrence Strategy . 
  table.audit.rates <- read.csv("Inputs/IRS Examination coverage in FY2015.csv",
                                stringsAsFactors = F)
  table.audit.rates<- table.audit.rates[-1,]
  tmp <- table.audit.rates[,c(1:3)]
  colnames(tmp) <- c("from","to","rate")
  tmp$rate <- tmp$rate/100
  output$DS<- renderRHandsontable({
    if (is.null(input$DS)) {
      DF <- tmp
    } else {
      DF <- hot_to_r(input$DS)
    }
    rhandsontable(DF, height = "100%" ,useTypes = TRUE, rowHeaders = NULL) %>%
      hot_col(col = 1, format = "0$", readOnly = TRUE) %>%
      hot_col(col = 2, format = "0$")%>%
      hot_col(col = 3, format = "0.00%") 
  })
  
  
  
  #############################################################
  ###                                                       ###
  ###        CODE FROM THE PREVIOUS TOOL        
  ###                                                       ###
  #############################################################
  
  
  withProgress(message = "Configuring the model", {
    config <<- read.csv("Inputs/model.config.csv", stringsAsFactors = F)
  })
  
  withProgress(message = "Reading in population data", {
    if(is.null(population.data))
      population.data <<- get.population.data(population.data.file)
    
    #Size of the population
    N <<- nrow(population.data) 
    #Overwrite the config population.size
    config[config$config.vars == 'population.size', 'value'] <<- N
    
    #Get Refund tables 
    table.refund <<- read.csv(tax.refund.proportions.file)
    population.data$refund.group <<- findInterval(population.data$income,c(table.refund$Lower.Income,Inf))
    
    #Get IRS 
    table.audit.rates <<- read.csv(deterrence.strategy.file)
    table.audit.rates <<- table.audit.rates[-1,]
    
    ### RV: this needs to be done by sampling the right c1 distribition.
    population.data$c1 <<- population.data$c1.tri.dist.dist
  })
  

  withProgress(message = "Configuring the model", {
    config <<- read.csv("Inputs/model.config.csv", stringsAsFactors = F)
  })
  
  withProgress(message = "Reading in population data", {
    if(is.null(population.data))
      population.data <<- get.population.data(population.data.file)
    
    #Size of the population
    N <<- nrow(population.data) 
    #Overwrite the config population.size
    config[config$config.vars == 'population.size', 'value'] <<- N
    
    #Get Refund tables 
    table.refund <<- read.csv(tax.refund.proportions.file)
    population.data$refund.group <<- findInterval(population.data$income,c(table.refund$Lower.Income,Inf))
    
    #Get IRS 
    table.audit.rates <<- read.csv(deterrence.strategy.file)
    table.audit.rates <<- table.audit.rates[-1,]
    
    ### RV: this needs to be done by sampling the right c1 distribition.
    population.data$c1 <<- population.data$c1.tri.dist.dist
  })
  
  withProgress(message = "Creating the network", {  
    network.data <- get.graph.from.network.data(network.data.file)
    ave.degree.tTaxes <- as.numeric(config[config$config.vars == 'ave.degree.tTaxes', 'value'])
    
    ### RV set up the network and edgelist 
    edges <- as.matrix(network.data[, c("id1", "id2")])
    g <<- graph_from_edgelist(el = edges)
    g.info <<- create.interacting.network.on.taxes(g,net.degree.info, population.data, ave.degree.tTaxes)
    #Overwriting g
    g <<- g.info$g
    #g.data <<- toVisNetworkData(g)
    g.info$net.degree.info <<- net.degree.info
    nn <<- g.info$nn
    
    setProgress(0.5)
  })
  
  #Default run
  withProgress(message="Running the model", value = 0.5, {
    retval <<- run.model()
    plots <<- retval$plots
    track.dyn <<- retval$track.dyn
    setProgress(0.8)
  })
  
  update.dynamics <- eventReactive(input$run.model, {
    #Updating the config
    withProgress(message = "Configuring the model", value = 0.2, {
      #print("Control in Update Dynamics")
      #config[config$config.vars=="tax.rate", "value"] <<- input$tax.rate
      population.data$tax.rate <<- population.data$tax.rate + input$tax.rate
      population.data$tax.rate <<- ifelse(population.data$tax.rate < 0, 0, population.data$tax.rate)
      population.data$tax.rate <<- ifelse(population.data$tax.rate > 1, 1, population.data$tax.rate)
      config[config$config.vars=="audit.rate", "value"] <<- input$audit.rate
      config[config$config.vars=="penalty.rate", "value"] <<- input$penalty.rate
      config[config$config.vars=="detection.eff", "value"] <<- input$detection.eff
      ifelse(input$network.model == "None", network.model <<- "FALSE", network.model <- "ER")
      config[config$config.vars=="network.model", "value"] <<- network.model
      config[config$config.vars=="bomb.crater.effect", "value"] <<- as.character(input$bomb.crater.effect)
      config[config$config.vars=="media.effect", "value"] <<- as.character(input$media.effect)
      config[config$config.vars=="tax.refund.effect", "value"] <<- as.character(input$tax.refund.effect)
    })
    
    
    withProgress(message = "Running the model", value = 0.5, {
      retval <<- run.model()
      setProgress(0.8)
    })
    
    
    
    withProgress(message = "Generating plots", value = 0.8, {
      plots <<- retval$plots
      #track.dyn <<- retval$track.dyn
      #net.plots <<- net.plot.at.time.t(track.dyn, g, unique(track.dyn$t))
      setProgress(1)
    })
    #print("All plots created - update dynamics")
  })
  
  #Run this code in the background 
  observe({
    #print("Control in observe")
    withProgress(message = "Generating plots", value = 0.8, {
      #net.plots <<- network.plots(track.dyn, g, unique(track.dyn$t))
    })
    #print("All plots created - observe")
  })
  
  output$aggDyn <- renderPlot({
    #print("Control in Aggregate plot")
    if(input$run.model != 0) update.dynamics()
    plots$aggregated.dyn.plot
  })
  
  output$tl.plot <- renderVisNetwork({
    #print("Control in tl.plot")
    if(input$run.model != 0) update.dynamics()
    tmp.dyn <- track.dyn[track.dyn$t == 1, ]
    tmp.dyn$evaded <- (100- tmp.dyn$hideable.reported)/100
    update.plot.data(tmp.dyn, group1 = "audited", group2 = "penalized")
    lnodes <- data.frame(label = c("Compliant", "Non-Compliant", "Audited", "Penalized"),
                         shape = rep("circle", 4), 
                         color = "cornflowerblue", "red", "yellow", "black")
    
    odds.ratios <- calculate.odds.ratios(g.data$nodes, g.data$edges)
    
    visNetwork(g.data$nodes, g.data$edges, 
               height = "380pt",width="380pt") %>%
      visNodes(color = list(highlight = "yellow"),
               x=g.data$nodes$x, y= g.data$nodes$y, 
               label= NULL, physics=F, fixed=T ) %>%
      visEdges( hoverWidth =1)%>%
      visInteraction(navigationButtons = F, hover = T)%>% 
      visOptions(manipulation =FALSE, 
                 highlightNearest = list(enabled = F,hover= T, degree = 1),
                 collapse = list(enabled=T, fit=T),
                 autoResize = T, clickToUse = FALSE)
    
    # visNetwork(nodes = g.data$nodes, edges = g.data$edges) %>%
    #   visPhysics(stabilization = F,   barnesHut = list(
    #     gravitationalConstant = -1200,
    #     springConstant = 0,
    #     springLength = 100,
    #     avoidOverlap = 0.5), adaptiveTimestep = T) %>%
    #   visEdges(smooth = F) %>%  visInteraction(navigationButtons = TRUE) %>% 
    #   visOptions(highlightNearest = list(enabled =TRUE, degree = 1, hover = T)) %>%
    #   visLayout(randomSeed = 123)
    #   #visLegend(addNodes = lnodes)
  })
  
  output$odds.ratio.plot <- renderPlot({
    #print("Control in odds.ratio.plot")
    t <- input$tl.slider
    odds.ratios <- calculate.odds.ratios(g.data$nodes, g.data$edges)
    ggplot(data = odds.ratios, aes(x=ratio, y=odds, fill=ratio)) + geom_bar(width=0.3, stat="identity") + 
      geom_text(aes(label=round(odds, 2)), vjust=-0.5) + 
      ggtitle(label = paste("Odds Ratio at year =", t)) + theme_light()
  })
  
  # observe({
  #   if(input$run.model != 0) update.dynamics()
  #   t <- input$tl.slider
  #   tmp.dyn <- track.dyn[track.dyn$t == t, ]
  #   tmp.dyn$evaded <- (100- tmp.dyn$hideable.reported)/100
  #   update.plot.data(tmp.dyn, group1 = "audited", group2 = "penalized")
  #   nodes <- g.data$nodes
  #   visNetworkProxy("tl.plot") %>% 
  #     visUpdateNodes(nodes = nodes)
  # })
  
  
  output$c1.plot <- renderPlot({
    #print("Control in c1.plot")
    if(input$run.model != 0) update.dynamics()
    plots$c1.plot
  })
  
  output$w.plot <- renderPlot({
    #print("Control in w.plot")
    if(input$run.model != 0) update.dynamics()    
    plots$w.plot
  })
  
  output$report.plot <- renderPlot({
    #print("Control in report plot")
    if(input$run.model != 0) update.dynamics()
    plots$report.plot
  })
  
  output$iterative.map.plot <- renderPlot({
    #print("Control in iterative plot")
    if(input$run.model != 0) update.dynamics()
    plots$iterative.map.plot
  })
  
  output$per.audit.rate.plot <- renderPlot({
    #print("Control in iterative plot")
    if(input$run.model != 0) update.dynamics()
    plots$per.audit.rate.plot
  })
  
  output$per.penalty.rate.plot <- renderPlot({
    #print("Control in iterative plot")
    if(input$run.model != 0) update.dynamics()
    plots$per.penalty.rate.plot
  })
  
  output$years.since.last.compliant.plot <- renderPlot({
    #print("Control in iterative plot")
    if(input$run.model != 0) update.dynamics()
    plots$years.since.last.compliant.plot
  })
  
  output$report.compliance.prob.plot <- renderPlot({
    #print("Control in iterative plot")
    if(input$run.model != 0) update.dynamics()
    plots$report.compliance.prob.plot
  })
  
  output$interesting.cases.plot <- renderPlot({
    #print("Control in iterative plot")
    if(input$run.model != 0) update.dynamics()
    plots$sample.report.trajectory.plot
  })
  
  
  
  
  #############################################################
  ###                                                       ###
  ###        Example code Below - THIS WILL BE DELETED - ITS NOT PART OF THE TAX MODEL.        
  ###                                                       ###
  #############################################################

  histdata <- rnorm(500)
  
  output$plot1 <- renderPlot({
    data <- histdata[seq_len(input$slider)]
    hist(data)
  })
  
  output$progressBox <- renderInfoBox({
    infoBox(
      "Progress", paste0(25 + input$count, "%"), icon = icon("list"),
      color = "purple"
    )
  })
  output$approvalBox <- renderInfoBox({
    infoBox(
      "Approval", "80%", icon = icon("thumbs-up", lib = "glyphicon"),
      color = "yellow"
    )
  })
  
  # Same as above, but with fill=TRUE
  output$progressBox2 <- renderInfoBox({
    infoBox(
      "Progress", paste0(25 + input$count, "%"), icon = icon("list"),
      color = "purple", fill = TRUE
    )
  })
  output$approvalBox2 <- renderInfoBox({
    infoBox(
      "Approval", "80%", icon = icon("thumbs-up", lib = "glyphicon"),
      color = "yellow", fill = TRUE
    )
  })
  
}

shinyApp(ui, server)