
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

shinyServer(function(input, output, session) {
  
  # should this be here?
  set.seed(55279) ### first seed given by Chris Marcum
  set.seed(55333) 
  
  withProgress(message = "Configuring the model", {
    config <<- read.csv("Inputs/best.case.model.config.csv", stringsAsFactors = F)
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
    
    ### RV: this needs to be done by sampling the right c1 distribition.
    population.data$c1 <<- population.data$c1.tri.dist.dist
  })
  
  withProgress(message = "Creating the network", {  
    if(is.null(network.data)) network.data <- get.graph.from.network.data(network.data.file)
    ave.degree.tTaxes <- as.numeric(config[config$config.vars == 'ave.degree.tTaxes', 'value'])
    
    ### RV set up the network and edgelist 
    edges <- as.matrix(network.data[, c("id1", "id2")])
    
    if(is.null(g.info))
      g.info <<- create.interacting.network.on.taxes(g,net.degree.info, population.data, ave.degree.tTaxes)
    #Overwriting g
    g <<- g.info$g
    #g.data <<- toVisNetworkData(g)
    g.info$net.degree.info <<- net.degree.info
    nn <<- g.info$nn
    nn.int <<- g.info$nn.int
    
    setProgress(0.5)
  })
  
  #Default run
  withProgress(message="Creating Plots", value = 0.5, {
    setProgress(0.8)
    if(is.null(plots)) {
      sim.data <- default.state$sim.data
      plots <<- create.plots(sim.data, track.dyn, aggregated.dyn, config, g.info, small = 12, big = 15)
      gov.plots <<- gov.dyn.plot(gov.dyn)
    }
    setProgress(1)
  })
  
  
  
  #
  # main dashboard ------------------------------------------------------------
  observe({
    #Do not remove this line as it serves to update the plots after the model has been re-run.
    check <- input$run.model #Do not Remove this line
    
    output$select.a <- renderUI({
      selectInput("select.chart.a", NULL, choices = names(plots))
    })
    
    output$select.b <- renderUI({
      selectInput("select.chart.b", NULL, choices = names(rev(plots)))
    })
    
    output$plot.a <- renderPlot({
      plots[input$select.chart.a]
    })
    
    output$plot.b <- renderPlot({
      plots[input$select.chart.b]
    })
  })
  
  # 
  # chart: macro --------------------------------------------------------------
  
  observe({
    check <- input$run.model #Do not Remove this line
    
    output$macro.select.a <- renderUI({
      selectInput("macro.select.chart.a", NULL, choices = names(gov.plots))
    })
    
    output$macro.select.b <- renderUI({
      selectInput("macro.select.chart.b", NULL, choices = names(rev(gov.plots)))
    })
    
    output$macro.plot.a <- renderPlot({
      gov.plots[input$macro.select.chart.a]
    })
    
    output$macro.plot.b <- renderPlot({
      gov.plots[input$macro.select.chart.b]
    })
  })
  
  # 
  # chart: micro --------------------------------------------------------------
  observe({
    check <- input$run.model #Do not remove this line
    tax.ids <- population.data$tax.ids
    
    # individual trajectory ---
    
    output$compare.ind <- renderUI({
      selectizeInput(
        "compare.id", 
        "", 
        choices = tax.ids, 
        options = list(
          placeholder = 'Please select an ID below',
          onInitialize = I('function() { this.setValue(""); }'),
          maxOptions = length(tax.ids)
        )
      )
    })
    
    output$selected.ind <- renderPlot({
      if (is.null(input$tl.plot_selected)) return(NULL)
      
      # get the IDs
      id.1 <- input$tl.plot_selected
      user.choice <- input$compare.id
      
      # get the plot
      if (user.choice == ""){
        if(id.1 == "") id.1 <- sample(population.data$tax.ids, 1)
        get.individual.trajectory(track.dyn, id.1)
      } else if (user.choice == "Avg of Nearest Neighbors"){ #Changes in this string must also be done in the UpdateSelectizeInput above
        if(id.1 == "") {
          id.1 <- sample(population.data$tax.ids, 1)
          get.individual.trajectory(track.dyn, id.1)
        } else {  
          id.2 <-  nn.int[[id.1]]
          get.individual.trajectory.pair(track.dyn, id.1, id.2)
        }
      } else {
        id.2 <- as.integer(user.choice)
        get.individual.trajectory.pair(track.dyn, id.1, id.2)
      }
      
    })
    
    
    # odds ratio ---
    
    output$odds.ratio.plot <- renderPlot({
      t <- input$tl.slider
      
      ggplot(data = odds.ratios, aes(x=measure, y=value, fill=measure)) + geom_bar(width=0.3, stat="identity") + 
        geom_text(aes(label=round(value, 2)), vjust=-0.5) + 
        ggtitle(label = paste("Odds Ratio at time =", t)) + 
        theme_light()
    })
    
    # mean tax gap infobox ---
    
    output$meanTaxGap <- renderInfoBox({
      #if(input$run.model != 0) update.dynamics()
      
      t <- input$tl.slider
      tax.gap.at.t <- round(aggregated.dyn[t, 'tax.gap']*100, 2)
      
      infoBox(
        title = tags$b("Mean Tax Gap"),
        value = paste(tax.gap.at.t, "%"),
        color = "aqua",
        fill = TRUE,
        icon = icon("thermometer")
      )})
    
    # mean audit rate infobox ---
    
    output$meanPerAuditRate <- renderInfoBox({
      #if(input$run.model != 0) update.dynamics()
      
      t <- input$tl.slider
      per.audit.rate.at.t <- round(aggregated.dyn[t, 'mean.per.audit.rate']*100, 2)
      
      infoBox(
        title = tags$b("Perceived Audit Rate"),
        value = paste(per.audit.rate.at.t, "%"),
        color = "yellow",
        fill = TRUE,
        icon = icon("thermometer")
      )})
    
    # mean deficit infobox ---
    
    output$meanDeficit <- renderInfoBox({
      #if(input$run.model != 0) update.dynamics()
      
      t <- input$tl.slider
      m.deficit <- round(gov.dyn[t, 'US.marginal.deficit']/1000000000, 2) #Experssing it in terms of billions 
      
      infoBox(
        title = tags$b("Marginal Deficit"),
        value = paste0("$", m.deficit, " Billion"),
        color = "orange",
        fill = TRUE,
        icon = icon("thermometer")
      )})
    
  })
  
  #Network time line ----
  observe({
    check <- input$run.model #Do not remove this line
    
    # network visualization ---
    
    output$tl.plot <- renderVisNetwork({
      
      #if(input$run.model != 0) update.dynamics()
      tmp.dyn <- track.dyn[track.dyn$t == 1, ]
      tmp.dyn$evaded <- (100- tmp.dyn$hideable.reported)/100
      tax.gap.at.t <<- aggregated.dyn[1, 'tax.gap']
      df.vis <- update.plot.data(tmp.dyn, group1 = "audited", group2 = "penalized")
      lnodes <- data.frame(label = c("Compliant", "Non-Compliant", "Audited", "Penalized"),
                           shape = rep("circle", 4), 
                           color = "cornflowerblue", "red", "yellow", "black")
      odds.ratios <<- calculate.odds.ratios(df.vis$nodes, df.vis$edges)
      
      # TODO: add comment
      visNetwork(df.vis$nodes, df.vis$edges, 
                 height = "380px", width="100%", background = get.background.color()
      ) %>%
        visNodes(
          color = list(highlight = "yellow"),
          x = df.vis$nodes$x, 
          y = df.vis$nodes$y, 
          label = NULL, 
          physics = FALSE,
          fixed = TRUE
        ) %>%
        visEdges(hoverWidth = 1) %>%
        visInteraction(navigationButtons = FALSE, hover = TRUE) %>% 
        visOptions(
          highlightNearest = list(enabled = FALSE, hover = FALSE, degree = 1),
          collapse = list(enabled = TRUE, fit = TRUE),
          autoResize = TRUE, 
          clickToUse = TRUE, 
          nodesIdSelection = list(enabled = TRUE)#, selected = paste(sample(population.data$tax.ids, 1))) #Random ID
        ) 
    })
  })
  
  
  # individual network plot
  output$ind.plot <- renderVisNetwork({
    
    tmp.dyn <- track.dyn[track.dyn$t == 1, ]
    tmp.dyn$evaded <- (100- tmp.dyn$hideable.reported)/100
    get.individual.network.plot(tmp.dyn, g.data, g, id.to.focus.on = sample(population.data$tax.ids, 1))
    
  })

  
  #
  # rhandsontable -------------------------------------------------------------
  
  # tax rate schedule ---
  
  # tax rate schedule for all filers
  tax.rates <- reactiveValues(
    Single = tax.schedule %>%
      filter(filing.status == "Single") %>%
      select(min, max, `tax rate`),
    Head.of.Household = tax.schedule %>%
      filter(filing.status == "Head.of.Household") %>%
      select(min, max, `tax rate`),
    Married.Filing.Jointly = tax.schedule %>%
      filter(filing.status == "Married.Filing.Jointly") %>%
      select(min, max, `tax rate`),
    Married.Filing.Separately = tax.schedule %>%
      filter(filing.status == "Married.Filing.Separately") %>%
      select(min, max, `tax rate`)
  )
  
  # handsontable data
  tax.rate.hot <- reactiveValues(data = NULL)

  # show the table
  output$hot.tax.rate.schedule <- renderRHandsontable({
    if (is.null(input$tax.rate.delta)) return(NULL)
    
    # update tax rates
    df <- tax.rate.hot$data %>%
      mutate(delta = `tax rate` + input$tax.rate.delta)
    
    columns <- c("From", "To", "Tax Rate", "+Delta")
    rhandsontable(df, colHeaders = columns, rowHeaders = NULL) %>%
      hot_col(col = 1, format = "$0,0", readOnly = TRUE) %>%
      hot_col(col = 2, format = "$0,0") %>%
      hot_col(col = 3, format = "0.0%") %>%
      hot_col(col = 4, format = "0.00%", readOnly = TRUE)
  })
  
  # output$effective.tax.rate <- shinydashboard::renderValueBox({
  #   shinydashboard::valueBox(disp.eff.tax.rate, "Effective tax rate", icon = icon("list"))
  # })
  
  # deterrence strategy ---

  # store audit rate table in reactive value
  audit.schedule <- reactiveValues(
    data = table.audit.rates %>%
      select(Lower.Income, Upper.Income, Examination.Coverage)
  )
  
  # display the handson table
  output$hot.deterrence.strategy <- renderRHandsontable({
    if (is.null(input$audit.rate.delta)) return(NULL)
    
    # update audit rates
    df <- audit.schedule$data %>%
      mutate(delta = Examination.Coverage + input$audit.rate.delta)
    
    df$delta <- ifelse(df$delta < 0, 0, df$delta)
    
    tmp.df <- df[, 1:2]
    tmp.df$Examination.Coverage <- df$delta
    
    effective.audit.rate <- get.effective.audit.rate(population.data, IRS.table.audit.rates, tmp.df)
    effective.audit.rate <- 100.00 * effective.audit.rate #Because the table was divided by 100, we convert it back
    
    #Updating the config
    config[config$config.vars == 'audit.rate', 'value'] <<- effective.audit.rate
    
    disp.eff.audit.rate <- round(100.00 * effective.audit.rate, 2) #This converts it to percentage
    
    output$effective.audit.rate <- shinydashboard::renderValueBox({
      shinydashboard::valueBox(paste(disp.eff.audit.rate, "%"), "Effective audit rate", icon = icon("list"))
    })
    
    # handson table
    columns <- c("From", "To", "Audit Rate", "+Delta")
    rhandsontable(df, colHeaders = columns, rowHeaders = NULL) %>%
      hot_col(col = 1, format = "$0,0", readOnly = TRUE) %>%
      hot_col(col = 2, format = "$0,0") %>%
      hot_col(col = 3, format = "0.00%") %>%
      hot_col(col = 4, format = "0.00%", readOnly = TRUE)
  })
  
  
  
  #
  # update the ux -------------------------------------------------------------
  
  # policy levers ---
  
  output$policy.levers <- renderUI({
    ux.grp <- ux %>%
      filter(group == "Policy Levers", include)
    
    lapply(ux.grp$`Input Name`, function(ux.ip) {
      params <- GetSliderParams(ux.grp, ux.ip)
      #Adding audit.rate.delta spearately, since all others are in the config
      config <- rbind(config, c(config.vars = "audit.rate.delta", value = 0))
      default.value <- as.numeric(config[config$config.vars == ux.ip, "value"])
      description <- str_replace_all(params$description, "\'", "\\\\\'")
      ux.config.params <<- unique(c(ux.config.params, params$inputId))
      tipify(sliderInput(params$inputId, params$label, params$min, params$max, default.value, step = 0.00001, round = -5, sep = ""), description)
    })
  })
  
  # time scale ---
  
  output$time.scale <- renderUI({
    ux.grp <- ux %>%
      filter(group == "Time Scale", include)
    
    lapply(ux.grp$`Input Name`, function(ux.ip) {
      params <- GetSliderParams(ux.grp, ux.ip)
      default.value <- as.numeric(config[config$config.vars == ux.ip, "value"])
      description <- str_replace_all(params$description, "\'", "\\\\\'")
      ux.config.params <<- unique(c(ux.config.params, params$inputId))
      tipify(sliderInput(params$inputId, params$label, params$min, params$max, default.value, step = 0.00001, round = -5, sep = ""), description)
    })
  })
  
  # weights ---
  
  output$weights <- renderUI({
    ux.grp <- ux %>%
      filter(group == "Weights", include)
    
    lapply(ux.grp$`Input Name`, function(ux.ip) {
      params <- GetSliderParams(ux.grp, ux.ip)
      default.value <- as.numeric(config[config$config.vars == ux.ip, "value"])
      description <- str_replace_all(params$description, "\'", "\\\\\'")
      ux.config.params <<- unique(c(ux.config.params, params$inputId))
      tipify(sliderInput(params$inputId, params$label, params$min, params$max, default.value, step = 0.00001, round = -5, sep = ""), description)
    })
  })
  
  # behavioral influences ---
  
  output$behavioral.influences <- renderUI({
    ux.grp <- ux %>%
      filter(group == "Behavioral Influences", include)
    
    lapply(ux.grp$`Input Name`, function(ux.ip) {
      params <- GetSliderParams(ux.grp, ux.ip)
      default.value <- as.numeric(config[config$config.vars == ux.ip, "value"])
      description <- str_replace_all(params$description, "\'", "\\\\\'")
      ux.config.params <<- unique(c(ux.config.params, params$inputId))
      tipify(sliderInput(params$inputId, params$label, params$min, params$max, default.value, step = 0.00001, round = -5, sep = ""), description)
    })
  })
  
  
  ###########################################
  ##      All Observe Functions here       ##
  ###########################################
  
  #This event will be the heart of this tool. 
  observeEvent(input$run.model, {
    
    # First, update the config
    withProgress(message = "Configuring the Model", value = 0.2, {
      config <<- update.config.params(config, ux.config.params, input)
    })
    
    
    #Then run the model
    withProgress(message = "Running the Model", value = 0.5, {
      if(is.null(initial.state))
      {  
        population.data <- initialize.risk.perceptions(population.data, config)
      } else {
        population.data <- initialize.risk.perceptions(population.data, config, initial.state)
      }
      
      # TODO: you need to change the this function to use the reactive values for audit rate!!
      all.data <- run.dynamics(population.data, initial.state = initial.state, config, g.info, 
                               table.audit.rates, table.refund, baseline.gov.dyn, final.year = NULL, cl=cl)
      
      track.dyn <<- as.data.frame(bind_rows(all.data$track.dyn))
      aggregated.dyn <<- as.data.frame(all.data[['aggregated.dyn']])  
      final.state <- all.data[['final.state']]
      gov.dyn <<- all.data[['gov.dyn']]
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
    })
    
    #Now create the plots
    withProgress(message = "Creating the Plots", value = 0.8, {
      sim.data <- get.sim.data(final.state, population.data,
                               amount.under.reporting.history,
                               perc.reporting.history,
                               perc.hideable.reporting.history,
                               compliant.history,
                               audit.history,
                               penalty.history)
      
      plots <<- create.plots(sim.data, track.dyn, aggregated.dyn, config, g.info, small = 12, big = 15)
      gov.plots <<- gov.dyn.plot(gov.dyn)
    })
    
  })
  
  # What happens on reset
  observeEvent(input$reset.model, {
    withProgress(message = "Resetting the model", {
      population.data <<- default.state$population.data #Population dataset with status quo tax brackets
      g.info <<- default.state$g.info
      initial.state <<- default.state$final.state
      aggregated.dyn <<- default.state$agg.dyn
      config <<- default.state$config
      #Just to make sure it doesn't run till equilibrium, unless requested explicitly through the tool
      config[config$config.vars == 'run.till.equilibrium', 'value'] <<- F
      gov.dyn <<- default.state$gov.dyn
      baseline.gov.dyn <<- default.state$baseline.gov.dyn
      network.data <<- default.state$network.data
      track.dyn <<- default.state$track.dyn
      g.data <- readRDS(file="Network data/PN1.visNetwork.hideableincome.Rdata")
      g.data$nodes$group <- as.character(g.data$nodes$group)
    })
    
    withProgress(message = "Creating the Plots", value = 0.8, {
      sim.data <- default.state$sim.data
      
      plots <<- create.plots(sim.data, track.dyn, aggregated.dyn, config, g.info, small = 12, big = 15)
      gov.plots <<- gov.dyn.plot(gov.dyn)
    })
    
  })
  
  # Whatever that changes as the timeline slider changes
  observe({
    
    #if(input$run.model != 0) update.dynamics()
    t <- input$tl.slider
    tmp.dyn <- track.dyn[track.dyn$t == t, ]
    tax.gap.at.t <<- aggregated.dyn[t, 'tax.gap']
    tmp.dyn$evaded <- (100- tmp.dyn$hideable.reported)/100
    
    df.vis <- update.plot.data(tmp.dyn, group1 = "audited", group2 = "penalized")
    nodes <- df.vis$nodes
    edges <- df.vis$edges
    
    odds.ratios <<- calculate.odds.ratios(nodes, edges)
    
    # Update the visnetwork here
    visNetworkProxy("tl.plot") %>%
      visUpdateNodes(nodes = nodes) #%>% 
      #visUpdateEdges(edges = edges) #This may be needed for windows. Takes up a lot of processing time though
    
  })
  
  observe({
    if (is.null(input$tl.plot_selected)) return(NULL)
    
    t <- input$tl.slider
    tmp.dyn <- track.dyn[track.dyn$t == t, ]
    tax.gap.at.t <<- aggregated.dyn[t, 'tax.gap']
    tmp.dyn$evaded <- (100- tmp.dyn$hideable.reported)/100
    
    df.vis <- update.plot.data(tmp.dyn, group1 = "audited", group2 = "penalized")
    
    id.sel <- input$tl.plot_selected
    
    #Switch Tab to display the selected individual's network
    if(id.sel != "") {
      withProgress(message = "Displaying Individual Network", value = 0.5, {
        
        updateTabsetPanel(session, "netPlots",
                          selected = "Individual Network")
        
      })
    }
  })
  
  
  
  # Updates the "select from" menu options once an ID has been selected on the network plot
  observe({
    if (is.null(input$tl.plot_selected)) return(NULL)
    id.sel <- input$tl.plot_selected
    if(id.sel == "") {
      updateSelectizeInput(session, inputId = "compare.id", #label = "Trajectory of:", 
                           choices = population.data$tax.ids)
    } else {
      updateSelectizeInput(session, inputId = "compare.id", label = paste("Compare Tax ID", id.sel, "With:"), 
                           choices = c("Avg of Nearest Neighbors", nn.int[[id.sel]]), #Change to first value will affect the plot below
                           options = list(placeholder = 'Select a Nearest Neighbor below'))
    }
    
  })
  
  # select which table of the tax schedule to display
  observeEvent(input$sel.tax.rate.schedule, {
    if (input$sel.tax.rate.schedule == "Single") {
      tax.rate.hot$data <- tax.rates$Single
      sel.f.stat <<- "Single"
    }
    if (input$sel.tax.rate.schedule == "Head of Household") {
      tax.rate.hot$data <- tax.rates$Head.of.Household
      sel.f.stat <<- "Head.of.Household"
    }
    if (input$sel.tax.rate.schedule == "Married - Filing Jointly") {
      tax.rate.hot$data <- tax.rates$Married.Filing.Jointly
      sel.f.stat <<- "Married.Filing.Jointly"
    }
    if (input$sel.tax.rate.schedule == "Married - Filing Separately") {
      tax.rate.hot$data <- tax.rates$Married.Filing.Separately
      sel.f.stat <<- "Married.Filing.Separately"
    }
  })
  
  # save changes made to the tax schedule
  observeEvent(input$hot.tax.rate.schedule, {
    if (!is.null(input$hot.tax.rate.schedule))
      df <- hot_to_r(input$hot.tax.rate.schedule)
    
    # update the from-to connections
    df <- df %>%
      mutate(min = lag(max, n = 1, default = 0))
    
    # update the correct tax schedule
    if (input$sel.tax.rate.schedule == "Single") tax.rates$Single <- df
    if (input$sel.tax.rate.schedule == "Head of Household") tax.rates$Head.of.Household <- df    
    if (input$sel.tax.rate.schedule == "Married - Filing Jointly") tax.rates$Married.Filing.Jointly <- df
    if (input$sel.tax.rate.schedule == "Married - Filing Separately") tax.rates$Married.Filing.Separately <- df
    
    # update the handsontable
    tax.rate.hot$data <- df
    
    #Also update the tax.schedule
    tax.schedule[tax.schedule$filing.status == sel.f.stat, c('min', 'max', 'tax rate')] <<- df[, c('min', 'max', 'delta')]
    incInfo <- population.data[, c('income', 'filing.status')]
    rval <- effective.taxes.and.tax.rates(incInfo, tax.schedule)
    
    #Update population's tax rates
    population.data$tax.rate <<- rval$ind.tax.rates
    population.data$tax.rate <<- ifelse(population.data$tax.rate < 0, 0, population.data$tax.rate)
    population.data$tax.rate <<- ifelse(population.data$tax.rate > 1, 1, population.data$tax.rate)
    
    disp.eff.tax.rate <<- round(100*rval$overall.tax.rate, 2)
    output$effective.tax.rate <- shinydashboard::renderValueBox({
      shinydashboard::valueBox(paste(disp.eff.tax.rate, "%"), "Effective Tax Rate", icon = icon("list"))
    })
  })
  
  # save changes made to the audit schedule table
  observeEvent(input$hot.deterrence.strategy, {
    if(is.null(input$audit.rate.delta)) return(NULL)
    
    audit.schedule$data <- hot_to_r(input$hot.deterrence.strategy) %>%
      mutate(Lower.Income = lag(Upper.Income, n = 1, default = 0))
    
  })
  
}) #End of shiny server function
