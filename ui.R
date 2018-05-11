
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

dashboardPage(
  dashboardHeader(title = "Tax Evasion Model"),
  
  ## Sidebar content
  dashboardSidebar(
    sidebarMenu(
      menuItem("Model Plots", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Policy Levers", tabName = "policylevers", icon = icon("th")),
      menuItem("Behavioral Parameters", tabName = "behavioralparameters", icon = icon("th")),
      menuItem("Macro Trends",  tabName = "macro", icon = icon("bar-chart")),
      menuItem("Micro Trends",  tabName = "micro", icon = icon("bar-chart")),
      actionButton(
        "run.model", 
        "Run", 
        icon = icon("play"), 
        width = "100px",
        style="color: #fff; background-color: #337ab7; border-color: #2e6da4"
      ),
      actionButton(
        "reset.model", 
        "Reset", 
        icon = icon("step-backward"), 
        width = "100px",
        style="color: #fff; background-color: #337ab7; border-color: #2e6da4"
      ),
      br(strong("Authors:"), br(tags$small("Raffaele Vardavas & Pavan Katkar")))
    )
  ),
  
  ## Body content
  dashboardBody(
    tabItems(
      
      # 
      # main dashboard --------------------------------------------------------

      tabItem(
        tabName = "dashboard",
        box(
          uiOutput("select.a"),
          plotOutput("plot.a")
        ),
        box(
          uiOutput("select.b"),
          plotOutput("plot.b")
        )
      ),
      
      #
      # policy levers ---------------------------------------------------------
      
      tabItem(
        tabName = "policylevers", 
        h2("Policy Inputs"),
        box(
          title = "General Levers", 
          width = 4,
          solidHeader = TRUE, 
          uiOutput("policy.levers")
        ),
        box(
          title = "Tax Rate Schedule",
          width = 4,
          solidHeader = TRUE,
          selectInput("sel.tax.rate.schedule", NULL, filing.status, "Single"),
          rHandsontableOutput("hot.tax.rate.schedule"),
          br(),
          shinydashboard::valueBoxOutput("effective.tax.rate", width = 12)
        ),
        box(
          title = "Deterrence Strategy",
          width = 4,
          solidHeader = TRUE,
          rHandsontableOutput("hot.deterrence.strategy"),
          br(),
          shinydashboard::valueBoxOutput("effective.audit.rate", width = 12)
        )
      ),
      
      #
      # behavioral parameters -------------------------------------------------
      
      tabItem(
        tabName = "behavioralparameters",
        h2("Behavioral Parameters"),
        box(
          title = "Time Scale", 
          width = 4,
          solidHeader = TRUE, 
          uiOutput("time.scale")
        ),
        box(
          title = "Weights", 
          width = 4,
          solidHeader = TRUE, 
          uiOutput("weights")
        ),
        box(
          title = "Behavioral Influences", 
          width = 4,
          solidHeader = TRUE, 
          uiOutput("behavioral.influences")
        )
      ),
      
      # 
      # macro dashboard --------------------------------------------------------
      
      tabItem(
        tabName = "macro",
        box(
          uiOutput("macro.select.a"),
          plotOutput("macro.plot.a")
        ),
        box(
          uiOutput("macro.select.b"),
          plotOutput("macro.plot.b")
        )
      ),
      
      # 
      # micro dashboard --------------------------------------------------------
      
      tabItem(
        tabName = "micro",
        fluidRow(
          column(
            width = 12,
            tabBox(id = "netPlots",
                   tabPanel(
                     title = "Full Network", 
                     visNetworkOutput("tl.plot"), 
                     sliderInput("tl.slider", tags$small("Timeline"), min = 1, max = 100, value = 1, step = 1, animate = T, animationOptions(interval = 200)),
                     icon = icon("group")
                   ),
                   tabPanel(
                     title = "Individual Network",
                     #uiOutput("ind.selector"),
                     visNetworkOutput("ind.plot")
                   )
            ),
            tabBox(
              tabPanel(
                title = "Individual Trajectory",
                uiOutput("compare.ind"),
                plotOutput("selected.ind")
              ),
              tabPanel(
                title = "Odds Radio",
                plotOutput("odds.ratio.plot")
              )
            )
          )
        ),
        fluidRow(
          infoBoxOutput("meanTaxGap"),
          infoBoxOutput("meanPerAuditRate"),
          infoBoxOutput("meanDeficit")
        )
      )
    )
  )
)
