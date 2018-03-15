# ui
library(shiny)
library(shinydashboard)

# Some data imports:
memberlist <- read.csv("data/memberlist.csv", stringsAsFactors=FALSE)[[1]]
memberlist <- append(memberlist, "All countries", after = 0)
choices    <- setNames(memberlist, memberlist)

# User Interface:
ui <- dashboardPage(
        dashboardHeader(title = "Procurement requests*"),
        dashboardSidebar(disable = TRUE),
        dashboardBody(
          fluidRow(
            br(),
            "   *: The data used here is heavily obfuscated and randomized and does not resemble real UN data.
            This project is merely a learning project for Shiny Apps", 
            br()
          ),
          fluidRow(
            shinydashboard::box(
              title = "Please select input (1)", width = 4,
              background = "light-blue",
              selectInput("partition", "Select UN or UNOPS:",
                          choices = list("UNOPS" = "unops", "UN" = "un"), 
                          selected = "un", multiple = FALSE)), 
            shinydashboard::box(
              title = "Please select input (2)", width = 4,
              background = "light-blue",
              selectInput("ctryID", "Select a country:",
                          choices = choices, 
                          selected = "Afghanistan", multiple = FALSE)
              )
          ),
          fluidRow(
            shinydashboard::box(
                title = "Quick stats", width = 3,
                background = "light-blue",
                "Total procurement by UN/UNOPS in this country:",
                verbatimTextOutput("totalsproc"),
                "Country's share in total UN/UNOPS procurement:",
                verbatimTextOutput("ctrysharetot")
              ),
            shinydashboard::box(
              title = "Supplier pie chart", width = 5, 
              status = "primary", solidHeader = TRUE,
              imageOutput("section2", height=350),
              downloadButton("dl_suppie", "Download", class = "butt")
            )
          ),
          fluidRow(
            shinydashboard::box(
              title = "Total procurement - plot", width = 5,
              status = "primary", solidHeader = TRUE,
              imageOutput("section1", height=300),
              downloadButton("dl_procbar", "Download", class = "butt"),
              tags$head(
                tags$style(".butt{background-color:#518ecb;} 
                           .butt{color: white;} .butt{font-family:Verdana;}"))
            ),
            shinydashboard::box(
              title = "Total procurement - raw", width = 3,
              status = "primary", solidHeader = TRUE,
              br(),
              tableOutput('totproc_table'),
              br(), br(),
              downloadButton("dl_totproc_raw", "Download", class = "butt")
            )
          ),
          fluidRow(
            shinydashboard::box(
              title = "Category distributions", width = 8,
              status = "primary", solidHeader = TRUE,
              imageOutput("gs_breakdown", height=400),
              br(),
              downloadButton("dl_gs_breakdown", "Download", class = "butt")
            )
          ),
          fluidRow(
            shinydashboard::box(
              title = "Procurement by agency - plot", width = 5,
              status = "primary", solidHeader = TRUE,
              imageOutput("sharebar", height = 300),
              br(),
              downloadButton("dl_sharebar", "Download", class = "butt"),
              strong("Only available without agency filter"),
              br()
            ),
            shinydashboard::box(
              title = "Procurement by agency - raw", width = 3,
              status = "primary", solidHeader = TRUE,
              br(),
              tableOutput("sharetable"),
              br(),
              downloadButton("dl_sharetab", "Download", class = "butt")
            )
          )
          
          
        )
  
    )
# End of UI
