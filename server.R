### TABLE OF CONTENTS: ############################
## - Import data & Load packages                  #
## - Server Function:                             #
##      - Reactiveness & Dataset selection        #
##      - Quick Statistics Box                    #
##      - Supplier Pie Chart                      #
##      - Total Procurement                       #
##      - Procurement Categories Pie Charts       #
##      - Procurement per Agency                  #
###################################################


##########################
### Import data & Load packages
##########################

library(shiny)
library(shinydashboard)
library(ggplot2)
library(data.table)
library(gridExtra)

source("Kens_Helpers.R")

rawdata      <- fread("data/dummy_procurement_data.csv")
u_map        <- fread("data/unspsc_map.csv")
agency_data  <- fread("data/agency_data.csv")
country_data <- fread("data/country_data.csv")
memberlist   <- read.csv("data/memberlist.csv", stringsAsFactors=FALSE)[[1]]
unops_data   <- fread("data/unops_data.csv")

rawdata <- merge(rawdata, u_map, by.x="segm_code", by.y="u_code", all.x=TRUE)

agency_list <- unique(rawdata$agency)
unopsraw    <- rawdata[rawdata$agency == "UNOPS", ]


##########################
### Server function
##########################

function(input, output){
  
#### Reactiveness & dataset selection:
    un_unops <- reactive({input$partition})
    ctryID   <- reactive({input$ctryID})
    datapart <- reactive({un_unops()})
  
    # UNOPS or full UN dataset
    dataset <- reactive({
      first_filter(rawdata, un_unops())
    })
    
    # Which country
    data1 <- reactive({
      if(ctryID() != "All countries") {
        subset(dataset(), vendor_country == ctryID())
      } else dataset()
    })
  

#### Quick Statistics Box:
  ctry_sum <- reactive({sum(data1()$amount)})
  totalsum <- reactive({sum(dataset()$amount)})
  
  output$ctrysharetot <- renderText({
    pct <- ctry_sum() / totalsum() * 100
    paste0(comma(round(pct, 2)), " %")
  })
  
  output$totalsproc <- renderText({
    paste0(comma(round(ctry_sum(), 2)), " USD")
  })
  
#### Supplier Pie Chart:
  suppie <- reactive({
    dt <- data_plot2(data1())
    section2_plot(dt)
  })
  
  # Plot in UI
  output$section2 <- renderPlot(suppie())
  
  # Connecting downloadHandler to a downloadbutton (supplier pie)
  output$dl_suppie <- downloadHandler(
    filename = function() {
      paste0(ctryID(), "_", datapart(), "_supplier_pie.png")
    },
    content = function(file) {
      png(file, pointsize = 12, res = 300, width = 2000, 
          height = 700, bg = "transparent")
      par(family = "ArialMT")          
      print(suppie())
      dev.off()
    },
    contentType = "image/png"
  )
  
  
#### Total Procurement bar chart:
  # Organize the data
  totproctab <- reactive({
    if (datapart() == "unops") {
      i <- which(ctryID() == unops_data[, 1])
      if (ctryID() == "All countries") {
        i <- "all"
      }
      dt <- data_plot1(unops_data, it = i)
    }
    if (datapart() == "un") {
      i  <- which(ctryID() == country_data[, 1])
      if (ctryID() == "All countries") {
        i <- "all"
      }
      dt <- data_plot1(country_data, it = i)
    }
    return(dt)
  })
  
  # Plot in UI
  output$section1 <- renderPlot(
    print(section1_plot(totproctab()))
  )
  
  # Connecting downloadHandler to downloadbutton (Total Procurement)
  output$dl_procbar <- downloadHandler(
    filename = function() {
      paste0(ctryID(), "_", datapart(), "_total_procurement_bars.png")
    },
    content = function(file) {
      png(file, pointsize = 12, res = 300, width = 1000, 
          height = 700, bg = "transparent")
      par(family = "ArialMT")          
      print(section1_plot(totproctab()))
      dev.off()
    },
    contentType = "image/png"
  )
  
  # Total Procurement data table - UI preview
  output$totproc_table <- renderTable(head(totproctab()[, 1:3]))
  
  # Connect downloadHandler to downloadbutton 
  output$dl_totproc_raw <- downloadHandler(
    filename = function() {
      paste0(ctryID(), "_", datapart(), "_total_procurement_table.csv")
    },
    content = function(file) {
      write.csv(totproctab()[, 1:3], file, row.names = FALSE)
    }
  )
  

#### Procurement Categories Pie Charts
   output$gs_breakdown <- renderPlot(
     run_section3_a(data1()), width = 700, height = 450)
                 
  # Connecting downloadHandler to a downloadbutton (catgoods)
  output$dl_gs_breakdown <- downloadHandler(
    filename = function() {
      paste0(ctryID(), "_", datapart(), "_gs_breakdown.png")
    },
    content = function(file) {
      png(file, pointsize = 12, res = 300, width = 3500, 
          height = 1500, bg = "transparent")
      par(family = "ArialMT")          
      run_section3_a(data1())
      dev.off()
    },
    contentType = "image/png"
  )

  
#### Agency Share Breakdown:
  # Get data
  sect_4_data <- reactive({data_plot4(data1())})
  sect_4_plot <- reactive({section4_plot(sect_4_data(), part=un_unops())})
  
  # Connecting downloadHandler to a downloadbutton (share bar, plot)
  output$dl_sharebar <- downloadHandler(
    filename = function() {
      paste0(ctryID(), "_", datapart(), "_share_bar.png")
    },
    content = function(file) {
      png(file, pointsize = 12, res = 300, width = 1500, 
          height = 1000, bg = "transparent")
      par(family = "ArialMT")          
      print(sect_4_plot())
      dev.off()
    },
    contentType = "image/png"
  )
  
  # UI preview of plot
  output$sharebar <- renderPlot(
    print(sect_4_plot()), height = 300, width = 400)

  # Connecting downloadHandler to a downloadbutton (share bar, table)
  output$sharetable <- renderTable(head(raw_section4(sect_4_data())))
  
  # UI preview of raw table
  output$dl_sharetab <- downloadHandler(
    filename = function() {
      paste0(ctryID(), "_", datapart(), "_share_table.csv")
    },
    content = function(file) {
      write.csv(raw_section4(sect_4_data()), file, row.names = FALSE)
    },
    contentType = "text/csv"
  )

}  # End server function

