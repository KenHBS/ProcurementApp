# This script defines the functions used in server.R
# The app is roughly divided into four sections. Each sections deals
# with its own perspective of the data, so I have separated them here, too

### TABLE OF CONTENTS: ############################
## - Lay-out definitions for ggplot               #
## - Other functions to select data               #
## - Section 1: Total Procurement                 #
## - Section 2: Supplier Pie Chart                #
## - Section 3: Procurement Categories Pie Charts #        
## - Section 4: Procurement per Agency            #
###################################################


########################
### Lay-out definitions for ggplot
########################

bars_theme <- theme_get() + 
  theme(
    axis.ticks = element_blank(),
    axis.line = element_blank(),
    axis.title = element_text(face = "bold", size = 8),
    axis.text = element_text(size = 8),
    panel.grid = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    plot.background = element_blank()
  )

pie_theme <- bars_theme + 
  theme(
    legend.key = element_rect(fill=NULL, colour = "white"),
    legend.key.size = unit(3, "mm"),
    legend.title = element_blank(),
    legend.text = element_text(size = 7),
    legend.background = element_rect(fill = "transparent"),
    axis.text = element_blank(),
    plot.margin = unit(c(0,0,0,0), "cm")
  )

piechart <- function(dt, colorscheme) {
  gg <- ggplot(dt, aes(x = "", y = a, fill = group)) +   
    colorscheme +
    geom_bar(width = 1, stat = "identity", col = "white") + 
    xlab("") + ylab("") + 
    geom_text(aes(x = 1.70, y = pos, label = lab), size = 2.5) + 
    coord_polar(theta = "y") + pie_theme
  return(gg)
} 

blue1 <- "#0a3250"
blue2 <- "#13517b"
blue3 <- "#2f659f"
blue4 <- "#518ecb"
blue5 <- "#aac8df"
blue6 <- "#d6e7f2"

###############################
### Other functions to select data:
###############################

first_filter <- function(x, part) {
  if (part == "unops") {
    return(subset(x, x$agency == "UNOPS"))
  } else return(x)
}


comma <- function (x, ...) {
  format(x, ..., big.mark = ",", scientific = FALSE, trim = TRUE)
}


empty_plot <- function(x = 0.5, y = 0.5, 
                       lab = "No data available for your selection") {
  df <- data.frame()
  ggplot(df) + geom_text(aes(x = x, y = y, label = lab)) + 
    bars_theme + theme(
      axis.text  = element_blank(),
      axis.title = element_blank()
    )
}


###############################
## - Section 1 - Total Procurement
###############################
  
data_plot1 <- function(raw, it) {  
  # Extract years in dataset (raw). 
  # Remove Country|Total columns. 
  # Only take row 'it' (= country) 
  years   <- unique(gsub("[^0-9]", "", colnames(raw)[-1]))
  badcols <- grep("(Country|Total)", colnames(raw))
  dt <- copy(raw)
  dt <- dt[, (badcols) := NULL]
  
  dt <- dt[it, ] / 1000
  dt <- reshape(dt, varying = lapply(c("Goods", "Service"), grep, colnames(dt)), 
                direction = "long", times = years)[, c(1,2,3)]
  colnames(dt) <- c("year", "goods", "services")
  
  dt <- melt(dt, id.vars = "year")
  return(dt)
}
  
section1_plot <- function(dt){
  # Calculate position of labels
  dt[, pos1 := cumsum(value), by = year]
  dt[, pos1 := pos1 - 0.5 * value]
  dt[, pos2 := sum(value), by = year]
  
  maxval <- max(dt[, pos2])
  thres  <- 0.05 * maxval
  
  # Get the label values
  dt[, lab1 := ifelse(value < thres, "", comma(round(value, 1)))]
  dt[, lab2 := comma(round(sum(value), 1)), by = year]
  
  dt[, variable := relevel(variable, 'services')]
  if (sum(dt$pos2) > 0) {
    plots <- ggplot(data = dt, aes(x=year, y=value, fill=variable)) +
      geom_bar(stat = "identity", width = 0.85) +
      scale_fill_manual(values = c(blue5, blue4)) +
      xlab("(in million USD)") + ylab("") +
      scale_y_continuous(expand = c(0, 0), limits = c(0, 1.1*maxval)) +
      geom_text(aes(label = lab1, y = pos1), size = 2.36, color = "white") +
      geom_text(aes(label = lab2, y = pos2), size = 2.36, color = "black", vjust = -0.75) +
      bars_theme + 
       theme(
        axis.text.y = element_blank(), 
        legend.position = "none"
       )
  return(plots)
  }
  else {
    return(empty_plot())
  }
}

  
###############################
## - Section 2: Supplier Pie Chart
###############################
  
data_plot2 <- function(c_sub) {
  # Get amount spent in country 'i' with large and small vendors:
  aggr  <- c_sub[, .(a = sum(amount)), by = vendor] 
  res   <- aggr[, .(a = sum(a), cnt = .N), by = (a < 1000000)]
  names(res) <- c("small", "a", "cnt")
  
  # Make sure always two lines are produced, also when no small/big vendors
  y   <- data.frame(small = c(TRUE, FALSE))
  res <- merge(res, y, all = TRUE)
  res[is.na(res)] <- 0
  return(res)
}
  

section2_plot <- function(dt) {
  # Get labels and their positions
  dt[, lab := a / sum(a)]
  dt[, lab := ifelse(lab < 0.03, "", paste0(comma(round(lab * 100, 1)), "%"))]
  dt[, pos := cumsum(a) - 0.5 * a]
  dt[, group := factor(-small, labels = c("Suppliers < 1,000,000 USD", 
                                          "Suppliers > 1,000,000 USD"))]
  # Create plot
  colorscheme <- scale_fill_manual(values = c(blue4, blue2))
  if (sum(dt$pos) > 0) {
    return(piechart(dt, colorscheme))
  } else {
    return(empty_plot())
  }
}


###############################
## - Section 3: Procurement Categories Pie Charts
###############################

data_plot3 <- function(country, section) {
  uclass  <- ifelse(section == "G", "Goods", "Service")
  aggr    <- country[u_class == uclass, .(a = sum(amount)), by = descr]
  
  aggr <- aggr[order(-a)] 
  aggr[ , spot := ifelse(.I < 6, .I, 6)]
  aggr[ , a := sum(a), by = spot]
  if(nrow(aggr) > 5){
    aggr[6, descr := "Others"]
    return(aggr[1:6, .(descr, a)])
  }
  return(aggr)
} 


section3_plot <- function(dt, plotname) {
  # Get labels and their positions right
  dt[, lab := a/sum(a)]
  dt[, lab := ifelse(lab < 0.03, "", paste0(comma(round(lab*100, 1)), "%"))]
  dt[, pos := cumsum(a) - 0.5 * a]
  dt[, group := factor(-.I, labels = rev(dt$descr))]
  
  # Create the plot
  scheme_vals <- c(blue6, blue5, blue4, blue3, blue2, blue1)[1:nrow(dt)]
  scheme_name <- ".                                        ."
  colorscheme <- scale_fill_manual(values = scheme_vals, name = scheme_name)
  
  if (nrow(dt) > 0) {
    gg <- piechart(dt, colorscheme)
    gg <- gg + 
      guides(fill = guide_legend(title = plotname, reverse = TRUE)) + 
      theme(legend.title = element_text(size = 13, colour = "#0099ff"))
    return(gg)
  } else return(empty_plot())
}
  

run_section3_a <- function(country) {
  # Gather, organize, plot and arrange both plots
  dt_a   <- data_plot3(country, "G")
  dt_b   <- data_plot3(country, "S")
  
  g_plot <- section3_plot(dt_a, "Goods")
  s_plot <- section3_plot(dt_b, "Services")
  
  return(grid.arrange(g_plot, s_plot, ncol=1))
}


###############################
## - Section 4: Procurement per Agency
###############################
  
data_plot4 <- function(country) {
  dt <- country[, sum(amount), by = agency]
  dt <- dt[order(-V1)]
  
  dt[, spot := ifelse(.I < 20, .I, 20)]
  dt[, a := sum(V1) / 1000, by = spot]
  if (nrow(dt) > 20) {
    dt[20, "agency"] <- "Rest"
    dt <- dt[1:20, ]
  }
  dt[, labs := ifelse(a < 0.01, "< 0.01", comma(round(a, 2)))]
  return(dt)
}
  
raw_section4 <- function(dt) {
  dt2 <- dt[, 1:2]
  names(dt2) <- c("Agency", "Spend")
  return(dt2)
}

section4_plot <- function(dt, part = "un") {
  if (nrow(dt) > 0 & part != "unops") {
    ymax <- max(dt$a) * 1.1
    
    plots <- ggplot(dt, aes(x = agency, y = a)) +
      geom_bar(stat="identity", fill = blue4, width = 0.55) + 
      coord_flip() +
      geom_text(aes(label = labs), hjust = -0.25, size = 2.9) +
      labs(y = "(in thousand USD)", x = "") +
      expand_limits(y = c(0, ymax)) +
      scale_x_discrete(limits = rev(dt$agency)) +
      bars_theme + theme(
        axis.text.y = element_text(size = 8),
        axis.text.x = element_blank(),
        axis.title.x = element_text(face = "bold", size = 8)
      )
  return(plots)
  } else if (part == "unops") {
    return(empty_plot(lab="Only UNOPS is selected. \n No sensible agency distribution availble"))
  } else return(empty_plot())
}
