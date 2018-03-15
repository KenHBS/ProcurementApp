# Shiny ProcurementApp

This app produces relevant plots on UN procurement (see <a href="https://www.ungm.org/Areas/Public/Downloads/ASR/2016/Document/Procurement%20profiles%20of%20Member%20states%202016%20-%20ENGLISH.pdf"> UN Annual Statistical Report 2016 </a>).
The data has been totally randomised and obfuscated. It may therefore be considered a fictional dataset. 

The main purpose of this application was for me to get more familiar with R's Shiny and data.table packages. 

# Usage

The easiest way is to use `shiny::runGitHub` or you can clone this repository and use `runApp`:

```R
# Either:
library(shiny)
runGitHub("ProcurementApp", "KenHBS")


# Or:
# First clone the repository with git. If you have cloned it into
# ~/procurement_app, first go to that directory, then use runApp().
setwd("~/procurement_app")
shiny::runApp()

```

![ProcurementApp by Ken](https://github.com/KenHBS/ProcurementApp/blob/master/screenshot.png)
