if(!require("shiny")){
  install.packages("shiny")
}
if(!require("shinydashboard")){
  install.packages("shinydashboard")
}

if(!require("plyr")){
  install.packages("plyr")
}
if(!require("plotly")){
  install.packages("plotly")
}

if(!require("ggplot2")){
  install.packages("ggplot2")
}

if(!require("rmarkdown")){
  install.packages("rmarkdown")
}

if(!require("data.table")){
  install.packages("data.table")
}
if(!require("shinyalert")){
  install.packages("shinyalert")
}

if(!require("dplyr")){
  install.packages("dplyr")
}

if(!require("stringr")){
  install.packages("stringr")
}

if(!require("DT")){
  install.packages("DT")
}

if(!require("tidyverse")){
  install.packages("tidyverse")
}

if(!require("ggthemes")){
  install.packages("ggthemes")
}
if(!require("leaflet")){
  install.packages("leaflet")
}

if(!require("gridExtra")){
  install.packages("gridExtra")
}

if(!require("reshape2")){
  install.packages("reshape2")
}
if(!require("RMySQL")){
  install.packages("RMySQL")
}

if(!require("ggthemes")){
  install.packages("ggthemes")
}


if(!require("crosstalk")){
  install.packages("crosstalk")
}



if(!require("readr")){install.packages("readr")
}

if(!require("lubridate")){install.packages("lubridate")
}

if(!require("gridExtra")){
  install.packages("gridExtra")
}

if(!require("magrittr")){install.packages("magrittr")
}



if(!require("rAmCharts")){install.packages("rAmCharts")
}


##Pour message de confirmation de lecture

if(!require("shinyWidgets")){ install.packages("shinyWidgets")
  
}

if(!require("shinycssloaders")){ install.packages("shinycssloaders")
  
}


if(!require("formattable")){ install.packages("formattable")
  
}


if(!require("devtools")){
  install.packages("devtools")
}


if(!require("shinyjs")){
  install.packages("shinyjs")
}


if(!require("flexdashboard")){
  install.packages("flexdashboard")
}

if(!require("V8")){
  install.packages("V8")
}



library(readr)
library(lubridate)
library(magrittr)
library(rAmCharts)
library(data.table)
library(shinyWidgets)
library(shinycssloaders)
library(crosstalk)
library(shinyjqui)
library(gridExtra)
library(stringr)
library(flexdashboard)
library(devtools)
library(shinyjs)
library(V8)
library(shiny)
library(shinydashboard)
library(plyr)
library(plotly)
library(ggplot2)
library(data.table)
library(shinyalert)
library(dplyr)
library(stringr)
library(DT)
library(tidyverse) # includes ggplot2
library(ggthemes) # includes pre-made themes we'll use near the end
library(leaflet)
library(rmarkdown)
library(gridExtra)
library(reshape2)
library(RMySQL)



###Chargement des données 
load('FAO.RData')

