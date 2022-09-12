library(shiny)
library(shinythemes)
library(ggplot2)
library(stringr)
require( tidyverse )
library( broom )

data <- iris %>% select( where(is.numeric) )
var_names <- colnames( data )
