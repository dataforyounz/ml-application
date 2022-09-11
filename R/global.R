library(shiny)
library(shinythemes)
library(ggplot2)
library(stringr)

vars <- setdiff( names(iris), "Species" )
var_names <- sapply(1:length(vars), function(x) str_flatten( str_split(vars[[x]], pattern = "[.]")[[1]], collapse = " ") )


var_lookup <- as.list( vars )
names( var_lookup ) <- var_names


