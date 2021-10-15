#  createGraphData --------------------------------------------------------------------
#' Function to create data for graphs
#'
#' This function creates weighted graphData to create most common graphs.
#' x axis is normallly the groupVar and y axis is the originVar.
#' if you don't want data to be weighted, use weightVar = NULL
#'
#' @param df default: intData
#' @param originVar Variable to be graphed. Default intData
#' @param groupVar Variable to be grouped by. Defaul Edad
#' @param weightVar Weight variable. Default ponde
#' @param totalColumn Should the total column be included in the data. Default: TRUE
#'
#' @author Gabriel N. Camargo-Toledo \email{gcamargo@@sensata.io}
#' @return Dataframe for graphs
#' @keywords sensata microdata metadata data-cleaning graphs
#' @import tidyverse
#'
#' @examples
#' TBD
#' @export

createGraphData5option <- function(df = intData,
                                   originVar,
                                   groupVar = "Edad",
                                   weigthVar = "ponde",
                                   totalColumn = T){
  require(tidyverse)
  graphData <- df %>% select(originVar, all_of(groupVar), all_of(weigthVar))
  graphData$y <- graphData[[originVar]]
  graphData <- na.omit(graphData)
  totalData <- graphData %>%
    summarise(val = list(prop.table(questionr::wtd.table(y, weights = ponde))*100)) %>%
    unnest_wider(val)
  totalData[[groupVar]] <- "Total"
  #TODO: FIX THIS PART FOR MORE OPTIONS
  totalData <- totalData %>%  pivot_longer(cols = starts_with("val"),
                                           names_to = "Value",
                                           names_prefix = "val",
                                           values_to = "Porcentaje")
  totalData$Value <- totalData$Value %>% factor(levels = c(1:5),
                                                labels = names(table(df[[originVar]])),
                                                ordered = T)

  graphData <- graphData %>%
    group_by(across(all_of(groupVar)))  %>%
    summarise(graphData %>%
                summarise(val = list(prop.table(questionr::wtd.table(y, weights = ponde))*100)) %>%
                unnest_wider(val))%>%
    #TODO: CHECK IF THIS IS NECESSARY:
    pivot_longer(cols = starts_with("val"),
                 names_to = "Value",
                 names_prefix = "val",
                 values_to = "Porcentaje")
    #TODO: FIX THIS PART FOR LENGTH OF OPTIONS
  graphData$Value <- graphData$Value %>% factor(levels = c(1:5),
                                                labels = names(table(df[[originVar]])),
                                                ordered = T)
  if (totalColumn){
    graphData <- bind_rows(graphData, totalData)
  }
  graphData
}
