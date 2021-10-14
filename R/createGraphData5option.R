#  5option Stack --------------------------------------------------------------------
#' Function to create data for edad stacked for graphs, final report as
#'
#' This function creates graphData to create stacked graph.
#' It assumes a 1-5 scale, values greater than 5 are missing.
#' It also assumes the dataStructure of the asFinalData
#' @param df default: intData
#' @param originVar Variable to be graphed. Default intData
#' @param groupVar Variable to be grouped by. Defaul Edad
#' @param weightVar Weight variable. Default ponde
#' @param totalColumn Should the total column be included in the data. Default: TRUE
#'
#' @author Gabriel N. Camargo-Toledo \email{gcamargo@@sensata.io}
#' @return Dataframe for graphs
#' @keywords sensata microdata metadata data-cleaning
#' @import tidyverse
#'
#' @examples
#' TBD
#' @export

createGraphData5option <- function(df = intData,
                                   originVar,
                                   groupVar = "Edad",
                                   totalColumn = T){
  require(tidyverse)
  graphData <- df %>% select(originVar, all_of(groupVar), ponde)
  graphData$y <- graphData[[originVar]]
  graphData <- na.omit(graphData)
  totalData <- graphData %>%
    summarise(val1 = prop.table(questionr::wtd.table(y, weights = ponde))[1]*100,
              val2 = prop.table(questionr::wtd.table(y, weights = ponde))[2]*100,
              val3 = prop.table(questionr::wtd.table(y, weights = ponde))[3]*100,
              val4 = prop.table(questionr::wtd.table(y, weights = ponde))[4]*100,
              val5 = prop.table(questionr::wtd.table(y, weights = ponde))[5]*100)
  totalData[[groupVar]] <- "Total"
  totalData <- totalData %>%  pivot_longer(cols = starts_with("val"),
                                           names_to = "Value",
                                           names_prefix = "val",
                                           values_to = "Porcentaje")
  totalData$Value <- totalData$Value %>% factor(levels = c(1:5),
                                                labels = names(table(df[[originVar]])),
                                                ordered = T)

  graphData <- graphData %>%
    group_by(across(all_of(groupVar)))  %>%
    summarise(val1 = prop.table(questionr::wtd.table(y, weights = ponde))[1]*100,
              val2 = prop.table(questionr::wtd.table(y, weights = ponde))[2]*100,
              val3 = prop.table(questionr::wtd.table(y, weights = ponde))[3]*100,
              val4 = prop.table(questionr::wtd.table(y, weights = ponde))[4]*100,
              val5 = prop.table(questionr::wtd.table(y, weights = ponde))[5]*100)%>%
    pivot_longer(cols = starts_with("val"),
                 names_to = "Value",
                 names_prefix = "val",
                 values_to = "Porcentaje")
  graphData$Value <- graphData$Value %>% factor(levels = c(1:5),
                                                labels = names(table(df[[originVar]])),
                                                ordered = T)
  if (totalColumn){
    graphData <- bind_rows(graphData, totalData)
  }
  graphData
}
