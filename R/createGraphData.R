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
#' @param weightVar Weight variable. Default ponde,
#' @param totalColumn Should the total column be included in the data. Default: TRUE
#'
#' @author Gabriel N. Camargo-Toledo \email{gcamargo@@sensata.io}
#' @return Dataframe for graphs
#' @keywords sensata microdata metadata data-cleaning graphs
#' @import sensataDataProg
#'
#' @examples
#' TBD
#' @export

createGraphData <- function(df = intData,
                            originVar,
                            groupVar = "Edad",
                            weigthVar = "ponde",
                            totalColumn = T){

  graphData <- df %>% select(all_of(originVar), all_of(groupVar), all_of(weigthVar))
  graphData$y <- graphData[[originVar]]
  if(is.null(weigthVar)){
    graphData$ponde <- 1
  } else {
    graphData$ponde <- graphData[[weigthVar]]
  }
  if(is.factor(graphData[[originVar]])){
    vals <- levels(graphData[[originVar]])
  } else {
    vals <- c(1:max(as.integer(factor(graphData[[originVar]]))))
  }

  graphData <- na.omit(graphData)

  valsData <- graphData %>%
    group_by(across(all_of(groupVar)))  %>%
    summarise(Porcentaje = prop.table(questionr::wtd.table(y, weights = ponde),)*100,
              Value = vals)

  valsData$Porcentaje <- as.double(valsData$Porcentaje)

  valsData$Value <- valsData$Value %>% factor(levels = vals, ordered = T)

  if (totalColumn){
    totalData <- graphData %>%
      summarise(Porcentaje = prop.table(questionr::wtd.table(y, weights = ponde),)*100,
                Value = vals)
    totalData[[groupVar]] <- "Total"

    totalData$Porcentaje <- as.double(totalData$Porcentaje)
    totalData$Value <- totalData$Value %>% factor(levels = vals, ordered = T)
    outData <- bind_rows(valsData, totalData)
  } else {
    outData <- valsData
  }
  outData
}
