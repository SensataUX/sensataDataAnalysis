# createDf4GraphsAlimentacion.R
# Description: Set of functions to prepare AS microdata to create a graph
# Created by: Juan Pablo Castro
# Created on: Aug/31/2021
# Modified by: Gabriel N. Camargo-Toledo
# Modified on: Sep/06/2021
# Contact: gcamargo@sensata.io
# Sensata Asus VivoBook PopOs! 21.04 8gb Ram R4.1.2

#  8option Stack Gender --------------------------------------------------------------------
#' Function to create data for gender stacked for graphs, final report as
#'
#' This function creates graphData to create stacked graph.
#' It assumes a 1-8 scale, values greater than 8 are missing.
#' It also assumes the dataStructure of asData
#' @param df default: asData
#' @param originVar Variable to be graphed
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

createGraphData8optionGen <- function(df = asData,
                                      originVar,
                                      totalColumn = T){
  require(tidyverse)
  graphData <- df %>% select(originVar, q_EA_CA_10, ponde)
  graphData$y <- graphData[[originVar]]
  graphData <- na.omit(graphData)
  totalData <- graphData %>%
    summarise(val1 = prop.table(questionr::wtd.table(y, weights = ponde))[1]*100,
              val2 = prop.table(questionr::wtd.table(y, weights = ponde))[2]*100,
              val3 = prop.table(questionr::wtd.table(y, weights = ponde))[3]*100,
              val4 = prop.table(questionr::wtd.table(y, weights = ponde))[4]*100,
              val5 = prop.table(questionr::wtd.table(y, weights = ponde))[5]*100,
              val6 = prop.table(questionr::wtd.table(y, weights = ponde))[6]*100,
              val7 = prop.table(questionr::wtd.table(y, weights = ponde))[7]*100,
              val8 = prop.table(questionr::wtd.table(y, weights = ponde))[8]*100)
  totalData[["q_EA_CA_10"]] <- "Total"
  totalData <- totalData %>%  pivot_longer(cols = starts_with("val"),
                                           names_to = "Value",
                                           names_prefix = "val",
                                           values_to = "Porcentaje")
  totalData$Value <- totalData$Value %>% factor(levels = c(1:8),
                                                labels = names(table(df[[originVar]])),
                                                ordered = T)
  graphData <- graphData %>%
    group_by(q_EA_CA_10) %>%
    summarise(val1 = prop.table(questionr::wtd.table(y, weights = ponde))[1]*100,
              val2 = prop.table(questionr::wtd.table(y, weights = ponde))[2]*100,
              val3 = prop.table(questionr::wtd.table(y, weights = ponde))[3]*100,
              val4 = prop.table(questionr::wtd.table(y, weights = ponde))[4]*100,
              val5 = prop.table(questionr::wtd.table(y, weights = ponde))[5]*100,
              val6 = prop.table(questionr::wtd.table(y, weights = ponde))[6]*100,
              val7 = prop.table(questionr::wtd.table(y, weights = ponde))[7]*100,
              val8 = prop.table(questionr::wtd.table(y, weights = ponde))[8]*100)%>%
    pivot_longer(cols = starts_with("val"),
                 names_to = "Value",
                 names_prefix = "val",
                 values_to = "Porcentaje")
  graphData$Value <- graphData$Value %>% factor(levels = c(1:8),
                                                labels = names(table(df[[originVar]])),
                                                ordered = T)
  if (totalColumn){
    graphData <- bind_rows(graphData, totalData)
  }
  graphData
}

#  8option Stack Edad --------------------------------------------------------------------
#' Function to create data for edad stacked for graphs, final report as
#'
#' This function creates graphData to create stacked graph.
#' It assumes a 1-8 scale, values greater than 8 are missing.
#' It also assumes the dataStructure of the asFinalData
#' @param df default: asData
#' @param originVar Variable to be graphed
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

createGraphData8optionEdad <- function(df = asData,
                                       originVar,
                                       totalColumn = T){
  require(tidyverse)
  graphData <- df %>% select(originVar, Edad, ponde)
  graphData$y <- graphData[[originVar]]
  graphData <- na.omit(graphData)
  totalData <- graphData %>%
    summarise(val1 = prop.table(questionr::wtd.table(y, weights = ponde))[1]*100,
              val2 = prop.table(questionr::wtd.table(y, weights = ponde))[2]*100,
              val3 = prop.table(questionr::wtd.table(y, weights = ponde))[3]*100,
              val4 = prop.table(questionr::wtd.table(y, weights = ponde))[4]*100,
              val5 = prop.table(questionr::wtd.table(y, weights = ponde))[5]*100,
              val6 = prop.table(questionr::wtd.table(y, weights = ponde))[6]*100,
              val7 = prop.table(questionr::wtd.table(y, weights = ponde))[7]*100,
              val8 = prop.table(questionr::wtd.table(y, weights = ponde))[8]*100)
  totalData[["Edad"]] <- "Total"
  totalData <- totalData %>%  pivot_longer(cols = starts_with("val"),
                                           names_to = "Value",
                                           names_prefix = "val",
                                           values_to = "Porcentaje")
  totalData$Value <- totalData$Value %>% factor(levels = c(1:8),
                                                labels = names(table(df[[originVar]])),
                                                ordered = T)

  graphData <- graphData %>%
    group_by(Edad) %>%
    summarise(val1 = prop.table(questionr::wtd.table(y, weights = ponde))[1]*100,
              val2 = prop.table(questionr::wtd.table(y, weights = ponde))[2]*100,
              val3 = prop.table(questionr::wtd.table(y, weights = ponde))[3]*100,
              val4 = prop.table(questionr::wtd.table(y, weights = ponde))[4]*100,
              val5 = prop.table(questionr::wtd.table(y, weights = ponde))[5]*100,
              val6 = prop.table(questionr::wtd.table(y, weights = ponde))[6]*100,
              val7 = prop.table(questionr::wtd.table(y, weights = ponde))[7]*100,
              val7 = prop.table(questionr::wtd.table(y, weights = ponde))[8]*100)%>%
    pivot_longer(cols = starts_with("val"),
                 names_to = "Value",
                 names_prefix = "val",
                 values_to = "Porcentaje")
  graphData$Value <- graphData$Value %>% factor(levels = c(1:8),
                                                labels = names(table(df[[originVar]])),
                                                ordered = T)
  if (totalColumn){
    graphData <- bind_rows(graphData, totalData)
  }
  graphData
}
#  8option Stack Educación Madre --------------------------------------------------------------------
#' Function to create data for educación madre stacked for graphs, final report as
#'
#' This function creates graphData to create stacked graph.
#' It assumes a 1-8 scale, values greater than 8 are missing.
#' It also assumes the dataStructure of the asData
#' @param df default: asData
#' @param originVar Variable to be graphed
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

createGraphData8optionEducM <- function(df = asData,
                                        originVar,
                                        totalColumn = T){
  require(tidyverse)
  graphData <- df %>% select(originVar, q_EA_CA_13, ponde)
  graphData$y <- graphData[[originVar]]
  graphData <- na.omit(graphData)
  totalData <- graphData %>%
    summarise(val1 = prop.table(questionr::wtd.table(y, weights = ponde))[1]*100,
              val2 = prop.table(questionr::wtd.table(y, weights = ponde))[2]*100,
              val3 = prop.table(questionr::wtd.table(y, weights = ponde))[3]*100,
              val4 = prop.table(questionr::wtd.table(y, weights = ponde))[4]*100,
              val5 = prop.table(questionr::wtd.table(y, weights = ponde))[5]*100,
              val6 = prop.table(questionr::wtd.table(y, weights = ponde))[6]*100,
              val7 = prop.table(questionr::wtd.table(y, weights = ponde))[7]*100,
              val8 = prop.table(questionr::wtd.table(y, weights = ponde))[8]*100)
  totalData[["q_EA_CA_13"]] <- factor("Total",
                                      levels = c(levels(df[["q_EA_CA_13"]]),"Total"),
                                      labels = c(levels(df[["q_EA_CA_13"]]),"Total"),
                                      ordered = T)
  totalData <- totalData %>%  pivot_longer(cols = starts_with("val"),
                                           names_to = "Value",
                                           names_prefix = "val",
                                           values_to = "Porcentaje")
  totalData$Value <- totalData$Value %>% factor(levels = c(1:8),
                                                labels = names(table(df[[originVar]])),
                                                ordered = T)

  graphData <- graphData %>%
    group_by(q_EA_CA_13) %>%
    summarise(val1 = prop.table(questionr::wtd.table(y, weights = ponde))[1]*100,
              val2 = prop.table(questionr::wtd.table(y, weights = ponde))[2]*100,
              val3 = prop.table(questionr::wtd.table(y, weights = ponde))[3]*100,
              val4 = prop.table(questionr::wtd.table(y, weights = ponde))[4]*100,
              val5 = prop.table(questionr::wtd.table(y, weights = ponde))[5]*100,
              val6 = prop.table(questionr::wtd.table(y, weights = ponde))[6]*100,
              val7 = prop.table(questionr::wtd.table(y, weights = ponde))[7]*100,
              val8 = prop.table(questionr::wtd.table(y, weights = ponde))[8]*100)%>%
    pivot_longer(cols = starts_with("val"),
                 names_to = "Value",
                 names_prefix = "val",
                 values_to = "Porcentaje")
  graphData$Value <- graphData$Value %>% factor(levels = c(1:8),
                                                labels = names(table(df[[originVar]])),
                                                ordered = T)
  graphData[["q_EA_CA_13"]] <- graphData[["q_EA_CA_13"]] %>% factor(levels = c(levels(df[["q_EA_CA_13"]]),"Total"),
                                                                    labels = c(levels(df[["q_EA_CA_13"]]),"Total"),
                                                                    ordered = T)
  if (totalColumn){
    graphData <- bind_rows(graphData, totalData)
  }
  graphData
}
#  8option Stack País --------------------------------------------------------------------
#' Function to create data for país stacked for graphs, final report as
#'
#' This function creates graphData to create stacked graph.
#' It assumes a 1-8 scale, values greater than 8 are missing.
#' It also assumes the dataStructure of the asData
#' @param df default: asData
#' @param originVar Variable to be graphed
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

createGraphData8optionPais <- function(df = asData,
                                       originVar,
                                       totalColumn = T){
  require(tidyverse)
  graphData <- df %>% select(all_of(originVar), País, ponde)
  graphData$y <- graphData[[originVar]]
  graphData <- na.omit(graphData)
  totalData <- graphData %>%
    summarise(val1 = prop.table(questionr::wtd.table(y, weights = ponde))[1]*100,
              val2 = prop.table(questionr::wtd.table(y, weights = ponde))[2]*100,
              val3 = prop.table(questionr::wtd.table(y, weights = ponde))[3]*100,
              val4 = prop.table(questionr::wtd.table(y, weights = ponde))[4]*100,
              val5 = prop.table(questionr::wtd.table(y, weights = ponde))[5]*100,
              val6 = prop.table(questionr::wtd.table(y, weights = ponde))[6]*100,
              val7 = prop.table(questionr::wtd.table(y, weights = ponde))[7]*100,
              val8 = prop.table(questionr::wtd.table(y, weights = ponde))[8]*100)
  totalData[["País"]] <- factor("Total",
                                levels = c(levels(df[["País"]]),"Total"),
                                labels = c(levels(df[["País"]]),"Total"))
  totalData <- totalData %>%  pivot_longer(cols = starts_with("val"),
                                           names_to = "Value",
                                           names_prefix = "val",
                                           values_to = "Porcentaje")
  totalData$Value <- totalData$Value %>% factor(levels = c(1:8),
                                                labels = names(table(df[[originVar]])),
                                                ordered = T)
  graphData <- graphData %>%
    group_by(País) %>%
    summarise(val1 = prop.table(questionr::wtd.table(y, weights = ponde))[1]*100,
              val2 = prop.table(questionr::wtd.table(y, weights = ponde))[2]*100,
              val3 = prop.table(questionr::wtd.table(y, weights = ponde))[3]*100,
              val4 = prop.table(questionr::wtd.table(y, weights = ponde))[4]*100,
              val5 = prop.table(questionr::wtd.table(y, weights = ponde))[5]*100,
              val6 = prop.table(questionr::wtd.table(y, weights = ponde))[6]*100,
              val7 = prop.table(questionr::wtd.table(y, weights = ponde))[7]*100,
              val8 = prop.table(questionr::wtd.table(y, weights = ponde))[8]*100)%>%
    pivot_longer(cols = starts_with("val"),
                 names_to = "Value",
                 names_prefix = "val",
                 values_to = "Porcentaje")
  graphData$Value <- graphData$Value %>% factor(levels = c(1:8),
                                                labels = names(table(df[[originVar]])),
                                                ordered = T)
  graphData[["País"]] <- graphData[["País"]] %>% factor(levels = c(names(table(df[["País"]])),"Total"),
                                                        labels = c(names(table(df[["País"]])),"Total"))

  if (totalColumn){
    graphData <- bind_rows(graphData, totalData)
  }
  graphData
}
#  8option Stack Educación Propia --------------------------------------------------------------------
#' Function to create data for educación madre stacked for graphs, final report as
#'
#' This function creates graphData to create stacked graph.
#' It assumes a 1-8 scale, values greater than 8 are missing.
#' It also assumes the dataStructure of the asData
#' @param df default: asData
#' @param originVar Variable to be graphed
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

createGraphData8optionEducP <- function(df = asData,
                                        originVar,
                                        totalColumn = T){
  require(tidyverse)
  graphData <- df %>% select(originVar, q_EA_CA_12, ponde)
  graphData$y <- graphData[[originVar]]
  graphData <- na.omit(graphData)
  totalData <- graphData %>%
    summarise(val1 = prop.table(questionr::wtd.table(y, weights = ponde))[1]*100,
              val2 = prop.table(questionr::wtd.table(y, weights = ponde))[2]*100,
              val3 = prop.table(questionr::wtd.table(y, weights = ponde))[3]*100,
              val4 = prop.table(questionr::wtd.table(y, weights = ponde))[4]*100,
              val5 = prop.table(questionr::wtd.table(y, weights = ponde))[5]*100,
              val6 = prop.table(questionr::wtd.table(y, weights = ponde))[6]*100,
              val7 = prop.table(questionr::wtd.table(y, weights = ponde))[7]*100,
              val8 = prop.table(questionr::wtd.table(y, weights = ponde))[8]*100)
  totalData[["q_EA_CA_12"]] <- factor("Total",
                                      levels = c(levels(df[["q_EA_CA_12"]]),"Total"),
                                      labels = c(levels(df[["q_EA_CA_12"]]),"Total"),
                                      ordered = T)
  totalData <- totalData %>%  pivot_longer(cols = starts_with("val"),
                                           names_to = "Value",
                                           names_prefix = "val",
                                           values_to = "Porcentaje")
  totalData$Value <- totalData$Value %>% factor(levels = c(1:8),
                                                labels = names(table(df[[originVar]])),
                                                ordered = T)

  graphData <- graphData %>%
    group_by(q_EA_CA_12) %>%
    summarise(val1 = prop.table(questionr::wtd.table(y, weights = ponde))[1]*100,
              val2 = prop.table(questionr::wtd.table(y, weights = ponde))[2]*100,
              val3 = prop.table(questionr::wtd.table(y, weights = ponde))[3]*100,
              val4 = prop.table(questionr::wtd.table(y, weights = ponde))[4]*100,
              val5 = prop.table(questionr::wtd.table(y, weights = ponde))[5]*100,
              val6 = prop.table(questionr::wtd.table(y, weights = ponde))[6]*100,
              val7 = prop.table(questionr::wtd.table(y, weights = ponde))[7]*100,
              val8 = prop.table(questionr::wtd.table(y, weights = ponde))[8]*100)%>%
    pivot_longer(cols = starts_with("val"),
                 names_to = "Value",
                 names_prefix = "val",
                 values_to = "Porcentaje")
  graphData$Value <- graphData$Value %>% factor(levels = c(1:8),
                                                labels = names(table(df[[originVar]])),
                                                ordered = T)
  graphData[["q_EA_CA_12"]] <- graphData[["q_EA_CA_12"]] %>% factor(levels = c(levels(df[["q_EA_CA_12"]]),"Total"),
                                                                    labels = c(levels(df[["q_EA_CA_12"]]),"Total"),
                                                                    ordered = T)
  if (totalColumn){
    graphData <- bind_rows(graphData, totalData)
  }
  graphData
}

#  7option Stack Gender --------------------------------------------------------------------
#' Function to create data for gender stacked for graphs, final report as
#'
#' This function creates graphData to create stacked graph.
#' It assumes a 1-7 scale, values greater than 7 are missing.
#' It also assumes the dataStructure of asData
#' @param df default: asData
#' @param originVar Variable to be graphed
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

createGraphData7optionGen <- function(df = asData,
                                      originVar,
                                      totalColumn = T){
  require(tidyverse)
  graphData <- df %>% select(originVar, q_EA_CA_10, ponde)
  graphData$y <- graphData[[originVar]]
  graphData <- na.omit(graphData)
  totalData <- graphData %>%
    summarise(val1 = prop.table(questionr::wtd.table(y, weights = ponde))[1]*100,
              val2 = prop.table(questionr::wtd.table(y, weights = ponde))[2]*100,
              val3 = prop.table(questionr::wtd.table(y, weights = ponde))[3]*100,
              val4 = prop.table(questionr::wtd.table(y, weights = ponde))[4]*100,
              val5 = prop.table(questionr::wtd.table(y, weights = ponde))[5]*100,
              val6 = prop.table(questionr::wtd.table(y, weights = ponde))[6]*100,
              val7 = prop.table(questionr::wtd.table(y, weights = ponde))[7]*100)
  totalData[["q_EA_CA_10"]] <- "Total"
  totalData <- totalData %>%  pivot_longer(cols = starts_with("val"),
                                           names_to = "Value",
                                           names_prefix = "val",
                                           values_to = "Porcentaje")
  totalData$Value <- totalData$Value %>% factor(levels = c(1:7),
                                                labels = names(table(df[[originVar]])),
                                                ordered = T)
  graphData <- graphData %>%
    group_by(q_EA_CA_10) %>%
    summarise(val1 = prop.table(questionr::wtd.table(y, weights = ponde))[1]*100,
              val2 = prop.table(questionr::wtd.table(y, weights = ponde))[2]*100,
              val3 = prop.table(questionr::wtd.table(y, weights = ponde))[3]*100,
              val4 = prop.table(questionr::wtd.table(y, weights = ponde))[4]*100,
              val5 = prop.table(questionr::wtd.table(y, weights = ponde))[5]*100,
              val6 = prop.table(questionr::wtd.table(y, weights = ponde))[6]*100,
              val7 = prop.table(questionr::wtd.table(y, weights = ponde))[7]*100)%>%
    pivot_longer(cols = starts_with("val"),
                 names_to = "Value",
                 names_prefix = "val",
                 values_to = "Porcentaje")
  graphData$Value <- graphData$Value %>% factor(levels = c(1:7),
                                                labels = names(table(df[[originVar]])),
                                                ordered = T)
  if (totalColumn){
    graphData <- bind_rows(graphData, totalData)
  }
  graphData
}

#  7option Stack Edad --------------------------------------------------------------------
#' Function to create data for edad stacked for graphs, final report as
#'
#' This function creates graphData to create stacked graph.
#' It assumes a 1-7 scale, values greater than 7 are missing.
#' It also assumes the dataStructure of the asFinalData
#' @param df default: asData
#' @param originVar Variable to be graphed
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

createGraphData7optionEdad <- function(df = asData,
                                       originVar,
                                       totalColumn = T){
  require(tidyverse)
  graphData <- df %>% select(originVar, Edad, ponde)
  graphData$y <- graphData[[originVar]]
  graphData <- na.omit(graphData)
  totalData <- graphData %>%
    summarise(val1 = prop.table(questionr::wtd.table(y, weights = ponde))[1]*100,
              val2 = prop.table(questionr::wtd.table(y, weights = ponde))[2]*100,
              val3 = prop.table(questionr::wtd.table(y, weights = ponde))[3]*100,
              val4 = prop.table(questionr::wtd.table(y, weights = ponde))[4]*100,
              val5 = prop.table(questionr::wtd.table(y, weights = ponde))[5]*100,
              val6 = prop.table(questionr::wtd.table(y, weights = ponde))[6]*100,
              val7 = prop.table(questionr::wtd.table(y, weights = ponde))[7]*100)
  totalData[["Edad"]] <- "Total"
  totalData <- totalData %>%  pivot_longer(cols = starts_with("val"),
                                           names_to = "Value",
                                           names_prefix = "val",
                                           values_to = "Porcentaje")
  totalData$Value <- totalData$Value %>% factor(levels = c(1:7),
                                                labels = names(table(df[[originVar]])),
                                                ordered = T)

  graphData <- graphData %>%
    group_by(Edad) %>%
    summarise(val1 = prop.table(questionr::wtd.table(y, weights = ponde))[1]*100,
              val2 = prop.table(questionr::wtd.table(y, weights = ponde))[2]*100,
              val3 = prop.table(questionr::wtd.table(y, weights = ponde))[3]*100,
              val4 = prop.table(questionr::wtd.table(y, weights = ponde))[4]*100,
              val5 = prop.table(questionr::wtd.table(y, weights = ponde))[5]*100,
              val6 = prop.table(questionr::wtd.table(y, weights = ponde))[6]*100,
              val7 = prop.table(questionr::wtd.table(y, weights = ponde))[7]*100)%>%
    pivot_longer(cols = starts_with("val"),
                 names_to = "Value",
                 names_prefix = "val",
                 values_to = "Porcentaje")
  graphData$Value <- graphData$Value %>% factor(levels = c(1:7),
                                                labels = names(table(df[[originVar]])),
                                                ordered = T)
  if (totalColumn){
    graphData <- bind_rows(graphData, totalData)
  }
  graphData
}
#  7option Stack Educación Madre --------------------------------------------------------------------
#' Function to create data for educación madre stacked for graphs, final report as
#'
#' This function creates graphData to create stacked graph.
#' It assumes a 1-7 scale, values greater than 7 are missing.
#' It also assumes the dataStructure of the asData
#' @param df default: asData
#' @param originVar Variable to be graphed
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

createGraphData7optionEducM <- function(df = asData,
                                        originVar,
                                        totalColumn = T){
  require(tidyverse)
  graphData <- df %>% select(originVar, q_EA_CA_13, ponde)
  graphData$y <- graphData[[originVar]]
  graphData <- na.omit(graphData)
  totalData <- graphData %>%
    summarise(val1 = prop.table(questionr::wtd.table(y, weights = ponde))[1]*100,
              val2 = prop.table(questionr::wtd.table(y, weights = ponde))[2]*100,
              val3 = prop.table(questionr::wtd.table(y, weights = ponde))[3]*100,
              val4 = prop.table(questionr::wtd.table(y, weights = ponde))[4]*100,
              val5 = prop.table(questionr::wtd.table(y, weights = ponde))[5]*100,
              val6 = prop.table(questionr::wtd.table(y, weights = ponde))[6]*100,
              val7 = prop.table(questionr::wtd.table(y, weights = ponde))[7]*100)
  totalData[["q_EA_CA_13"]] <- factor("Total",
                                      levels = c(levels(df[["q_EA_CA_13"]]),"Total"),
                                      labels = c(levels(df[["q_EA_CA_13"]]),"Total"),
                                      ordered = T)
  totalData <- totalData %>%  pivot_longer(cols = starts_with("val"),
                                           names_to = "Value",
                                           names_prefix = "val",
                                           values_to = "Porcentaje")
  totalData$Value <- totalData$Value %>% factor(levels = c(1:7),
                                                labels = names(table(df[[originVar]])),
                                                ordered = T)

  graphData <- graphData %>%
    group_by(q_EA_CA_13) %>%
    summarise(val1 = prop.table(questionr::wtd.table(y, weights = ponde))[1]*100,
              val2 = prop.table(questionr::wtd.table(y, weights = ponde))[2]*100,
              val3 = prop.table(questionr::wtd.table(y, weights = ponde))[3]*100,
              val4 = prop.table(questionr::wtd.table(y, weights = ponde))[4]*100,
              val5 = prop.table(questionr::wtd.table(y, weights = ponde))[5]*100,
              val6 = prop.table(questionr::wtd.table(y, weights = ponde))[6]*100,
              val7 = prop.table(questionr::wtd.table(y, weights = ponde))[7]*100)%>%
    pivot_longer(cols = starts_with("val"),
                 names_to = "Value",
                 names_prefix = "val",
                 values_to = "Porcentaje")
  graphData$Value <- graphData$Value %>% factor(levels = c(1:7),
                                                labels = names(table(df[[originVar]])),
                                                ordered = T)
  graphData[["q_EA_CA_13"]] <- graphData[["q_EA_CA_13"]] %>% factor(levels = c(levels(df[["q_EA_CA_13"]]),"Total"),
                                                                    labels = c(levels(df[["q_EA_CA_13"]]),"Total"),
                                                                    ordered = T)
  if (totalColumn){
    graphData <- bind_rows(graphData, totalData)
  }
  graphData
}
#  7option Stack País --------------------------------------------------------------------
#' Function to create data for país stacked for graphs, final report as
#'
#' This function creates graphData to create stacked graph.
#' It assumes a 1-7 scale, values greater than 7 are missing.
#' It also assumes the dataStructure of the asData
#' @param df default: asData
#' @param originVar Variable to be graphed
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

createGraphData7optionPais <- function(df = asData,
                                       originVar,
                                       totalColumn = T){
  require(tidyverse)
  graphData <- df %>% select(all_of(originVar), País, ponde)
  graphData$y <- graphData[[originVar]]
  graphData <- na.omit(graphData)
  totalData <- graphData %>%
    summarise(val1 = prop.table(questionr::wtd.table(y, weights = ponde))[1]*100,
              val2 = prop.table(questionr::wtd.table(y, weights = ponde))[2]*100,
              val3 = prop.table(questionr::wtd.table(y, weights = ponde))[3]*100,
              val4 = prop.table(questionr::wtd.table(y, weights = ponde))[4]*100,
              val5 = prop.table(questionr::wtd.table(y, weights = ponde))[5]*100,
              val6 = prop.table(questionr::wtd.table(y, weights = ponde))[6]*100,
              val7 = prop.table(questionr::wtd.table(y, weights = ponde))[7]*100)
  totalData[["País"]] <- factor("Total",
                                levels = c(levels(df[["País"]]),"Total"),
                                labels = c(levels(df[["País"]]),"Total"))
  totalData <- totalData %>%  pivot_longer(cols = starts_with("val"),
                                           names_to = "Value",
                                           names_prefix = "val",
                                           values_to = "Porcentaje")
  totalData$Value <- totalData$Value %>% factor(levels = c(1:7),
                                                labels = names(table(df[[originVar]])),
                                                ordered = T)
  graphData <- graphData %>%
    group_by(País) %>%
    summarise(val1 = prop.table(questionr::wtd.table(y, weights = ponde))[1]*100,
              val2 = prop.table(questionr::wtd.table(y, weights = ponde))[2]*100,
              val3 = prop.table(questionr::wtd.table(y, weights = ponde))[3]*100,
              val4 = prop.table(questionr::wtd.table(y, weights = ponde))[4]*100,
              val5 = prop.table(questionr::wtd.table(y, weights = ponde))[5]*100,
              val6 = prop.table(questionr::wtd.table(y, weights = ponde))[6]*100,
              val7 = prop.table(questionr::wtd.table(y, weights = ponde))[7]*100)%>%
    pivot_longer(cols = starts_with("val"),
                 names_to = "Value",
                 names_prefix = "val",
                 values_to = "Porcentaje")
  graphData$Value <- graphData$Value %>% factor(levels = c(1:7),
                                                labels = names(table(df[[originVar]])),
                                                ordered = T)
  graphData[["País"]] <- graphData[["País"]] %>% factor(levels = c(names(table(df[["País"]])),"Total"),
                                                        labels = c(names(table(df[["País"]])),"Total"))

  if (totalColumn){
    graphData <- bind_rows(graphData, totalData)
  }
  graphData
}
#  7option Stack Educación Propia --------------------------------------------------------------------
#' Function to create data for educación madre stacked for graphs, final report as
#'
#' This function creates graphData to create stacked graph.
#' It assumes a 1-7 scale, values greater than 7 are missing.
#' It also assumes the dataStructure of the asData
#' @param df default: asData
#' @param originVar Variable to be graphed
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

createGraphData7optionEducP <- function(df = asData,
                                        originVar,
                                        totalColumn = T){
  require(tidyverse)
  graphData <- df %>% select(originVar, q_EA_CA_12, ponde)
  graphData$y <- graphData[[originVar]]
  graphData <- na.omit(graphData)
  totalData <- graphData %>%
    summarise(val1 = prop.table(questionr::wtd.table(y, weights = ponde))[1]*100,
              val2 = prop.table(questionr::wtd.table(y, weights = ponde))[2]*100,
              val3 = prop.table(questionr::wtd.table(y, weights = ponde))[3]*100,
              val4 = prop.table(questionr::wtd.table(y, weights = ponde))[4]*100,
              val5 = prop.table(questionr::wtd.table(y, weights = ponde))[5]*100,
              val6 = prop.table(questionr::wtd.table(y, weights = ponde))[6]*100,
              val7 = prop.table(questionr::wtd.table(y, weights = ponde))[7]*100)
  totalData[["q_EA_CA_12"]] <- factor("Total",
                                      levels = c(levels(df[["q_EA_CA_12"]]),"Total"),
                                      labels = c(levels(df[["q_EA_CA_12"]]),"Total"),
                                      ordered = T)
  totalData <- totalData %>%  pivot_longer(cols = starts_with("val"),
                                           names_to = "Value",
                                           names_prefix = "val",
                                           values_to = "Porcentaje")
  totalData$Value <- totalData$Value %>% factor(levels = c(1:7),
                                                labels = names(table(df[[originVar]])),
                                                ordered = T)

  graphData <- graphData %>%
    group_by(q_EA_CA_12) %>%
    summarise(val1 = prop.table(questionr::wtd.table(y, weights = ponde))[1]*100,
              val2 = prop.table(questionr::wtd.table(y, weights = ponde))[2]*100,
              val3 = prop.table(questionr::wtd.table(y, weights = ponde))[3]*100,
              val4 = prop.table(questionr::wtd.table(y, weights = ponde))[4]*100,
              val5 = prop.table(questionr::wtd.table(y, weights = ponde))[5]*100,
              val6 = prop.table(questionr::wtd.table(y, weights = ponde))[6]*100,
              val7 = prop.table(questionr::wtd.table(y, weights = ponde))[7]*100)%>%
    pivot_longer(cols = starts_with("val"),
                 names_to = "Value",
                 names_prefix = "val",
                 values_to = "Porcentaje")
  graphData$Value <- graphData$Value %>% factor(levels = c(1:7),
                                                labels = names(table(df[[originVar]])),
                                                ordered = T)
  graphData[["q_EA_CA_12"]] <- graphData[["q_EA_CA_12"]] %>% factor(levels = c(levels(df[["q_EA_CA_12"]]),"Total"),
                                                                    labels = c(levels(df[["q_EA_CA_12"]]),"Total"),
                                                                    ordered = T)
  if (totalColumn){
    graphData <- bind_rows(graphData, totalData)
  }
  graphData
}

#  6option Stack Gender --------------------------------------------------------------------
#' Function to create data for gender stacked for graphs, final report as
#'
#' This function creates graphData to create stacked graph.
#' It assumes a 1-6 scale, values greater than 6 are missing.
#' It also assumes the dataStructure of asData
#' @param df default: asData
#' @param originVar Variable to be graphed
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

createGraphData6optionGen <- function(df = asData,
                                      originVar,
                                      totalColumn = T){
  require(tidyverse)
  graphData <- df %>% select(originVar, q_EA_CA_10, ponde)
  graphData$y <- graphData[[originVar]]
  graphData <- na.omit(graphData)
  totalData <- graphData %>%
    summarise(val1 = prop.table(questionr::wtd.table(y, weights = ponde))[1]*100,
              val2 = prop.table(questionr::wtd.table(y, weights = ponde))[2]*100,
              val3 = prop.table(questionr::wtd.table(y, weights = ponde))[3]*100,
              val4 = prop.table(questionr::wtd.table(y, weights = ponde))[4]*100,
              val5 = prop.table(questionr::wtd.table(y, weights = ponde))[5]*100,
              val6 = prop.table(questionr::wtd.table(y, weights = ponde))[6]*100)
  totalData[["q_EA_CA_10"]] <- "Total"
  totalData <- totalData %>%  pivot_longer(cols = starts_with("val"),
                                           names_to = "Value",
                                           names_prefix = "val",
                                           values_to = "Porcentaje")
  totalData$Value <- totalData$Value %>% factor(levels = c(1:6),
                                                labels = names(table(df[[originVar]])),
                                                ordered = T)
  graphData <- graphData %>%
    group_by(q_EA_CA_10) %>%
    summarise(val1 = prop.table(questionr::wtd.table(y, weights = ponde))[1]*100,
              val2 = prop.table(questionr::wtd.table(y, weights = ponde))[2]*100,
              val3 = prop.table(questionr::wtd.table(y, weights = ponde))[3]*100,
              val4 = prop.table(questionr::wtd.table(y, weights = ponde))[4]*100,
              val5 = prop.table(questionr::wtd.table(y, weights = ponde))[5]*100,
              val6 = prop.table(questionr::wtd.table(y, weights = ponde))[6]*100)%>%
    pivot_longer(cols = starts_with("val"),
                 names_to = "Value",
                 names_prefix = "val",
                 values_to = "Porcentaje")
  graphData$Value <- graphData$Value %>% factor(levels = c(1:6),
                                                labels = names(table(df[[originVar]])),
                                                ordered = T)
  if (totalColumn){
    graphData <- bind_rows(graphData, totalData)
  }
  graphData
}

#  6option Stack Edad --------------------------------------------------------------------
#' Function to create data for edad stacked for graphs, final report as
#'
#' This function creates graphData to create stacked graph.
#' It assumes a 1-6 scale, values greater than 6 are missing.
#' It also assumes the dataStructure of the asFinalData
#' @param df default: asData
#' @param originVar Variable to be graphed
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

createGraphData6optionEdad <- function(df = asData,
                                       originVar,
                                       totalColumn = T){
  require(tidyverse)
  graphData <- df %>% select(originVar, Edad, ponde)
  graphData$y <- graphData[[originVar]]
  graphData <- na.omit(graphData)
  totalData <- graphData %>%
    summarise(val1 = prop.table(questionr::wtd.table(y, weights = ponde))[1]*100,
              val2 = prop.table(questionr::wtd.table(y, weights = ponde))[2]*100,
              val3 = prop.table(questionr::wtd.table(y, weights = ponde))[3]*100,
              val4 = prop.table(questionr::wtd.table(y, weights = ponde))[4]*100,
              val5 = prop.table(questionr::wtd.table(y, weights = ponde))[5]*100,
              val6 = prop.table(questionr::wtd.table(y, weights = ponde))[6]*100)
  totalData[["Edad"]] <- "Total"
  totalData <- totalData %>%  pivot_longer(cols = starts_with("val"),
                                           names_to = "Value",
                                           names_prefix = "val",
                                           values_to = "Porcentaje")
  totalData$Value <- totalData$Value %>% factor(levels = c(1:6),
                                                labels = names(table(df[[originVar]])),
                                                ordered = T)

  graphData <- graphData %>%
    group_by(Edad) %>%
    summarise(val1 = prop.table(questionr::wtd.table(y, weights = ponde))[1]*100,
              val2 = prop.table(questionr::wtd.table(y, weights = ponde))[2]*100,
              val3 = prop.table(questionr::wtd.table(y, weights = ponde))[3]*100,
              val4 = prop.table(questionr::wtd.table(y, weights = ponde))[4]*100,
              val5 = prop.table(questionr::wtd.table(y, weights = ponde))[5]*100,
              val6 = prop.table(questionr::wtd.table(y, weights = ponde))[6]*100)%>%
    pivot_longer(cols = starts_with("val"),
                 names_to = "Value",
                 names_prefix = "val",
                 values_to = "Porcentaje")
  graphData$Value <- graphData$Value %>% factor(levels = c(1:6),
                                                labels = names(table(df[[originVar]])),
                                                ordered = T)
  if (totalColumn){
    graphData <- bind_rows(graphData, totalData)
  }
  graphData
}
#  6option Stack Educación Madre --------------------------------------------------------------------
#' Function to create data for educación madre stacked for graphs, final report as
#'
#' This function creates graphData to create stacked graph.
#' It assumes a 1-6 scale, values greater than 6 are missing.
#' It also assumes the dataStructure of the asData
#' @param df default: asData
#' @param originVar Variable to be graphed
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

createGraphData6optionEducM <- function(df = asData,
                                        originVar,
                                        totalColumn = T){
  require(tidyverse)
  graphData <- df %>% select(originVar, q_EA_CA_13, ponde)
  graphData$y <- graphData[[originVar]]
  graphData <- na.omit(graphData)
  totalData <- graphData %>%
    summarise(val1 = prop.table(questionr::wtd.table(y, weights = ponde))[1]*100,
              val2 = prop.table(questionr::wtd.table(y, weights = ponde))[2]*100,
              val3 = prop.table(questionr::wtd.table(y, weights = ponde))[3]*100,
              val4 = prop.table(questionr::wtd.table(y, weights = ponde))[4]*100,
              val5 = prop.table(questionr::wtd.table(y, weights = ponde))[5]*100,
              val6 = prop.table(questionr::wtd.table(y, weights = ponde))[6]*100)
  totalData[["q_EA_CA_13"]] <- factor("Total",
                                      levels = c(levels(df[["q_EA_CA_13"]]),"Total"),
                                      labels = c(levels(df[["q_EA_CA_13"]]),"Total"),
                                      ordered = T)
  totalData <- totalData %>%  pivot_longer(cols = starts_with("val"),
                                           names_to = "Value",
                                           names_prefix = "val",
                                           values_to = "Porcentaje")
  totalData$Value <- totalData$Value %>% factor(levels = c(1:6),
                                                labels = names(table(df[[originVar]])),
                                                ordered = T)

  graphData <- graphData %>%
    group_by(q_EA_CA_13) %>%
    summarise(val1 = prop.table(questionr::wtd.table(y, weights = ponde))[1]*100,
              val2 = prop.table(questionr::wtd.table(y, weights = ponde))[2]*100,
              val3 = prop.table(questionr::wtd.table(y, weights = ponde))[3]*100,
              val4 = prop.table(questionr::wtd.table(y, weights = ponde))[4]*100,
              val5 = prop.table(questionr::wtd.table(y, weights = ponde))[5]*100,
              val6 = prop.table(questionr::wtd.table(y, weights = ponde))[6]*100)%>%
    pivot_longer(cols = starts_with("val"),
                 names_to = "Value",
                 names_prefix = "val",
                 values_to = "Porcentaje")
  graphData$Value <- graphData$Value %>% factor(levels = c(1:6),
                                                labels = names(table(df[[originVar]])),
                                                ordered = T)
  graphData[["q_EA_CA_13"]] <- graphData[["q_EA_CA_13"]] %>% factor(levels = c(levels(df[["q_EA_CA_13"]]),"Total"),
                                                                    labels = c(levels(df[["q_EA_CA_13"]]),"Total"),
                                                                    ordered = T)
  if (totalColumn){
    graphData <- bind_rows(graphData, totalData)
  }
  graphData
}
#  6option Stack País --------------------------------------------------------------------
#' Function to create data for país stacked for graphs, final report as
#'
#' This function creates graphData to create stacked graph.
#' It assumes a 1-6 scale, values greater than 6 are missing.
#' It also assumes the dataStructure of the asData
#' @param df default: asData
#' @param originVar Variable to be graphed
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

createGraphData6optionPais <- function(df = asData,
                                       originVar,
                                       totalColumn = T){
  require(tidyverse)
  graphData <- df %>% select(all_of(originVar), País, ponde)
  graphData$y <- graphData[[originVar]]
  graphData <- na.omit(graphData)
  totalData <- graphData %>%
    summarise(val1 = prop.table(questionr::wtd.table(y, weights = ponde))[1]*100,
              val2 = prop.table(questionr::wtd.table(y, weights = ponde))[2]*100,
              val3 = prop.table(questionr::wtd.table(y, weights = ponde))[3]*100,
              val4 = prop.table(questionr::wtd.table(y, weights = ponde))[4]*100,
              val5 = prop.table(questionr::wtd.table(y, weights = ponde))[5]*100,
              val6 = prop.table(questionr::wtd.table(y, weights = ponde))[6]*100)
  totalData[["País"]] <- factor("Total",
                                levels = c(levels(df[["País"]]),"Total"),
                                labels = c(levels(df[["País"]]),"Total"))
  totalData <- totalData %>%  pivot_longer(cols = starts_with("val"),
                                           names_to = "Value",
                                           names_prefix = "val",
                                           values_to = "Porcentaje")
  totalData$Value <- totalData$Value %>% factor(levels = c(1:6),
                                                labels = names(table(df[[originVar]])),
                                                ordered = T)
  graphData <- graphData %>%
    group_by(País) %>%
    summarise(val1 = prop.table(questionr::wtd.table(y, weights = ponde))[1]*100,
              val2 = prop.table(questionr::wtd.table(y, weights = ponde))[2]*100,
              val3 = prop.table(questionr::wtd.table(y, weights = ponde))[3]*100,
              val4 = prop.table(questionr::wtd.table(y, weights = ponde))[4]*100,
              val5 = prop.table(questionr::wtd.table(y, weights = ponde))[5]*100,
              val6 = prop.table(questionr::wtd.table(y, weights = ponde))[6]*100)%>%
    pivot_longer(cols = starts_with("val"),
                 names_to = "Value",
                 names_prefix = "val",
                 values_to = "Porcentaje")
  graphData$Value <- graphData$Value %>% factor(levels = c(1:6),
                                                labels = names(table(df[[originVar]])),
                                                ordered = T)
  graphData[["País"]] <- graphData[["País"]] %>% factor(levels = c(names(table(df[["País"]])),"Total"),
                                                        labels = c(names(table(df[["País"]])),"Total"))

  if (totalColumn){
    graphData <- bind_rows(graphData, totalData)
  }
  graphData
}
#  6option Stack Educación Propia --------------------------------------------------------------------
#' Function to create data for educación madre stacked for graphs, final report as
#'
#' This function creates graphData to create stacked graph.
#' It assumes a 1-6 scale, values greater than 6 are missing.
#' It also assumes the dataStructure of the asData
#' @param df default: asData
#' @param originVar Variable to be graphed
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

createGraphData6optionEducP <- function(df = asData,
                                        originVar,
                                        totalColumn = T){
  require(tidyverse)
  graphData <- df %>% select(originVar, q_EA_CA_12, ponde)
  graphData$y <- graphData[[originVar]]
  graphData <- na.omit(graphData)
  totalData <- graphData %>%
    summarise(val1 = prop.table(questionr::wtd.table(y, weights = ponde))[1]*100,
              val2 = prop.table(questionr::wtd.table(y, weights = ponde))[2]*100,
              val3 = prop.table(questionr::wtd.table(y, weights = ponde))[3]*100,
              val4 = prop.table(questionr::wtd.table(y, weights = ponde))[4]*100,
              val5 = prop.table(questionr::wtd.table(y, weights = ponde))[5]*100,
              val6 = prop.table(questionr::wtd.table(y, weights = ponde))[6]*100)
  totalData[["q_EA_CA_12"]] <- factor("Total",
                                      levels = c(levels(df[["q_EA_CA_12"]]),"Total"),
                                      labels = c(levels(df[["q_EA_CA_12"]]),"Total"),
                                      ordered = T)
  totalData <- totalData %>%  pivot_longer(cols = starts_with("val"),
                                           names_to = "Value",
                                           names_prefix = "val",
                                           values_to = "Porcentaje")
  totalData$Value <- totalData$Value %>% factor(levels = c(1:6),
                                                labels = names(table(df[[originVar]])),
                                                ordered = T)

  graphData <- graphData %>%
    group_by(q_EA_CA_12) %>%
    summarise(val1 = prop.table(questionr::wtd.table(y, weights = ponde))[1]*100,
              val2 = prop.table(questionr::wtd.table(y, weights = ponde))[2]*100,
              val3 = prop.table(questionr::wtd.table(y, weights = ponde))[3]*100,
              val4 = prop.table(questionr::wtd.table(y, weights = ponde))[4]*100,
              val5 = prop.table(questionr::wtd.table(y, weights = ponde))[5]*100,
              val6 = prop.table(questionr::wtd.table(y, weights = ponde))[6]*100)%>%
    pivot_longer(cols = starts_with("val"),
                 names_to = "Value",
                 names_prefix = "val",
                 values_to = "Porcentaje")
  graphData$Value <- graphData$Value %>% factor(levels = c(1:6),
                                                labels = names(table(df[[originVar]])),
                                                ordered = T)
  graphData[["q_EA_CA_12"]] <- graphData[["q_EA_CA_12"]] %>% factor(levels = c(levels(df[["q_EA_CA_12"]]),"Total"),
                                                                    labels = c(levels(df[["q_EA_CA_12"]]),"Total"),
                                                                    ordered = T)
  if (totalColumn){
    graphData <- bind_rows(graphData, totalData)
  }
  graphData
}

#  5option Stack Gender --------------------------------------------------------------------
#' Function to create data for gender stacked for graphs, final report as
#'
#' This function creates graphData to create stacked graph.
#' It assumes a 1-5 scale, values greater than 5 are missing.
#' It also assumes the dataStructure of asData
#' @param df default: asData
#' @param originVar Variable to be graphed
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

createGraphData5optionGen <- function(df = asData,
                                      originVar,
                                      totalColumn = T){
  require(tidyverse)
  graphData <- df %>% select(originVar, q_EA_CA_10, ponde)
  graphData$y <- graphData[[originVar]]
  graphData <- na.omit(graphData)
  totalData <- graphData %>%
    summarise(val1 = prop.table(questionr::wtd.table(y, weights = ponde))[1]*100,
              val2 = prop.table(questionr::wtd.table(y, weights = ponde))[2]*100,
              val3 = prop.table(questionr::wtd.table(y, weights = ponde))[3]*100,
              val4 = prop.table(questionr::wtd.table(y, weights = ponde))[4]*100,
              val5 = prop.table(questionr::wtd.table(y, weights = ponde))[5]*100)
  totalData[["q_EA_CA_10"]] <- "Total"
  totalData <- totalData %>%  pivot_longer(cols = starts_with("val"),
                                           names_to = "Value",
                                           names_prefix = "val",
                                           values_to = "Porcentaje")
  totalData$Value <- totalData$Value %>% factor(levels = c(1:5),
                                                labels = names(table(df[[originVar]])),
                                                ordered = T)
  graphData <- graphData %>%
    group_by(q_EA_CA_10) %>%
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

#  5option Stack Edad --------------------------------------------------------------------
#' Function to create data for edad stacked for graphs, final report as
#'
#' This function creates graphData to create stacked graph.
#' It assumes a 1-5 scale, values greater than 5 are missing.
#' It also assumes the dataStructure of the asFinalData
#' @param df default: asData
#' @param originVar Variable to be graphed
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

createGraphData5optionEdad <- function(df = asData,
                                       originVar,
                                       totalColumn = T){
  require(tidyverse)
  graphData <- df %>% select(originVar, Edad, ponde)
  graphData$y <- graphData[[originVar]]
  graphData <- na.omit(graphData)
  totalData <- graphData %>%
    summarise(val1 = prop.table(questionr::wtd.table(y, weights = ponde))[1]*100,
              val2 = prop.table(questionr::wtd.table(y, weights = ponde))[2]*100,
              val3 = prop.table(questionr::wtd.table(y, weights = ponde))[3]*100,
              val4 = prop.table(questionr::wtd.table(y, weights = ponde))[4]*100,
              val5 = prop.table(questionr::wtd.table(y, weights = ponde))[5]*100)
  totalData[["Edad"]] <- "Total"
  totalData <- totalData %>%  pivot_longer(cols = starts_with("val"),
                                           names_to = "Value",
                                           names_prefix = "val",
                                           values_to = "Porcentaje")
  totalData$Value <- totalData$Value %>% factor(levels = c(1:5),
                                                labels = names(table(df[[originVar]])),
                                                ordered = T)

  graphData <- graphData %>%
    group_by(Edad) %>%
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
#  5option Stack Educación Madre --------------------------------------------------------------------
#' Function to create data for educación madre stacked for graphs, final report as
#'
#' This function creates graphData to create stacked graph.
#' It assumes a 1-5 scale, values greater than 5 are missing.
#' It also assumes the dataStructure of the asData
#' @param df default: asData
#' @param originVar Variable to be graphed
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

createGraphData5optionEducM <- function(df = asData,
                                        originVar,
                                        totalColumn = T){
  require(tidyverse)
  graphData <- df %>% select(originVar, q_EA_CA_13, ponde)
  graphData$y <- graphData[[originVar]]
  graphData <- na.omit(graphData)
  totalData <- graphData %>%
    summarise(val1 = prop.table(questionr::wtd.table(y, weights = ponde))[1]*100,
              val2 = prop.table(questionr::wtd.table(y, weights = ponde))[2]*100,
              val3 = prop.table(questionr::wtd.table(y, weights = ponde))[3]*100,
              val4 = prop.table(questionr::wtd.table(y, weights = ponde))[4]*100,
              val5 = prop.table(questionr::wtd.table(y, weights = ponde))[5]*100)
  totalData[["q_EA_CA_13"]] <- factor("Total",
                                      levels = c(levels(df[["q_EA_CA_13"]]),"Total"),
                                      labels = c(levels(df[["q_EA_CA_13"]]),"Total"),
                                      ordered = T)
  totalData <- totalData %>%  pivot_longer(cols = starts_with("val"),
                                           names_to = "Value",
                                           names_prefix = "val",
                                           values_to = "Porcentaje")
  totalData$Value <- totalData$Value %>% factor(levels = c(1:5),
                                                labels = names(table(df[[originVar]])),
                                                ordered = T)

  graphData <- graphData %>%
    group_by(q_EA_CA_13) %>%
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
  graphData[["q_EA_CA_13"]] <- graphData[["q_EA_CA_13"]] %>% factor(levels = c(levels(df[["q_EA_CA_13"]]),"Total"),
                                                                    labels = c(levels(df[["q_EA_CA_13"]]),"Total"),
                                                                    ordered = T)
  if (totalColumn){
    graphData <- bind_rows(graphData, totalData)
  }
  graphData
}
#  5option Stack País --------------------------------------------------------------------
#' Function to create data for país stacked for graphs, final report as
#'
#' This function creates graphData to create stacked graph.
#' It assumes a 1-5 scale, values greater than 5 are missing.
#' It also assumes the dataStructure of the asData
#' @param df default: asData
#' @param originVar Variable to be graphed
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

createGraphData5optionPais <- function(df = asData,
                                       originVar,
                                       totalColumn = T){
  require(tidyverse)
  graphData <- df %>% select(all_of(originVar), País, ponde)
  graphData$y <- graphData[[originVar]]
  graphData <- na.omit(graphData)
  totalData <- graphData %>%
    summarise(val1 = prop.table(questionr::wtd.table(y, weights = ponde))[1]*100,
              val2 = prop.table(questionr::wtd.table(y, weights = ponde))[2]*100,
              val3 = prop.table(questionr::wtd.table(y, weights = ponde))[3]*100,
              val4 = prop.table(questionr::wtd.table(y, weights = ponde))[4]*100,
              val5 = prop.table(questionr::wtd.table(y, weights = ponde))[5]*100)
  totalData[["País"]] <- factor("Total",
                                levels = c(levels(df[["País"]]),"Total"),
                                labels = c(levels(df[["País"]]),"Total"))
  totalData <- totalData %>%  pivot_longer(cols = starts_with("val"),
                                           names_to = "Value",
                                           names_prefix = "val",
                                           values_to = "Porcentaje")
  totalData$Value <- totalData$Value %>% factor(levels = c(1:5),
                                                labels = names(table(df[[originVar]])),
                                                ordered = T)
  graphData <- graphData %>%
    group_by(País) %>%
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
  graphData[["País"]] <- graphData[["País"]] %>% factor(levels = c(names(table(df[["País"]])),"Total"),
                                                        labels = c(names(table(df[["País"]])),"Total"))

  if (totalColumn){
    graphData <- bind_rows(graphData, totalData)
  }
  graphData
}
#  5option Stack Educación Propia --------------------------------------------------------------------
#' Function to create data for educación madre stacked for graphs, final report as
#'
#' This function creates graphData to create stacked graph.
#' It assumes a 1-5 scale, values greater than 5 are missing.
#' It also assumes the dataStructure of the asData
#' @param df default: asData
#' @param originVar Variable to be graphed
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

createGraphData5optionEducP <- function(df = asData,
                                        originVar,
                                        totalColumn = T){
  require(tidyverse)
  graphData <- df %>% select(originVar, q_EA_CA_12, ponde)
  graphData$y <- graphData[[originVar]]
  graphData <- na.omit(graphData)
  totalData <- graphData %>%
    summarise(val1 = prop.table(questionr::wtd.table(y, weights = ponde))[1]*100,
              val2 = prop.table(questionr::wtd.table(y, weights = ponde))[2]*100,
              val3 = prop.table(questionr::wtd.table(y, weights = ponde))[3]*100,
              val4 = prop.table(questionr::wtd.table(y, weights = ponde))[4]*100,
              val5 = prop.table(questionr::wtd.table(y, weights = ponde))[5]*100)
  totalData[["q_EA_CA_12"]] <- factor("Total",
                                      levels = c(levels(df[["q_EA_CA_12"]]),"Total"),
                                      labels = c(levels(df[["q_EA_CA_12"]]),"Total"),
                                      ordered = T)
  totalData <- totalData %>%  pivot_longer(cols = starts_with("val"),
                                           names_to = "Value",
                                           names_prefix = "val",
                                           values_to = "Porcentaje")
  totalData$Value <- totalData$Value %>% factor(levels = c(1:5),
                                                labels = names(table(df[[originVar]])),
                                                ordered = T)

  graphData <- graphData %>%
    group_by(q_EA_CA_12) %>%
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
  graphData[["q_EA_CA_12"]] <- graphData[["q_EA_CA_12"]] %>% factor(levels = c(levels(df[["q_EA_CA_12"]]),"Total"),
                                                                    labels = c(levels(df[["q_EA_CA_12"]]),"Total"),
                                                                    ordered = T)
  if (totalColumn){
    graphData <- bind_rows(graphData, totalData)
  }
  graphData
}

#  4option Stack Gender --------------------------------------------------------------------
#' Function to create data for gender stacked for graphs, final report as
#'
#' This function creates graphData to create stacked graph.
#' It assumes a 1-4 scale, values greater than 4 are missing.
#' It also assumes the dataStructure of asData
#' @param df default: asData
#' @param originVar Variable to be graphed
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

createGraphData4OptionGen <- function(df = asData,
                                      originVar,
                                      totalColumn = T){
  require(tidyverse)
  graphData <- df %>% select(originVar, q_EA_CA_10, ponde)
  graphData$y <- graphData[[originVar]]
  graphData <- na.omit(graphData)
  totalData <- graphData %>%
    summarise(val1 = prop.table(questionr::wtd.table(y, weights = ponde))[1]*100,
              val2 = prop.table(questionr::wtd.table(y, weights = ponde))[2]*100,
              val3 = prop.table(questionr::wtd.table(y, weights = ponde))[3]*100,
              val4 = prop.table(questionr::wtd.table(y, weights = ponde))[4]*100)
  totalData[["q_EA_CA_10"]] <- "Total"
  totalData <- totalData %>%  pivot_longer(cols = starts_with("val"),
                                           names_to = "Value",
                                           names_prefix = "val",
                                           values_to = "Porcentaje")
  totalData$Value <- totalData$Value %>% factor(levels = c(1:4),
                                                labels = names(table(df[[originVar]])),
                                                ordered = T)
  graphData <- graphData %>%
    group_by(q_EA_CA_10) %>%
    summarise(val1 = prop.table(questionr::wtd.table(y, weights = ponde))[1]*100,
              val2 = prop.table(questionr::wtd.table(y, weights = ponde))[2]*100,
              val3 = prop.table(questionr::wtd.table(y, weights = ponde))[3]*100,
              val4 = prop.table(questionr::wtd.table(y, weights = ponde))[4]*100)%>%
    pivot_longer(cols = starts_with("val"),
                 names_to = "Value",
                 names_prefix = "val",
                 values_to = "Porcentaje")
  graphData$Value <- graphData$Value %>% factor(levels = c(1:4),
                                                labels = names(table(df[[originVar]])),
                                                ordered = T)
  if (totalColumn){
    graphData <- bind_rows(graphData, totalData)
  }
  graphData
}

#  4option Stack Edad --------------------------------------------------------------------
#' Function to create data for edad stacked for graphs, final report as
#'
#' This function creates graphData to create stacked graph.
#' It assumes a 1-4 scale, values greater than 4 are missing.
#' It also assumes the dataStructure of the asFinalData
#' @param df default: asData
#' @param originVar Variable to be graphed
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

createGraphData4OptionEdad <- function(df = asData,
                                       originVar,
                                       totalColumn = T){
  require(tidyverse)
  graphData <- df %>% select(originVar, Edad, ponde)
  graphData$y <- graphData[[originVar]]
  graphData <- na.omit(graphData)
  totalData <- graphData %>%
    summarise(val1 = prop.table(questionr::wtd.table(y, weights = ponde))[1]*100,
              val2 = prop.table(questionr::wtd.table(y, weights = ponde))[2]*100,
              val3 = prop.table(questionr::wtd.table(y, weights = ponde))[3]*100,
              val4 = prop.table(questionr::wtd.table(y, weights = ponde))[4]*100)
  totalData[["Edad"]] <- "Total"
  totalData <- totalData %>%  pivot_longer(cols = starts_with("val"),
                                           names_to = "Value",
                                           names_prefix = "val",
                                           values_to = "Porcentaje")
  totalData$Value <- totalData$Value %>% factor(levels = c(1:4),
                                                labels = names(table(df[[originVar]])),
                                                ordered = T)

  graphData <- graphData %>%
    group_by(Edad) %>%
    summarise(val1 = prop.table(questionr::wtd.table(y, weights = ponde))[1]*100,
              val2 = prop.table(questionr::wtd.table(y, weights = ponde))[2]*100,
              val3 = prop.table(questionr::wtd.table(y, weights = ponde))[3]*100,
              val4 = prop.table(questionr::wtd.table(y, weights = ponde))[4]*100)%>%
    pivot_longer(cols = starts_with("val"),
                 names_to = "Value",
                 names_prefix = "val",
                 values_to = "Porcentaje")
  graphData$Value <- graphData$Value %>% factor(levels = c(1:4),
                                                labels = names(table(df[[originVar]])),
                                                ordered = T)
  if (totalColumn){
    graphData <- bind_rows(graphData, totalData)
  }
  graphData
}
#  4Option Stack Educación Madre --------------------------------------------------------------------
#' Function to create data for educación madre stacked for graphs, final report as
#'
#' This function creates graphData to create stacked graph.
#' It assumes a 1-4 scale, values greater than 4 are missing.
#' It also assumes the dataStructure of the asData
#' @param df default: asData
#' @param originVar Variable to be graphed
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

createGraphData4OptionEducM <- function(df = asData,
                                        originVar,
                                        totalColumn = T){
  require(tidyverse)
  graphData <- df %>% select(originVar, q_EA_CA_13, ponde)
  graphData$y <- graphData[[originVar]]
  graphData <- na.omit(graphData)
  totalData <- graphData %>%
    summarise(val1 = prop.table(questionr::wtd.table(y, weights = ponde))[1]*100,
              val2 = prop.table(questionr::wtd.table(y, weights = ponde))[2]*100,
              val3 = prop.table(questionr::wtd.table(y, weights = ponde))[3]*100,
              val4 = prop.table(questionr::wtd.table(y, weights = ponde))[4]*100)
  totalData[["q_EA_CA_13"]] <- factor("Total",
                                      levels = c(levels(df[["q_EA_CA_13"]]),"Total"),
                                      labels = c(levels(df[["q_EA_CA_13"]]),"Total"),
                                      ordered = T)
  totalData <- totalData %>%  pivot_longer(cols = starts_with("val"),
                                           names_to = "Value",
                                           names_prefix = "val",
                                           values_to = "Porcentaje")
  totalData$Value <- totalData$Value %>% factor(levels = c(1:4),
                                                labels = names(table(df[[originVar]])),
                                                ordered = T)

  graphData <- graphData %>%
    group_by(q_EA_CA_13) %>%
    summarise(val1 = prop.table(questionr::wtd.table(y, weights = ponde))[1]*100,
              val2 = prop.table(questionr::wtd.table(y, weights = ponde))[2]*100,
              val3 = prop.table(questionr::wtd.table(y, weights = ponde))[3]*100,
              val4 = prop.table(questionr::wtd.table(y, weights = ponde))[4]*100)%>%
    pivot_longer(cols = starts_with("val"),
                 names_to = "Value",
                 names_prefix = "val",
                 values_to = "Porcentaje")
  graphData$Value <- graphData$Value %>% factor(levels = c(1:4),
                                                labels = names(table(df[[originVar]])),
                                                ordered = T)
  graphData[["q_EA_CA_13"]] <- graphData[["q_EA_CA_13"]] %>% factor(levels = c(levels(df[["q_EA_CA_13"]]),"Total"),
                                                                    labels = c(levels(df[["q_EA_CA_13"]]),"Total"),
                                                                    ordered = T)
  if (totalColumn){
    graphData <- bind_rows(graphData, totalData)
  }
  graphData
}
#  4Option Stack País --------------------------------------------------------------------
#' Function to create data for país stacked for graphs, final report as
#'
#' This function creates graphData to create stacked graph.
#' It assumes a 1-4 scale, values greater than 4 are missing.
#' It also assumes the dataStructure of the asData
#' @param df default: asData
#' @param originVar Variable to be graphed
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

createGraphData4OptionPais <- function(df = asData,
                                       originVar,
                                       totalColumn = T){
  require(tidyverse)
  graphData <- df %>% select(all_of(originVar), País, ponde)
  graphData$y <- graphData[[originVar]]
  graphData <- na.omit(graphData)
  totalData <- graphData %>%
    summarise(val1 = prop.table(questionr::wtd.table(y, weights = ponde))[1]*100,
              val2 = prop.table(questionr::wtd.table(y, weights = ponde))[2]*100,
              val3 = prop.table(questionr::wtd.table(y, weights = ponde))[3]*100,
              val4 = prop.table(questionr::wtd.table(y, weights = ponde))[4]*100)
  totalData[["País"]] <- factor("Total",
                                levels = c(levels(df[["País"]]),"Total"),
                                labels = c(levels(df[["País"]]),"Total"))
  totalData <- totalData %>%  pivot_longer(cols = starts_with("val"),
                                           names_to = "Value",
                                           names_prefix = "val",
                                           values_to = "Porcentaje")
  totalData$Value <- totalData$Value %>% factor(levels = c(1:4),
                                                labels = names(table(df[[originVar]])),
                                                ordered = T)
  graphData <- graphData %>%
    group_by(País) %>%
    summarise(val1 = prop.table(questionr::wtd.table(y, weights = ponde))[1]*100,
              val2 = prop.table(questionr::wtd.table(y, weights = ponde))[2]*100,
              val3 = prop.table(questionr::wtd.table(y, weights = ponde))[3]*100,
              val4 = prop.table(questionr::wtd.table(y, weights = ponde))[4]*100)%>%
    pivot_longer(cols = starts_with("val"),
                 names_to = "Value",
                 names_prefix = "val",
                 values_to = "Porcentaje")
  graphData$Value <- graphData$Value %>% factor(levels = c(1:4),
                                                labels = names(table(df[[originVar]])),
                                                ordered = T)
  graphData[["País"]] <- graphData[["País"]] %>% factor(levels = c(names(table(df[["País"]])),"Total"),
                                                        labels = c(names(table(df[["País"]])),"Total"))

  if (totalColumn){
    graphData <- bind_rows(graphData, totalData)
  }
  graphData
}
#  4Option Stack Educación --------------------------------------------------------------------
#' Function to create data for educación madre stacked for graphs, final report as
#'
#' This function creates graphData to create stacked graph.
#' It assumes a 1-4 scale, values greater than 4 are missing.
#' It also assumes the dataStructure of the asData
#' @param df default: asData
#' @param originVar Variable to be graphed
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

createGraphData4OptionEducP <- function(df = asData,
                                        originVar,
                                        totalColumn = T){
  require(tidyverse)
  graphData <- df %>% select(originVar, q_EA_CA_12, ponde)
  graphData$y <- graphData[[originVar]]
  graphData <- na.omit(graphData)
  totalData <- graphData %>%
    summarise(val1 = prop.table(questionr::wtd.table(y, weights = ponde))[1]*100,
              val2 = prop.table(questionr::wtd.table(y, weights = ponde))[2]*100,
              val3 = prop.table(questionr::wtd.table(y, weights = ponde))[3]*100,
              val4 = prop.table(questionr::wtd.table(y, weights = ponde))[4]*100)
  totalData[["q_EA_CA_12"]] <- factor("Total",
                                      levels = c(levels(df[["q_EA_CA_12"]]),"Total"),
                                      labels = c(levels(df[["q_EA_CA_12"]]),"Total"),
                                      ordered = T)
  totalData <- totalData %>%  pivot_longer(cols = starts_with("val"),
                                           names_to = "Value",
                                           names_prefix = "val",
                                           values_to = "Porcentaje")
  totalData$Value <- totalData$Value %>% factor(levels = c(1:4),
                                                labels = names(table(df[[originVar]])),
                                                ordered = T)

  graphData <- graphData %>%
    group_by(q_EA_CA_12) %>%
    summarise(val1 = prop.table(questionr::wtd.table(y, weights = ponde))[1]*100,
              val2 = prop.table(questionr::wtd.table(y, weights = ponde))[2]*100,
              val3 = prop.table(questionr::wtd.table(y, weights = ponde))[3]*100,
              val4 = prop.table(questionr::wtd.table(y, weights = ponde))[4]*100)%>%
    pivot_longer(cols = starts_with("val"),
                 names_to = "Value",
                 names_prefix = "val",
                 values_to = "Porcentaje")
  graphData$Value <- graphData$Value %>% factor(levels = c(1:4),
                                                labels = names(table(df[[originVar]])),
                                                ordered = T)
  graphData[["q_EA_CA_12"]] <- graphData[["q_EA_CA_12"]] %>% factor(levels = c(levels(df[["q_EA_CA_12"]]),"Total"),
                                                                    labels = c(levels(df[["q_EA_CA_12"]]),"Total"),
                                                                    ordered = T)
  if (totalColumn){
    graphData <- bind_rows(graphData, totalData)
  }
  graphData
}

#  3option Stack Gender --------------------------------------------------------------------
#' Function to create data for gender stacked for graphs, final report as
#'
#' This function creates graphData to create stacked graph.
#' It assumes a 1-3 scale, values greater than 3 are missing.
#' It also assumes the dataStructure of asData
#' @param df default: asData
#' @param originVar Variable to be graphed
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

createGraphData3OptionGen <- function(df = asData,
                                      originVar,
                                      totalColumn = T){
  require(tidyverse)
  graphData <- df %>% select(originVar, q_EA_CA_10, ponde)
  graphData$y <- graphData[[originVar]]
  graphData <- na.omit(graphData)
  totalData <- graphData %>%
    summarise(val1 = prop.table(questionr::wtd.table(y, weights = ponde))[1]*100,
              val2 = prop.table(questionr::wtd.table(y, weights = ponde))[2]*100,
              val3 = prop.table(questionr::wtd.table(y, weights = ponde))[3]*100)
  totalData[["q_EA_CA_10"]] <- "Total"
  totalData <- totalData %>%  pivot_longer(cols = starts_with("val"),
                                           names_to = "Value",
                                           names_prefix = "val",
                                           values_to = "Porcentaje")
  totalData$Value <- totalData$Value %>% factor(levels = c(1:3),
                                                labels = names(table(df[[originVar]])),
                                                ordered = T)
  graphData <- graphData %>%
    group_by(q_EA_CA_10) %>%
    summarise(val1 = prop.table(questionr::wtd.table(y, weights = ponde))[1]*100,
              val2 = prop.table(questionr::wtd.table(y, weights = ponde))[2]*100,
              val3 = prop.table(questionr::wtd.table(y, weights = ponde))[3]*100)%>%
    pivot_longer(cols = starts_with("val"),
                 names_to = "Value",
                 names_prefix = "val",
                 values_to = "Porcentaje")
  graphData$Value <- graphData$Value %>% factor(levels = c(1:3),
                                                labels = names(table(df[[originVar]])),
                                                ordered = T)
  if (totalColumn){
    graphData <- bind_rows(graphData, totalData)
  }
  graphData
}

#  3option Stack Edad --------------------------------------------------------------------
#' Function to create data for edad stacked for graphs, final report as
#'
#' This function creates graphData to create stacked graph.
#' It assumes a 1-3 scale, values greater than 3 are missing.
#' It also assumes the dataStructure of the asFinalData
#' @param df default: asData
#' @param originVar Variable to be graphed
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

createGraphData3OptionEdad <- function(df = asData,
                                       originVar,
                                       totalColumn = T){
  require(tidyverse)
  graphData <- df %>% select(originVar, Edad, ponde)
  graphData$y <- graphData[[originVar]]
  graphData <- na.omit(graphData)
  totalData <- graphData %>%
    summarise(val1 = prop.table(questionr::wtd.table(y, weights = ponde))[1]*100,
              val2 = prop.table(questionr::wtd.table(y, weights = ponde))[2]*100,
              val3 = prop.table(questionr::wtd.table(y, weights = ponde))[3]*100)
  totalData[["Edad"]] <- "Total"
  totalData <- totalData %>%  pivot_longer(cols = starts_with("val"),
                                           names_to = "Value",
                                           names_prefix = "val",
                                           values_to = "Porcentaje")
  totalData$Value <- totalData$Value %>% factor(levels = c(1:3),
                                                labels = names(table(df[[originVar]])),
                                                ordered = T)

  graphData <- graphData %>%
    group_by(Edad) %>%
    summarise(val1 = prop.table(questionr::wtd.table(y, weights = ponde))[1]*100,
              val2 = prop.table(questionr::wtd.table(y, weights = ponde))[2]*100,
              val3 = prop.table(questionr::wtd.table(y, weights = ponde))[3]*100)%>%
    pivot_longer(cols = starts_with("val"),
                 names_to = "Value",
                 names_prefix = "val",
                 values_to = "Porcentaje")
  graphData$Value <- graphData$Value %>% factor(levels = c(1:3),
                                                labels = names(table(df[[originVar]])),
                                                ordered = T)
  if (totalColumn){
    graphData <- bind_rows(graphData, totalData)
  }
  graphData
}
#  3Option Stack Educación Madre --------------------------------------------------------------------
#' Function to create data for educación madre stacked for graphs, final report as
#'
#' This function creates graphData to create stacked graph.
#' It assumes a 1-3 scale, values greater than 3 are missing.
#' It also assumes the dataStructure of the asData
#' @param df default: asData
#' @param originVar Variable to be graphed
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

createGraphData3OptionEducM <- function(df = asData,
                                        originVar,
                                        totalColumn = T){
  require(tidyverse)
  graphData <- df %>% select(originVar, q_EA_CA_13, ponde)
  graphData$y <- graphData[[originVar]]
  graphData <- na.omit(graphData)
  totalData <- graphData %>%
    summarise(val1 = prop.table(questionr::wtd.table(y, weights = ponde))[1]*100,
              val2 = prop.table(questionr::wtd.table(y, weights = ponde))[2]*100,
              val3 = prop.table(questionr::wtd.table(y, weights = ponde))[3]*100)
  totalData[["q_EA_CA_13"]] <- factor("Total",
                                      levels = c(levels(df[["q_EA_CA_13"]]),"Total"),
                                      labels = c(levels(df[["q_EA_CA_13"]]),"Total"),
                                      ordered = T)
  totalData <- totalData %>%  pivot_longer(cols = starts_with("val"),
                                           names_to = "Value",
                                           names_prefix = "val",
                                           values_to = "Porcentaje")
  totalData$Value <- totalData$Value %>% factor(levels = c(1:3),
                                                labels = names(table(df[[originVar]])),
                                                ordered = T)

  graphData <- graphData %>%
    group_by(q_EA_CA_13) %>%
    summarise(val1 = prop.table(questionr::wtd.table(y, weights = ponde))[1]*100,
              val2 = prop.table(questionr::wtd.table(y, weights = ponde))[2]*100,
              val3 = prop.table(questionr::wtd.table(y, weights = ponde))[3]*100)%>%
    pivot_longer(cols = starts_with("val"),
                 names_to = "Value",
                 names_prefix = "val",
                 values_to = "Porcentaje")
  graphData$Value <- graphData$Value %>% factor(levels = c(1:3),
                                                labels = names(table(df[[originVar]])),
                                                ordered = T)
  graphData[["q_EA_CA_13"]] <- graphData[["q_EA_CA_13"]] %>% factor(levels = c(levels(df[["q_EA_CA_13"]]),"Total"),
                                                                    labels = c(levels(df[["q_EA_CA_13"]]),"Total"),
                                                                    ordered = T)
  if (totalColumn){
    graphData <- bind_rows(graphData, totalData)
  }
  graphData
}
#  3Option Stack País --------------------------------------------------------------------
#' Function to create data for país stacked for graphs, final report as
#'
#' This function creates graphData to create stacked graph.
#' It assumes a 1-3 scale, values greater than 3 are missing.
#' It also assumes the dataStructure of the asData
#' @param df default: asData
#' @param originVar Variable to be graphed
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

createGraphData3OptionPais <- function(df = asData,
                                       originVar,
                                       totalColumn = T){
  require(tidyverse)
  graphData <- df %>% select(all_of(originVar), País, ponde)
  graphData$y <- graphData[[originVar]]
  graphData <- na.omit(graphData)
  totalData <- graphData %>%
    summarise(val1 = prop.table(questionr::wtd.table(y, weights = ponde))[1]*100,
              val2 = prop.table(questionr::wtd.table(y, weights = ponde))[2]*100,
              val3 = prop.table(questionr::wtd.table(y, weights = ponde))[3]*100)
  totalData[["País"]] <- factor("Total",
                                levels = c(levels(df[["País"]]),"Total"),
                                labels = c(levels(df[["País"]]),"Total"))
  totalData <- totalData %>%  pivot_longer(cols = starts_with("val"),
                                           names_to = "Value",
                                           names_prefix = "val",
                                           values_to = "Porcentaje")
  totalData$Value <- totalData$Value %>% factor(levels = c(1:3),
                                                labels = names(table(df[[originVar]])),
                                                ordered = T)
  graphData <- graphData %>%
    group_by(País) %>%
    summarise(val1 = prop.table(questionr::wtd.table(y, weights = ponde))[1]*100,
              val2 = prop.table(questionr::wtd.table(y, weights = ponde))[2]*100,
              val3 = prop.table(questionr::wtd.table(y, weights = ponde))[3]*100)%>%
    pivot_longer(cols = starts_with("val"),
                 names_to = "Value",
                 names_prefix = "val",
                 values_to = "Porcentaje")
  graphData$Value <- graphData$Value %>% factor(levels = c(1:3),
                                                labels = names(table(df[[originVar]])),
                                                ordered = T)
  graphData[["País"]] <- graphData[["País"]] %>% factor(levels = c(names(table(df[["País"]])),"Total"),
                                                        labels = c(names(table(df[["País"]])),"Total"))

  if (totalColumn){
    graphData <- bind_rows(graphData, totalData)
  }
  graphData
}
#  3Option Stack Educación --------------------------------------------------------------------
#' Function to create data for educación madre stacked for graphs, final report as
#'
#' This function creates graphData to create stacked graph.
#' It assumes a 1-3 scale, values greater than 3 are missing.
#' It also assumes the dataStructure of the asData
#' @param df default: asData
#' @param originVar Variable to be graphed
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

createGraphData3OptionEducP <- function(df = asData,
                                        originVar,
                                        totalColumn = T){
  require(tidyverse)
  graphData <- df %>% select(originVar, q_EA_CA_12, ponde)
  graphData$y <- graphData[[originVar]]
  graphData <- na.omit(graphData)
  totalData <- graphData %>%
    summarise(val1 = prop.table(questionr::wtd.table(y, weights = ponde))[1]*100,
              val2 = prop.table(questionr::wtd.table(y, weights = ponde))[2]*100,
              val3 = prop.table(questionr::wtd.table(y, weights = ponde))[3]*100)
  totalData[["q_EA_CA_12"]] <- factor("Total",
                                      levels = c(levels(df[["q_EA_CA_12"]]),"Total"),
                                      labels = c(levels(df[["q_EA_CA_12"]]),"Total"),
                                      ordered = T)
  totalData <- totalData %>%  pivot_longer(cols = starts_with("val"),
                                           names_to = "Value",
                                           names_prefix = "val",
                                           values_to = "Porcentaje")
  totalData$Value <- totalData$Value %>% factor(levels = c(1:3),
                                                labels = names(table(df[[originVar]])),
                                                ordered = T)

  graphData <- graphData %>%
    group_by(q_EA_CA_12) %>%
    summarise(val1 = prop.table(questionr::wtd.table(y, weights = ponde))[1]*100,
              val2 = prop.table(questionr::wtd.table(y, weights = ponde))[2]*100,
              val3 = prop.table(questionr::wtd.table(y, weights = ponde))[3]*100)%>%
    pivot_longer(cols = starts_with("val"),
                 names_to = "Value",
                 names_prefix = "val",
                 values_to = "Porcentaje")
  graphData$Value <- graphData$Value %>% factor(levels = c(1:3),
                                                labels = names(table(df[[originVar]])),
                                                ordered = T)
  graphData[["q_EA_CA_12"]] <- graphData[["q_EA_CA_12"]] %>% factor(levels = c(levels(df[["q_EA_CA_12"]]),"Total"),
                                                                    labels = c(levels(df[["q_EA_CA_12"]]),"Total"),
                                                                    ordered = T)
  if (totalColumn){
    graphData <- bind_rows(graphData, totalData)
  }
  graphData
}

#  2option Stack Gender --------------------------------------------------------------------
#' Function to create data for gender stacked for graphs, final report as
#'
#' This function creates graphData to create stacked graph.
#' It assumes a 1-2 scale, values greater than 2 are missing.
#' It also assumes the dataStructure of asData
#' @param df default: asData
#' @param originVar Variable to be graphed
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

createGraphData2OptionGen <- function(df = asData,
                                      originVar,
                                      totalColumn = T){
  require(tidyverse)
  graphData <- df %>% select(originVar, q_EA_CA_10, ponde)
  graphData$y <- graphData[[originVar]]
  graphData <- na.omit(graphData)
  totalData <- graphData %>%
    summarise(val1 = prop.table(questionr::wtd.table(y, weights = ponde))[1]*100,
              val2 = prop.table(questionr::wtd.table(y, weights = ponde))[2]*100)
  totalData[["q_EA_CA_10"]] <- "Total"
  totalData <- totalData %>%  pivot_longer(cols = starts_with("val"),
                                           names_to = "Value",
                                           names_prefix = "val",
                                           values_to = "Porcentaje")
  totalData$Value <- totalData$Value %>% factor(levels = c(1:2),
                                                labels = names(table(df[[originVar]])),
                                                ordered = T)
  graphData <- graphData %>%
    group_by(q_EA_CA_10) %>%
    summarise(val1 = prop.table(questionr::wtd.table(y, weights = ponde))[1]*100,
              val2 = prop.table(questionr::wtd.table(y, weights = ponde))[2]*100)%>%
    pivot_longer(cols = starts_with("val"),
                 names_to = "Value",
                 names_prefix = "val",
                 values_to = "Porcentaje")
  graphData$Value <- graphData$Value %>% factor(levels = c(1:2),
                                                labels = names(table(df[[originVar]])),
                                                ordered = T)
  if (totalColumn){
    graphData <- bind_rows(graphData, totalData)
  }
  graphData
}

#  2option Stack Edad --------------------------------------------------------------------
#' Function to create data for edad stacked for graphs, final report as
#'
#' This function creates graphData to create stacked graph.
#' It assumes a 1-2 scale, values greater than 2 are missing.
#' It also assumes the dataStructure of the asFinalData
#' @param df default: asData
#' @param originVar Variable to be graphed
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

createGraphData2OptionEdad <- function(df = asData,
                                       originVar,
                                       totalColumn = T){
  require(tidyverse)
  graphData <- df %>% select(originVar, Edad, ponde)
  graphData$y <- graphData[[originVar]]
  graphData <- na.omit(graphData)
  totalData <- graphData %>%
    summarise(val1 = prop.table(questionr::wtd.table(y, weights = ponde))[1]*100,
              val2 = prop.table(questionr::wtd.table(y, weights = ponde))[2]*100)
  totalData[["Edad"]] <- "Total"
  totalData <- totalData %>%  pivot_longer(cols = starts_with("val"),
                                           names_to = "Value",
                                           names_prefix = "val",
                                           values_to = "Porcentaje")
  totalData$Value <- totalData$Value %>% factor(levels = c(1:2),
                                                labels = names(table(df[[originVar]])),
                                                ordered = T)

  graphData <- graphData %>%
    group_by(Edad) %>%
    summarise(val1 = prop.table(questionr::wtd.table(y, weights = ponde))[1]*100,
              val2 = prop.table(questionr::wtd.table(y, weights = ponde))[2]*100)%>%
    pivot_longer(cols = starts_with("val"),
                 names_to = "Value",
                 names_prefix = "val",
                 values_to = "Porcentaje")
  graphData$Value <- graphData$Value %>% factor(levels = c(1:2),
                                                labels = names(table(df[[originVar]])),
                                                ordered = T)
  if (totalColumn){
    graphData <- bind_rows(graphData, totalData)
  }
  graphData
}
#  2Option Stack Educación Madre --------------------------------------------------------------------
#' Function to create data for educación madre stacked for graphs, final report as
#'
#' This function creates graphData to create stacked graph.
#' It assumes a 1-2 scale, values greater than 2 are missing.
#' It also assumes the dataStructure of the asData
#' @param df default: asData
#' @param originVar Variable to be graphed
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

createGraphData2OptionEducM <- function(df = asData,
                                        originVar,
                                        totalColumn = T){
  require(tidyverse)
  graphData <- df %>% select(originVar, q_EA_CA_13, ponde)
  graphData$y <- graphData[[originVar]]
  graphData <- na.omit(graphData)
  totalData <- graphData %>%
    summarise(val1 = prop.table(questionr::wtd.table(y, weights = ponde))[1]*100,
              val2 = prop.table(questionr::wtd.table(y, weights = ponde))[2]*100)
  totalData[["q_EA_CA_13"]] <- factor("Total",
                                      levels = c(levels(df[["q_EA_CA_13"]]),"Total"),
                                      labels = c(levels(df[["q_EA_CA_13"]]),"Total"),
                                      ordered = T)
  totalData <- totalData %>%  pivot_longer(cols = starts_with("val"),
                                           names_to = "Value",
                                           names_prefix = "val",
                                           values_to = "Porcentaje")
  totalData$Value <- totalData$Value %>% factor(levels = c(1:2),
                                                labels = names(table(df[[originVar]])),
                                                ordered = T)

  graphData <- graphData %>%
    group_by(q_EA_CA_13) %>%
    summarise(val1 = prop.table(questionr::wtd.table(y, weights = ponde))[1]*100,
              val2 = prop.table(questionr::wtd.table(y, weights = ponde))[2]*100)%>%
    pivot_longer(cols = starts_with("val"),
                 names_to = "Value",
                 names_prefix = "val",
                 values_to = "Porcentaje")
  graphData$Value <- graphData$Value %>% factor(levels = c(1:2),
                                                labels = names(table(df[[originVar]])),
                                                ordered = T)
  graphData[["q_EA_CA_13"]] <- graphData[["q_EA_CA_13"]] %>% factor(levels = c(levels(df[["q_EA_CA_13"]]),"Total"),
                                                                    labels = c(levels(df[["q_EA_CA_13"]]),"Total"),
                                                                    ordered = T)
  if (totalColumn){
    graphData <- bind_rows(graphData, totalData)
  }
  graphData
}
#  2Option Stack País --------------------------------------------------------------------
#' Function to create data for país stacked for graphs, final report as
#'
#' This function creates graphData to create stacked graph.
#' It assumes a 1-2 scale, values greater than 2 are missing.
#' It also assumes the dataStructure of the asData
#' @param df default: asData
#' @param originVar Variable to be graphed
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

createGraphData2OptionPais <- function(df = asData,
                                       originVar,
                                       totalColumn = T){
  require(tidyverse)
  graphData <- df %>% select(all_of(originVar), País, ponde)
  graphData$y <- graphData[[originVar]]
  graphData <- na.omit(graphData)
  totalData <- graphData %>%
    summarise(val1 = prop.table(questionr::wtd.table(y, weights = ponde))[1]*100,
              val2 = prop.table(questionr::wtd.table(y, weights = ponde))[2]*100)
  totalData[["País"]] <- factor("Total",
                                levels = c(levels(df[["País"]]),"Total"),
                                labels = c(levels(df[["País"]]),"Total"))
  totalData <- totalData %>%  pivot_longer(cols = starts_with("val"),
                                           names_to = "Value",
                                           names_prefix = "val",
                                           values_to = "Porcentaje")
  totalData$Value <- totalData$Value %>% factor(levels = c(1:2),
                                                labels = names(table(df[[originVar]])),
                                                ordered = T)
  graphData <- graphData %>%
    group_by(País) %>%
    summarise(val1 = prop.table(questionr::wtd.table(y, weights = ponde))[1]*100,
              val2 = prop.table(questionr::wtd.table(y, weights = ponde))[2]*100)%>%
    pivot_longer(cols = starts_with("val"),
                 names_to = "Value",
                 names_prefix = "val",
                 values_to = "Porcentaje")
  graphData$Value <- graphData$Value %>% factor(levels = c(1:2),
                                                labels = names(table(df[[originVar]])),
                                                ordered = T)
  graphData[["País"]] <- graphData[["País"]] %>% factor(levels = c(names(table(df[["País"]])),"Total"),
                                                        labels = c(names(table(df[["País"]])),"Total"))

  if (totalColumn){
    graphData <- bind_rows(graphData, totalData)
  }
  graphData
}
#  2Option Stack Educación --------------------------------------------------------------------
#' Function to create data for educación madre stacked for graphs, final report as
#'
#' This function creates graphData to create stacked graph.
#' It assumes a 1-2 scale, values greater than 2 are missing.
#' It also assumes the dataStructure of the asData
#' @param df default: asData
#' @param originVar Variable to be graphed
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

createGraphData2OptionEducP <- function(df = asData,
                                        originVar,
                                        totalColumn = T){
  require(tidyverse)
  graphData <- df %>% select(originVar, q_EA_CA_12, ponde)
  graphData$y <- graphData[[originVar]]
  graphData <- na.omit(graphData)
  totalData <- graphData %>%
    summarise(val1 = prop.table(questionr::wtd.table(y, weights = ponde))[1]*100,
              val2 = prop.table(questionr::wtd.table(y, weights = ponde))[2]*100)
  totalData[["q_EA_CA_12"]] <- factor("Total",
                                      levels = c(levels(df[["q_EA_CA_12"]]),"Total"),
                                      labels = c(levels(df[["q_EA_CA_12"]]),"Total"),
                                      ordered = T)
  totalData <- totalData %>%  pivot_longer(cols = starts_with("val"),
                                           names_to = "Value",
                                           names_prefix = "val",
                                           values_to = "Porcentaje")
  totalData$Value <- totalData$Value %>% factor(levels = c(1:2),
                                                labels = names(table(df[[originVar]])),
                                                ordered = T)

  graphData <- graphData %>%
    group_by(q_EA_CA_12) %>%
    summarise(val1 = prop.table(questionr::wtd.table(y, weights = ponde))[1]*100,
              val2 = prop.table(questionr::wtd.table(y, weights = ponde))[2]*100)%>%
    pivot_longer(cols = starts_with("val"),
                 names_to = "Value",
                 names_prefix = "val",
                 values_to = "Porcentaje")
  graphData$Value <- graphData$Value %>% factor(levels = c(1:2),
                                                labels = names(table(df[[originVar]])),
                                                ordered = T)
  graphData[["q_EA_CA_12"]] <- graphData[["q_EA_CA_12"]] %>% factor(levels = c(levels(df[["q_EA_CA_12"]]),"Total"),
                                                                    labels = c(levels(df[["q_EA_CA_12"]]),"Total"),
                                                                    ordered = T)
  if (totalColumn){
    graphData <- bind_rows(graphData, totalData)
  }
  graphData
}

