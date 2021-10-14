# createDf4GraphsCuidado.R
# Description: Set of functions to prepare CCV microdata to create a graph 
# Created by: Gabriel N. Camargo-Toledo
# Created on: Apr/19/2021			
# Modified by: Gabriel N. Camargo-Toledo
# Modified on: May/06/2021
# Contact: gcamargo@sensata.io
# Sensata Asus VivoBook PopOs! 20.04 8gb Ram R4.0.4

# Likert Stack Gender --------------------------------------------------------------------
#' Function to create data for gender stacked for graphs, final report CCV
#'
#' This function creates graphData to create stacked graph. 
#' It assumes a 1-5 scale, values greater than 5 are missing.
#' It also assumes the dataStructure of cuidData
#' @param df default: cuidData
#' @param originVar Variable to be graphed
#' 
#' @author Gabriel N. Camargo-Toledo \email{gcamargo@@sensata.io}
#' @return Dataframe for graphs 
#' @keywords sensata microdata metadata data-cleaning
#' @import tidyverse
#'
#' @examples
#' TBD
#' @export

createGraphDataStackedGen <- function(df = cuidData, 
                                        originVar){
  require(tidyverse)
  graphData <- df %>% select(originVar, q_QT_13, ponde) 
  graphData$y <- graphData[[originVar]]
  graphData <- na.omit(graphData)
  totalData <- graphData %>%
    summarise(val1 = prop.table(wtd.table(y, weights = ponde))[1]*100,
              val2 = prop.table(wtd.table(y, weights = ponde))[2]*100,
              val3 = prop.table(wtd.table(y, weights = ponde))[3]*100,
              val4 = prop.table(wtd.table(y, weights = ponde))[4]*100,
              val5 = prop.table(wtd.table(y, weights = ponde))[5]*100)
  totalData[["q_QT_13"]] <- "Total"
  totalData <- totalData %>%  pivot_longer(cols = starts_with("val"), 
                                           names_to = "Value", 
                                           names_prefix = "val",
                                           values_to = "Porcentaje")
  totalData$Value <- totalData$Value %>% factor(levels = c(1:6),
                                                labels = names(table(df[[originVar]])),
                                                ordered = T)
  graphData <- graphData %>%
    group_by(q_QT_13) %>%
    summarise(val1 = prop.table(wtd.table(y, weights = ponde))[1]*100,
              val2 = prop.table(wtd.table(y, weights = ponde))[2]*100,
              val3 = prop.table(wtd.table(y, weights = ponde))[3]*100,
              val4 = prop.table(wtd.table(y, weights = ponde))[4]*100,
              val5 = prop.table(wtd.table(y, weights = ponde))[5]*100)%>%
    pivot_longer(cols = starts_with("val"), 
                 names_to = "Value", 
                 names_prefix = "val",
                 values_to = "Porcentaje")
  graphData$Value <- graphData$Value %>% factor(levels = c(1:6),
                                                labels = names(table(df[[originVar]])),
                                                ordered = T)
  graphData <- bind_rows(graphData, totalData)
  graphData
}

# Likert Stack Edad --------------------------------------------------------------------
#' Function to create data for edad stacked for graphs, final report CCV
#'
#' This function creates graphData to create stacked graph. 
#' It assumes a 1-5 scale, values greater than 5 are missing.
#' It also assumes the dataStructure of the CCVFinalData
#' @param df default: cuidData
#' @param originVar Variable to be graphed
#' 
#' @author Gabriel N. Camargo-Toledo \email{gcamargo@@sensata.io}
#' @return Dataframe for graphs 
#' @keywords sensata microdata metadata data-cleaning
#' @import tidyverse
#'
#' @examples
#' TBD
#' @export

createGraphDataStackedEdad <- function(df = cuidData, 
                                      originVar){
  require(tidyverse)
  graphData <- df %>% select(originVar, Edad, ponde) 
  graphData$y <- graphData[[originVar]]
  graphData <- na.omit(graphData)
  totalData <- graphData %>%
    summarise(val1 = prop.table(wtd.table(y, weights = ponde))[1]*100,
              val2 = prop.table(wtd.table(y, weights = ponde))[2]*100,
              val3 = prop.table(wtd.table(y, weights = ponde))[3]*100,
              val4 = prop.table(wtd.table(y, weights = ponde))[4]*100,
              val5 = prop.table(wtd.table(y, weights = ponde))[5]*100)
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
    summarise(val1 = prop.table(wtd.table(y, weights = ponde))[1]*100,
              val2 = prop.table(wtd.table(y, weights = ponde))[2]*100,
              val3 = prop.table(wtd.table(y, weights = ponde))[3]*100,
              val4 = prop.table(wtd.table(y, weights = ponde))[4]*100,
              val5 = prop.table(wtd.table(y, weights = ponde))[5]*100)%>%
    pivot_longer(cols = starts_with("val"), 
                 names_to = "Value", 
                 names_prefix = "val",
                 values_to = "Porcentaje")
  graphData$Value <- graphData$Value %>% factor(levels = c(1:5),
                                                labels = names(table(df[[originVar]])),
                                                ordered = T)
  graphData <- bind_rows(graphData, totalData)
  graphData
}
# Likert Stack Educación Madre --------------------------------------------------------------------
#' Function to create data for educación madre stacked for graphs, final report CCV
#'
#' This function creates graphData to create stacked graph. 
#' It assumes a 1-5 scale, values greater than 5 are missing.
#' It also assumes the dataStructure of the cuidData
#' @param df default: cuidData
#' @param originVar Variable to be graphed
#' 
#' @author Gabriel N. Camargo-Toledo \email{gcamargo@@sensata.io}
#' @return Dataframe for graphs 
#' @keywords sensata microdata metadata data-cleaning
#' @import tidyverse
#'
#' @examples
#' TBD
#' @export

createGraphDataStackedEduc <- function(df = cuidData, 
                                       originVar){
  require(tidyverse)
  graphData <- df %>% select(originVar, q_QT_15, ponde) 
  graphData$y <- graphData[[originVar]]
  graphData <- na.omit(graphData)
  totalData <- graphData %>%
    summarise(val1 = prop.table(wtd.table(y, weights = ponde))[1]*100,
              val2 = prop.table(wtd.table(y, weights = ponde))[2]*100,
              val3 = prop.table(wtd.table(y, weights = ponde))[3]*100,
              val4 = prop.table(wtd.table(y, weights = ponde))[4]*100,
              val5 = prop.table(wtd.table(y, weights = ponde))[5]*100)
  totalData[["q_QT_15"]] <- factor("Total",
                                   levels = c(levels(df[["q_QT_15"]]),"Total"),
                                   labels = c(levels(df[["q_QT_15"]]),"Total"),
                                   ordered = T)
  totalData <- totalData %>%  pivot_longer(cols = starts_with("val"), 
                                           names_to = "Value", 
                                           names_prefix = "val",
                                           values_to = "Porcentaje")
  totalData$Value <- totalData$Value %>% factor(levels = c(1:5),
                                                labels = names(table(df[[originVar]])),
                                                ordered = T)
  
  graphData <- graphData %>%
    group_by(q_QT_15) %>%
    summarise(val1 = prop.table(wtd.table(y, weights = ponde))[1]*100,
              val2 = prop.table(wtd.table(y, weights = ponde))[2]*100,
              val3 = prop.table(wtd.table(y, weights = ponde))[3]*100,
              val4 = prop.table(wtd.table(y, weights = ponde))[4]*100,
              val5 = prop.table(wtd.table(y, weights = ponde))[5]*100)%>%
    pivot_longer(cols = starts_with("val"), 
                 names_to = "Value", 
                 names_prefix = "val",
                 values_to = "Porcentaje")
  graphData$Value <- graphData$Value %>% factor(levels = c(1:5),
                                                labels = names(table(df[[originVar]])),
                                                ordered = T)
  graphData[["q_QT_15"]] <- graphData[["q_QT_15"]] %>% factor(levels = c(levels(df[["q_QT_15"]]),"Total"),
                                                              labels = c(levels(df[["q_QT_15"]]),"Total"),
                                                              ordered = T)
  graphData <- bind_rows(graphData, totalData)
  graphData
}
# Likert Stack País --------------------------------------------------------------------
#' Function to create data for país stacked for graphs, final report CCV
#'
#' This function creates graphData to create stacked graph. 
#' It assumes a 1-5 scale, values greater than 5 are missing.
#' It also assumes the dataStructure of the cuidData
#' @param df default: cuidData
#' @param originVar Variable to be graphed
#' 
#' @author Gabriel N. Camargo-Toledo \email{gcamargo@@sensata.io}
#' @return Dataframe for graphs 
#' @keywords sensata microdata metadata data-cleaning
#' @import tidyverse
#'
#' @examples
#' TBD
#' @export

createGraphDataStackedPais <- function(df = cuidData, 
                                       originVar){
  require(tidyverse)
  graphData <- df %>% select(all_of(originVar), Pais, ponde)
  graphData$y <- graphData[[originVar]]
  graphData <- na.omit(graphData)
  totalData <- graphData %>%
    summarise(val1 = prop.table(wtd.table(y, weights = ponde))[1]*100,
              val2 = prop.table(wtd.table(y, weights = ponde))[2]*100,
              val3 = prop.table(wtd.table(y, weights = ponde))[3]*100,
              val4 = prop.table(wtd.table(y, weights = ponde))[4]*100,
              val5 = prop.table(wtd.table(y, weights = ponde))[5]*100)
  totalData[["Pais"]] <- factor("Total",
                                levels = c(levels(df[["Pais"]]),"Total"),
                                labels = c(levels(df[["Pais"]]),"Total"))
  totalData <- totalData %>%  pivot_longer(cols = starts_with("val"), 
                                           names_to = "Value", 
                                           names_prefix = "val",
                                           values_to = "Porcentaje")
  totalData$Value <- totalData$Value %>% factor(levels = c(1:5),
                                                labels = names(table(df[[originVar]])),
                                                ordered = T)
  graphData <- graphData %>%
    group_by(Pais) %>%
    summarise(val1 = prop.table(wtd.table(y, weights = ponde))[1]*100,
              val2 = prop.table(wtd.table(y, weights = ponde))[2]*100,
              val3 = prop.table(wtd.table(y, weights = ponde))[3]*100,
              val4 = prop.table(wtd.table(y, weights = ponde))[4]*100,
              val5 = prop.table(wtd.table(y, weights = ponde))[5]*100)%>%
    pivot_longer(cols = starts_with("val"),
                 names_to = "Value",
                 names_prefix = "val",
                 values_to = "Porcentaje")
  graphData$Value <- graphData$Value %>% factor(levels = c(1:5),
                                                labels = names(table(df[[originVar]])),
                                                ordered = T)
  graphData[["Pais"]] <- graphData[["Pais"]] %>% factor(levels = c(names(table(df[["Pais"]])),"Total"),
                                                        labels = c(names(table(df[["Pais"]])),"Total"))
  
  graphData <- bind_rows(graphData, totalData)
  graphData
}
# Likert Stack Número de hijos --------------------------------------------------------------------
#' Function to create data for número de hijos stacked for graphs, final report CCV
#'
#' This function creates graphData to create stacked graph. 
#' It assumes a 1-5 scale, values greater than 5 are missing.
#' It also assumes the dataStructure of the cuidData
#' @param df default: cuidData
#' @param originVar Variable to be graphed
#' 
#' @author Gabriel N. Camargo-Toledo \email{gcamargo@@sensata.io}
#' @return Dataframe for graphs 
#' @keywords sensata microdata metadata data-cleaning
#' @import tidyverse
#'
#' @examples
#' TBD
#' @export

createGraphDataStackedHijos <- function(df = cuidData, 
                                       originVar){
  require(tidyverse)
  graphData <- cuidData %>% select(originVar, q_QT_02, ponde)
  graphData$y <- graphData[["originVar"]]
  graphData <- na.omit(graphData)
  totalData <- graphData %>%
    summarise(val1 = prop.table(wtd.table(y, weights = ponde))[1]*100,
              val2 = prop.table(wtd.table(y, weights = ponde))[2]*100,
              val3 = prop.table(wtd.table(y, weights = ponde))[3]*100,
              val4 = prop.table(wtd.table(y, weights = ponde))[4]*100,
              val5 = prop.table(wtd.table(y, weights = ponde))[5]*100)
  totalData[["q_QT_02"]] <- factor("Total",
                                   levels = c(levels(cuidData[["q_QT_02"]]),"Total"),
                                   labels = c(levels(cuidData[["q_QT_02"]]),"Total"),
                                   ordered = T)
  totalData <- totalData %>%  pivot_longer(cols = starts_with("val"), 
                                           names_to = "Value", 
                                           names_prefix = "val",
                                           values_to = "Porcentaje")
  totalData$Value <- totalData$Value %>% factor(levels = c(1:5),
                                                labels = names(table(cuidData[["originVar"]])),
                                                ordered = T)
  graphData <- graphData %>%
    group_by(q_QT_02) %>%
    summarise(val1 = prop.table(wtd.table(y, weights = ponde))[1]*100,
              val2 = prop.table(wtd.table(y, weights = ponde))[2]*100,
              val3 = prop.table(wtd.table(y, weights = ponde))[3]*100,
              val4 = prop.table(wtd.table(y, weights = ponde))[4]*100,
              val5 = prop.table(wtd.table(y, weights = ponde))[5]*100)%>%
    pivot_longer(cols = starts_with("val"),
                 names_to = "Value",
                 names_prefix = "val",
                 values_to = "Porcentaje")
  graphData$Value <- graphData$Value %>% factor(levels = c(1:5),
                                                labels = names(table(cuidData[["originVar"]])),
                                                ordered = T)
  graphData[["q_QT_02"]] <- graphData[["q_QT_02"]] %>% factor(levels = c(levels(cuidData[["q_QT_02"]]),"Total"),
                                                              labels = c(levels(cuidData[["q_QT_02"]]),"Total"),
                                                              ordered = T)
  
  graphData <- bind_rows(graphData, totalData)
  graphData
}

# Likert Stack Educación --------------------------------------------------------------------
#' Function to create data for educación madre stacked for graphs, final report CCV
#'
#' This function creates graphData to create stacked graph. 
#' It assumes a 1-5 scale, values greater than 5 are missing.
#' It also assumes the dataStructure of the cuidData
#' @param df default: cuidData
#' @param originVar Variable to be graphed
#' 
#' @author Gabriel N. Camargo-Toledo \email{gcamargo@@sensata.io}
#' @return Dataframe for graphs 
#' @keywords sensata microdata metadata data-cleaning
#' @import tidyverse
#'
#' @examples
#' TBD
#' @export

createGraphDataStackedEducI <- function(df = cuidData, 
                                       originVar){
  require(tidyverse)
  graphData <- df %>% select(originVar, q_QT_14, ponde) 
  graphData$y <- graphData[[originVar]]
  graphData <- na.omit(graphData)
  totalData <- graphData %>%
    summarise(val1 = prop.table(wtd.table(y, weights = ponde))[1]*100,
              val2 = prop.table(wtd.table(y, weights = ponde))[2]*100,
              val3 = prop.table(wtd.table(y, weights = ponde))[3]*100,
              val4 = prop.table(wtd.table(y, weights = ponde))[4]*100,
              val5 = prop.table(wtd.table(y, weights = ponde))[5]*100)
  totalData[["q_QT_14"]] <- factor("Total",
                                   levels = c(levels(df[["q_QT_14"]]),"Total"),
                                   labels = c(levels(df[["q_QT_14"]]),"Total"),
                                   ordered = T)
  totalData <- totalData %>%  pivot_longer(cols = starts_with("val"), 
                                           names_to = "Value", 
                                           names_prefix = "val",
                                           values_to = "Porcentaje")
  totalData$Value <- totalData$Value %>% factor(levels = c(1:5),
                                                labels = names(table(df[[originVar]])),
                                                ordered = T)
  
  graphData <- graphData %>%
    group_by(q_QT_14) %>%
    summarise(val1 = prop.table(wtd.table(y, weights = ponde))[1]*100,
              val2 = prop.table(wtd.table(y, weights = ponde))[2]*100,
              val3 = prop.table(wtd.table(y, weights = ponde))[3]*100,
              val4 = prop.table(wtd.table(y, weights = ponde))[4]*100,
              val5 = prop.table(wtd.table(y, weights = ponde))[5]*100)%>%
    pivot_longer(cols = starts_with("val"), 
                 names_to = "Value", 
                 names_prefix = "val",
                 values_to = "Porcentaje")
  graphData$Value <- graphData$Value %>% factor(levels = c(1:5),
                                                labels = names(table(df[[originVar]])),
                                                ordered = T)
  graphData[["q_QT_14"]] <- graphData[["q_QT_14"]] %>% factor(levels = c(levels(df[["q_QT_14"]]),"Total"),
                                                              labels = c(levels(df[["q_QT_14"]]),"Total"),
                                                              ordered = T)
  graphData <- bind_rows(graphData, totalData)
  graphData
}


# Likert Stack Genero --------------------------------------------------------------------
#' Function to create data for edad stacked for graphs, final report CCV
#'
#' This function creates graphData to create stacked graph. 
#' It assumes a 1-5 scale, values greater than 5 are missing.
#' It also assumes the dataStructure of the CCVFinalData
#' @param df default: cuidData
#' @param originVar Variable to be graphed
#' 
#' @author Gabriel N. Camargo-Toledo \email{gcamargo@@sensata.io}
#' @return Dataframe for graphs 
#' @keywords sensata microdata metadata data-cleaning
#' @import tidyverse
#'
#' @examples
#' TBD
#' @export

createGraphDataStackedGenero <- function(df = cuidData, 
                                       originVar){
  require(tidyverse)
  graphData <- df %>% select(originVar, q_QT_13, ponde) 
  graphData$y <- graphData[[originVar]]
  graphData <- na.omit(graphData)
  totalData <- graphData %>%
    summarise(val1 = prop.table(wtd.table(y, weights = ponde))[1]*100,
              val2 = prop.table(wtd.table(y, weights = ponde))[2]*100,
              val3 = prop.table(wtd.table(y, weights = ponde))[3]*100,
              val4 = prop.table(wtd.table(y, weights = ponde))[4]*100,
              val5 = prop.table(wtd.table(y, weights = ponde))[5]*100)
  totalData[["q_QT_13"]] <- factor("Total",
                                   levels = c(levels(df[["q_QT_13"]]),"Total"),
                                   labels = c(levels(df[["q_QT_13"]]),"Total"),
                                   ordered = T)
  totalData <- totalData %>%  pivot_longer(cols = starts_with("val"), 
                                           names_to = "Value", 
                                           names_prefix = "val",
                                           values_to = "Porcentaje")
  totalData$Value <- totalData$Value %>% factor(levels = c(1:5),
                                                labels = names(table(df[[originVar]])),
                                                ordered = T)
  
  graphData <- graphData %>%
    group_by(q_QT_13) %>%
    summarise(val1 = prop.table(wtd.table(y, weights = ponde))[1]*100,
              val2 = prop.table(wtd.table(y, weights = ponde))[2]*100,
              val3 = prop.table(wtd.table(y, weights = ponde))[3]*100,
              val4 = prop.table(wtd.table(y, weights = ponde))[4]*100,
              val5 = prop.table(wtd.table(y, weights = ponde))[5]*100)%>%
    pivot_longer(cols = starts_with("val"), 
                 names_to = "Value", 
                 names_prefix = "val",
                 values_to = "Porcentaje")
  graphData$Value <- graphData$Value %>% factor(levels = c(1:5),
                                                labels = names(table(df[[originVar]])),
                                                ordered = T)
  graphData[["q_QT_13"]] <- graphData[["q_QT_13"]] %>% factor(levels = c(levels(df[["q_QT_13"]]),"Total"),
                                                              labels = c(levels(df[["q_QT_13"]]),"Total"),
                                                              ordered = T)
  graphData <- bind_rows(graphData, totalData)
  graphData
}
