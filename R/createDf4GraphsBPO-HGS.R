# createDf4GraphsBPO.R
# Description: Set of functions to prepare AS microdata to create a graph 
# Created by: Juan Pablo Castro
# Created on: Sep/17/2021			


#  7option Stack Gen--------------------------------------------------------------------
#' Function to create data for age stacked for graphs, final report as
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

createGraphData7OptionGen <- function(df = asData, 
                                      originVar,
                                      totalColumn = T){
  require(tidyverse)
  graphData <- df %>% select(originVar, q_AB_CA_03) 
  graphData$y <- graphData[[originVar]]
  graphData <- na.omit(graphData)
  totalData <- graphData %>%
    summarise(val1 = prop.table(wtd.table(y))[1]*100,
              val2 = prop.table(wtd.table(y))[2]*100,
              val3 = prop.table(wtd.table(y))[3]*100,
              val4 = prop.table(wtd.table(y))[4]*100,
              val5 = prop.table(wtd.table(y))[5]*100,
              val6 = prop.table(wtd.table(y))[6]*100,
              val7 = prop.table(wtd.table(y))[7]*100)
  totalData[["q_AB_CA_03"]] <- "Total"
  totalData <- totalData %>%  pivot_longer(cols = starts_with("val"), 
                                           names_to = "Value", 
                                           names_prefix = "val",
                                           values_to = "Porcentaje")
  totalData$Value <- totalData$Value %>% factor(levels = c(1:7),
                                                labels = names(table(df[[originVar]])),
                                                ordered = T)
  
  graphData <- graphData %>%
    group_by(age) %>%
    summarise(val1 = prop.table(wtd.table(y))[1]*100,
              val2 = prop.table(wtd.table(y))[2]*100,
              val3 = prop.table(wtd.table(y))[3]*100,
              val4 = prop.table(wtd.table(y))[4]*100,
              val5 = prop.table(wtd.table(y))[5]*100,
              val6 = prop.table(wtd.table(y))[6]*100,
              val7 = prop.table(wtd.table(y))[7]*100)%>%
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


#  7option Stack age --------------------------------------------------------------------
#' Function to create data for age stacked for graphs, final report as
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

createGraphData7OptionAge <- function(df = asData, 
                                       originVar,
                                       totalColumn = T){
  require(tidyverse)
  graphData <- df %>% select(originVar, age) 
  graphData$y <- graphData[[originVar]]
  graphData <- na.omit(graphData)
  totalData <- graphData %>%
    summarise(val1 = prop.table(wtd.table(y))[1]*100,
              val2 = prop.table(wtd.table(y))[2]*100,
              val3 = prop.table(wtd.table(y))[3]*100,
              val4 = prop.table(wtd.table(y))[4]*100,
              val5 = prop.table(wtd.table(y))[5]*100,
              val6 = prop.table(wtd.table(y))[6]*100,
              val7 = prop.table(wtd.table(y))[7]*100)
  totalData[["age"]] <- "Total"
  totalData <- totalData %>%  pivot_longer(cols = starts_with("val"), 
                                           names_to = "Value", 
                                           names_prefix = "val",
                                           values_to = "Porcentaje")
  totalData$Value <- totalData$Value %>% factor(levels = c(1:7),
                                                labels = names(table(df[[originVar]])),
                                                ordered = T)
  
  graphData <- graphData %>%
    group_by(age) %>%
    summarise(val1 = prop.table(wtd.table(y))[1]*100,
              val2 = prop.table(wtd.table(y))[2]*100,
              val3 = prop.table(wtd.table(y))[3]*100,
              val4 = prop.table(wtd.table(y))[4]*100,
              val5 = prop.table(wtd.table(y))[5]*100,
              val6 = prop.table(wtd.table(y))[6]*100,
              val7 = prop.table(wtd.table(y))[7]*100)%>%
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

createGraphData7OptionEnglishLevel <- function(df = asData, 
                                        originVar,
                                        totalColumn = T){
  require(tidyverse)
  graphData <- df %>% select(originVar, q_AB_NI_01) 
  graphData$y <- graphData[[originVar]]
  graphData <- na.omit(graphData)
  totalData <- graphData %>%
    summarise(val1 = prop.table(wtd.table(y))[1]*100,
              val2 = prop.table(wtd.table(y))[2]*100,
              val3 = prop.table(wtd.table(y))[3]*100,
              val4 = prop.table(wtd.table(y))[4]*100,
              val5 = prop.table(wtd.table(y))[5]*100,
              val6 = prop.table(wtd.table(y))[6]*100,
              val7 = prop.table(wtd.table(y))[7]*100)
  totalData[["q_AB_NI_01"]] <- factor("Total",
                                      levels = c(levels(df[["q_AB_NI_01"]]),"Total"),
                                      labels = c(levels(df[["q_AB_NI_01"]]),"Total"),
                                      ordered = T)
  totalData <- totalData %>%  pivot_longer(cols = starts_with("val"), 
                                           names_to = "Value", 
                                           names_prefix = "val",
                                           values_to = "Porcentaje")
  totalData$Value <- totalData$Value %>% factor(levels = c(1:7),
                                                labels = names(table(df[[originVar]])),
                                                ordered = T)
  
  graphData <- graphData %>%
    group_by(q_AB_NI_01) %>%
    summarise(val1 = prop.table(wtd.table(y))[1]*100,
              val2 = prop.table(wtd.table(y))[2]*100,
              val3 = prop.table(wtd.table(y))[3]*100,
              val4 = prop.table(wtd.table(y))[4]*100,
              val5 = prop.table(wtd.table(y))[5]*100,
              val6 = prop.table(wtd.table(y))[6]*100,
              val7 = prop.table(wtd.table(y))[7]*100)%>%
    pivot_longer(cols = starts_with("val"), 
                 names_to = "Value", 
                 names_prefix = "val",
                 values_to = "Porcentaje")
  graphData$Value <- graphData$Value %>% factor(levels = c(1:7),
                                                labels = names(table(df[[originVar]])),
                                                ordered = T)
  graphData[["q_AB_NI_01"]] <- graphData[["q_AB_NI_01"]] %>% factor(levels = c(levels(df[["q_AB_NI_01"]]),"Total"),
                                                                    labels = c(levels(df[["q_AB_NI_01"]]),"Total"),
                                                                    ordered = T)
  if (totalColumn){
    graphData <- bind_rows(graphData, totalData)  
  }
  graphData
}
#  7option Stack q_AB_CA_01 --------------------------------------------------------------------
#' Function to create data for q_AB_CA_01 stacked for graphs, final report as
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

createGraphData7OptionBilingualAgent <- function(df = asData, 
                                       originVar,
                                       totalColumn = T){
  require(tidyverse)
  graphData <- df %>% select(all_of(originVar), q_AB_CA_01)
  graphData$y <- graphData[[originVar]]
  graphData <- na.omit(graphData)
  totalData <- graphData %>%
    summarise(val1 = prop.table(wtd.table(y))[1]*100,
              val2 = prop.table(wtd.table(y))[2]*100,
              val3 = prop.table(wtd.table(y))[3]*100,
              val4 = prop.table(wtd.table(y))[4]*100,
              val5 = prop.table(wtd.table(y))[5]*100,
              val6 = prop.table(wtd.table(y))[6]*100,
              val7 = prop.table(wtd.table(y))[7]*100)
  totalData[["q_AB_CA_01"]] <- factor("Total",
                                levels = c(levels(df[["q_AB_CA_01"]]),"Total"),
                                labels = c(levels(df[["q_AB_CA_01"]]),"Total"))
  totalData <- totalData %>%  pivot_longer(cols = starts_with("val"), 
                                           names_to = "Value", 
                                           names_prefix = "val",
                                           values_to = "Porcentaje")
  totalData$Value <- totalData$Value %>% factor(levels = c(1:7),
                                                labels = names(table(df[[originVar]])),
                                                ordered = T)
  graphData <- graphData %>%
    group_by(q_AB_CA_01) %>%
    summarise(val1 = prop.table(wtd.table(y))[1]*100,
              val2 = prop.table(wtd.table(y))[2]*100,
              val3 = prop.table(wtd.table(y))[3]*100,
              val4 = prop.table(wtd.table(y))[4]*100,
              val5 = prop.table(wtd.table(y))[5]*100,
              val6 = prop.table(wtd.table(y))[6]*100,
              val7 = prop.table(wtd.table(y))[7]*100)%>%
    pivot_longer(cols = starts_with("val"),
                 names_to = "Value",
                 names_prefix = "val",
                 values_to = "Porcentaje")
  graphData$Value <- graphData$Value %>% factor(levels = c(1:7),
                                                labels = names(table(df[[originVar]])),
                                                ordered = T)
  graphData[["q_AB_CA_01"]] <- graphData[["q_AB_CA_01"]] %>% factor(levels = c(names(table(df[["q_AB_CA_01"]])),"Total"),
                                                        labels = c(names(table(df[["q_AB_CA_01"]])),"Total"))
  
  if (totalColumn){
    graphData <- bind_rows(graphData, totalData)  
  }
  graphData
}
#  7option Stack Estrato --------------------------------------------------------------------
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

createGraphData7OptionEstrato <- function(df = asData, 
                                        originVar,
                                        totalColumn = T){
  require(tidyverse)
  graphData <- df %>% select(originVar, q_AB_CA_08) 
  graphData$y <- graphData[[originVar]]
  graphData <- na.omit(graphData)
  totalData <- graphData %>%
    summarise(val1 = prop.table(wtd.table(y))[1]*100,
              val2 = prop.table(wtd.table(y))[2]*100,
              val3 = prop.table(wtd.table(y))[3]*100,
              val4 = prop.table(wtd.table(y))[4]*100,
              val5 = prop.table(wtd.table(y))[5]*100,
              val6 = prop.table(wtd.table(y))[6]*100,
              val7 = prop.table(wtd.table(y))[7]*100)
  totalData[["q_AB_CA_08"]] <- factor("Total",
                                      levels = c(levels(df[["q_AB_CA_08"]]),"Total"),
                                      labels = c(levels(df[["q_AB_CA_08"]]),"Total"),
                                      ordered = T)
  totalData <- totalData %>%  pivot_longer(cols = starts_with("val"), 
                                           names_to = "Value", 
                                           names_prefix = "val",
                                           values_to = "Porcentaje")
  totalData$Value <- totalData$Value %>% factor(levels = c(1:7),
                                                labels = names(table(df[[originVar]])),
                                                ordered = T)
  
  graphData <- graphData %>%
    group_by(q_AB_CA_05) %>%
    summarise(val1 = prop.table(wtd.table(y))[1]*100,
              val2 = prop.table(wtd.table(y))[2]*100,
              val3 = prop.table(wtd.table(y))[3]*100,
              val4 = prop.table(wtd.table(y))[4]*100,
              val5 = prop.table(wtd.table(y))[5]*100,
              val6 = prop.table(wtd.table(y))[6]*100,
              val7 = prop.table(wtd.table(y))[7]*100)%>%
    pivot_longer(cols = starts_with("val"), 
                 names_to = "Value", 
                 names_prefix = "val",
                 values_to = "Porcentaje")
  graphData$Value <- graphData$Value %>% factor(levels = c(1:7),
                                                labels = names(table(df[[originVar]])),
                                                ordered = T)
  graphData[["q_AB_CA_08"]] <- graphData[["q_AB_CA_08"]] %>% factor(levels = c(levels(df[["q_AB_CA_08"]]),"Total"),
                                                                    labels = c(levels(df[["q_AB_CA_08"]]),"Total"),
                                                                    ordered = T)
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

createGraphData7OptionEducP <- function(df = asData, 
                                        originVar,
                                        totalColumn = T){
  require(tidyverse)
  graphData <- df %>% select(originVar, q_AB_CA_05) 
  graphData$y <- graphData[[originVar]]
  graphData <- na.omit(graphData)
  totalData <- graphData %>%
    summarise(val1 = prop.table(wtd.table(y))[1]*100,
              val2 = prop.table(wtd.table(y))[2]*100,
              val3 = prop.table(wtd.table(y))[3]*100,
              val4 = prop.table(wtd.table(y))[4]*100,
              val5 = prop.table(wtd.table(y))[5]*100,
              val6 = prop.table(wtd.table(y))[6]*100,
              val7 = prop.table(wtd.table(y))[7]*100)
  totalData[["q_AB_CA_05"]] <- factor("Total",
                                      levels = c(levels(df[["q_AB_CA_05"]]),"Total"),
                                      labels = c(levels(df[["q_AB_CA_05"]]),"Total"),
                                      ordered = T)
  totalData <- totalData %>%  pivot_longer(cols = starts_with("val"), 
                                           names_to = "Value", 
                                           names_prefix = "val",
                                           values_to = "Porcentaje")
  totalData$Value <- totalData$Value %>% factor(levels = c(1:7),
                                                labels = names(table(df[[originVar]])),
                                                ordered = T)
  
  graphData <- graphData %>%
    group_by(q_AB_CA_05) %>%
    summarise(val1 = prop.table(wtd.table(y))[1]*100,
              val2 = prop.table(wtd.table(y))[2]*100,
              val3 = prop.table(wtd.table(y))[3]*100,
              val4 = prop.table(wtd.table(y))[4]*100,
              val5 = prop.table(wtd.table(y))[5]*100,
              val6 = prop.table(wtd.table(y))[6]*100,
              val7 = prop.table(wtd.table(y))[7]*100)%>%
    pivot_longer(cols = starts_with("val"), 
                 names_to = "Value", 
                 names_prefix = "val",
                 values_to = "Porcentaje")
  graphData$Value <- graphData$Value %>% factor(levels = c(1:7),
                                                labels = names(table(df[[originVar]])),
                                                ordered = T)
  graphData[["q_AB_CA_05"]] <- graphData[["q_AB_CA_05"]] %>% factor(levels = c(levels(df[["q_AB_CA_05"]]),"Total"),
                                                                    labels = c(levels(df[["q_AB_CA_05"]]),"Total"),
                                                                    ordered = T)
  if (totalColumn){
    graphData <- bind_rows(graphData, totalData)  
  }
  graphData
}




#----------------------------------------------------------------------------------------------------------------------------------------------------------------------
  

  
  #  6option Stack Gen--------------------------------------------------------------------
#' Function to create data for age stacked for graphs, final report as
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

createGraphData6OptionGen <- function(df = asData, 
                                      originVar,
                                      totalColumn = T){
  require(tidyverse)
  graphData <- df %>% select(originVar, q_AB_CA_03) 
  graphData$y <- graphData[[originVar]]
  graphData <- na.omit(graphData)
  totalData <- graphData %>%
    summarise(val1 = prop.table(wtd.table(y))[1]*100,
              val2 = prop.table(wtd.table(y))[2]*100,
              val3 = prop.table(wtd.table(y))[3]*100,
              val4 = prop.table(wtd.table(y))[4]*100,
              val5 = prop.table(wtd.table(y))[5]*100,
              val6 = prop.table(wtd.table(y))[6]*100)
  totalData[["q_AB_CA_03"]] <- "Total"
  totalData <- totalData %>%  pivot_longer(cols = starts_with("val"), 
                                           names_to = "Value", 
                                           names_prefix = "val",
                                           values_to = "Porcentaje")
  totalData$Value <- totalData$Value %>% factor(levels = c(1:6),
                                                labels = names(table(df[[originVar]])),
                                                ordered = T)
  
  graphData <- graphData %>%
    group_by(age) %>%
    summarise(val1 = prop.table(wtd.table(y))[1]*100,
              val2 = prop.table(wtd.table(y))[2]*100,
              val3 = prop.table(wtd.table(y))[3]*100,
              val4 = prop.table(wtd.table(y))[4]*100,
              val5 = prop.table(wtd.table(y))[5]*100,
              val6 = prop.table(wtd.table(y))[6]*100)%>%
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


#  7option Stack age --------------------------------------------------------------------
#' Function to create data for age stacked for graphs, final report as
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

createGraphData6OptionAge <- function(df = asData, 
                                      originVar,
                                      totalColumn = T){
  require(tidyverse)
  graphData <- df %>% select(originVar, age) 
  graphData$y <- graphData[[originVar]]
  graphData <- na.omit(graphData)
  totalData <- graphData %>%
    summarise(val1 = prop.table(wtd.table(y))[1]*100,
              val2 = prop.table(wtd.table(y))[2]*100,
              val3 = prop.table(wtd.table(y))[3]*100,
              val4 = prop.table(wtd.table(y))[4]*100,
              val5 = prop.table(wtd.table(y))[5]*100,
              val6 = prop.table(wtd.table(y))[6]*100)
  totalData[["age"]] <- "Total"
  totalData <- totalData %>%  pivot_longer(cols = starts_with("val"), 
                                           names_to = "Value", 
                                           names_prefix = "val",
                                           values_to = "Porcentaje")
  totalData$Value <- totalData$Value %>% factor(levels = c(1:6),
                                                labels = names(table(df[[originVar]])),
                                                ordered = T)
  
  graphData <- graphData %>%
    group_by(age) %>%
    summarise(val1 = prop.table(wtd.table(y))[1]*100,
              val2 = prop.table(wtd.table(y))[2]*100,
              val3 = prop.table(wtd.table(y))[3]*100,
              val4 = prop.table(wtd.table(y))[4]*100,
              val5 = prop.table(wtd.table(y))[5]*100,
              val6 = prop.table(wtd.table(y))[6]*100)%>%
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

createGraphData6OptionEnglishLevel <- function(df = asData, 
                                               originVar,
                                               totalColumn = T){
  require(tidyverse)
  graphData <- df %>% select(originVar, q_AB_NI_01) 
  graphData$y <- graphData[[originVar]]
  graphData <- na.omit(graphData)
  totalData <- graphData %>%
    summarise(val1 = prop.table(wtd.table(y))[1]*100,
              val2 = prop.table(wtd.table(y))[2]*100,
              val3 = prop.table(wtd.table(y))[3]*100,
              val4 = prop.table(wtd.table(y))[4]*100,
              val5 = prop.table(wtd.table(y))[5]*100,
              val6 = prop.table(wtd.table(y))[6]*100)
  totalData[["q_AB_NI_01"]] <- factor("Total",
                                      levels = c(levels(df[["q_AB_NI_01"]]),"Total"),
                                      labels = c(levels(df[["q_AB_NI_01"]]),"Total"),
                                      ordered = T)
  totalData <- totalData %>%  pivot_longer(cols = starts_with("val"), 
                                           names_to = "Value", 
                                           names_prefix = "val",
                                           values_to = "Porcentaje")
  totalData$Value <- totalData$Value %>% factor(levels = c(1:6),
                                                labels = names(table(df[[originVar]])),
                                                ordered = T)
  
  graphData <- graphData %>%
    group_by(q_AB_NI_01) %>%
    summarise(val1 = prop.table(wtd.table(y))[1]*100,
              val2 = prop.table(wtd.table(y))[2]*100,
              val3 = prop.table(wtd.table(y))[3]*100,
              val4 = prop.table(wtd.table(y))[4]*100,
              val5 = prop.table(wtd.table(y))[5]*100,
              val6 = prop.table(wtd.table(y))[6]*100)%>%
    pivot_longer(cols = starts_with("val"), 
                 names_to = "Value", 
                 names_prefix = "val",
                 values_to = "Porcentaje")
  graphData$Value <- graphData$Value %>% factor(levels = c(1:6),
                                                labels = names(table(df[[originVar]])),
                                                ordered = T)
  graphData[["q_AB_NI_01"]] <- graphData[["q_AB_NI_01"]] %>% factor(levels = c(levels(df[["q_AB_NI_01"]]),"Total"),
                                                                    labels = c(levels(df[["q_AB_NI_01"]]),"Total"),
                                                                    ordered = T)
  if (totalColumn){
    graphData <- bind_rows(graphData, totalData)  
  }
  graphData
}
#  7option Stack q_AB_CA_01 --------------------------------------------------------------------
#' Function to create data for q_AB_CA_01 stacked for graphs, final report as
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

createGraphData6OptionBilingualAgent <- function(df = asData, 
                                                 originVar,
                                                 totalColumn = T){
  require(tidyverse)
  graphData <- df %>% select(all_of(originVar), q_AB_CA_01)
  graphData$y <- graphData[[originVar]]
  graphData <- na.omit(graphData)
  totalData <- graphData %>%
    summarise(val1 = prop.table(wtd.table(y))[1]*100,
              val2 = prop.table(wtd.table(y))[2]*100,
              val3 = prop.table(wtd.table(y))[3]*100,
              val4 = prop.table(wtd.table(y))[4]*100,
              val5 = prop.table(wtd.table(y))[5]*100,
              val6 = prop.table(wtd.table(y))[6]*100)
  totalData[["q_AB_CA_01"]] <- factor("Total",
                                      levels = c(levels(df[["q_AB_CA_01"]]),"Total"),
                                      labels = c(levels(df[["q_AB_CA_01"]]),"Total"))
  totalData <- totalData %>%  pivot_longer(cols = starts_with("val"), 
                                           names_to = "Value", 
                                           names_prefix = "val",
                                           values_to = "Porcentaje")
  totalData$Value <- totalData$Value %>% factor(levels = c(1:6),
                                                labels = names(table(df[[originVar]])),
                                                ordered = T)
  graphData <- graphData %>%
    group_by(q_AB_CA_01) %>%
    summarise(val1 = prop.table(wtd.table(y))[1]*100,
              val2 = prop.table(wtd.table(y))[2]*100,
              val3 = prop.table(wtd.table(y))[3]*100,
              val4 = prop.table(wtd.table(y))[4]*100,
              val5 = prop.table(wtd.table(y))[5]*100,
              val6 = prop.table(wtd.table(y))[6]*100)%>%
    pivot_longer(cols = starts_with("val"),
                 names_to = "Value",
                 names_prefix = "val",
                 values_to = "Porcentaje")
  graphData$Value <- graphData$Value %>% factor(levels = c(1:6),
                                                labels = names(table(df[[originVar]])),
                                                ordered = T)
  graphData[["q_AB_CA_01"]] <- graphData[["q_AB_CA_01"]] %>% factor(levels = c(names(table(df[["q_AB_CA_01"]])),"Total"),
                                                                    labels = c(names(table(df[["q_AB_CA_01"]])),"Total"))
  
  if (totalColumn){
    graphData <- bind_rows(graphData, totalData)  
  }
  graphData
}
#  7option Stack Estrato --------------------------------------------------------------------
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

createGraphData6OptionEstrato <- function(df = asData, 
                                          originVar,
                                          totalColumn = T){
  require(tidyverse)
  graphData <- df %>% select(originVar, q_AB_CA_08) 
  graphData$y <- graphData[[originVar]]
  graphData <- na.omit(graphData)
  totalData <- graphData %>%
    summarise(val1 = prop.table(wtd.table(y))[1]*100,
              val2 = prop.table(wtd.table(y))[2]*100,
              val3 = prop.table(wtd.table(y))[3]*100,
              val4 = prop.table(wtd.table(y))[4]*100,
              val5 = prop.table(wtd.table(y))[5]*100,
              val6 = prop.table(wtd.table(y))[6]*100)
  totalData[["q_AB_CA_08"]] <- factor("Total",
                                      levels = c(levels(df[["q_AB_CA_08"]]),"Total"),
                                      labels = c(levels(df[["q_AB_CA_08"]]),"Total"),
                                      ordered = T)
  totalData <- totalData %>%  pivot_longer(cols = starts_with("val"), 
                                           names_to = "Value", 
                                           names_prefix = "val",
                                           values_to = "Porcentaje")
  totalData$Value <- totalData$Value %>% factor(levels = c(1:6),
                                                labels = names(table(df[[originVar]])),
                                                ordered = T)
  
  graphData <- graphData %>%
    group_by(q_AB_CA_05) %>%
    summarise(val1 = prop.table(wtd.table(y))[1]*100,
              val2 = prop.table(wtd.table(y))[2]*100,
              val3 = prop.table(wtd.table(y))[3]*100,
              val4 = prop.table(wtd.table(y))[4]*100,
              val5 = prop.table(wtd.table(y))[5]*100,
              val6 = prop.table(wtd.table(y))[6]*100)%>%
    pivot_longer(cols = starts_with("val"), 
                 names_to = "Value", 
                 names_prefix = "val",
                 values_to = "Porcentaje")
  graphData$Value <- graphData$Value %>% factor(levels = c(1:6),
                                                labels = names(table(df[[originVar]])),
                                                ordered = T)
  graphData[["q_AB_CA_08"]] <- graphData[["q_AB_CA_08"]] %>% factor(levels = c(levels(df[["q_AB_CA_08"]]),"Total"),
                                                                    labels = c(levels(df[["q_AB_CA_08"]]),"Total"),
                                                                    ordered = T)
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

createGraphData6OptionEducP <- function(df = asData, 
                                        originVar,
                                        totalColumn = T){
  require(tidyverse)
  graphData <- df %>% select(originVar, q_AB_CA_05) 
  graphData$y <- graphData[[originVar]]
  graphData <- na.omit(graphData)
  totalData <- graphData %>%
    summarise(val1 = prop.table(wtd.table(y))[1]*100,
              val2 = prop.table(wtd.table(y))[2]*100,
              val3 = prop.table(wtd.table(y))[3]*100,
              val4 = prop.table(wtd.table(y))[4]*100,
              val5 = prop.table(wtd.table(y))[5]*100,
              val6 = prop.table(wtd.table(y))[6]*100)
  totalData[["q_AB_CA_05"]] <- factor("Total",
                                      levels = c(levels(df[["q_AB_CA_05"]]),"Total"),
                                      labels = c(levels(df[["q_AB_CA_05"]]),"Total"),
                                      ordered = T)
  totalData <- totalData %>%  pivot_longer(cols = starts_with("val"), 
                                           names_to = "Value", 
                                           names_prefix = "val",
                                           values_to = "Porcentaje")
  totalData$Value <- totalData$Value %>% factor(levels = c(1:7),
                                                labels = names(table(df[[originVar]])),
                                                ordered = T)
  
  graphData <- graphData %>%
    group_by(q_AB_CA_05) %>%
    summarise(val1 = prop.table(wtd.table(y))[1]*100,
              val2 = prop.table(wtd.table(y))[2]*100,
              val3 = prop.table(wtd.table(y))[3]*100,
              val4 = prop.table(wtd.table(y))[4]*100,
              val5 = prop.table(wtd.table(y))[5]*100,
              val6 = prop.table(wtd.table(y))[6]*100)%>%
    pivot_longer(cols = starts_with("val"), 
                 names_to = "Value", 
                 names_prefix = "val",
                 values_to = "Porcentaje")
  graphData$Value <- graphData$Value %>% factor(levels = c(1:6),
                                                labels = names(table(df[[originVar]])),
                                                ordered = T)
  graphData[["q_AB_CA_05"]] <- graphData[["q_AB_CA_05"]] %>% factor(levels = c(levels(df[["q_AB_CA_05"]]),"Total"),
                                                                    labels = c(levels(df[["q_AB_CA_05"]]),"Total"),
                                                                    ordered = T)
  if (totalColumn){
    graphData <- bind_rows(graphData, totalData)  
  }
  graphData
}




#-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  
  
  
  
#  4option Stack Gen--------------------------------------------------------------------
#' Function to create data for age stacked for graphs, final report as
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

createGraphData4OptionGen <- function(df = asData, 
                                      originVar,
                                      totalColumn = T){
  require(tidyverse)
  graphData <- df %>% select(originVar, q_AB_CA_03) 
  graphData$y <- graphData[[originVar]]
  graphData <- na.omit(graphData)
  totalData <- graphData %>%
    summarise(val1 = prop.table(wtd.table(y))[1]*100,
              val2 = prop.table(wtd.table(y))[2]*100,
              val3 = prop.table(wtd.table(y))[3]*100,
              val4 = prop.table(wtd.table(y))[4]*100)
  totalData[["q_AB_CA_03"]] <- "Total"
  totalData <- totalData %>%  pivot_longer(cols = starts_with("val"), 
                                           names_to = "Value", 
                                           names_prefix = "val",
                                           values_to = "Porcentaje")
  totalData$Value <- totalData$Value %>% factor(levels = c(1:4),
                                                labels = names(table(df[[originVar]])),
                                                ordered = T)
  
  graphData <- graphData %>%
    group_by(age) %>%
    summarise(val1 = prop.table(wtd.table(y))[1]*100,
              val2 = prop.table(wtd.table(y))[2]*100,
              val3 = prop.table(wtd.table(y))[3]*100,
              val4 = prop.table(wtd.table(y))[4]*100)%>%
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


#  4option Stack age --------------------------------------------------------------------
#' Function to create data for age stacked for graphs, final report as
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

createGraphData4OptionAge <- function(df = asData, 
                                      originVar,
                                      totalColumn = T){
  require(tidyverse)
  graphData <- df %>% select(originVar, age) 
  graphData$y <- graphData[[originVar]]
  graphData <- na.omit(graphData)
  totalData <- graphData %>%
    summarise(val1 = prop.table(wtd.table(y))[1]*100,
              val2 = prop.table(wtd.table(y))[2]*100,
              val3 = prop.table(wtd.table(y))[3]*100,
              val4 = prop.table(wtd.table(y))[4]*100)
  totalData[["age"]] <- "Total"
  totalData <- totalData %>%  pivot_longer(cols = starts_with("val"), 
                                           names_to = "Value", 
                                           names_prefix = "val",
                                           values_to = "Porcentaje")
  totalData$Value <- totalData$Value %>% factor(levels = c(1:4),
                                                labels = names(table(df[[originVar]])),
                                                ordered = T)
  
  graphData <- graphData %>%
    group_by(age) %>%
    summarise(val1 = prop.table(wtd.table(y))[1]*100,
              val2 = prop.table(wtd.table(y))[2]*100,
              val3 = prop.table(wtd.table(y))[3]*100,
              val4 = prop.table(wtd.table(y))[4]*100)%>%
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

createGraphData4OptionEnglishLevel <- function(df = asData, 
                                               originVar,
                                               totalColumn = T){
  require(tidyverse)
  graphData <- df %>% select(originVar, q_AB_NI_01) 
  graphData$y <- graphData[[originVar]]
  graphData <- na.omit(graphData)
  totalData <- graphData %>%
    summarise(val1 = prop.table(wtd.table(y))[1]*100,
              val2 = prop.table(wtd.table(y))[2]*100,
              val3 = prop.table(wtd.table(y))[3]*100,
              val4 = prop.table(wtd.table(y))[4]*100)
  totalData[["q_AB_NI_01"]] <- factor("Total",
                                      levels = c(levels(df[["q_AB_NI_01"]]),"Total"),
                                      labels = c(levels(df[["q_AB_NI_01"]]),"Total"),
                                      ordered = T)
  totalData <- totalData %>%  pivot_longer(cols = starts_with("val"), 
                                           names_to = "Value", 
                                           names_prefix = "val",
                                           values_to = "Porcentaje")
  totalData$Value <- totalData$Value %>% factor(levels = c(1:4),
                                                labels = names(table(df[[originVar]])),
                                                ordered = T)
  
  graphData <- graphData %>%
    group_by(q_AB_NI_01) %>%
    summarise(val1 = prop.table(wtd.table(y))[1]*100,
              val2 = prop.table(wtd.table(y))[2]*100,
              val3 = prop.table(wtd.table(y))[3]*100,
              val4 = prop.table(wtd.table(y))[4]*100)%>%
    pivot_longer(cols = starts_with("val"), 
                 names_to = "Value", 
                 names_prefix = "val",
                 values_to = "Porcentaje")
  graphData$Value <- graphData$Value %>% factor(levels = c(1:4),
                                                labels = names(table(df[[originVar]])),
                                                ordered = T)
  graphData[["q_AB_NI_01"]] <- graphData[["q_AB_NI_01"]] %>% factor(levels = c(levels(df[["q_AB_NI_01"]]),"Total"),
                                                                    labels = c(levels(df[["q_AB_NI_01"]]),"Total"),
                                                                    ordered = T)
  if (totalColumn){
    graphData <- bind_rows(graphData, totalData)  
  }
  graphData
}
#  4option Stack q_AB_CA_01 --------------------------------------------------------------------
#' Function to create data for q_AB_CA_01 stacked for graphs, final report as
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

createGraphData4OptionBilingualAgent <- function(df = asData, 
                                                 originVar,
                                                 totalColumn = T){
  require(tidyverse)
  graphData <- df %>% select(all_of(originVar), q_AB_CA_01)
  graphData$y <- graphData[[originVar]]
  graphData <- na.omit(graphData)
  totalData <- graphData %>%
    summarise(val1 = prop.table(wtd.table(y))[1]*100,
              val2 = prop.table(wtd.table(y))[2]*100,
              val3 = prop.table(wtd.table(y))[3]*100,
              val4 = prop.table(wtd.table(y))[4]*100)
  totalData[["q_AB_CA_01"]] <- factor("Total",
                                      levels = c(levels(df[["q_AB_CA_01"]]),"Total"),
                                      labels = c(levels(df[["q_AB_CA_01"]]),"Total"))
  totalData <- totalData %>%  pivot_longer(cols = starts_with("val"), 
                                           names_to = "Value", 
                                           names_prefix = "val",
                                           values_to = "Porcentaje")
  totalData$Value <- totalData$Value %>% factor(levels = c(1:4),
                                                labels = names(table(df[[originVar]])),
                                                ordered = T)
  graphData <- graphData %>%
    group_by(q_AB_CA_01) %>%
    summarise(val1 = prop.table(wtd.table(y))[1]*100,
              val2 = prop.table(wtd.table(y))[2]*100,
              val3 = prop.table(wtd.table(y))[3]*100,
              val4 = prop.table(wtd.table(y))[4]*100)%>%
    pivot_longer(cols = starts_with("val"),
                 names_to = "Value",
                 names_prefix = "val",
                 values_to = "Porcentaje")
  graphData$Value <- graphData$Value %>% factor(levels = c(1:4),
                                                labels = names(table(df[[originVar]])),
                                                ordered = T)
  graphData[["q_AB_CA_01"]] <- graphData[["q_AB_CA_01"]] %>% factor(levels = c(names(table(df[["q_AB_CA_01"]])),"Total"),
                                                                    labels = c(names(table(df[["q_AB_CA_01"]])),"Total"))
  
  if (totalColumn){
    graphData <- bind_rows(graphData, totalData)  
  }
  graphData
}
#  5option Stack Estrato --------------------------------------------------------------------
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

createGraphData4OptionEstrato <- function(df = asData, 
                                          originVar,
                                          totalColumn = T){
  require(tidyverse)
  graphData <- df %>% select(originVar, q_AB_CA_08) 
  graphData$y <- graphData[[originVar]]
  graphData <- na.omit(graphData)
  totalData <- graphData %>%
    summarise(val1 = prop.table(wtd.table(y))[1]*100,
              val2 = prop.table(wtd.table(y))[2]*100,
              val3 = prop.table(wtd.table(y))[3]*100,
              val4 = prop.table(wtd.table(y))[4]*100)
  totalData[["q_AB_CA_08"]] <- factor("Total",
                                      levels = c(levels(df[["q_AB_CA_08"]]),"Total"),
                                      labels = c(levels(df[["q_AB_CA_08"]]),"Total"),
                                      ordered = T)
  totalData <- totalData %>%  pivot_longer(cols = starts_with("val"), 
                                           names_to = "Value", 
                                           names_prefix = "val",
                                           values_to = "Porcentaje")
  totalData$Value <- totalData$Value %>% factor(levels = c(1:4),
                                                labels = names(table(df[[originVar]])),
                                                ordered = T)
  
  graphData <- graphData %>%
    group_by(q_AB_CA_05) %>%
    summarise(val1 = prop.table(wtd.table(y))[1]*100,
              val2 = prop.table(wtd.table(y))[2]*100,
              val3 = prop.table(wtd.table(y))[3]*100,
              val4 = prop.table(wtd.table(y))[4]*100)%>%
    pivot_longer(cols = starts_with("val"), 
                 names_to = "Value", 
                 names_prefix = "val",
                 values_to = "Porcentaje")
  graphData$Value <- graphData$Value %>% factor(levels = c(1:4),
                                                labels = names(table(df[[originVar]])),
                                                ordered = T)
  graphData[["q_AB_CA_08"]] <- graphData[["q_AB_CA_08"]] %>% factor(levels = c(levels(df[["q_AB_CA_08"]]),"Total"),
                                                                    labels = c(levels(df[["q_AB_CA_08"]]),"Total"),
                                                                    ordered = T)
  if (totalColumn){
    graphData <- bind_rows(graphData, totalData)  
  }
  graphData
}



#  4option Stack Educación Propia --------------------------------------------------------------------
#' Function to create data for educación madre stacked for graphs, final report as
#'
#' This function creates graphData to create stacked graph. 
#' It assumes a 1-5 scale, values greater than 7 are missing.
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
  graphData <- df %>% select(originVar, q_AB_CA_05) 
  graphData$y <- graphData[[originVar]]
  graphData <- na.omit(graphData)
  totalData <- graphData %>%
    summarise(val1 = prop.table(wtd.table(y))[1]*100,
              val2 = prop.table(wtd.table(y))[2]*100,
              val3 = prop.table(wtd.table(y))[3]*100,
              val4 = prop.table(wtd.table(y))[4]*100)
  totalData[["q_AB_CA_05"]] <- factor("Total",
                                      levels = c(levels(df[["q_AB_CA_05"]]),"Total"),
                                      labels = c(levels(df[["q_AB_CA_05"]]),"Total"),
                                      ordered = T)
  totalData <- totalData %>%  pivot_longer(cols = starts_with("val"), 
                                           names_to = "Value", 
                                           names_prefix = "val",
                                           values_to = "Porcentaje")
  totalData$Value <- totalData$Value %>% factor(levels = c(1:4),
                                                labels = names(table(df[[originVar]])),
                                                ordered = T)
  
  graphData <- graphData %>%
    group_by(q_AB_CA_05) %>%
    summarise(val1 = prop.table(wtd.table(y))[1]*100,
              val2 = prop.table(wtd.table(y))[2]*100,
              val3 = prop.table(wtd.table(y))[3]*100,
              val4 = prop.table(wtd.table(y))[4]*100)%>%
    pivot_longer(cols = starts_with("val"), 
                 names_to = "Value", 
                 names_prefix = "val",
                 values_to = "Porcentaje")
  graphData$Value <- graphData$Value %>% factor(levels = c(1:4),
                                                labels = names(table(df[[originVar]])),
                                                ordered = T)
  graphData[["q_AB_CA_05"]] <- graphData[["q_AB_CA_05"]] %>% factor(levels = c(levels(df[["q_AB_CA_05"]]),"Total"),
                                                                    labels = c(levels(df[["q_AB_CA_05"]]),"Total"),
                                                                    ordered = T)
  if (totalColumn){
    graphData <- bind_rows(graphData, totalData)  
  }
  graphData
}


#---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

  #  5option Stack Gen--------------------------------------------------------------------
#' Function to create data for age stacked for graphs, final report as
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

createGraphData5OptionGen <- function(df = asData, 
                                      originVar,
                                      totalColumn = T){
  require(tidyverse)
  graphData <- df %>% select(originVar, q_AB_CA_03) 
  graphData$y <- graphData[[originVar]]
  graphData <- na.omit(graphData)
  totalData <- graphData %>%
    summarise(val1 = prop.table(wtd.table(y))[1]*100,
              val2 = prop.table(wtd.table(y))[2]*100,
              val3 = prop.table(wtd.table(y))[3]*100,
              val4 = prop.table(wtd.table(y))[4]*100,
              val5 = prop.table(wtd.table(y))[5]*100)
  totalData[["q_AB_CA_03"]] <- "Total"
  totalData <- totalData %>%  pivot_longer(cols = starts_with("val"), 
                                           names_to = "Value", 
                                           names_prefix = "val",
                                           values_to = "Porcentaje")
  totalData$Value <- totalData$Value %>% factor(levels = c(1:5),
                                                labels = names(table(df[[originVar]])),
                                                ordered = T)
  
  graphData <- graphData %>%
    group_by(age) %>%
    summarise(val1 = prop.table(wtd.table(y))[1]*100,
              val2 = prop.table(wtd.table(y))[2]*100,
              val3 = prop.table(wtd.table(y))[3]*100,
              val4 = prop.table(wtd.table(y))[4]*100,
              val5 = prop.table(wtd.table(y))[5]*100)%>%
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


#  5option Stack age --------------------------------------------------------------------
#' Function to create data for age stacked for graphs, final report as
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

createGraphData5OptionAge <- function(df = asData, 
                                      originVar,
                                      totalColumn = T){
  require(tidyverse)
  graphData <- df %>% select(originVar, age) 
  graphData$y <- graphData[[originVar]]
  graphData <- na.omit(graphData)
  totalData <- graphData %>%
    summarise(val1 = prop.table(wtd.table(y))[1]*100,
              val2 = prop.table(wtd.table(y))[2]*100,
              val3 = prop.table(wtd.table(y))[3]*100,
              val4 = prop.table(wtd.table(y))[4]*100,
              val5 = prop.table(wtd.table(y))[5]*100)
  totalData[["age"]] <- "Total"
  totalData <- totalData %>%  pivot_longer(cols = starts_with("val"), 
                                           names_to = "Value", 
                                           names_prefix = "val",
                                           values_to = "Porcentaje")
  totalData$Value <- totalData$Value %>% factor(levels = c(1:5),
                                                labels = names(table(df[[originVar]])),
                                                ordered = T)
  
  graphData <- graphData %>%
    group_by(age) %>%
    summarise(val1 = prop.table(wtd.table(y))[1]*100,
              val2 = prop.table(wtd.table(y))[2]*100,
              val3 = prop.table(wtd.table(y))[3]*100,
              val4 = prop.table(wtd.table(y))[4]*100,
              val5 = prop.table(wtd.table(y))[5]*100)%>%
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

createGraphData5OptionEnglishLevel <- function(df = asData, 
                                               originVar,
                                               totalColumn = T){
  require(tidyverse)
  graphData <- df %>% select(originVar, q_AB_NI_01) 
  graphData$y <- graphData[[originVar]]
  graphData <- na.omit(graphData)
  totalData <- graphData %>%
    summarise(val1 = prop.table(wtd.table(y))[1]*100,
              val2 = prop.table(wtd.table(y))[2]*100,
              val3 = prop.table(wtd.table(y))[3]*100,
              val4 = prop.table(wtd.table(y))[4]*100,
              val5 = prop.table(wtd.table(y))[5]*100)
  totalData[["q_AB_NI_01"]] <- factor("Total",
                                      levels = c(levels(df[["q_AB_NI_01"]]),"Total"),
                                      labels = c(levels(df[["q_AB_NI_01"]]),"Total"),
                                      ordered = T)
  totalData <- totalData %>%  pivot_longer(cols = starts_with("val"), 
                                           names_to = "Value", 
                                           names_prefix = "val",
                                           values_to = "Porcentaje")
  totalData$Value <- totalData$Value %>% factor(levels = c(1:5),
                                                labels = names(table(df[[originVar]])),
                                                ordered = T)
  
  graphData <- graphData %>%
    group_by(q_AB_NI_01) %>%
    summarise(val1 = prop.table(wtd.table(y))[1]*100,
              val2 = prop.table(wtd.table(y))[2]*100,
              val3 = prop.table(wtd.table(y))[3]*100,
              val4 = prop.table(wtd.table(y))[4]*100,
              val5 = prop.table(wtd.table(y))[5]*100)%>%
    pivot_longer(cols = starts_with("val"), 
                 names_to = "Value", 
                 names_prefix = "val",
                 values_to = "Porcentaje")
  graphData$Value <- graphData$Value %>% factor(levels = c(1:5),
                                                labels = names(table(df[[originVar]])),
                                                ordered = T)
  graphData[["q_AB_NI_01"]] <- graphData[["q_AB_NI_01"]] %>% factor(levels = c(levels(df[["q_AB_NI_01"]]),"Total"),
                                                                    labels = c(levels(df[["q_AB_NI_01"]]),"Total"),
                                                                    ordered = T)
  if (totalColumn){
    graphData <- bind_rows(graphData, totalData)  
  }
  graphData
}
#  5option Stack q_AB_CA_01 --------------------------------------------------------------------
#' Function to create data for q_AB_CA_01 stacked for graphs, final report as
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

createGraphData5OptionBilingualAgent <- function(df = asData, 
                                                 originVar,
                                                 totalColumn = T){
  require(tidyverse)
  graphData <- df %>% select(all_of(originVar), q_AB_CA_01)
  graphData$y <- graphData[[originVar]]
  graphData <- na.omit(graphData)
  totalData <- graphData %>%
    summarise(val1 = prop.table(wtd.table(y))[1]*100,
              val2 = prop.table(wtd.table(y))[2]*100,
              val3 = prop.table(wtd.table(y))[3]*100,
              val4 = prop.table(wtd.table(y))[4]*100,
              val5 = prop.table(wtd.table(y))[5]*100)
  totalData[["q_AB_CA_01"]] <- factor("Total",
                                      levels = c(levels(df[["q_AB_CA_01"]]),"Total"),
                                      labels = c(levels(df[["q_AB_CA_01"]]),"Total"))
  totalData <- totalData %>%  pivot_longer(cols = starts_with("val"), 
                                           names_to = "Value", 
                                           names_prefix = "val",
                                           values_to = "Porcentaje")
  totalData$Value <- totalData$Value %>% factor(levels = c(1:5),
                                                labels = names(table(df[[originVar]])),
                                                ordered = T)
  graphData <- graphData %>%
    group_by(q_AB_CA_01) %>%
    summarise(val1 = prop.table(wtd.table(y))[1]*100,
              val2 = prop.table(wtd.table(y))[2]*100,
              val3 = prop.table(wtd.table(y))[3]*100,
              val4 = prop.table(wtd.table(y))[4]*100,
              val5 = prop.table(wtd.table(y))[5]*100)%>%
    pivot_longer(cols = starts_with("val"),
                 names_to = "Value",
                 names_prefix = "val",
                 values_to = "Porcentaje")
  graphData$Value <- graphData$Value %>% factor(levels = c(1:5),
                                                labels = names(table(df[[originVar]])),
                                                ordered = T)
  graphData[["q_AB_CA_01"]] <- graphData[["q_AB_CA_01"]] %>% factor(levels = c(names(table(df[["q_AB_CA_01"]])),"Total"),
                                                                    labels = c(names(table(df[["q_AB_CA_01"]])),"Total"))
  
  if (totalColumn){
    graphData <- bind_rows(graphData, totalData)  
  }
  graphData
}
#  5option Stack Estrato --------------------------------------------------------------------
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

createGraphData5OptionEstrato <- function(df = asData, 
                                          originVar,
                                          totalColumn = T){
  require(tidyverse)
  graphData <- df %>% select(originVar, q_AB_CA_08) 
  graphData$y <- graphData[[originVar]]
  graphData <- na.omit(graphData)
  totalData <- graphData %>%
    summarise(val1 = prop.table(wtd.table(y))[1]*100,
              val2 = prop.table(wtd.table(y))[2]*100,
              val3 = prop.table(wtd.table(y))[3]*100,
              val4 = prop.table(wtd.table(y))[4]*100,
              val5 = prop.table(wtd.table(y))[5]*100)
  totalData[["q_AB_CA_08"]] <- factor("Total",
                                      levels = c(levels(df[["q_AB_CA_08"]]),"Total"),
                                      labels = c(levels(df[["q_AB_CA_08"]]),"Total"),
                                      ordered = T)
  totalData <- totalData %>%  pivot_longer(cols = starts_with("val"), 
                                           names_to = "Value", 
                                           names_prefix = "val",
                                           values_to = "Porcentaje")
  totalData$Value <- totalData$Value %>% factor(levels = c(1:5),
                                                labels = names(table(df[[originVar]])),
                                                ordered = T)
  
  graphData <- graphData %>%
    group_by(q_AB_CA_05) %>%
    summarise(val1 = prop.table(wtd.table(y))[1]*100,
              val2 = prop.table(wtd.table(y))[2]*100,
              val3 = prop.table(wtd.table(y))[3]*100,
              val4 = prop.table(wtd.table(y))[4]*100,
              val5 = prop.table(wtd.table(y))[5]*100)%>%
    pivot_longer(cols = starts_with("val"), 
                 names_to = "Value", 
                 names_prefix = "val",
                 values_to = "Porcentaje")
  graphData$Value <- graphData$Value %>% factor(levels = c(1:5),
                                                labels = names(table(df[[originVar]])),
                                                ordered = T)
  graphData[["q_AB_CA_08"]] <- graphData[["q_AB_CA_08"]] %>% factor(levels = c(levels(df[["q_AB_CA_08"]]),"Total"),
                                                                    labels = c(levels(df[["q_AB_CA_08"]]),"Total"),
                                                                    ordered = T)
  if (totalColumn){
    graphData <- bind_rows(graphData, totalData)  
  }
  graphData
}



#  5option Stack Educación Propia --------------------------------------------------------------------
#' Function to create data for educación madre stacked for graphs, final report as
#'
#' This function creates graphData to create stacked graph. 
#' It assumes a 1-5 scale, values greater than 7 are missing.
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

createGraphData5OptionEducP <- function(df = asData, 
                                        originVar,
                                        totalColumn = T){
  require(tidyverse)
  graphData <- df %>% select(originVar, q_AB_CA_05) 
  graphData$y <- graphData[[originVar]]
  graphData <- na.omit(graphData)
  totalData <- graphData %>%
    summarise(val1 = prop.table(wtd.table(y))[1]*100,
              val2 = prop.table(wtd.table(y))[2]*100,
              val3 = prop.table(wtd.table(y))[3]*100,
              val4 = prop.table(wtd.table(y))[4]*100,
              val5 = prop.table(wtd.table(y))[5]*100)
  totalData[["q_AB_CA_05"]] <- factor("Total",
                                      levels = c(levels(df[["q_AB_CA_05"]]),"Total"),
                                      labels = c(levels(df[["q_AB_CA_05"]]),"Total"),
                                      ordered = T)
  totalData <- totalData %>%  pivot_longer(cols = starts_with("val"), 
                                           names_to = "Value", 
                                           names_prefix = "val",
                                           values_to = "Porcentaje")
  totalData$Value <- totalData$Value %>% factor(levels = c(1:5),
                                                labels = names(table(df[[originVar]])),
                                                ordered = T)
  
  graphData <- graphData %>%
    group_by(q_AB_CA_05) %>%
    summarise(val1 = prop.table(wtd.table(y))[1]*100,
              val2 = prop.table(wtd.table(y))[2]*100,
              val3 = prop.table(wtd.table(y))[3]*100,
              val4 = prop.table(wtd.table(y))[4]*100,
              val5 = prop.table(wtd.table(y))[5]*100)%>%
    pivot_longer(cols = starts_with("val"), 
                 names_to = "Value", 
                 names_prefix = "val",
                 values_to = "Porcentaje")
  graphData$Value <- graphData$Value %>% factor(levels = c(1:5),
                                                labels = names(table(df[[originVar]])),
                                                ordered = T)
  graphData[["q_AB_CA_05"]] <- graphData[["q_AB_CA_05"]] %>% factor(levels = c(levels(df[["q_AB_CA_05"]]),"Total"),
                                                                    labels = c(levels(df[["q_AB_CA_05"]]),"Total"),
                                                                    ordered = T)
  if (totalColumn){
    graphData <- bind_rows(graphData, totalData)  
  }
  graphData
}



#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

  
  #  3option Stack Gen--------------------------------------------------------------------
#' Function to create data for age stacked for graphs, final report as
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

createGraphData3OptionGen <- function(df = asData, 
                                      originVar,
                                      totalColumn = T){
  require(tidyverse)
  graphData <- df %>% select(originVar, q_AB_CA_03) 
  graphData$y <- graphData[[originVar]]
  graphData <- na.omit(graphData)
  totalData <- graphData %>%
    summarise(val1 = prop.table(wtd.table(y))[1]*100,
              val2 = prop.table(wtd.table(y))[2]*100,
              val3 = prop.table(wtd.table(y))[3]*100)
  totalData[["q_AB_CA_03"]] <- "Total"
  totalData <- totalData %>%  pivot_longer(cols = starts_with("val"), 
                                           names_to = "Value", 
                                           names_prefix = "val",
                                           values_to = "Porcentaje")
  totalData$Value <- totalData$Value %>% factor(levels = c(1:3),
                                                labels = names(table(df[[originVar]])),
                                                ordered = T)
  
  graphData <- graphData %>%
    group_by(age) %>%
    summarise(val1 = prop.table(wtd.table(y))[1]*100,
              val2 = prop.table(wtd.table(y))[2]*100,
              val3 = prop.table(wtd.table(y))[3]*100)%>%
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


#  3option Stack age --------------------------------------------------------------------
#' Function to create data for age stacked for graphs, final report as
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

createGraphData3OptionAge <- function(df = asData, 
                                      originVar,
                                      totalColumn = T){
  require(tidyverse)
  graphData <- df %>% select(originVar, age) 
  graphData$y <- graphData[[originVar]]
  graphData <- na.omit(graphData)
  totalData <- graphData %>%
    summarise(val1 = prop.table(wtd.table(y))[1]*100,
              val2 = prop.table(wtd.table(y))[2]*100,
              val3 = prop.table(wtd.table(y))[3]*100)
  totalData[["age"]] <- "Total"
  totalData <- totalData %>%  pivot_longer(cols = starts_with("val"), 
                                           names_to = "Value", 
                                           names_prefix = "val",
                                           values_to = "Porcentaje")
  totalData$Value <- totalData$Value %>% factor(levels = c(1:3),
                                                labels = names(table(df[[originVar]])),
                                                ordered = T)
  
  graphData <- graphData %>%
    group_by(age) %>%
    summarise(val1 = prop.table(wtd.table(y))[1]*100,
              val2 = prop.table(wtd.table(y))[2]*100,
              val3 = prop.table(wtd.table(y))[3]*100)%>%
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
#  3option Stack Educación Madre --------------------------------------------------------------------
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

createGraphData3OptionEnglishLevel <- function(df = asData, 
                                               originVar,
                                               totalColumn = T){
  require(tidyverse)
  graphData <- df %>% select(originVar, q_AB_NI_01) 
  graphData$y <- graphData[[originVar]]
  graphData <- na.omit(graphData)
  totalData <- graphData %>%
    summarise(val1 = prop.table(wtd.table(y))[1]*100,
              val2 = prop.table(wtd.table(y))[2]*100,
              val3 = prop.table(wtd.table(y))[3]*100)
  totalData[["q_AB_NI_01"]] <- factor("Total",
                                      levels = c(levels(df[["q_AB_NI_01"]]),"Total"),
                                      labels = c(levels(df[["q_AB_NI_01"]]),"Total"),
                                      ordered = T)
  totalData <- totalData %>%  pivot_longer(cols = starts_with("val"), 
                                           names_to = "Value", 
                                           names_prefix = "val",
                                           values_to = "Porcentaje")
  totalData$Value <- totalData$Value %>% factor(levels = c(1:3),
                                                labels = names(table(df[[originVar]])),
                                                ordered = T)
  
  graphData <- graphData %>%
    group_by(q_AB_NI_01) %>%
    summarise(val1 = prop.table(wtd.table(y))[1]*100,
              val2 = prop.table(wtd.table(y))[2]*100,
              val3 = prop.table(wtd.table(y))[3]*100)%>%
    pivot_longer(cols = starts_with("val"), 
                 names_to = "Value", 
                 names_prefix = "val",
                 values_to = "Porcentaje")
  graphData$Value <- graphData$Value %>% factor(levels = c(1:3),
                                                labels = names(table(df[[originVar]])),
                                                ordered = T)
  graphData[["q_AB_NI_01"]] <- graphData[["q_AB_NI_01"]] %>% factor(levels = c(levels(df[["q_AB_NI_01"]]),"Total"),
                                                                    labels = c(levels(df[["q_AB_NI_01"]]),"Total"),
                                                                    ordered = T)
  if (totalColumn){
    graphData <- bind_rows(graphData, totalData)  
  }
  graphData
}
#  3option Stack q_AB_CA_01 --------------------------------------------------------------------
#' Function to create data for q_AB_CA_01 stacked for graphs, final report as
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

createGraphData3OptionBilingualAgent <- function(df = asData, 
                                                 originVar,
                                                 totalColumn = T){
  require(tidyverse)
  graphData <- df %>% select(all_of(originVar), q_AB_CA_01)
  graphData$y <- graphData[[originVar]]
  graphData <- na.omit(graphData)
  totalData <- graphData %>%
    summarise(val1 = prop.table(wtd.table(y))[1]*100,
              val2 = prop.table(wtd.table(y))[2]*100,
              val3 = prop.table(wtd.table(y))[3]*100)
  totalData[["q_AB_CA_01"]] <- factor("Total",
                                      levels = c(levels(df[["q_AB_CA_01"]]),"Total"),
                                      labels = c(levels(df[["q_AB_CA_01"]]),"Total"))
  totalData <- totalData %>%  pivot_longer(cols = starts_with("val"), 
                                           names_to = "Value", 
                                           names_prefix = "val",
                                           values_to = "Porcentaje")
  totalData$Value <- totalData$Value %>% factor(levels = c(1:3),
                                                labels = names(table(df[[originVar]])),
                                                ordered = T)
  graphData <- graphData %>%
    group_by(q_AB_CA_01) %>%
    summarise(val1 = prop.table(wtd.table(y))[1]*100,
              val2 = prop.table(wtd.table(y))[2]*100,
              val3 = prop.table(wtd.table(y))[3]*100)%>%
    pivot_longer(cols = starts_with("val"),
                 names_to = "Value",
                 names_prefix = "val",
                 values_to = "Porcentaje")
  graphData$Value <- graphData$Value %>% factor(levels = c(1:3),
                                                labels = names(table(df[[originVar]])),
                                                ordered = T)
  graphData[["q_AB_CA_01"]] <- graphData[["q_AB_CA_01"]] %>% factor(levels = c(names(table(df[["q_AB_CA_01"]])),"Total"),
                                                                    labels = c(names(table(df[["q_AB_CA_01"]])),"Total"))
  
  if (totalColumn){
    graphData <- bind_rows(graphData, totalData)  
  }
  graphData
}
#  3option Stack Estrato --------------------------------------------------------------------
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

createGraphData3OptionEstrato <- function(df = asData, 
                                          originVar,
                                          totalColumn = T){
  require(tidyverse)
  graphData <- df %>% select(originVar, q_AB_CA_08) 
  graphData$y <- graphData[[originVar]]
  graphData <- na.omit(graphData)
  totalData <- graphData %>%
    summarise(val1 = prop.table(wtd.table(y))[1]*100,
              val2 = prop.table(wtd.table(y))[2]*100,
              val3 = prop.table(wtd.table(y))[3]*100)
  totalData[["q_AB_CA_08"]] <- factor("Total",
                                      levels = c(levels(df[["q_AB_CA_08"]]),"Total"),
                                      labels = c(levels(df[["q_AB_CA_08"]]),"Total"),
                                      ordered = T)
  totalData <- totalData %>%  pivot_longer(cols = starts_with("val"), 
                                           names_to = "Value", 
                                           names_prefix = "val",
                                           values_to = "Porcentaje")
  totalData$Value <- totalData$Value %>% factor(levels = c(1:3),
                                                labels = names(table(df[[originVar]])),
                                                ordered = T)
  
  graphData <- graphData %>%
    group_by(q_AB_CA_05) %>%
    summarise(val1 = prop.table(wtd.table(y))[1]*100,
              val2 = prop.table(wtd.table(y))[2]*100,
              val3 = prop.table(wtd.table(y))[3]*100)%>%
    pivot_longer(cols = starts_with("val"), 
                 names_to = "Value", 
                 names_prefix = "val",
                 values_to = "Porcentaje")
  graphData$Value <- graphData$Value %>% factor(levels = c(1:3),
                                                labels = names(table(df[[originVar]])),
                                                ordered = T)
  graphData[["q_AB_CA_08"]] <- graphData[["q_AB_CA_08"]] %>% factor(levels = c(levels(df[["q_AB_CA_08"]]),"Total"),
                                                                    labels = c(levels(df[["q_AB_CA_08"]]),"Total"),
                                                                    ordered = T)
  if (totalColumn){
    graphData <- bind_rows(graphData, totalData)  
  }
  graphData
}



#  3option Stack Educación Propia --------------------------------------------------------------------
#' Function to create data for educación madre stacked for graphs, final report as
#'
#' This function creates graphData to create stacked graph. 
#' It assumes a 1-5 scale, values greater than 7 are missing.
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
  graphData <- df %>% select(originVar, q_AB_CA_05) 
  graphData$y <- graphData[[originVar]]
  graphData <- na.omit(graphData)
  totalData <- graphData %>%
    summarise(val1 = prop.table(wtd.table(y))[1]*100,
              val2 = prop.table(wtd.table(y))[2]*100,
              val3 = prop.table(wtd.table(y))[3]*100)
  totalData[["q_AB_CA_05"]] <- factor("Total",
                                      levels = c(levels(df[["q_AB_CA_05"]]),"Total"),
                                      labels = c(levels(df[["q_AB_CA_05"]]),"Total"),
                                      ordered = T)
  totalData <- totalData %>%  pivot_longer(cols = starts_with("val"), 
                                           names_to = "Value", 
                                           names_prefix = "val",
                                           values_to = "Porcentaje")
  totalData$Value <- totalData$Value %>% factor(levels = c(1:3),
                                                labels = names(table(df[[originVar]])),
                                                ordered = T)
  
  graphData <- graphData %>%
    group_by(q_AB_CA_05) %>%
    summarise(val1 = prop.table(wtd.table(y))[1]*100,
              val2 = prop.table(wtd.table(y))[2]*100,
              val3 = prop.table(wtd.table(y))[3]*100)%>%
    pivot_longer(cols = starts_with("val"), 
                 names_to = "Value", 
                 names_prefix = "val",
                 values_to = "Porcentaje")
  graphData$Value <- graphData$Value %>% factor(levels = c(1:3),
                                                labels = names(table(df[[originVar]])),
                                                ordered = T)
  graphData[["q_AB_CA_05"]] <- graphData[["q_AB_CA_05"]] %>% factor(levels = c(levels(df[["q_AB_CA_05"]]),"Total"),
                                                                    labels = c(levels(df[["q_AB_CA_05"]]),"Total"),
                                                                    ordered = T)
  if (totalColumn){
    graphData <- bind_rows(graphData, totalData)  
  }
  graphData
}
  
  

#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

  
#  2option Stack Gen--------------------------------------------------------------------
#' Function to create data for age stacked for graphs, final report as
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

createGraphData2OptionGen <- function(df = asData, 
                                      originVar,
                                      totalColumn = T){
  require(tidyverse)
  graphData <- df %>% select(originVar, q_AB_CA_03) 
  graphData$y <- graphData[[originVar]]
  graphData <- na.omit(graphData)
  totalData <- graphData %>%
    summarise(val1 = prop.table(wtd.table(y))[1]*100,
              val2 = prop.table(wtd.table(y))[2]*100)
  totalData[["q_AB_CA_03"]] <- "Total"
  totalData <- totalData %>%  pivot_longer(cols = starts_with("val"), 
                                           names_to = "Value", 
                                           names_prefix = "val",
                                           values_to = "Porcentaje")
  totalData$Value <- totalData$Value %>% factor(levels = c(1:2),
                                                labels = names(table(df[[originVar]])),
                                                ordered = T)
  
  graphData <- graphData %>%
    group_by(q_AB_CA_03) %>%
    summarise(val1 = prop.table(wtd.table(y))[1]*100,
              val2 = prop.table(wtd.table(y))[2]*100)%>%
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


#  2option Stack age --------------------------------------------------------------------
#' Function to create data for age stacked for graphs, final report as
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

createGraphData2OptionAge <- function(df = asData, 
                                      originVar,
                                      totalColumn = T){
  require(tidyverse)
  graphData <- df %>% select(originVar, age) 
  graphData$y <- graphData[[originVar]]
  graphData <- na.omit(graphData)
  totalData <- graphData %>%
    summarise(val1 = prop.table(wtd.table(y))[1]*100,
              val2 = prop.table(wtd.table(y))[2]*100)
  totalData[["age"]] <- "Total"
  totalData <- totalData %>%  pivot_longer(cols = starts_with("val"), 
                                           names_to = "Value", 
                                           names_prefix = "val",
                                           values_to = "Porcentaje")
  totalData$Value <- totalData$Value %>% factor(levels = c(1:2),
                                                labels = names(table(df[[originVar]])),
                                                ordered = T)
  
  graphData <- graphData %>%
    group_by(age) %>%
    summarise(val1 = prop.table(wtd.table(y))[1]*100,
              val2 = prop.table(wtd.table(y))[2]*100)%>%
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
#  2option Stack Educación Madre --------------------------------------------------------------------
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

createGraphData2OptionEnglishLevel <- function(df = asData, 
                                               originVar,
                                               totalColumn = T){
  require(tidyverse)
  graphData <- df %>% select(originVar, q_AB_NI_01) 
  graphData$y <- graphData[[originVar]]
  graphData <- na.omit(graphData)
  totalData <- graphData %>%
    summarise(val1 = prop.table(wtd.table(y))[1]*100,
              val2 = prop.table(wtd.table(y))[2]*100)
  totalData[["q_AB_NI_01"]] <- factor("Total",
                                      levels = c(levels(df[["q_AB_NI_01"]]),"Total"),
                                      labels = c(levels(df[["q_AB_NI_01"]]),"Total"),
                                      ordered = T)
  totalData <- totalData %>%  pivot_longer(cols = starts_with("val"), 
                                           names_to = "Value", 
                                           names_prefix = "val",
                                           values_to = "Porcentaje")
  totalData$Value <- totalData$Value %>% factor(levels = c(1:2),
                                                labels = names(table(df[[originVar]])),
                                                ordered = T)
  
  graphData <- graphData %>%
    group_by(q_AB_NI_01) %>%
    summarise(val1 = prop.table(wtd.table(y))[1]*100,
              val2 = prop.table(wtd.table(y))[2]*100)%>%
    pivot_longer(cols = starts_with("val"), 
                 names_to = "Value", 
                 names_prefix = "val",
                 values_to = "Porcentaje")
  graphData$Value <- graphData$Value %>% factor(levels = c(1:2),
                                                labels = names(table(df[[originVar]])),
                                                ordered = T)
  graphData[["q_AB_NI_01"]] <- graphData[["q_AB_NI_01"]] %>% factor(levels = c(levels(df[["q_AB_NI_01"]]),"Total"),
                                                                    labels = c(levels(df[["q_AB_NI_01"]]),"Total"),
                                                                    ordered = T)
  if (totalColumn){
    graphData <- bind_rows(graphData, totalData)  
  }
  graphData
}
#  2option Stack q_AB_CA_01 --------------------------------------------------------------------
#' Function to create data for q_AB_CA_01 stacked for graphs, final report as
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

createGraphData2OptionBilingualAgent <- function(df = asData, 
                                                 originVar,
                                                 totalColumn = T){
  require(tidyverse)
  graphData <- df %>% select(all_of(originVar), q_AB_CA_01)
  graphData$y <- graphData[[originVar]]
  graphData <- na.omit(graphData)
  totalData <- graphData %>%
    summarise(val1 = prop.table(wtd.table(y))[1]*100,
              val2 = prop.table(wtd.table(y))[2]*100)
  totalData[["q_AB_CA_01"]] <- factor("Total",
                                      levels = c(levels(df[["q_AB_CA_01"]]),"Total"),
                                      labels = c(levels(df[["q_AB_CA_01"]]),"Total"))
  totalData <- totalData %>%  pivot_longer(cols = starts_with("val"), 
                                           names_to = "Value", 
                                           names_prefix = "val",
                                           values_to = "Porcentaje")
  totalData$Value <- totalData$Value %>% factor(levels = c(1:2),
                                                labels = names(table(df[[originVar]])),
                                                ordered = T)
  graphData <- graphData %>%
    group_by(q_AB_CA_01) %>%
    summarise(val1 = prop.table(wtd.table(y))[1]*100,
              val2 = prop.table(wtd.table(y))[2]*100)%>%
    pivot_longer(cols = starts_with("val"),
                 names_to = "Value",
                 names_prefix = "val",
                 values_to = "Porcentaje")
  graphData$Value <- graphData$Value %>% factor(levels = c(1:2),
                                                labels = names(table(df[[originVar]])),
                                                ordered = T)
  graphData[["q_AB_CA_01"]] <- graphData[["q_AB_CA_01"]] %>% factor(levels = c(names(table(df[["q_AB_CA_01"]])),"Total"),
                                                                    labels = c(names(table(df[["q_AB_CA_01"]])),"Total"))
  
  if (totalColumn){
    graphData <- bind_rows(graphData, totalData)  
  }
  graphData
}
#  2option Stack Estrato --------------------------------------------------------------------
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

createGraphData2OptionEstrato <- function(df = asData, 
                                          originVar,
                                          totalColumn = T){
  require(tidyverse)
  graphData <- df %>% select(originVar, q_AB_CA_08) 
  graphData$y <- graphData[[originVar]]
  graphData <- na.omit(graphData)
  totalData <- graphData %>%
    summarise(val1 = prop.table(wtd.table(y))[1]*100,
              val2 = prop.table(wtd.table(y))[2]*100)
  totalData[["q_AB_CA_08"]] <- factor("Total",
                                      levels = c(levels(df[["q_AB_CA_08"]]),"Total"),
                                      labels = c(levels(df[["q_AB_CA_08"]]),"Total"),
                                      ordered = T)
  totalData <- totalData %>%  pivot_longer(cols = starts_with("val"), 
                                           names_to = "Value", 
                                           names_prefix = "val",
                                           values_to = "Porcentaje")
  totalData$Value <- totalData$Value %>% factor(levels = c(1:2),
                                                labels = names(table(df[[originVar]])),
                                                ordered = T)
  
  graphData <- graphData %>%
    group_by(q_AB_CA_08) %>%
    summarise(val1 = prop.table(wtd.table(y))[1]*100,
              val2 = prop.table(wtd.table(y))[2]*100)%>%
    pivot_longer(cols = starts_with("val"), 
                 names_to = "Value", 
                 names_prefix = "val",
                 values_to = "Porcentaje")
  graphData$Value <- graphData$Value %>% factor(levels = c(1:2),
                                                labels = names(table(df[[originVar]])),
                                                ordered = T)
  graphData[["q_AB_CA_08"]] <- graphData[["q_AB_CA_08"]] %>% factor(levels = c(levels(df[["q_AB_CA_08"]]),"Total"),
                                                                    labels = c(levels(df[["q_AB_CA_08"]]),"Total"),
                                                                    ordered = T)
  if (totalColumn){
    graphData <- bind_rows(graphData, totalData)  
  }
  graphData
}



#  2option Stack Educación Propia --------------------------------------------------------------------
#' Function to create data for educación madre stacked for graphs, final report as
#'
#' This function creates graphData to create stacked graph. 
#' It assumes a 1-5 scale, values greater than 7 are missing.
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
  graphData <- df %>% select(originVar, q_AB_CA_05) 
  graphData$y <- graphData[[originVar]]
  graphData <- na.omit(graphData)
  totalData <- graphData %>%
    summarise(val1 = prop.table(wtd.table(y))[1]*100,
              val2 = prop.table(wtd.table(y))[2]*100)
  totalData[["q_AB_CA_05"]] <- factor("Total",
                                      levels = c(levels(df[["q_AB_CA_05"]]),"Total"),
                                      labels = c(levels(df[["q_AB_CA_05"]]),"Total"),
                                      ordered = T)
  totalData <- totalData %>%  pivot_longer(cols = starts_with("val"), 
                                           names_to = "Value", 
                                           names_prefix = "val",
                                           values_to = "Porcentaje")
  totalData$Value <- totalData$Value %>% factor(levels = c(1:2),
                                                labels = names(table(df[[originVar]])),
                                                ordered = T)
  
  graphData <- graphData %>%
    group_by(q_AB_CA_05) %>%
    summarise(val1 = prop.table(wtd.table(y))[1]*100,
              val2 = prop.table(wtd.table(y))[2]*100)%>%
    pivot_longer(cols = starts_with("val"), 
                 names_to = "Value", 
                 names_prefix = "val",
                 values_to = "Porcentaje")
  graphData$Value <- graphData$Value %>% factor(levels = c(1:2),
                                                labels = names(table(df[[originVar]])),
                                                ordered = T)
  graphData[["q_AB_CA_05"]] <- graphData[["q_AB_CA_05"]] %>% factor(levels = c(levels(df[["q_AB_CA_05"]]),"Total"),
                                                                    labels = c(levels(df[["q_AB_CA_05"]]),"Total"),
                                                                    ordered = T)
  if (totalColumn){
    graphData <- bind_rows(graphData, totalData)  
  }
  graphData
}



#  7option Stack Gen--------------------------------------------------------------------
#' Function to create data for age stacked for graphs, final report as
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

createGraphData7OptionCluster3Agents <- function(df = asData, 
                                      originVar,
                                      totalColumn = T){
  require(tidyverse)
  graphData <- df %>% select(originVar, Cluster3Agents) 
  graphData$y <- graphData[[originVar]]
  graphData <- na.omit(graphData)
  totalData <- graphData %>%
    summarise(val1 = prop.table(wtd.table(y))[1]*100,
              val2 = prop.table(wtd.table(y))[2]*100,
              val3 = prop.table(wtd.table(y))[3]*100,
              val4 = prop.table(wtd.table(y))[4]*100,
              val5 = prop.table(wtd.table(y))[5]*100,
              val6 = prop.table(wtd.table(y))[6]*100,
              val7 = prop.table(wtd.table(y))[7]*100)
  totalData[["Cluster3Agents"]] <- "Total"
  totalData <- totalData %>%  pivot_longer(cols = starts_with("val"), 
                                           names_to = "Value", 
                                           names_prefix = "val",
                                           values_to = "Porcentaje")
  totalData$Value <- totalData$Value %>% factor(levels = c(1:7),
                                                labels = names(table(df[[originVar]])),
                                                ordered = T)
  
  graphData <- graphData %>%
    group_by(Cluster3Agents) %>%
    summarise(val1 = prop.table(wtd.table(y))[1]*100,
              val2 = prop.table(wtd.table(y))[2]*100,
              val3 = prop.table(wtd.table(y))[3]*100,
              val4 = prop.table(wtd.table(y))[4]*100,
              val5 = prop.table(wtd.table(y))[5]*100,
              val6 = prop.table(wtd.table(y))[6]*100,
              val7 = prop.table(wtd.table(y))[7]*100)%>%
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


#  7option Stack Gen--------------------------------------------------------------------
#' Function to create data for age stacked for graphs, final report as
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

createGraphData6OptionCluster3Agents <- function(df = asData, 
                                          originVar,
                                          totalColumn = T){
  require(tidyverse)
  graphData <- df %>% select(originVar, Cluster3Agents) 
  graphData$y <- graphData[[originVar]]
  graphData <- na.omit(graphData)
  totalData <- graphData %>%
    summarise(val1 = prop.table(wtd.table(y))[1]*100,
              val2 = prop.table(wtd.table(y))[2]*100,
              val3 = prop.table(wtd.table(y))[3]*100,
              val4 = prop.table(wtd.table(y))[4]*100,
              val5 = prop.table(wtd.table(y))[5]*100,
              val6 = prop.table(wtd.table(y))[6]*100)
  totalData[["Cluster3Agents"]] <- "Total"
  totalData <- totalData %>%  pivot_longer(cols = starts_with("val"), 
                                           names_to = "Value", 
                                           names_prefix = "val",
                                           values_to = "Porcentaje")
  totalData$Value <- totalData$Value %>% factor(levels = c(1:6),
                                                labels = names(table(df[[originVar]])),
                                                ordered = T)
  
  graphData <- graphData %>%
    group_by(Cluster3Agents) %>%
    summarise(val1 = prop.table(wtd.table(y))[1]*100,
              val2 = prop.table(wtd.table(y))[2]*100,
              val3 = prop.table(wtd.table(y))[3]*100,
              val4 = prop.table(wtd.table(y))[4]*100,
              val5 = prop.table(wtd.table(y))[5]*100,
              val6 = prop.table(wtd.table(y))[6]*100)%>%
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



#  7option Stack Gen--------------------------------------------------------------------
#' Function to create data for age stacked for graphs, final report as
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

createGraphData5OptionCluster3Agents <- function(df = asData, 
                                          originVar,
                                          totalColumn = T){
  require(tidyverse)
  graphData <- df %>% select(originVar, Cluster3Agents) 
  graphData$y <- graphData[[originVar]]
  graphData <- na.omit(graphData)
  totalData <- graphData %>%
    summarise(val1 = prop.table(wtd.table(y))[1]*100,
              val2 = prop.table(wtd.table(y))[2]*100,
              val3 = prop.table(wtd.table(y))[3]*100,
              val4 = prop.table(wtd.table(y))[4]*100,
              val5 = prop.table(wtd.table(y))[5]*100)
  totalData[["Cluster3Agents"]] <- "Total"
  totalData <- totalData %>%  pivot_longer(cols = starts_with("val"), 
                                           names_to = "Value", 
                                           names_prefix = "val",
                                           values_to = "Porcentaje")
  totalData$Value <- totalData$Value %>% factor(levels = c(1:5),
                                                labels = names(table(df[[originVar]])),
                                                ordered = T)
  
  graphData <- graphData %>%
    group_by(Cluster3Agents) %>%
    summarise(val1 = prop.table(wtd.table(y))[1]*100,
              val2 = prop.table(wtd.table(y))[2]*100,
              val3 = prop.table(wtd.table(y))[3]*100,
              val4 = prop.table(wtd.table(y))[4]*100,
              val5 = prop.table(wtd.table(y))[5]*100)%>%
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
