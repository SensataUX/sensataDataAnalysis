# createDf4GraphsCCV.R
# Description: Set of functions to prepare CCV microdata to create a graph 
# Created by: Gabriel N. Camargo-Toledo
# Created on: Mar/18/2021			
# Modified by: Gabriel N. Camargo-Toledo
# Modified on: Mar/24/2021
# Contact: gcamargo@sensata.io
# Sensata Asus VivoBook PopOs! 20.04 8gb Ram R4.0.4
# Likert Prom --------------------------------------------------------------------
#' Function to create data for time series stacked for graphs, final report CCV
#'
#' This function creates graphData to create time series graph. 
#' It assumes a 1-5 scale, values greater than 5 are missing.
#' It also assumes the dataStructure of the CCVFinalData
#' @param df default: CCVFinalData
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

createDataGraphProm7Options <- function(df = CCVFinalData, 
                                        originVar){
  require(tidyverse)
  graphData <- df %>% select(originVar, wave, Ponder)
  graphData[[originVar]] <- as.numeric(graphData[[originVar]])
  graphData$y <- graphData[[originVar]]
  graphData <- na.omit(graphData)
  graphData <- graphData %>%
    group_by(wave) %>%
    summarise(val1 = prop.table(wtd.table(y, weights = Ponder))[1]*100,
              val2 = prop.table(wtd.table(y, weights = Ponder))[2]*100,
              val3 = prop.table(wtd.table(y, weights = Ponder))[3]*100,
              val4 = prop.table(wtd.table(y, weights = Ponder))[4]*100,
              val5 = prop.table(wtd.table(y, weights = Ponder))[5]*100,
              val6 = prop.table(wtd.table(y, weights = Ponder))[6]*100,
              val7 = prop.table(wtd.table(y, weights = Ponder))[7]*100) %>%
    pivot_longer(cols = starts_with("val"), 
                 names_to = "Value", 
                 names_prefix = "val",
                 values_to = "Porcentaje")
  graphData
}

# Likert Prom --------------------------------------------------------------------
#' Function to create data for time series stacked for graphs, final report CCV
#'
#' This function creates graphData to create time series graph. 
#' It assumes a 1-5 scale, values greater than 5 are missing.
#' It also assumes the dataStructure of the CCVFinalData
#' @param df default: CCVFinalData
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

createDataGraphProm_stacked <- function(df = CCVFinalData, 
                                        originVar){
  require(tidyverse)
  graphData <- df %>% select(originVar, wave, Ponder)
  graphData[[originVar]] <- as.numeric(graphData[[originVar]])
  graphData$y <- graphData[[originVar]]
  graphData$y[graphData$y>5] <- NA
  graphData <- na.omit(graphData)
  graphData <- graphData %>%
    group_by(wave) %>%
    summarise(val1 = prop.table(wtd.table(y, weights = Ponder))[1]*100,
              val2 = prop.table(wtd.table(y, weights = Ponder))[2]*100,
              val3 = prop.table(wtd.table(y, weights = Ponder))[3]*100,
              val4 = prop.table(wtd.table(y, weights = Ponder))[4]*100,
              val5 = prop.table(wtd.table(y, weights = Ponder))[5]*100) %>%
    pivot_longer(cols = starts_with("val"), 
                 names_to = "Value", 
                 names_prefix = "val",
                 values_to = "Porcentaje")
  graphData
}


# Movilidad --------------------------------------------------------------------
#' Function to create data for time series stacked for graphs, final report CCV
#'
#' This function creates graphData to create time series graph. 
#' It assumes a 1-5 scale, values greater than 5 are missing.
#' It also assumes the dataStructure of the CCVFinalData
#' @param df default: CCVFinalData
#' @param originVar1 Variable to be graphed 1
#' @param originVar2 Variable to be graphed 2
#' 
#' @author Gabriel N. Camargo-Toledo \email{gcamargo@@sensata.io}
#' @return Dataframe for graphs 
#' @keywords sensata microdata metadata data-cleaning
#' @import tidyverse
#'
#' @examples
#' TBD
#' @export

createDataGraphPro2Vars <- function(df = CCVFinalData){
  require(tidyverse)
  graphData <- df %>% select(originVar, wave, Ponder)
  graphData[[originVar]] <- as.numeric(graphData[[originVar]])
  graphData$y <- graphData[[originVar]]
  graphData$y[graphData$y>5] <- NA
  graphData <- na.omit(graphData)
  graphData <- graphData %>%
    group_by(wave) %>%
    summarise(val1 = prop.table(wtd.table(y, weights = Ponder))[1]*100,
              val2 = prop.table(wtd.table(y, weights = Ponder))[2]*100,
              val3 = prop.table(wtd.table(y, weights = Ponder))[3]*100,
              val4 = prop.table(wtd.table(y, weights = Ponder))[4]*100,
              val5 = prop.table(wtd.table(y, weights = Ponder))[5]*100) %>%
    pivot_longer(cols = starts_with("val"), 
                 names_to = "Value", 
                 names_prefix = "val",
                 values_to = "Porcentaje")
  graphData
}


# Likert Stacked grouped by cities ------------------------------------------------------------------
#' Function to create data for time series stacked for graphs, final report CCV
#'
#' This function creates graphData to create time series graph. 
#' It assumes a 1-5 scale, values greater than 5 are missing.
#' It also assumes the dataStructure of the CCVFinalData
#' @param df default: CCVFinalData
#' @param ciudades either "Capital" or "No capital"
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

createDataGraphTs_stacked <- function(df = CCVFinalData, 
                                  ciudades,
                                  originVar){
  require(tidyverse)
  graphData <- subset(df, ClasificacionMunicipio == ciudades) %>% 
    select(all_of(originVar), CiudadOrd, wave, Ponder)
  graphData[[originVar]] <- as.numeric(graphData[[originVar]])
  graphData$y <- graphData[[originVar]]
  graphData$y[graphData$y>5] <- NA
  graphData <- na.omit(graphData)
  graphData <- graphData %>%
    group_by(CiudadOrd, wave) %>%
    summarise(val1 = prop.table(wtd.table(y, weights = Ponder))[1]*100,
              val2 = prop.table(wtd.table(y, weights = Ponder))[2]*100,
              val3 = prop.table(wtd.table(y, weights = Ponder))[3]*100,
              val4 = prop.table(wtd.table(y, weights = Ponder))[4]*100,
              val5 = prop.table(wtd.table(y, weights = Ponder))[5]*100) %>%
    pivot_longer(cols = starts_with("val"), 
                 names_to = "Value", 
                 names_prefix = "val",
                 values_to = "Porcentaje")
  graphData
}


# Likert 5-4 --------------------------------------------------------------
#' Function to create data for time series 4 and 5 as satisfied for graphs, final report CCV
#'
#' This function creates graphData to create time series graph. 
#' It assumes a 1-5 scale, values greater than 5 are missing. 4-5 are counted for the percentage.
#' It also assumes the dataStructure of the CCVFinalData
#' @param df default: CCVFinalData
#' @param ciudades either "Capital" or "No capital"
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

createDataGraphTs_5_4 <- function(df = CCVFinalData, 
                              ciudades,
                              originVar){
  require(tidyverse)
  graphData <- subset(df, ClasificacionMunicipio == ciudades) %>% 
    select(all_of(originVar), CiudadOrd, wave, Ponder)
  graphData[[originVar]] <- as.numeric(graphData[[originVar]])
  graphData$y <- 0
  graphData$y[graphData[[originVar]]>=4] <- 1
  graphData$y[graphData[[originVar]]>5] <- NA
  graphData <- na.omit(graphData)
  graphData <- graphData %>%
    group_by(CiudadOrd, wave) %>%
    summarise(y = prop.table(wtd.table(y, weights = Ponder))[2]*100)
  graphData
}

# Likert 1-2 --------------------------------------------------------------
#' Function to create data for time series 1 and 2 as dissatisfied for graphs, final report CCV
#'
#' This function creates graphData to create time series graph. 
#' It assumes a 1-5 scale, values greater than 5 are missing. 1-2 are counted for the percentage.
#' It also assumes the dataStructure of the CCVFinalData
#' @param df default: CCVFinalData
#' @param ciudades either "Capital" or "No capital"
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

createDataGraphTs_1_2 <- function(df = CCVFinalData, 
                                  ciudades,
                                  originVar){
  require(tidyverse)
  graphData <- subset(df, ClasificacionMunicipio == ciudades) %>% 
    select(all_of(originVar), CiudadOrd, wave, Ponder)
  graphData[[originVar]] <- as.numeric(graphData[[originVar]])
  graphData$y <- 0
  graphData$y[graphData[[originVar]]<=2] <- 1
  graphData$y[graphData[[originVar]]>5] <- NA
  graphData <- na.omit(graphData)
  graphData <- graphData %>%
    group_by(CiudadOrd, wave) %>%
    summarise(y = prop.table(wtd.table(y, weights = Ponder))[2]*100)
  graphData
}

# Lo requirió/no lo requirió --------------------------------------------------------------
#' Function to create data for alt option as requirió for graphs, final report CCV
#'
#' This function creates graphData to create time series graph. 
#' It assumes a 1-5 scale, values greater than 7 are missing. 6 are counted for the percentage.
#' It also assumes the dataStructure of the CCVFinalData
#' @param df default: CCVFinalData
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

createDataGraphNoReq <- function(df = CCVFinalData,
                                  originVar){
  require(tidyverse)
  graphData <- CCVFinalData %>% select(all_of(originVar), wave, Ponder)
  graphData[[originVar]] <- as.numeric(graphData[[originVar]])
  graphData$y <- 0
  graphData$y[graphData[[originVar]]==6] <- 1
  graphData$y[graphData[[originVar]]==7] <- NA
  graphData <- na.omit(graphData)
  graphData <- graphData %>%
    group_by(wave) %>%
    summarise(PerNoReq = prop.table(wtd.table(y, weights = Ponder))[2],
              PerReq = prop.table(wtd.table(y, weights = Ponder))[1]) %>%
    pivot_longer(cols = starts_with("Per"),
                 names_to = "Condition",
                 names_prefix = "Per",
                 values_to = "Porcentaje")
  graphData$per <- graphData$Porcentaje
  graphData$per[graphData$Condition=="NoReq"] <- NA
  graphData
}

# Lo requirió/no lo requirió por ciudad --------------------------------------------------------------
#' Function to create data for alt option as requirió for graphs, final report CCV
#'
#' This function creates graphData to create time series graph. 
#' It assumes a 1-5 scale, values greater than 7 are missing. 6 are counted for the percentage.
#' It also assumes the dataStructure of the CCVFinalData
#' @param df default: CCVFinalData
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

createDataGraphNoReqCiu <- function(df = CCVFinalData,
                                          originVar){
  require(tidyverse)
  graphData <- CCVFinalData %>% select(all_of(originVar), CiudadOrd, ClasificacionMunicipio, Ponder)
  graphData[[originVar]] <- as.numeric(graphData[[originVar]])
  graphData$y <- 0
  graphData$y[graphData[[originVar]]==6] <- 1
  graphData$y[graphData[[originVar]]==7] <- NA
  graphData <- na.omit(graphData)
  graphData <- graphData %>%
    group_by(CiudadOrd, ClasificacionMunicipio) %>%
    summarise(PerNoReq = prop.table(wtd.table(y, weights = Ponder))[2],
              PerReq = prop.table(wtd.table(y, weights = Ponder))[1]) %>%
    pivot_longer(cols = starts_with("Per"),
                 names_to = "Condition",
                 names_prefix = "Per",
                 values_to = "Porcentaje")
  graphData$per <- graphData$Porcentaje
  graphData$per[graphData$Condition=="NoReq"] <- NA
  graphData
}


# Likert 5-4 Genero --------------------------------------------------------------
#' Function to create data for time series 4 and 5 as satisfied for graphs, final report CCV
#'
#' This function creates graphData to create time series graph. 
#' It assumes a 1-5 scale, values greater than 5 are missing. 4-5 are counted for the percentage.
#' It also assumes the dataStructure of the CCVFinalData
#' @param df default: CCVFinalData
#' @param originVar Variable to be graphed
#' 
#' 
#' @author Gabriel N. Camargo-Toledo \email{gcamargo@@sensata.io}
#' @return Dataframe for graphs 
#' @keywords sensata microdata metadata data-cleaning
#' @import tidyverse
#'
#' @examples
#' TBD
#' @export

createDataGraphGen_5_4 <- function(df = CCVFinalData, 
                                  originVar){
  require(tidyverse)
  graphData <- df %>% 
    select(all_of(originVar), EVCS1, wave, Ponder)
  graphData[[originVar]] <- as.numeric(graphData[[originVar]])
  graphData$y <- 0
  graphData$y[graphData[[originVar]]>=4] <- 1
  graphData$y[graphData[[originVar]]>5] <- NA
  graphData <- na.omit(graphData)
  graphData <- graphData %>%
    group_by(EVCS1)  %>%
    summarise(Porcentaje = prop.table(wtd.table(y, weights = Ponder))[2])
  graphData
}


# Likert 1-2 Genero --------------------------------------------------------------
#' Function to create data for gender graph 1 and 2 as dissatisfied for graphs, final report CCV
#'
#' This function creates graphData to create time series graph. 
#' It assumes a 1-5 scale, values greater than 5 are missing. 1-2 are counted for the percentage.
#' It also assumes the dataStructure of the CCVFinalData
#' @param df default: CCVFinalData
#' @param originVar Variable to be graphed
#' 
#' 
#' @author Gabriel N. Camargo-Toledo \email{gcamargo@@sensata.io}
#' @return Dataframe for graphs 
#' @keywords sensata microdata metadata data-cleaning
#' @import tidyverse
#'
#' @examples
#' TBD
#' @export

createDataGraphGen_1_2 <- function(df = CCVFinalData, 
                                   originVar){
  require(tidyverse)
  graphData <- df %>% 
    select(all_of(originVar), EVCS1, wave, Ponder)
  graphData[[originVar]] <- as.numeric(graphData[[originVar]])
  graphData$y <- 0
  graphData$y[graphData[[originVar]]<=2] <- 1
  graphData$y[graphData[[originVar]]>5] <- NA
  graphData <- na.omit(graphData)
  graphData <- graphData %>%
    group_by(EVCS1)  %>%
    summarise(Porcentaje = prop.table(wtd.table(y, weights = Ponder))[2])
  graphData
}

# Likert 5-4 NSE --------------------------------------------------------------
#' Function to create data for time series 4 and 5 as satisfied for graphs, final report CCV
#'
#' This function creates graphData to create time series graph. 
#' It assumes a 1-5 scale, values greater than 5 are missing. 4-5 are counted for the percentage.
#' It also assumes the dataStructure of the CCVFinalData
#' @param df default: CCVFinalData
#' @param originVar Variable to be graphed
#' 
#' 
#' @author Gabriel N. Camargo-Toledo \email{gcamargo@@sensata.io}
#' @return Dataframe for graphs 
#' @keywords sensata microdata metadata data-cleaning
#' @import tidyverse
#'
#' @examples
#' TBD
#' @export

createDataGraphNse_5_4 <- function(df = CCVFinalData, 
                                   originVar){
  require(tidyverse)
  graphData <- df %>% 
    select(all_of(originVar), NSE, wave, Ponder)
  graphData[[originVar]] <- as.numeric(graphData[[originVar]])
  graphData$y <- 0
  graphData$y[graphData[[originVar]]>=4] <- 1
  graphData$y[graphData[[originVar]]>5] <- NA
  graphData <- na.omit(graphData)
  graphData <- graphData %>%
    group_by(NSE)  %>%
    summarise(Porcentaje = prop.table(wtd.table(y, weights = Ponder))[2])
  graphData
}

# Likert 1-2 NSE --------------------------------------------------------------
#' Function to create data for NSE graphs 1 and 2 as dissatisfied for graphs, final report CCV
#'
#' This function creates graphData to create time series graph. 
#' It assumes a 1-5 scale, values greater than 5 are missing. 1-2 are counted for the percentage.
#' It also assumes the dataStructure of the CCVFinalData
#' @param df default: CCVFinalData
#' @param originVar Variable to be graphed
#' 
#' 
#' @author Gabriel N. Camargo-Toledo \email{gcamargo@@sensata.io}
#' @return Dataframe for graphs 
#' @keywords sensata microdata metadata data-cleaning
#' @import tidyverse
#'
#' @examples
#' TBD
#' @export

createDataGraphNse_1_2 <- function(df = CCVFinalData, 
                                   originVar){
  require(tidyverse)
  graphData <- df %>% 
    select(all_of(originVar), NSE, wave, Ponder)
  graphData[[originVar]] <- as.numeric(graphData[[originVar]])
  graphData$y <- 0
  graphData$y[graphData[[originVar]]<=2] <- 1
  graphData$y[graphData[[originVar]]>5] <- NA
  graphData <- na.omit(graphData)
  graphData <- graphData %>%
    group_by(NSE)  %>%
    summarise(Porcentaje = prop.table(wtd.table(y, weights = Ponder))[2])
  graphData
}

# Likert 5-4 Edad --------------------------------------------------------------
#' Function to create data for time series 4 and 5 as satisfied for graphs, final report CCV
#'
#' This function creates graphData to create time series graph. 
#' It assumes a 1-5 scale, values greater than 5 are missing. 4-5 are counted for the percentage.
#' It also assumes the dataStructure of the CCVFinalData
#' @param df default: CCVFinalData
#' @param originVar Variable to be graphed
#' 
#' 
#' @author Gabriel N. Camargo-Toledo \email{gcamargo@@sensata.io}
#' @return Dataframe for graphs 
#' @keywords sensata microdata metadata data-cleaning
#' @import tidyverse
#'
#' @examples
#' TBD
#' @export

createDataGraphEdad_5_4 <- function(df = CCVFinalData, 
                                   originVar){
  require(tidyverse)
  graphData <- df %>% 
    select(all_of(originVar), edad, wave, Ponder)
  graphData[[originVar]] <- as.numeric(graphData[[originVar]])
  graphData$y <- 0
  graphData$y[graphData[[originVar]]>=4] <- 1
  graphData$y[graphData[[originVar]]>5] <- NA
  graphData <- na.omit(graphData)
  graphData <- graphData %>%
    group_by(edad)  %>%
    summarise(Porcentaje = prop.table(wtd.table(y, weights = Ponder))[2])
  graphData
}

# Likert 1-2 Edad --------------------------------------------------------------
#' Function to create data for age graph 1 and 2 as dissatisfied for graphs, final report CCV
#'
#' This function creates graphData to create time series graph. 
#' It assumes a 1-5 scale, values greater than 5 are missing. 1-2 are counted for the percentage.
#' It also assumes the dataStructure of the CCVFinalData
#' @param df default: CCVFinalData
#' @param originVar Variable to be graphed
#' 
#' 
#' @author Gabriel N. Camargo-Toledo \email{gcamargo@@sensata.io}
#' @return Dataframe for graphs 
#' @keywords sensata microdata metadata data-cleaning
#' @import tidyverse
#'
#' @examples
#' TBD
#' @export

createDataGraphEdad_1_2 <- function(df = CCVFinalData, 
                                    originVar){
  require(tidyverse)
  graphData <- df %>% 
    select(all_of(originVar), edad, wave, Ponder)
  graphData[[originVar]] <- as.numeric(graphData[[originVar]])
  graphData$y <- 0
  graphData$y[graphData[[originVar]]<=2] <- 1
  graphData$y[graphData[[originVar]]>5] <- NA
  graphData <- na.omit(graphData)
  graphData <- graphData %>%
    group_by(edad)  %>%
    summarise(Porcentaje = prop.table(wtd.table(y, weights = Ponder))[2])
  graphData
}

# Likert 5-4 Agregando Capitales --------------------------------------------------------------
#' Function to create data for time series 4 and 5 as satisfied for graphs, final report CCV
#'
#' This function creates graphData to create time series graph. 
#' It assumes a 1-5 scale, values greater than 5 are missing. 4-5 are counted for the percentage.
#' It also assumes the dataStructure of the CCVFinalData
#' @param df default: CCVFinalData
#' @param originVar Variable to be graphed
#' 
#' 
#' @author Gabriel N. Camargo-Toledo \email{gcamargo@@sensata.io}
#' @return Dataframe for graphs 
#' @keywords sensata microdata metadata data-cleaning
#' @import tidyverse
#'
#' @examples
#' TBD
#' @export

createDataGraphPromCap_5_4 <- function(df = CCVFinalData, 
                                    originVar){
  require(tidyverse)
  graphData <- df %>% 
    select(all_of(originVar), ClasificacionMunicipio, wave, Ponder)
  graphData[[originVar]] <- as.numeric(graphData[[originVar]])
  graphData$y <- 0
  graphData$y[graphData[[originVar]]>=4] <- 1
  graphData$y[graphData[[originVar]]>5] <- NA
  graphData <- na.omit(graphData)
  total <- graphData %>% 
    group_by(wave) %>%
    summarise(Porcentaje = prop.table(wtd.table(y, weights = Ponder))[2]*100)
  total$ClasificacionMunicipio <- "Total"
  graphData <- graphData %>%
    group_by(ClasificacionMunicipio, wave)  %>%
    summarise(Porcentaje = prop.table(wtd.table(y, weights = Ponder))[2]*100)
  graphData <- bind_rows(graphData, total)
}
# Likert 1-2 Agregando Capitales --------------------------------------------------------------
#' Function to create data for time series 1 and 2 as dissatisfied for graphs, final report CCV
#'
#' This function creates graphData to create time series graph. 
#' It assumes a 1-5 scale, values greater than 5 are missing. 1-2 are counted for the percentage.
#' It also assumes the dataStructure of the CCVFinalData
#' @param df default: CCVFinalData
#' @param originVar Variable to be graphed
#' 
#' 
#' @author Gabriel N. Camargo-Toledo \email{gcamargo@@sensata.io}
#' @return Dataframe for graphs 
#' @keywords sensata microdata metadata data-cleaning
#' @import tidyverse
#'
#' @examples
#' TBD
#' @export

createDataGraphPromCap_1_2 <- function(df = CCVFinalData, 
                                       originVar){
  require(tidyverse)
  graphData <- df %>% 
    select(all_of(originVar), ClasificacionMunicipio, wave, Ponder)
  graphData[[originVar]] <- as.numeric(graphData[[originVar]])
  graphData$y <- 0
  graphData$y[graphData[[originVar]]<=2] <- 1
  graphData$y[graphData[[originVar]]>5] <- NA
  graphData <- na.omit(graphData)
  total <- graphData %>% 
    group_by(wave) %>%
    summarise(Porcentaje = prop.table(wtd.table(y, weights = Ponder))[2]*100)
  total$ClasificacionMunicipio <- "Total"
  graphData <- graphData %>%
    group_by(ClasificacionMunicipio, wave)  %>%
    summarise(Porcentaje = prop.table(wtd.table(y, weights = Ponder))[2]*100)
  graphData <- bind_rows(graphData, total)
}
# DummyCreator TS --------------------------------------------------------------
#' Function to create data for time series for level selected for graphs, final report CCV
#'
#' This function creates graphData to create time series graph. 
#' The level required must be chosen with the position of the level, not the text.
#' It also assumes the dataStructure of the CCVFinalData
#' @param df default: CCVFinalData
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

createDataGraphDumTs <- function(df = CCVFinalData,
                                   ciudades,
                                   originVar,
                                   levelToDummy = 1){
  require(tidyverse)
  graphData <- subset(df, ClasificacionMunicipio == ciudades) %>% 
    select(all_of(originVar), CiudadOrd, wave, Ponder)
  graphData$y <- 0
  graphData$y[graphData[[originVar]]==levels(graphData[[originVar]])[levelToDummy]] <- 1
  graphData <- na.omit(graphData)
  graphData <- graphData %>%
    group_by(CiudadOrd, wave) %>%
    summarise(y = prop.table(wtd.table(y, weights = Ponder))[2]*100) 
  graphData
}


# DummyCreator Género --------------------------------------------------------------
#' Function to create data for for dummy and gender for graphs, final report CCV
#'
#' This function creates graphData to create time series graph. 
#' It assumes a 1-5 scale, values greater than 5 are missing. 4-5 are counted for the percentage.
#' It also assumes the dataStructure of the CCVFinalData
#' @param df default: CCVFinalData
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

createDataGraphDumGen <- function(df = CCVFinalData,
                                 originVar,
                                 levelToDummy = 1){
  require(tidyverse)
  graphData <- df %>% 
    select(all_of(originVar), EVCS1, wave, Ponder)
  graphData$y <- 0
  graphData$y[graphData[[originVar]]==levels(graphData[[originVar]])[levelToDummy]] <- 1
  graphData <- na.omit(graphData)
  graphData <- graphData %>%
    group_by(EVCS1) %>%
    summarise(Porcentaje = prop.table(wtd.table(y, weights = Ponder))[2]) 
  graphData
}

# DummyCreator NSE --------------------------------------------------------------
#' Function to create data for edad and dummy for graphs, final report CCV
#'
#' This function creates graphData to create time series graph. 
#' It assumes a 1-5 scale, values greater than 5 are missing. 4-5 are counted for the percentage.
#' It also assumes the dataStructure of the CCVFinalData
#' @param df default: CCVFinalData
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

createDataGraphDumNSE <- function(df = CCVFinalData,
                                  originVar,
                                  levelToDummy = 1){
  require(tidyverse)
  graphData <- df %>% 
    select(all_of(originVar), NSE, wave, Ponder)
  graphData$y <- 0
  graphData$y[graphData[[originVar]]==levels(graphData[[originVar]])[levelToDummy]] <- 1
  graphData <- na.omit(graphData)
  graphData <- graphData %>%
    group_by(NSE) %>%
    summarise(Porcentaje = prop.table(wtd.table(y, weights = Ponder))[2]) 
  graphData
}


# DummyCreator Edad --------------------------------------------------------------
#' Function to create data for edad and dummy graphs, final report CCV
#'
#' This function creates graphData to create time series graph. 
#' It assumes a 1-5 scale, values greater than 5 are missing. 4-5 are counted for the percentage.
#' It also assumes the dataStructure of the CCVFinalData
#' @param df default: CCVFinalData
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

createDataGraphDumEd <- function(df = CCVFinalData,
                                  originVar,
                                  levelToDummy = 1){
  require(tidyverse)
  graphData <- df %>% 
    select(all_of(originVar), edad, wave, Ponder)
  graphData$y <- 0
  graphData$y[graphData[[originVar]]==levels(graphData[[originVar]])[levelToDummy]] <- 1
  graphData <- na.omit(graphData)
  graphData <- graphData %>%
    group_by(edad) %>%
    summarise(Porcentaje = prop.table(wtd.table(y, weights = Ponder))[2]) 
  graphData
}

# DummyCreator Educación --------------------------------------------------------------
#' Function to create data for edad and dummy graphs, final report CCV
#'
#' This function creates graphData to create time series graph. 
#' It also assumes the dataStructure of the CCVFinalData
#' @param df default: CCVFinalData
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

createDataGraphDumEduc <- function(df = CCVFinalData,
                                 originVar,
                                 levelToDummy = 1){
  require(tidyverse)
  graphData <- df %>% 
    select(all_of(originVar), EVCS4, wave, Ponder)
  graphData$y <- 0
  graphData$y[graphData[[originVar]]==levels(graphData[[originVar]])[levelToDummy]] <- 1
  graphData <- na.omit(graphData)
  graphData <- graphData %>%
    group_by(EVCS4) %>%
    summarise(Porcentaje = prop.table(wtd.table(y, weights = Ponder))[2]) 
  graphData
}


# Dummy Agregando Capitales --------------------------------------------------------------
#' Function to create data for time series 4 and 5 as satisfied for graphs, final report CCV
#'
#' This function creates graphData to create time series graph. 
#' It assumes a 1-5 scale, values greater than 5 are missing. 4-5 are counted for the percentage.
#' It also assumes the dataStructure of the CCVFinalData
#' @param df default: CCVFinalData
#' @param originVar Variable to be graphed
#' 
#' 
#' @author Gabriel N. Camargo-Toledo \email{gcamargo@@sensata.io}
#' @return Dataframe for graphs 
#' @keywords sensata microdata metadata data-cleaning
#' @import tidyverse
#'
#' @examples
#' TBD
#' @export

createDataGraphPromCapDummy <- function(df = CCVFinalData, 
                                       originVar,
                                       levelToDummy = 1){
  require(tidyverse)
  graphData <- df %>% 
    select(all_of(originVar), ClasificacionMunicipio, wave, Ponder)
  graphData$y <- 0
  graphData$y[graphData[[originVar]]==levels(graphData[[originVar]])[levelToDummy]] <- 1
  graphData <- na.omit(graphData)
  total <- graphData %>% 
    group_by(wave) %>%
    summarise(Porcentaje = prop.table(wtd.table(y, weights = Ponder))[2]*100)
  total$ClasificacionMunicipio <- "Total"
  graphData <- graphData %>%
    group_by(ClasificacionMunicipio, wave)  %>%
    summarise(Porcentaje = prop.table(wtd.table(y, weights = Ponder))[2]*100)
  graphData <- bind_rows(graphData, total)
}
