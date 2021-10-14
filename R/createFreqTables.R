# createFreqTables ---------------------------------------------------------------
#Created by: Gabriel N. Camargo-Toledo
#Modified by: Gabriel N. Camargo-Toledo
#Modified on: Apr/12/2021
#Contact: gcamargo@sensata.io
#Sensata Asus VivoBook Pop!_OS 20.10 8gb Ram R4.0.4

#' Function to create  crosstabulation tables 
#'
#' This function creates a df with all frequency tables, weighted if necessary
#' @param df data cleaned and ready for analysis
#' @param rows vector of variables that will be on the rows of the table
#' @param cols vector of variables that will be on the cols of the table
#' @param weight variable that identifies weights
#' @param wide logical, if TRUE expand for each value in cols
#' @param labels vector of labels for cols
#' @param percent logical, if TRUE table will have percentage instead of count
#' @param addIdentifier logical, if TRUE column Pregunta will include identifiers.

#' 
#'   
#' @author Gabriel N. Camargo-Toledo \email{gcamargo@@sensata.io}
#' @return Df of frequency tables
#' @keywords sensata microdata frequency tables analysis
#' @import tidyverse
#'
#' @examples
#' TBD
#' @export

# TODO: Arreglar nombres de identifiers cuando wide como lista de labels manual y de columna pregunta siempre
# TODO: No sirve cuando cols = NULL


createFreqTables <- function(df,
                             rows,
                             cols = NULL,
                             weight = NULL,
                             wide = FALSE,
                             labels = "Total",
                             percent = FALSE,
                             addIdentifier = FALSE){
  
  # Packages ----------------------------------------------------------------
  require(tidyverse)
  require(questionr)

  # Forcing wide to FALSE if no cols ----------------------------------------
  if (is.null(cols) && wide){
    rlang::inform("No columns supplied and wide parameter set to TRUE, this is not possible so set as FALSE")
    wide <- FALSE
  }
  # TODO: No sirve sin columns
  # Creating objects --------------------------------------------------------
  tab <- list()
  total <- list()
  i <- 0
  for(c in cols){
    i <- i + 1
    for(r in rows){
      rowsVec <- df[[r]]
      if(!is.null(c)){
        colsVec <- df[[c]]
        marginPropTable <- 2
      } else {
        colsVec <- NULL
        marginPropTable <- NULL
        c <- "Total"
      }
      if(!is.null(weight)){
        weightVec <- df[[weight]]
      } else {
        weightVec <- NULL
      }
      
      # Saving var_label to change column Pregunta ------------------------------
      labr <- labelled::var_label(df[[r]])
      
      if (is.null(labr)){
        labr <- paste0(r, " no tiene etiqueta de pregunta")
      }
      if (addIdentifier){
        labr <- paste0(r, ": ", labr)  
      }
      
      
      if(is.null(labels)){
        labc <- c
      } else {
        labc <- labels[i]
      }
      
      # Creating table ----------------------------------------------------------
      t <- wtd.table(rowsVec, colsVec, weights = weightVec)
      #total[[paste0(r,c)]] <- addmargins(t, 2)[,ncol(t)+1]
      # Creating percentage table if asked --------------------------------------
      if (percent) {
        tab[[paste0(r,c)]] <- as.data.frame(prop.table(t, marginPropTable)*100)
        tab[[paste0(r,c)]] <- tab[[paste0(r,c)]] %>% rename("%" = Freq)
      } else {
        tab[[paste0(r,c)]] <- as.data.frame(t)
      }
      tab[[paste0(r,c)]]$Pregunta <- NA
      tab[[paste0(r,c)]][["Pregunta"]] <- labr
      print(labr)
      if (!is.null(colsVec)) {
        tab[[paste0(r,c)]][["VarCruce"]] <- labc
        tab[[paste0(r,c)]] <- tab[[paste0(r,c)]] %>% rename("Respuesta" = Var1, "Cruce" = Var2)
        tab[[paste0(r,c)]] <- tab[[paste0(r,c)]] %>% relocate(Pregunta, VarCruce, Respuesta, Cruce)
      } else {
        tab[[paste0(r,c)]] <- tab[[paste0(r,c)]] %>% rename("Respuesta" = Var1)
        tab[[paste0(r,c)]] <- tab[[paste0(r,c)]] %>% relocate(Pregunta, Respuesta)
      }
    }
  }
  tab <- do.call(bind_rows, tab)
  if (percent) {
    tab[["%"]] <- round(as.numeric(tab[["%"]]), 2)
    tab <- tab %>% mutate(`%`=str_c(`%`, "%"))
  } else {
    tab [["Freq"]] <- round(as.numeric(tab[["Freq"]]), 0)
  }
  if (wide && !percent) {
    tab <- tab %>% pivot_wider(names_from = c("VarCruce", "Cruce"), values_from = Freq)
  }
  if (wide && percent) {
    tab <- tab %>% pivot_wider(names_from = c("VarCruce", "Cruce"), values_from = `%`)
  }
  # checking labels and rows sizes ------------------------------------------
  
  if (length(cols)!=length(labels) && !is.null(cols)) {
    rlang::inform("Labels not supplied or different length than cols. Used identifier instead")
  } 
  return(tab)
}


# createTotalTables ---------------------------------------------------------------
#Created by: Gabriel N. Camargo-Toledo
#Modified by: Gabriel N. Camargo-Toledo
#Modified on: Apr/12/2021
#Contact: gcamargo@sensata.io
#Sensata Asus VivoBook Pop!_OS 20.10 8gb Ram R4.0.4

#' Function to create Total tables 
#'
#' This function creates a df with all frequency tables, weighted if necessary
#' @param df data cleaned and ready for analysis
#' @param rows vector of variables that will be on the rows of the table
#' @param weight variable that identifies weights
#' @param percent logical, if TRUE table will have percentage instead of count
#' 
#'   
#' @author Gabriel N. Camargo-Toledo \email{gcamargo@@sensata.io}
#' @return Df of frequency tables
#' @keywords sensata microdata frequency tables analysis
#' @import tidyverse
#'
#' @examples
#' TBD
#' @export


createTotalTables <- function(df,
                             rows,
                             weight = NULL,
                             percent = F,
                             addIdentifier = F){
  
  # Packages ----------------------------------------------------------------
  require(tidyverse)
  require(questionr)
  
  # Creating objects --------------------------------------------------------
  tab <- list()
  for(r in rows){
    rowsVec <- df[[r]]
    if(!is.null(weight)){
      weightVec <- df[[weight]]
    } else {
      weightVec <- NULL
    }
    
    # Saving var_label to change column Pregunta ------------------------------
    labr <- labelled::var_label(df[[r]])
    if(is.null(labr)){
      labr <- paste0(r, " no tiene etiqueta de pregunta")
    }
    if (addIdentifier){
      labr <- paste0(r, ": ", labr)  
    }

    # Creating table ----------------------------------------------------------
    t <- wtd.table(rowsVec, weights = weightVec)
    # Creating percentage table if asked --------------------------------------
    if (percent) {
      tab[[r]] <- as.data.frame(prop.table(t, NULL)*100)
      tab[[r]] <- tab[[r]] %>% rename("%" = Freq)
    } else {
      tab[[r]] <- as.data.frame(t)
    }
    tab[[r]]$Pregunta <- NA
    tab[[r]][["Pregunta"]] <- labr
    print(labr)
    tab[[r]] <- tab[[r]] %>% rename("Respuesta" = Var1)
    tab[[r]] <- tab[[r]] %>% relocate(Pregunta, Respuesta)

  }

  tab <- do.call(bind_rows, tab)
  if (percent) {
    tab[["%"]] <- round(as.numeric(tab[["%"]]), 2)
    tab <- tab %>% mutate(`%`=str_c(`%`, "%"))
  } else {
    tab [["Freq"]] <- round(as.numeric(tab[["Freq"]]), 0)
  }
  # checking labels and rows sizes ------------------------------------------
  return(tab)
}

