# createExtendedDict ---------------------------------------------------------------
# Created by: Gabriel N. Camargo-Toledo
# Modified on: Feb/18/2022
# Modified by: Gabriel N. Camargo-Toledo
# Modified on: Feb/21/2022
# Contact: gcamargo@sensata.io
# Sensata Asus VivoBook Pop!_OS 21.10 8gb Ram R4.1.2

#' Function to extend dictionary for dashboard matrices, like tableau
#'
#' This function creates an extend dictionary with the info needed for a dashboard matrix
#' @param dict dictionary from dictGenerator
#' @param questions vector of variables (question identifiers) that should be kept from the dictionary
#' @param disag vector of variables (question identifiers) that will be on the cols of the tableau matrices, the disaggregations
#' @param labels labels for the disag variables
#' @param topic vector of topics for each question
#' @param comments (optional) vector of comment for each question, default NULL
#' @param abbrev (optional) vector of abbreviated text for questions, default NULL
#'
#'
#' @author Gabriel N. Camargo-Toledo \email{gcamargo@@sensata.io}
#' @return extended dictionary
#' @keywords sensata microdata tableau tables dictionary
#' @import sensataDataProg
#'
#' @examples
#' rawData <- sensataDataProg::sensataExample
#' Dict <- dictGenerator(
#'   df = rawData,
#'   questionPrefix = "",
#'   forceOrdered = "q_AB_NI_01")

#' questions <- c("q_AB_NI_01", "q_AB_EA_07", "q_AB_RA_01")

#' disag <- c("q_AB_CA_07", "q_AB_CA_09")

#' labels <- c("Marital status", "Student")

#' topic <- c("TEMA 1", "TEMA 2", "TEMA 2")

#' createExtendedDict(dict = Dict,
#'                    questions = questions,
#'                    disag = disag,
#'                    labels = labels,
#'                    topic = topic)
#' @export

createExtendedDict <- function(dict,
                               questions,
                               disag,
                               labels,
                               topic,
                               comments = NULL,
                               abbrev = NULL) {
  # Errors
  if (length(questions) > nrow(dict)){
    rlang::abort("You provided more questions than there are on the dictionary")
  }
  if (length(disag) > length(questions)){
    rlang::abort("You provided more disaggregations than questions")
  }
  if (length(labels) != length(disag)){
    rlang::abort("You provided a different number of labels than disaggregations")
  }
  if (length(topic) != length(questions)){
    rlang::abort("You provided a different number of topics than questions")
  }
  if (!is.null(comments) && (length(comments) != length(questions))){
    rlang::abort("You provided a different number of comments than questions")
  }
  if (!is.null(abbrev) && (length(abbrev) != length(questions))){
    rlang::abort("You provided a different number of abbreviations than questions")
  }
  # Select columns from dict
  dict <- dict %>%
    select(identifier, question, type, numberOfOptions)

  # Keep questions
  dict <- dict %>% filter(identifier %in% questions | identifier %in% disag) %>%
    unique()

  # Create disag column
  dict$disag <- FALSE
  dict$disag[dict$identifier %in% disag] <- TRUE

  # Create labels column
  dict$labels <- NA
  dict$labels[dict$disag == T] <- labels

  # Create topic column
  dict$topic <- NA
  dict$topic[!(dict$identifier %in% disag)] <- topic

  # Create comments column
  if (!is.null(comments)){
    dict$comments <- comments
  }

  # Create abbrev column
  if (!is.null(abbrev)){
    dict$abbrev <- abbrev
  }

  dict
}

# createDashboardMatrix ---------------------------------------------------------------
# Created by: Gabriel N. Camargo-Toledo
# Modified on: Feb/18/2022
# Modified by: Gabriel N. Camargo-Toledo
# Modified on: Feb/21/2022
# Contact: gcamargo@sensata.io
# Sensata Asus VivoBook Pop!_OS 21.10 8gb Ram R4.1.2

#' Function to create dashboard matrices
#'
#' This function creates a df with all frequency tables, weighted if necessary
#' @param dict Extended dictionary from createExtendedDict
#' @param df interim dataframe to create tables from
#' @param weight variable that identifies weights, default NULL
#' @param addIdentifier logical, if TRUE column Pregunta will include identifiers.
#' @param total logical, if TRUE includes total tables
#'
#'
#' @author Gabriel N. Camargo-Toledo \email{gcamargo@@sensata.io}
#' @return dashboard matrices
#' @keywords sensata microdata tableau tables dictionary
#' @import sensataDataProg
#'
#' @examples
#' TBD
#' @export

createDashboardMatrix <- function(extDict,
                                  df,
                                  weight = NULL,
                                  addIdentifier = FALSE,
                                  total = TRUE){
  extDict <- extDict %>% rename("Pregunta" = "question")

  countTab <- createFreqTables(
    df = df,
    rows = extDict$identifier,
    cols = extDict$identifier[extDict$disag],
    weight = weight,
    wide = F,
    labels = extDict$labels[extDict$disag],
    percent = F,
    addIdentifier = addIdentifier
  )

  #create topic column
  topicData <- extDict %>% select(Pregunta, topic)
  countTab <- countTab %>% left_join(topicData, by = "Pregunta")
  rm(topicData)

  # Create comments column
  if (("comments" %in% colnames(extDict))){
    commentsData <- extDict %>% select(Pregunta, comments)
    countTab <- countTab %>% left_join(commentsData, by = "Pregunta")
    rm(commentsData)
  }

  # Create abbrev column
  if (("abbrev" %in% colnames(extDict))){
    abbrevData <- extDict %>% select(Pregunta, abbrev)
    countTab <- countTab %>% left_join(abbrevData, by = "Pregunta")
    rm(abbrevData)
  }

  perTab <- createFreqTables(
    df = df,
    rows = extDict$identifier,
    cols = extDict$identifier[extDict$disag],
    weight = weight,
    wide = F,
    labels = extDict$labels[extDict$disag],
    percent = T
  )

  #create topic column
  topicData <- extDict %>% select(Pregunta, topic)
  perTab <- perTab %>% left_join(topicData, by = "Pregunta")
  rm(topicData)

  # Create comments column
  if (("comments" %in% colnames(extDict))){
    commentsData <- extDict %>% select(Pregunta, comments)
    perTab <- perTab %>% left_join(commentsData, by = "Pregunta")
    rm(commentsData)
  }

  # Create abbrev column
  if (("abbrev" %in% colnames(extDict))){
    abbrevData <- extDict %>% select(Pregunta, abbrev)
    perTab <- perTab %>% left_join(abbrevData, by = "Pregunta")
    rm(abbrevData)
  }

  if(nrow(perTab) != nrow(countTab)){
    rlang::warn("Rows of percent and count are not identical")
    outputTab <- list(countTab, perTab)
  } else {
    outputTab <- full_join(countTab, perTab)
    outputTab <- outputTab %>% relocate("%", .after = "Freq")
  }



  if(nrow(perTab) != nrow(countTab)){
    rlang::warn("Joining of percent and count failed")
  }

  outputTab

}
