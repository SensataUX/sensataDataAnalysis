# createExtendedDict ---------------------------------------------------------------
# Created by: Gabriel N. Camargo-Toledo
# Modified on: Feb/18/2022
# Modified by: Gabriel N. Camargo-Toledo
# Modified on: Feb/18/2022
# Contact: gcamargo@sensata.io
# Sensata Asus VivoBook Pop!_OS 21.10 8gb Ram R4.1.2

#' Function to extend dictionary for tableau matrices
#'
#' This function creates a df with all frequency tables, weighted if necessary
#' @param dict data cleaned and ready for analysis
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
#' @import tidyverse
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
