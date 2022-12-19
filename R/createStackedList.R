#  createStackedList --------------------------------------------------------------------
#' Function to create data for graphs
#'
#' This function creates weighted graphData and plots of the stacked variety (normally likerts).
#' if you don't want data to be weighted, use weightVar = NULL
#TODO: Add df param and several others like fill labs, types of graphs, etc
#' @param varString Vector of variables to be graphed
#' @param groupVar Variable to be grouped by
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


createStackedList <- function(varString,
                          groupVar,
                          weightVar = "ponde",
                          totalColumn = F) {
  graphData <- createGraphData(df = intData,
                               originVar = varString,
                               groupVar = groupVar,
                               weightVar = "ponde",
                               totalColumn = F)

  original_title <- var_label(intData[[varString]])
  plot <- ggplot(graphData, aes(x = .data[[groupVar]], y = .data[["Porcentaje"]])) +
    geom_col(aes(fill = .data[["Value"]]),  width = 0.4) +
    theme_sensata +
    labs(fill = "Respuesta",
         title = paste(strwrap(original_title, width = 60), collapse = "\n")) +
    scale_fill_manual(values = c(PSPaleta)) +
    scale_x_discrete(labels = scales::label_wrap(13)) +
    xlab(var_label(intData[[groupVar]]))

  output <- list(graphData = graphData,
                 plot = plot)
  output
}
