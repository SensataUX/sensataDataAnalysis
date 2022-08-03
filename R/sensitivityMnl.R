# TODO: UPDATE to create confidence intervals
#' Function for creating data for a share - sensitivity chart
#' Based on chapter 13 of R for Marketing Research and Analytics https://link.springer.com/book/10.1007/978-3-319-14436-8
#' licensed with apache

#  sensitivityMnl --------------------------------------------------------------------
#' Function for creating data for a share - sensitivity chart
#' Based on chapter 13 of R for Marketing Research and Analytics https://link.springer.com/book/10.1007/978-3-319-14436-8
#' licensed with apache
#'
#'
#' @param model mlogit object returned by mlogit()
#' @param attrib list of vectors with attribute levels to be used in sensitivity
#' @param base.data : data frame containing baseline design of target product
#' @param competitor.data : data frame containing design of competitive set
#'
#' @author Gabriel N. Camargo-Toledo \email{gcamargo@@sensata.io}
#' @return Dataframe with changes in share for sensitivity chart
#' @keywords sensata microdata analysis multinomial-logit conjoint
#' @import tidyverse
#' @import sensataDataProg
#'
#' @examples
#' TBD
#' @export

sensitivityMnl <- function(model, attrib, base.data, competitor.data){
  data <- rbind(base.data , competitor.data)
  base.share <- predict.mnl(model,data)[1,1]
  share <- NULL
  for(a in seq_along(attrib)){
    for(i in attrib[[a]]){
      data[1,] <- base.data
      data[1,a] <- i
      share <- c(share, predictMnl(model,data)[1 ,1])
    }
  }
  data.frame(level = unlist(attrib), share = share , increase = share-base.share)
}
