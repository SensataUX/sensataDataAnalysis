# TODO: UPDATE to create confidence intervals
#' Function for predicting % shares from a multinomial logit model
#' Based on chapter 13 of R for Marketing Research and Analytics https://link.springer.com/book/10.1007/978-3-319-14436-8
#' licensed with apache

#  predictMnl --------------------------------------------------------------------
#' Function for predicting % shares from a multinomial logit model
#' Based on chapter 13 of R for Marketing Research and Analytics https://link.springer.com/book/10.1007/978-3-319-14436-8
#' licensed with apache
#'
#'
#' @param model mlogit object returned by mlogit()
#' @param data a data frame containing the set of designs for which you want to predict shares. Same format as the data used to estimate model.
#'
#' @author Gabriel N. Camargo-Toledo \email{gcamargo@@sensata.io}
#' @return Dataframe with shares per level
#' @keywords sensata microdata analysis multinomial-logit conjoint
#' @import tidyverse
#' @import sensataDataProg
#'
#' @examples
#' TBD
#' @export


predictMnl <- function(model,data){
  data.model <- model.matrix(update(model$formula, 0~.) , data = data) [,-1]

  utility <- data.model %*% model$coef
  share <- exp(utility)/sum(exp(utility))*100
  cbind(share,data)
}
