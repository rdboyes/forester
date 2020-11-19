#' Create a forest plot from an rma object
#'
#' Converts an rma object into the required format for forestable before passing to the main function
#'
#' @param rma_object The rma object to use for the forest plot
#' @param exp_coef Logical. Should the coefficients in the rma object be exponentiated before display?
#' @param ... Control arguments to be passed to the main forestable function
#'
#' @return image
#' @export
#'
#' @examples
forestable_rma <- function(rma_object,
                           exp_coef = TRUE,
                           ...){

  args <- list(...)

  se <- sqrt(rma_object$vi)/rma_object$ni

  if(exp_coef){
    table <- data.frame(Study = rma_object$slab,
             est = exp(rma_object$yi),
             ci_low = exp(rma_object$yi - 1.96 * se),
             ci_high = exp(rma_object$yi + 1.96 * se))

    table <- tibble::add_row(table)
    table <- tibble::add_row(table, tibble::tibble_row(Study = "Overall",
                                                       est = exp(rma_object$beta),
                                                       ci_low = exp(rma_object$beta - 1.96 * rma_object$se),
                                                       ci_high = exp(rma_object$beta + 1.96 * rma_object$se)))
  }else{
    table <- data.frame(Study = rma_object$slab,
               est = rma_object$yi,
               ci_low = rma_object$yi - 1.96 * se,
               ci_high = rma_object$yi + 1.96 * se)

    table <- tibble::add_row(table)
    table <- tibble::add_row(table, tibble::tibble_row(Study = "Overall",
                                                       est = rma_object$beta,
                                                       ci_low = rma_object$beta - 1.96 * rma_object$se,
                                                       ci_high = rma_object$beta + 1.96 * rma_object$se))
  }



  args[['left_side_data']] <- dplyr::select(table, Study)
  args[['estimate']] <- table$est
  args[['ci_low']] <- table$ci_low
  args[['ci_high']] <- table$ci_high

  do.call(forestable, args)

}
