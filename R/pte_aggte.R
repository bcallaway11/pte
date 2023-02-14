#' @title pte_aggte
#'
#' This is a slight edit of the aggte function from the `did` package.
#' Currently, it only provides aggregations for "overall" treatment effects
#' and event studies.  It also will provide the weights directly which is
#' currently used for constructing aggregations based on distributions.
#' The other difference is that, `pte_aggte` provides inference results
#' where the only randomness is coming from the outcomes (not from the group
#' assignment nor from the covariates).
#' 
pte_aggte <- function(attgt,
                      type="overall") {

  group <- attgt$group
  time.period <- attgt$time.period
  att <- attgt$att
  ptep <- attgt$ptep
  bstrap <- ptep$bstrap
  cband <- ptep$bstrap
  alp <- ptep$alp


}
