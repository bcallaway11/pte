#' @title ggpte
#'
#' @description Simple event study plots for panel treatment effects
#'
#' @param pte_results A \code{pte_results} object
#'
#' @export
ggpte <- function(pte_results) {
  plot_df <- summary(pte_results)$event_study
  colnames(plot_df) <- c("e", "att", "se", "cil", "ciu")
  plot_df$post <- as.factor(1*(plot_df$e >= 0))
  ggplot(plot_df, aes(x=e, y=att)) +
    geom_line(aes(color=post)) +
    geom_point(aes(color=post)) + 
    geom_line(aes(y=ciu), linetype="dashed", alpha=0.5) +
    geom_line(aes(y=cil), linetype="dashed", alpha=0.5) +
    theme_bw() +
    theme(legend.position="bottom")
}
