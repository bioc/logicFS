plot.predict.survivalFS <- function (x, select_obs, xlab = "time", ylab = NULL, 
                                     ylim = NULL, type = "l", main = NULL, sub = NULL, 
                                     vec_col = NULL, vec_lty = NULL, addLegend = TRUE, ...)
{
  if (missing(select_obs))
    select_obs <- c(1:min(5, nrow(x$sf)))
  if (!all(select_obs %in% c(1:nrow(x$sf)))){
    stop("For some of the selected observations there is no survival curve prediction.")
  }
  if (is.null(vec_col))
    vec_col <- 1:length(select_obs)
  else {
    if (length(vec_col) != length(select_obs))
      stop("select_obs and vec_col must be of the same length.")
  }
  if (is.null(vec_lty))
    vec_lty <- 1:length(select_obs)
  else {
    if (length(vec_lty) != length(select_obs))
      stop("select_obs and vec_lty must be of the same length.")
  }
  if (is.null(ylab))
    ylab <- ifelse(names(x$pred_score) == "Brier score", "survival probability", "cumulative hazard")
  if (is.null(ylim))
    ylim <- c(0, max(x$sf[select_obs,]))
  if (is.null(main))
    main <- ifelse(names(x$pred_score) == "Brier score", "Survival function", "Cumulative hazard function")
  if (is.null(sub))
    sub <- paste0(names(x$pred_score), " of prediction model:", round(x$pred_score, 5))
  # First plot one survival curve
  plot(x$uni.death.times, x$sf[select_obs[1], ], xlab = xlab, ylab = ylab, type = type,
       ylim = ylim, col = vec_col[1], lty = vec_lty[1], ...)
  # Add title to plot
  title(main = main, sub = sub)
  # Add other survival curves to plot
  if (length(select_obs) > 1){
    for (j in 2:length(select_obs)){
      lines(x$uni.death.times, x$sf[select_obs[j], ], col = vec_col[j], lty = vec_lty[j])
    }
  }
  if (addLegend){
    cord <- if (names(x$pred_score) == "Brier score") "topright" else "bottomright"
    
    legend(cord, legend = paste0(rownames(x$sf)[select_obs], 
                                 if (names(x$pred_score) != "Brier score") " (PO = ", 
                                 if (names(x$pred_score) != "Brier score") round(rowSums(x$sf)[select_obs], 1), 
                                 if (names(x$pred_score) != "Brier score") ")"), 
           col = vec_col, lwd = rep(1, length(select_obs)), lty = vec_lty)
  }
}