# Create ordered (clustered) heatmap using ggplot

# ordering is passed to function using an ordering (sort) variable

#> Warning: Vectorized input to `element_text()` is not officially supported.
#> Results may be unexpected or may change in future versions of ggplot2.

# PM why xvar vs xlab_var
# do we want x values or the labels of the x values


ggorder_heatmap <- function(res,
                           xvar = "x",
                           yvar = "y",
                           col_var = "value",
                           label_var = NA,
                           order_var = NA,
                           xlab_var = NA,
                           xlab = "",
                           ylab = "",
                           text_colorvar = "red",
                           legend = TRUE,
                           round = TRUE,
                           round.digits = 2){

  if(is.na(label_var)){
    label_var <- col_var
  }
  # create reorder string
    if(!is.na(order_var)) {
    xstring <- paste0("reorder(",xvar,",", order_var, ")")
  } else {
    # default is factor order of xlab_Var, but we want as default the dataframe row order
    res$order_var <- 1:nrow(res)
    order_var <- "order_var"
    xstring <- paste0("reorder(",xvar,",", order_var, ")")
  }
  # format label string
  res <- as.data.table(res)
  if(round) {
    res[, (label_var) := round(get(label_var), round.digits)]
  } else {
    res[, (label_var) := get(label_var)]
  }

  # build plot
  gp <- ggplot(res, aes_string(x = xstring,
                               y = yvar)) +
    geom_tile(aes_string(fill = col_var)) +
    geom_text(aes_string(label = label_var)) +
    scale_fill_gradient(low = "white", high = "dodgerblue3") +
    theme(axis.text.x = element_text(angle = 90,
                                     hjust = 1,
                                     vjust = 0.4,
                                     colour = "red"),
          axis.text.y = element_text(size = 8),
          panel.border = element_blank(),
          panel.grid.major = element_blank()) +
    xlab(xlab) + ylab(ylab)
  if(legend == FALSE) {
    gp <- gp + theme(legend.position = "none")
  }
  # return plot
  return(gp)
}
