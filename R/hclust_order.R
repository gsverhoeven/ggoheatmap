# Takes as argument a dataframe or matrix that contains x, y, and value col

# returns the same object but with a column that contains the cluster order of the x-values


hclust_order <- function(df,
                         xvar = "x",
                         yvar = "y",
                         value_var = "value",
                         clust_method = "complete",
                         dist_method = "euclidean") {
  df <- as.data.table(df)


  # transform from long to wide using dcast
  formula_string <- paste0(xvar, " ~ ", yvar) # x vs y
  formula <- as.formula(formula_string)

  df_wide <- data.table::dcast(formula,
                   data = df,
                   fun.aggregate = NULL, value.var = value_var)

  # move first column into rownames
  rownames(df_wide) <- as.character(df_wide[, get(xvar)])

  df_wide <- df_wide[, (xvar) := NULL]
  print(rownames(df_wide))
  # cluster using hclust / dist
  clust <- hclust(d = dist(x = df_wide,
                           method = dist_method),
                  method = clust_method )

  # extract ordering
  obs_label <- rownames(df_wide)[clust$order]

  order_table <- data.table(first_col = obs_label,
                            cluster_order = 1:nrow(df_wide))

  setnames(order_table, "first_col", xvar)

  # add ordering to original dataset
  setkeyv(order_table, xvar)  # it orders the x value
  setkeyv(df, xvar)

  df <- order_table[df]

  # return df
  return(df)
}
