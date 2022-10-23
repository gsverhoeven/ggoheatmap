# Takes as argument a dataframe or matrix that contains x, y, and value col

# returns the same object but with a column that contains the cluster order of the x-values


hclust_order <- function(df,
                         clust_method = "complete",
                         dist_method = "euclidean") {
  df <- as.data.table(df)

  var_names <- colnames(df)

  # transform from long to wide using dcast
  formula_string <- paste0(var_names[1], " ~ ", var_names[2])
  formula <- as.formula(formula_string)

  df_wide <- data.table::dcast(formula,
                   data = df,
                   fun.aggregate = length)

  # move first column into rownames
  rownames(df_wide) <- as.character(df_wide[, get(var_names[1])])

  df_wide <- df_wide[, var_names[1] := NULL]

  # cluster using hclust / dist
  clust <- hclust(d = dist(x = df_wide,
                           method = dist_method),
                  method = clust_method )

  # extract ordering
  obs_label <- as.integer(rownames(df_wide)[clust$order])

  order_table <- data.table(first_col = obs_label,
                            cluster_order = 1:nrow(df_wide))

  setnames(order_table, "first_col", var_names[1])

  # add ordering to original dataset
  setkeyv(order_table, var_names[1])
  setkeyv(df, var_names[1])

  df <- order_table[df]

  # return df
  return(df)
}
