# source("f.remove.outliers.r")

f.remove.outliers <- function (df.data, c.variable, c.transform = NULL, trim = 3)
{
  if(!is.null(c.transform))
  {
    df.data[[c.variable]] = eval(call(c.transform, df.data[[c.variable]]))
  }
  df.data$z.score = as.vector(scale(df.data[[c.variable]]))
  print(data.frame(as.character(df.data$c.id), df.data[[c.variable]], df.data$z.score))
  row.names(df.data) = 1:nrow(df.data)
  outliers = as.numeric(row.names(df.data[abs(df.data$z.score) > trim, ]))
  df.data0 = df.data
  if(length(outliers) > 0)
  {
    df.data = df.data[-outliers, , drop = TRUE]
  }
  cat("n.removed =", (nrow(df.data0) - nrow(df.data)), "\n")
  cat("percent.removed =", (nrow(df.data0) - nrow(df.data))/nrow(df.data0) * 100, "\n")
  return(list(df.data = df.data, df.data0 = df.data0, n.removed = nrow(df.data0) -
      nrow(df.data), percent.removed = (nrow(df.data0) - nrow(df.data))/nrow(df.data0) * 100))
}

