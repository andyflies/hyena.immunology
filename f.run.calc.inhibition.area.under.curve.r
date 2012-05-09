# source("f.run.calc.inhibition.area.under.curve.r")
# Calls the function f.run.calc.inhibition.area.under.curve to find the area under the curve of i.sample.dilution x (1 - d.percent.control.abs)
# df.input.list should be the data frame that contains all the  samples (i.e. c.id = 104, i.sample.number = 104
# df.input.raw is the full bka data
# *** df.input.list needs to contain the data from only one bacterial strain, so this function needs to be run twice to add the new variable to the main data frame for each strain
f.run.calc.inhibition.area.under.curve <- function(df.input.list, df.input.raw, ...)
{
  set.seed(123)                      # set the seed to ensure repeatability in random generator functions
  d.start.time <- Sys.time()

  i.number.samples <- length(df.input.list[,1])  # Get the number of records in the dataframe
  print(i.number.samples)
  d.sintegral.mic <- rep(NA, i.number.samples)
  i.counter <- 1
  for(i.counter in i.counter:i.number.samples)
  {
    print(i.counter)
    print(as.character(df.input.list[i.counter, "c.id"]))
    df.sample <- subset(df.input.raw, c.id == as.character(df.input.list[i.counter, "c.id"])
      & i.sample.number == as.character(df.input.list[i.counter, "i.sample.number"]))
    d.sintegral.mic[i.counter] <- f.calc.inhibition.area.under.curve(df.sample)
  }
  d.sintegral.mic
}

