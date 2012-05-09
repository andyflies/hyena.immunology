# source("f.calc.inhibition.area.under.curve.r")
# This function uses "sintegral" to calculate the area under the dose-reponse curve for the control and an individual sample
#  The area under the sample curve is then subtracted from the control curve to yeild the proportion inhibition for the sample and this values is returned
f.calc.inhibition.area.under.curve <- function(df.input, c.name, i.sample, c.bacteria, ...)
{
  set.seed(123)                      # set the seed to ensure repeatability in random generator functions
  d.start.time <- Sys.time()
  
  df.sample <- subset(df.input, c.id == c.name & i.sample.number == i.sample & c.strain == c.bacteria)
#  print(df.sample)

# This code is used to get the means of the replicates; it is not needed because the output of the function "sintegral" is the same using the raw values and the means of the replicates
#  df.sample.means <- aggregate(d.percent.control.abs
#  ~ i.sample.dilution, FUN = mean, data = df.sample)
#  print(df.sample.means)

#  d.area.control <- sintegral(x = log2(df.sample.means$i.sample.dilution), fx = rep(1, 8), n.pts = 1000)
#  d.area.1000 <- sintegral(x = log2(df.sample.means$i.sample.dilution), fx = df.sample.means$d.percent.control.abs, n.pts = 1000)
#  print(d.area.control$value)
#  print(d.area.1000$value)

  d.area.control <- sintegral(x = log2(df.sample$i.sample.dilution), fx = rep(1, length(df.sample$d.percent.control.abs)), n.pts = 1000)
  d.area.sample <- sintegral(x = log2(df.sample$i.sample.dilution), fx = df.sample$d.percent.control.abs, n.pts = 1000)
  d.inhibition <- (d.area.control$value - d.area.sample$value)       # value is the estimated area under the curve
  d.prop.inhibition <- (d.inhibition / d.area.control$value)       # calculate the proportion of growth inhibited
  if(d.prop.inhibition > 1) {        d.prop.inhibition <- 1      }
  if(d.prop.inhibition < 0) {        d.prop.inhibition <- 0      }

#  print(d.area.control$value)
#  print(d.area.sample$value)
  print(d.inhibition)
  d.prop.inhibition
}

