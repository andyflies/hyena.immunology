# source("f.gompertz.4.r")

# This function takes all replicates of the 8 serial dilutions for one sample from one individual and returns the gompertz dose-response fit
#   The parameters can then be used to calculate the area under the curve, which is the MIC.
#   The four-parameter Gompertz model is given by the formula
#   f(x, (b, c, d, e)) = c + (d - c) exp{- exp{b(log(x) - e)}}. (3)
#   The parameters c and d are the lower and upper limits, as for four-parameter logistic model,
#   b is the relative slope around e, and the e parameter is the logarithm of the inflection point.
#   The Gompertz model is not symmetric around any point.

#   The alteredGompertz function, in this case, relates the fractional
#   area (y) to the log of antimicrobial concentration (x), Eq.1.
#   y . A . CeÿeB.xÿM. eqn 1 where A is the lower asymptote of y (approximately zero), B
#   is a slope parameter, C is the distance between the upper and
#   lower asymptote (approximately 1) and M is the log concen-
#   tration of the in£exion point.The values of theNIC andMIC
#   are de¢ned as the intersection of the lines y.A.C andy. A, with the equation of the line tangential to the point (M, (A
#   .Ceÿ1)), respectively.
#   MIC = 10^(M + 1/B)
#   NIC = 10^(M-1.718/b) from Lambert 2000


f.gompertz.4 <- function(df.input, ...)
{
  set.seed(123)                      # set the seed to ensure repeatability in random generator functions
  d.start.time <- Sys.time()
  
  a<-subset(df.bka.raw, c.id == "art" & i.sample.number == 2 & i.time.point == 3)
  print(a)
  drm.art.1 <- drm(d.percent.control.abs ~ log2(i.sample.dilution), data = a, fct = G.4())
  print(summary(drm.art.1))
  plot(drm.art.1)

  M <- drm.art.1$coef[[4]]
  print(M)
  print("(drm.art.1$coef[[3]] - drm.art.1$coef[[2]])")
  print((drm.art.1$coef[[3]] - drm.art.1$coef[[2]]))
  points(y = (drm.art.1$coef[[3]] - drm.art.1$coef[[2]]) / 2, x = M, pch = 19)
  B <- drm.art.1$coef[[1]]
  print(B)
  d.mic <- 10^(log(M) + 1/B)
  print(d.mic)
  drm.art.1
  
  df.a.means <- aggregate(d.percent.control.abs
  ~ i.sample.dilution, FUN = mean, data = a)
  print(df.a.means)

  d.area.control <- sintegral(x = log2(df.a.means$i.sample.dilution), fx = rep(1, 8), n.pts = 1000)
  d.area.1000 <- sintegral(x = log2(df.a.means$i.sample.dilution), fx = df.a.means$d.percent.control.abs, n.pts = 1000)
  print(d.area.control$int)
  print(d.area.1000$int)

  d.area.control <- sintegral(x = log2(a$i.sample.dilution), fx = rep(1, 24), n.pts = 1000)
  d.area.1000 <- sintegral(x = log2(a$i.sample.dilution), fx = a$d.percent.control.abs, n.pts = 1000)
  print(d.area.control$int)
  print(d.area.1000$int)

}

