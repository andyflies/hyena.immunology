# source("f.plot.wbc.r")

f.plot.wbc <- function()
{
  par(mfrow = c(1,1))
  plot(1,1)
#  df.temp.full <- subset(df.data.flat, c.age == "adult")
  df.temp.full <- subset(df.data.flat, c.sex == "f" & c.age == "adult")
#  df.temp.full <- subset(df.temp.full, !(c.id == "mp" & i.sample.number.2 == 2))
#  df.temp.full <- subset(df.temp.full, !(c.id == "gol" & i.sample.number.2 == 3))
#  df.temp.full <- subset(df.temp.full, !(c.id == "cr" & i.sample.number.2 == 1))
  df.temp.full <- subset(df.temp.full, c.reproductive.status != "bado")
  df.temp.full <- subset(df.temp.full, !(is.na(i.rank)))
  df.temp.full <- subset(df.temp.full, !(is.na(d.total.wbc)))

  # relevel to remove "bado"
  df.temp.full$c.reproductive.status <- factor(as.character(df.temp.full$c.reproductive.status))

  c.dependent <- c("d.neutrophils")
  c.dependent.1 <- c("d.neutrophils")
  c.dependent.2 <- c("d.lymphocytes")
  print(c.dependent)
  print(c.dependent.1)
  print(c.dependent.1)
  d.dependent <- df.temp.full[[c.dependent.1]] / df.temp.full[[c.dependent.2]]
  d.dependent <- log((df.temp.full[[c.dependent]]))
  c.independent.1 <- "i.rank"
  d.independent.1 <- df.temp.full$i.rank
  c.independent.2 <- "d.age.months"
  d.independent.2 <- df.temp.full$d.age.months
  c.independent.3 <- "c.reproductive.status"
  d.independent.3 <- df.temp.full$c.reproductive.status

  par(mfrow = c(2,3))
  print("*************************")
  print(c.independent.1)
  lmer.0 <- lmer(d.dependent ~ 1 + (1 | c.id), data = df.temp.full, REML = FALSE)
  lmer.1 <- lmer(d.dependent ~ d.independent.1 + (1 | c.id), data = df.temp.full, REML = FALSE)
  print(cftest(lmer.1))
  print(summary(lmer.1))
  plot(d.dependent ~ d.independent.1, data = df.temp.full, main = paste(c.dependent, " ~ ", c.independent.1))
  abline(a = fixef(lmer.1)[1][[1]], b = fixef(lmer.1)[2][[1]])
  hist(resid(lmer.1), main = paste("Histogram of residuals - ", (dimnames(lmer.1@frame[1])[[2]]),
    "\nr.squared: ", signif(sqrt((vcov(lmer.1)@factors$Cholesky)[1,1]), digits = 5)))
  lines(density(resid(lmer.1)))                                     #
  qqnorm(resid(lmer.1), main = paste("qqnorm\n", (dimnames(lmer.1@frame[1])[[2]]), " ~ ", (dimnames(lmer.1@frame[2])[[2]])))
  qqline(resid(lmer.1))
  plot(fitted(lmer.1), resid(lmer.1), main = "fitted x resid", xlab = "fitted", ylab = "resid")

  print(ad.test(resid(lmer.1)))
  print(cvm.test(resid(lmer.1)))

  par(mfrow = c(2,3))

  print("*************************")
  print(c.independent.2)
  lmer.0 <- lmer(d.dependent ~ 1 + (1 | c.id), data = df.temp.full, REML = FALSE)
  lmer.1 <- lmer(d.dependent ~ d.independent.2 + (1 | c.id), data = df.temp.full, REML = FALSE)
  print(cftest(lmer.1))
  print(summary(lmer.1))
  plot(d.dependent ~ d.independent.2, data = df.temp.full, main = paste(c.dependent, " ~ ", c.independent.2))
  abline(a = fixef(lmer.1)[1][[1]], b = fixef(lmer.1)[2][[1]])
  hist(resid(lmer.1), main = paste("Histogram of residuals - ", (dimnames(lmer.1@frame[1])[[2]]),
    "\nr.squared: ", signif(sqrt((vcov(lmer.1)@factors$Cholesky)[1,1]), digits = 5)))
  lines(density(resid(lmer.1)))                                     #
  qqnorm(resid(lmer.1), main = paste("qqnorm\n", (dimnames(lmer.1@frame[1])[[2]]), " ~ ", (dimnames(lmer.1@frame[2])[[2]])))
  qqline(resid(lmer.1))
  plot(fitted(lmer.1), resid(lmer.1), main = "fitted x resid", xlab = "fitted", ylab = "resid")

  print(ad.test(resid(lmer.1)))
  print(cvm.test(resid(lmer.1)))


  par(mfrow = c(2,3))
  print("*************************")
  print(c.independent.3)
  lmer.0 <- lmer(d.dependent ~ 1 + (1 | c.id), data = df.temp.full, REML = FALSE)
  lmer.1 <- lmer(d.dependent ~ d.independent.3 + (1 | c.id), data = df.temp.full, REML = FALSE)
  print(cftest(lmer.1))
  print(summary(lmer.1))
  plot(d.dependent ~ d.independent.3, data = df.temp.full, main = paste(c.dependent, " ~ ", c.independent.3))
  abline(a = fixef(lmer.1)[1][[1]], b = fixef(lmer.1)[2][[1]])
  hist(resid(lmer.1), main = paste("Histogram of residuals - ", (dimnames(lmer.1@frame[1])[[2]]),
    "\nr.squared: ", signif(sqrt((vcov(lmer.1)@factors$Cholesky)[1,1]), digits = 5)))
  lines(density(resid(lmer.1)))                                     #
  qqnorm(resid(lmer.1), main = paste("qqnorm\n", (dimnames(lmer.1@frame[1])[[2]]), " ~ ", (dimnames(lmer.1@frame[2])[[2]])))
  qqline(resid(lmer.1))
  plot(fitted(lmer.1), resid(lmer.1), main = "fitted x resid", xlab = "fitted", ylab = "resid")

  print(ad.test(resid(lmer.1)))
  print(cvm.test(resid(lmer.1)))

  par(mfrow = c(2,3))
  print("*************************")
  print(paste("full model, no interactions"))
  lmer.0 <- lmer(d.dependent ~ 1 + (1 | c.id), data = df.temp.full, REML = FALSE)
  lmer.1 <- lmer(d.dependent ~ d.independent.1  + d.independent.2 + d.independent.3 + 
    (1 | c.id), data = df.temp.full, REML = FALSE)
  print(cftest(lmer.1))
  print(summary(lmer.1))
  plot(d.dependent ~ d.independent.1, data = df.temp.full, main = paste(c.dependent, " ~ ", c.independent.1))
#  abline(a = fixef(lmer.1)[1][[1]], b = fixef(lmer.1)[2][[1]])
  plot(d.dependent ~ d.independent.2, data = df.temp.full, main = paste(c.dependent, " ~ ", c.independent.2))
#  abline(a = fixef(lmer.1)[1][[1]], b = fixef(lmer.1)[2][[1]])
  plot(d.dependent ~ d.independent.3, data = df.temp.full, main = paste(c.dependent, " ~ ", c.independent.3))
#  abline(a = fixef(lmer.1)[1][[1]], b = fixef(lmer.1)[2][[1]])
  hist(resid(lmer.1), main = paste("Histogram of residuals - ", (dimnames(lmer.1@frame[1])[[2]]),
    "\nr.squared: ", signif(sqrt((vcov(lmer.1)@factors$Cholesky)[1,1]), digits = 5)))
  lines(density(resid(lmer.1)))                                     #
  qqnorm(resid(lmer.1), main = paste("qqnorm\n", (dimnames(lmer.1@frame[1])[[2]]), " ~ ", (dimnames(lmer.1@frame[2])[[2]])))
  qqline(resid(lmer.1))
  plot(fitted(lmer.1), resid(lmer.1), main = "fitted x resid", xlab = "fitted", ylab = "resid")

  print(ad.test(resid(lmer.1)))
  print(cvm.test(resid(lmer.1)))

  par(mfrow = c(2,3))
  print("*************************")
  print(paste("reduced 2-way interaction model (dependent 2 and 3 only)"))
  lmer.0 <- lmer(d.dependent ~ 1 + (1 | c.id), data = df.temp.full, REML = FALSE)
  lmer.1 <- lmer(d.dependent ~ d.independent.1  + d.independent.2 * d.independent.3 + 
    (1 | c.id), data = df.temp.full, REML = FALSE)
  print(cftest(lmer.1))
  print(summary(lmer.1))
  plot(d.dependent ~ d.independent.1, data = df.temp.full, main = paste(c.dependent, " ~ ", c.independent.1))
#  abline(a = fixef(lmer.1)[1][[1]], b = fixef(lmer.1)[2][[1]])
  plot(d.dependent ~ d.independent.2, data = df.temp.full, main = paste(c.dependent, " ~ ", c.independent.2))
#  abline(a = fixef(lmer.1)[1][[1]], b = fixef(lmer.1)[2][[1]])
  plot(d.dependent ~ d.independent.3, data = df.temp.full, main = paste(c.dependent, " ~ ", c.independent.3))
#  abline(a = fixef(lmer.1)[1][[1]], b = fixef(lmer.1)[2][[1]])
  hist(resid(lmer.1), main = paste("Histogram of residuals - ", (dimnames(lmer.1@frame[1])[[2]]),
    "\nr.squared: ", signif(sqrt((vcov(lmer.1)@factors$Cholesky)[1,1]), digits = 5)))
  lines(density(resid(lmer.1)))                                     #
  qqnorm(resid(lmer.1), main = paste("qqnorm\n", (dimnames(lmer.1@frame[1])[[2]]), " ~ ", (dimnames(lmer.1@frame[2])[[2]])))
  qqline(resid(lmer.1))
  plot(fitted(lmer.1), resid(lmer.1), main = "fitted x resid", xlab = "fitted", ylab = "resid")

  print(ad.test(resid(lmer.1)))
  print(cvm.test(resid(lmer.1)))

  par(mfrow = c(2,3))
  print("*************************")
  print(paste("reduced 2-way interaction model (no rank:age interaction)"))
  lmer.0 <- lmer(d.dependent ~ 1 + (1 | c.id), data = df.temp.full, REML = FALSE)
  lmer.1 <- lmer(d.dependent ~ d.independent.1  + d.independent.1 * d.independent.3 + d.independent.2 * d.independent.3 + 
    (1 | c.id), data = df.temp.full, REML = FALSE)
  print(cftest(lmer.1))
  print(summary(lmer.1))
  plot(d.dependent ~ d.independent.1, data = df.temp.full, main = paste(c.dependent, " ~ ", c.independent.1))
#  abline(a = fixef(lmer.1)[1][[1]], b = fixef(lmer.1)[2][[1]])
  plot(d.dependent ~ d.independent.2, data = df.temp.full, main = paste(c.dependent, " ~ ", c.independent.2))
#  abline(a = fixef(lmer.1)[1][[1]], b = fixef(lmer.1)[2][[1]])
  plot(d.dependent ~ d.independent.3, data = df.temp.full, main = paste(c.dependent, " ~ ", c.independent.3))
#  abline(a = fixef(lmer.1)[1][[1]], b = fixef(lmer.1)[2][[1]])
  hist(resid(lmer.1), main = paste("Histogram of residuals - ", (dimnames(lmer.1@frame[1])[[2]]),
    "\nr.squared: ", signif(sqrt((vcov(lmer.1)@factors$Cholesky)[1,1]), digits = 5)))
  lines(density(resid(lmer.1)))                                     #
  qqnorm(resid(lmer.1), main = paste("qqnorm\n", (dimnames(lmer.1@frame[1])[[2]]), " ~ ", (dimnames(lmer.1@frame[2])[[2]])))
  qqline(resid(lmer.1))
  plot(fitted(lmer.1), resid(lmer.1), main = "fitted x resid", xlab = "fitted", ylab = "resid")

  print(ad.test(resid(lmer.1)))
  print(cvm.test(resid(lmer.1)))


  print("*************************")
  print(paste("full 2-way interaction model"))
  lmer.0 <- lmer(d.dependent ~ 1 + (1 | c.id), data = df.temp.full, REML = FALSE)
  lmer.1 <- lmer(d.dependent ~ d.independent.1 * d.independent.2 + d.independent.1 * d.independent.3 + d.independent.2 * d.independent.3 + 
    (1 | c.id), data = df.temp.full, REML = FALSE)
  print(cftest(lmer.1))
  print(summary(lmer.1))
  plot(d.dependent ~ d.independent.1, data = df.temp.full, main = paste(c.dependent, " ~ ", c.independent.1))
#  abline(a = fixef(lmer.1)[1][[1]], b = fixef(lmer.1)[2][[1]])
  plot(d.dependent ~ d.independent.2, data = df.temp.full, main = paste(c.dependent, " ~ ", c.independent.2))
#  abline(a = fixef(lmer.1)[1][[1]], b = fixef(lmer.1)[2][[1]])
  plot(d.dependent ~ d.independent.3, data = df.temp.full, main = paste(c.dependent, " ~ ", c.independent.3))
#  abline(a = fixef(lmer.1)[1][[1]], b = fixef(lmer.1)[2][[1]])
  hist(resid(lmer.1), main = paste("Histogram of residuals - ", (dimnames(lmer.1@frame[1])[[2]]),
    "\nr.squared: ", signif(sqrt((vcov(lmer.1)@factors$Cholesky)[1,1]), digits = 5)))
  lines(density(resid(lmer.1)))                                     #
  qqnorm(resid(lmer.1), main = paste("qqnorm\n", (dimnames(lmer.1@frame[1])[[2]]), " ~ ", (dimnames(lmer.1@frame[2])[[2]])))
  qqline(resid(lmer.1))
  plot(fitted(lmer.1), resid(lmer.1), main = "fitted x resid", xlab = "fitted", ylab = "resid")

  print(ad.test(resid(lmer.1)))
  print(cvm.test(resid(lmer.1)))

  par(mfrow = c(1,1))
  plot(1,1)
}
