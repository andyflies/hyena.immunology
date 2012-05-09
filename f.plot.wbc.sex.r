# source("f.plot.wbc.sex.r")

f.plot.wbc.sex <- function()
{

  df.temp.full <- subset(df.data.flat, c.age == "adult")
#  df.temp.full <- subset(df.data.flat, c.sex == "f" & c.age == "adult")
#  df.temp.full <- subset(df.temp.full, !(c.id == "mp" & i.sample.number.2 == 2))
#  df.temp.full <- subset(df.temp.full, !(c.id == "gol" & i.sample.number.2 == 3))
#  df.temp.full <- subset(df.temp.full, !(c.id == "cr" & i.sample.number.2 == 1))
#  df.temp.full <- subset(df.temp.full, c.reproductive.status != "bado")
  df.temp.full <- subset(df.temp.full, !(is.na(c.sex)))
  df.temp.full <- subset(df.temp.full, !(is.na(d.total.wbc)))
  df.temp.full <- subset(df.temp.full, (c.sex == "m" & c.natal.immigrant == "immigrant") | (c.sex == "f"))

  # relevel to remove "bado"
  df.temp.full$c.reproductive.status <- factor(as.character(df.temp.full$c.reproductive.status))
  df.temp.full$c.sex <- factor(as.character(df.temp.full$c.sex))
  print("levels(df.temp.full$c.sex)")
  print(levels(df.temp.full$c.sex))
  print("levels(df.temp.full$d.age.months)")
  print(levels(df.temp.full$d.age.months))
  
  c.dependent <- c("d.lymphocytes")
  c.dependent.1 <- c("d.neutrophils")
  c.dependent.2 <- c("d.lymphocytes")
  print(c.dependent)
#  print(c.dependent.1)
#  print(c.dependent.1)
  d.dependent <- df.temp.full[[c.dependent.1]] / df.temp.full[[c.dependent.2]]
  d.dependent <- log(df.temp.full[[c.dependent]])
  c.independent.1 <- "c.sex"
  d.independent.1 <- df.temp.full$c.sex
  c.independent.2 <- "d.age.months"
  d.independent.2 <- df.temp.full$d.age.months

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
  plot(fitted(lmer.1), resid(lmer.1), main = "model 1, fitted x resid", xlab = "fitted", ylab = "resid")

  print(ad.test(resid(lmer.1)))
  print(cvm.test(resid(lmer.1)))

  par(mfrow = c(2,3))
  # Many NAs in d.age.months for males, so we need to remove the records with NAs to use this model and rederive the d.dependent and d.independent variables
  df.temp.full$d.age.months[is.na(df.temp.full$d.age.months)] <- df.temp.full$d.estimated.age.months[is.na(df.temp.full$d.age.months)]
  d.dependent <- df.temp.full[[c.dependent.1]] / df.temp.full[[c.dependent.2]]
  d.dependent <- log(df.temp.full[[c.dependent]])
  c.independent.1 <- "c.sex"
  d.independent.1 <- df.temp.full$c.sex
  c.independent.2 <- "d.age.months"
  d.independent.2 <- df.temp.full$d.age.months

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
  plot(fitted(lmer.1), resid(lmer.1), main = "model 2, fitted x resid", xlab = "fitted", ylab = "resid")

  print(ad.test(resid(lmer.1)))
  print(cvm.test(resid(lmer.1)))

  par(mfrow = c(2,3))
  print("*************************")
  print(paste("full model, no interactions"))
  lmer.0 <- lmer(d.dependent ~ 1 + (1 | c.id), data = df.temp.full, REML = FALSE)
  lmer.1 <- lmer(d.dependent ~ d.independent.1  + d.independent.2  + 
    (1 | c.id), data = df.temp.full, REML = FALSE)
  print(cftest(lmer.1))
  print(summary(lmer.1))
  print(str(lmer.1))
  plot(lmer.1@frame$"d.dependent" ~ lmer.1@frame$"d.independent.1", main = paste(c.dependent, " ~ ", c.independent.1))
##  abline(a = fixef(lmer.1)[1][[1]], b = fixef(lmer.1)[2][[1]])
  plot(lmer.1@frame$"d.dependent" ~ lmer.1@frame$"d.independent.2", main = paste(c.dependent, " ~ ", c.independent.2),
   xlab = "Age (months)", ylab = "ln (Lymphocytes)")
  abline(a = fixef(lmer.1)["(Intercept)"], b = fixef(lmer.1)["d.independent.2"])
  hist(resid(lmer.1), main = paste("Histogram of residuals - ", (dimnames(lmer.1@frame[1])[[2]]),
    "\nr.squared: ", signif(sqrt((vcov(lmer.1)@factors$Cholesky)[1,1]), digits = 5)))
  lines(density(resid(lmer.1)))                                     #
  qqnorm(resid(lmer.1), main = paste("qqnorm\n", (dimnames(lmer.1@frame[1])[[2]]), " ~ ", (dimnames(lmer.1@frame[2])[[2]])))
  qqline(resid(lmer.1))
  plot(fitted(lmer.1), resid(lmer.1), main = "model 3, fitted x resid", xlab = "fitted", ylab = "resid")

  print(ad.test(resid(lmer.1)))
  print(cvm.test(resid(lmer.1)))

  par(mfrow = c(2,3))
  print("*************************")
  print(paste("reduced 2-way interaction model (dependent 1 and 2 only)"))
  lmer.0 <- lmer(d.dependent ~ 1 + (1 | c.id), data = df.temp.full, REML = FALSE)
  lmer.1 <- lmer(d.dependent ~ d.independent.1  * d.independent.2 + 
    (1 | c.id), data = df.temp.full, REML = FALSE)
  print(cftest(lmer.1))
  print(summary(lmer.1))
  plot(d.dependent ~ d.independent.1, data = df.temp.full, main = paste(c.dependent, " ~ ", c.independent.1))
#  abline(a = fixef(lmer.1)[1][[1]], b = fixef(lmer.1)[2][[1]])
  plot(d.dependent ~ d.independent.2, data = df.temp.full, main = paste(c.dependent, " ~ ", c.independent.2))
#  abline(a = fixef(lmer.1)[1][[1]], b = fixef(lmer.1)[2][[1]])
  hist(resid(lmer.1), main = paste("Histogram of residuals - ", (dimnames(lmer.1@frame[1])[[2]]),
    "\nr.squared: ", signif(sqrt((vcov(lmer.1)@factors$Cholesky)[1,1]), digits = 5)))
  lines(density(resid(lmer.1)))                                     #
  qqnorm(resid(lmer.1), main = paste("qqnorm\n", (dimnames(lmer.1@frame[1])[[2]]), " ~ ", (dimnames(lmer.1@frame[2])[[2]])))
  qqline(resid(lmer.1))
  plot(fitted(lmer.1), resid(lmer.1), main = "model 4, fitted x resid", xlab = "fitted", ylab = "resid")

  print(ad.test(resid(lmer.1)))
  print(cvm.test(resid(lmer.1)))


  par(mfrow = c(1,1))
  plot(1,1)
}
