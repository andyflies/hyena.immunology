# source("f.bka.binomial.mic.matched.r")

# print(df.data[df.data$c.id=="mls", 1:10])
# df.temp <- subset(df.data, !(c.id == "mls" & c.age == "subadult"))
# print(df.temp[df.temp$c.id=="mls",])

f.bka.binomial.mic.matched <- function()
{
  d.start.time <- Sys.time()
   
  df.temp.full <- df.bka.full.dilution
  df.temp.matched <- df.temp.full[grep("yes", df.temp.full$c.matched.subadult.adult), ]
  df.temp.matched <- df.temp.matched[grep(3, df.temp.matched$i.sample.number, invert = TRUE), ]
    
#  df.temp.matched <- subset(df.temp.matched, !(c.id == "mp" & i.sample.number.2 == 2))
#  df.temp.matched <- subset(df.temp.matched, !(c.id == "gol" & i.sample.number.2 == 3))
#  df.temp.matched <- subset(df.temp.matched, !(c.id == "cr" & i.sample.number.2 == 1))  
  df.temp.matched.ec <- subset(df.temp.matched, c.strain == "ec")
  df.temp.matched.pm <- subset(df.temp.matched, c.strain == "pm")

#  df.temp <- subset(df.temp, !(c.id == "cr" & i.sample.number.2 == 1))
#  df.temp <- subset(df.temp, !(c.id == "ger" & i.sample.number.2 == 1))
#  df.temp <- subset(df.temp, !(c.id == "jj" & i.sample.number.2 == 1))

  df.ec <- df.temp.matched.ec[, c("c.id", "i.rank", "c.age", "d.age.months", "c.reproductive.status", "d.sample.date", "c.strain", "d.mic.90", 
    "c.90.percent.inhibit", "i.sample.dilution", "i.replicate.number", "d.prey.low.density.prior", "d.prey.low.density.current")]
  df.ec$c.strain <- "Escherichia coli"
  df.ec$d.prey.density <- ((df.ec$d.prey.low.density.prior + df.ec$d.prey.low.density.current) / 2) 

  df.pm <- df.temp.matched.pm[, c("c.id", "i.rank", "c.age", "d.age.months", "c.reproductive.status", "d.sample.date", "c.strain", "d.mic.90", 
    "c.90.percent.inhibit", "i.sample.dilution", "i.replicate.number", "d.prey.low.density.prior", "d.prey.low.density.current")]
  df.pm$d.prey.density <- ((df.pm$d.prey.low.density.prior + df.pm$d.prey.low.density.current) / 2) 

  c.dependent.1 <- "c.90.percent.inhibit"
  c.independent.1 <- "c.age"
  df.ec$d.dependent.1 <- df.ec[[c.dependent.1]]
  df.pm$d.dependent.1 <- df.pm[[c.dependent.1]]
  df.ec$d.independent.1 <- df.ec[[c.independent.1]]
  df.pm$d.independent.1 <- df.pm[[c.independent.1]]

  print(summary(df.pm)) 

  print("******************************** EC *******************************")
  print("******************************** EC *******************************")
  print("******************************** model 1 *******************************")

  lmer.0.ec <- lmer(d.dependent.1 ~ 1 + (1 | c.id) + (1 | i.sample.dilution) + (1 | i.replicate.number), data = df.ec, REML = FALSE, family = binomial)
  lmer.1.ec <- lmer(d.dependent.1 ~ d.independent.1 + (1 | c.id) + (1 | i.sample.dilution) + (1 | i.replicate.number), data = df.ec, REML = FALSE, family = binomial)
  print(anova(lmer.0.ec, lmer.1.ec))
  print("summary(lmer.0.ec)") 
  print(summary(lmer.0.ec)) 
  print("summary(lmer.1.ec)") 
  print(summary(lmer.1.ec)) 
  
  plot(d.dependent.1 ~ d.independent.1, data = df.ec)

  print("******************************** PM *******************************")
  print("******************************** PM *******************************")
  print("******************************** model 1 *******************************")

  lmer.0.pm <- lmer(d.dependent.1 ~ 1 + (1 | c.id) + (1 | i.sample.dilution) + (1 | i.replicate.number), data = df.pm, REML = FALSE, family = binomial)
  lmer.1.pm <- lmer(d.dependent.1 ~ d.independent.1 + (1 | c.id) + (1 | i.sample.dilution) + (1 | i.replicate.number), data = df.pm, REML = FALSE, family = binomial)
  print(anova(lmer.0.pm, lmer.1.pm))
  print("summary(lmer.0.pm)") 
  print(summary(lmer.0.pm)) 
  print("summary(lmer.1.pm)") 
  print(summary(lmer.1.pm)) 

  plot(d.dependent.1 ~ d.independent.1, data = df.pm)

# **********************  parametric bootstrapping *****************************************
  # Compute the LR test stat for comparing to the bootstrap results
  d.lr.test.stats <- signif(as.numeric(2*(logLik(lmer.1.ec, REML = FALSE)-logLik(lmer.0.ec, REML = FALSE)), digits = 5))
  # Store the independent variable effect sizes for comparing to the bootstrap results
  d.fixef.test.stats <- signif(coef(summary(lmer.1.ec))[2, 1], digits = 5)  # ["d.independent", "Estimate"], digits = 5)
  # Store the t-stat for the independent for comparing to the bootstrap results
  d.t.test.stats <- signif(coef(summary(lmer.1.ec))[2, 3], digits = 5)    # ["d.independent", "t value"], digits = 5)
  dv.test.stats <- data.frame(d.lr.test.stats, d.fixef.test.stats, d.t.test.stats)  # store values and compare to bootstrap results
  print(dv.test.stats)
  print(paste("number of rows in fixef: ", length(coef(summary(lmer.1.ec))[,1])))
  print(paste("number of columns in fixef: ", length(coef(summary(lmer.1.ec))[1,])))

  par(mfrow = c(3,3))

  i.replicates <- 1
  c.boot.type <- "boot.parametric"
  i.output.row <- 1
  
  print(" ********************  staring f.boot.parametric ********************") 
  df.test.stats <- replicate(i.replicates, f.boot.parametric(lmer.0.ec, lmer.1.ec))
  rownames(df.test.stats) <- c("d.lr.stats", "d.fixef.stats", "d.t.stats")

  d.lr.proportion.greater.than.zero <- mean(df.test.stats[1,] < 0.00001)  # compute the proportion of LR tests that are close to zero to assess how well the LR test fit a chi square distribution
  d.lr.pval <- signif(mean(df.test.stats[1,] > d.lr.test.stats), digits = 5)  # compute the proportion of LR tests that are greater than original LR test from the LR(alt) - LR(null) test 
  d.lr.std.err <- signif(sqrt(d.lr.pval * 0.98/i.replicates), digits = 5) 
  hist(df.test.stats[1,], xlim = c(-2*max(abs(df.test.stats[1,])), 2*max(abs(df.test.stats[1,]))), 
    main = paste(c.boot.type, " of ", c.dependent.1, " ~ ", c.independent.1, 
      "\nd.lr.pval: ", signif(d.lr.pval, digits = 5),
      "\nd.lr.test.stat: ", signif(d.lr.test.stats, digits = 5),
      sep = ""))
  points(1 ~ d.lr.test.stats, pch = 8)

  d.fixef.proportion.greater.than.zero <- mean(abs(df.test.stats[2,]) < 0.00001)  # compute the proportion of regression coefficients greater than zero
  d.fixef.pval <- signif(mean(abs(df.test.stats[2,]) > abs(d.fixef.test.stats)), digits = 5)  # compute the proportion of regression coefficients that are greater than the absolute value of the original regression coefficients
  d.fixef.std.err <- signif(sqrt(abs(d.fixef.pval) * 0.98/i.replicates), digits = 5) 
  hist(df.test.stats[2,], xlim = c(-2*max(abs(df.test.stats[2,])), 2*max(abs(df.test.stats[2,]))), 
    main = paste("\nd.fixef.pval: ", 
      signif(d.fixef.pval, digits = 5), "\nd.fixef.test.stat: ", 
      signif(d.fixef.test.stats, digits = 5)))
  points(1 ~ d.fixef.test.stats, pch = 8)

  d.t.test.proportion.greater.than.zero <- mean(abs(df.test.stats[3,]) < 0.00001)  # compute the proportion of t stats greater than zero
  d.t.test.pval <- signif(mean(abs(df.test.stats[3,]) > abs(d.t.test.stats)), digits = 5)  # compute the proportion of t stats that are less than the absolute value of the original t stat
  d.t.test.std.err <- signif(sqrt(abs(d.t.test.pval) * 0.98/i.replicates), digits = 5) 
  hist(df.test.stats[3,], xlim = c(-2*max(abs(df.test.stats[3,])), 2*max(abs(df.test.stats[3,]))), 
    main = paste("d.t.test.pval: ", signif(d.t.test.pval, digits = 5), "     d.t.test.stats: ", signif(d.t.test.stats, digits = 5)))
  points(1 ~ d.t.test.stats, pch = 8)

     
################### parametric bootstrapping using simulation based on null model ###################################
######################################################################################


# *********************** Plots ****************************************************
  par(mfrow = c(3,4))
  hist(resid(lmer.1.ec), main = paste("ec", "resid(lmer.1.ec)"), freq = FALSE)
  lines(density(resid(lmer.1.ec)))                                     # 
  qqnorm(ranef(lmer.1.ec)$c.id[[1]])
  qqline(ranef(lmer.1.ec)$c.id[[1]])
  # QQ-Plot of the residual
  qqnorm(resid(lmer.1.ec))
  qqline(resid(lmer.1.ec))
  # Tukey-Anscombe plot
  plot(resid(lmer.1.ec, type="p")~fitted(lmer.1.ec))
  abline(h = 0, lty = 2)  
  dotplot(ranef(lmer.1.ec, which = "c.id", postVar = TRUE), aspect = 0.2, strip = FALSE)
  plot(fitted(lmer.1.ec), resid(lmer.1.ec), main = "fitted x resid", xlab = "fitted", ylab = "resid")
  text(fitted(lmer.1.ec), resid(lmer.1.ec), labels = paste(lmer.1.ec@frame$c.id, lmer.1.ec@frame$i.sample.number.2))      # use this to label the points in the plot
  abline(0,0)
  plot(resid(lmer.1.ec) ~ lmer.1.ec@frame[,2], main = "resid(lmer.1.ec) ~ lmer.1.ec@frame[,2]")  # lmer.rank@frame$i.rank gets the input independent variables (i.e. i.rank)
  text(resid(lmer.1.ec) ~ lmer.1.ec@frame[,2], labels = paste(lmer.1.ec@frame$c.id, lmer.1.ec@frame$i.sample.number.2))      # use this to label the points in the plot
  abline(0,0)


  par(mfrow = c(3,4))
  hist(resid(lmer.1.pm), main = paste("ec", "resid(lmer.1.pm)"), freq = FALSE)
  lines(density(resid(lmer.1.pm)))                                     # 
  qqnorm(ranef(lmer.1.pm)$c.id[[1]])
  qqline(ranef(lmer.1.pm)$c.id[[1]])
  # QQ-Plot of the residual
  qqnorm(resid(lmer.1.pm))
  qqline(resid(lmer.1.pm))
  # Tukey-Anscombe plot
  plot(resid(lmer.1.pm, type="p")~fitted(lmer.1.pm))
  abline(h = 0, lty = 2)  
  dotplot(ranef(lmer.1.pm, which = "c.id", postVar = TRUE), aspect = 0.2, strip = FALSE)
  plot(fitted(lmer.1.pm), resid(lmer.1.pm), main = "fitted x resid", xlab = "fitted", ylab = "resid")
  text(fitted(lmer.1.pm), resid(lmer.1.pm), labels = paste(lmer.1.pm@frame$c.id, lmer.1.pm@frame$i.sample.number.2))      # use this to label the points in the plot
  abline(0,0)
  plot(resid(lmer.1.pm) ~ lmer.1.pm@frame[,2], main = "resid(lmer.1.pm) ~ lmer.1.pm@frame[,2]")  # lmer.rank@frame$i.rank gets the input independent variables (i.e. i.rank)
  text(resid(lmer.1.pm) ~ lmer.1.pm@frame[,2], labels = paste(lmer.1.pm@frame$c.id, lmer.1.pm@frame$i.sample.number.2))      # use this to label the points in the plot
  abline(0,0)

    
  par(mfrow=c(1,1))
  plot(x = 1, y = 1)
  d.end.time <- Sys.time()
  print(d.end.time - d.start.time)
}