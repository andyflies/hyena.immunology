# source("f.clmm.matched.r")

f.clmm.matched <- function()
{
  d.start.time <- Sys.time()
   
  df.temp.full <- df.data.flat
  df.temp.matched <- df.temp.full[grep("yes", df.temp.full$c.matched.subadult.adult), ]
  df.temp.matched <- df.temp.matched[grep(3, df.temp.matched$i.sample.number, invert = TRUE), ]
  df.temp.matched <- subset(df.temp.matched, c.id != "mls")
#  df.temp.matched <- subset(df.temp.matched, c.id != "cr")  
  print(df.temp.matched[,1:8])

  df.ec <- df.temp.matched[, c("c.id", "i.rank", "c.age", "d.age.months", "c.reproductive.status", "d.sample.date", 
    "d.mic.90.bka.ec", "d.prey.low.density.prior", "d.prey.low.density.current")]
  df.ec$c.strain <- "Escherichia coli"
  df.ec$d.prey.density <- log((df.ec$d.prey.low.density.prior + df.ec$d.prey.low.density.current) / 2) 

  df.pm <- df.temp.matched[, c("c.id", "i.rank", "c.age", "d.age.months", "c.reproductive.status", "d.sample.date", "d.mic.90.bka.pm", 
    "d.prey.low.density.prior", "d.prey.low.density.current")]
  df.pm$d.prey.density <- log((df.pm$d.prey.low.density.prior + df.pm$d.prey.low.density.current) / 2) 

  c.independent.1 <- "c.age"
  df.ec$d.dependent.1 <- log2(df.ec[["d.mic.90.bka.ec"]])
  df.pm$d.dependent.1 <- log2(df.pm[["d.mic.90.bka.pm"]])
  df.ec$d.independent.1 <- df.ec[[c.independent.1]]
  df.pm$d.independent.1 <- df.pm[[c.independent.1]]

  print(summary(df.pm)) 

  print("******************************** EC *******************************")
  print("******************************** EC *******************************")
  print("******************************** model 1 *******************************")

  clmm.0.ec <- clmm(factor((d.dependent.1)) ~ 1 + (1 | c.id), data = df.ec, Hess = TRUE, link = "logit", threshold = "flexible")
  clmm.1.ec <- clmm(factor((d.dependent.1)) ~ d.independent.1 + (1 | c.id), data = df.ec, Hess = TRUE, link = "logit", threshold = "flexible")
#  print(anova(clmm.0.ec, clmm.1.ec))
  print("summary(clmm.0.ec)") 
  print(summary(clmm.0.ec))
  print("summary(clmm.1.ec)") 
  print(summary(clmm.1.ec))

  lmer.0.ec <- lmer(log2(d.dependent.1) ~ 1 + (1 | c.id), data = df.ec, REML = FALSE)
  lmer.1.ec <- lmer(log2(d.dependent.1) ~ d.independent.1 + (1 | c.id), data = df.ec, REML = FALSE)
  print(anova(lmer.0.ec, lmer.1.ec))
  print("cftest(lmer.0.ec)") 
  print(cftest(lmer.0.ec)) 
  print("cftest(lmer.1.ec)") 
  print(cftest(lmer.1.ec)) 
  print(ad.test(resid(lmer.1.ec)))

  tt.ec <- t.test(
    x = log2(df.temp.matched[df.temp.matched$c.age == "subadult", "d.mic.90.bka.ec"]), 
    y = log2(df.temp.matched[df.temp.matched$c.age == "adult", "d.mic.90.bka.ec"]), 
    alternative = "two.sided", paired = TRUE, var.equal = FALSE)
  print(tt.ec)    

  par(mfrow=c(3,3))
  plot(lmer.1.ec@frame[,1] ~ lmer.1.ec@frame[,2], 
    main = paste((dimnames(lmer.1.ec@frame[1])[[2]]), " ~ ", (dimnames(lmer.1.ec@frame[2])[[2]]), 
      "\ncftest pval: ", signif(((cftest(lmer.1.ec))$test$pvalues)[2], digits = 4),   # ["d.independent"], digits = 4), 
      "\nanova pval: ", signif((anova(lmer.0.ec, lmer.1.ec))$"Pr(>Chisq)"[2], digits = 4), 
      sep = ""), 
    xlab = "age", ylab = "log2 ( MIC )") 
  text(lmer.1.ec@frame[,1] ~ lmer.1.ec@frame[,2], labels = paste(lmer.1.ec@frame$c.id, lmer.1.ec@frame$i.sample.number.2), pos = 4, cex = 1)
    abline(a = (fixef(lmer.1.ec))[[1]], b = (fixef(lmer.1.ec))[[2]])    # a = intercept, b = slope (coefficient)
  hist(resid(lmer.1.ec), main = paste("ec", " ~ c.age"), freq = FALSE)
  lines(density(resid(lmer.1.ec)))                                     # 
  qqnorm(resid(lmer.1.ec), main = paste("qqnorm - "))
  qqline(resid(lmer.1.ec))
#      text(x = Bodywt, y = Brainwt, labels=row.names(primates), pos=4)
#      qqmath(ranef(lmer.alt, postVar = TRUE), strip = FALSE)$c.id
  plot(fitted(lmer.1.ec), resid(lmer.1.ec), main = "fitted x resid", xlab = "fitted", ylab = "resid")
  text(fitted(lmer.1.ec), resid(lmer.1.ec), labels = paste(lmer.1.ec@frame$c.id, lmer.1.ec@frame$i.sample.number.2))      # use this to label the points in the plot
  abline(0,0)
  plot(resid(lmer.1.ec) ~ lmer.1.ec@frame[,2], main = " Values x Residuals")  # lmer.rank@frame$i.rank gets the input independent variables (i.e. i.rank)
  abline(0,0)


  print("******************************** PM *******************************")
  print("******************************** PM *******************************")
  print("******************************** model 1 *******************************")
  c.dependent.1 <- "d.mic.90.bka.pm"

  clmm.0.pm <- clmm(factor((d.dependent.1)) ~ 1 + (1 | c.id), data = df.pm, Hess = TRUE, link = "logit", threshold = "flexible")
  clmm.1.pm <- clmm(factor((d.dependent.1)) ~ d.independent.1 + (1 | c.id), data = df.pm, Hess = TRUE, link = "logit", threshold = "flexible")
#  print(anova(clmm.0.pm, clmm.1.pm))
  print("summary(clmm.1.pm)") 
  print(summary(clmm.1.pm)) 

  lmer.0.pm <- lmer(log2(d.dependent.1) ~ 1 + (1 | c.id), data = df.pm, REML = FALSE)
  lmer.1.pm <- lmer(log2(d.dependent.1) ~ d.independent.1 + (1 | c.id), data = df.pm)
  print(anova(lmer.0.pm, lmer.1.pm))
  print("cftest(lmer.1.pm)") 
  print(cftest(lmer.1.pm)) 
  print(ad.test(resid(lmer.1.pm)))
  
  tt.pm <- t.test(
    x = log2(df.temp.matched[df.temp.matched$c.age == "subadult", "d.mic.90.bka.pm"]), 
    y = log2(df.temp.matched[df.temp.matched$c.age == "adult", "d.mic.90.bka.pm"]), 
    alternative = "two.sided", paired = TRUE, var.equal = FALSE)
  print(tt.pm)

  par(mfrow=c(3,3))
  plot(lmer.1.pm@frame[,1] ~ lmer.1.pm@frame[,2], 
    main = paste((dimnames(lmer.1.pm@frame[1])[[2]]), " ~ ", (dimnames(lmer.1.pm@frame[2])[[2]]), 
      "\ncftest pval: ", signif(((cftest(lmer.1.pm))$test$pvalues)[2], digits = 4),   # ["d.independent"], digits = 4), 
      "\nanova pval: ", signif((anova(lmer.0.pm, lmer.1.pm))$"Pr(>Chisq)"[2], digits = 4), 
      sep = ""), 
    xlab = "age", ylab = "log2 ( MIC )") 
  text(lmer.1.pm@frame[,1] ~ lmer.1.pm@frame[,2], labels = paste(lmer.1.pm@frame$c.id, lmer.1.pm@frame$i.sample.number.2), pos = 4, cex = 1)
    abline(a = (fixef(lmer.1.pm))[[1]], b = (fixef(lmer.1.pm))[[2]])    # a = intercept, b = slope (coefficient)
  hist(resid(lmer.1.pm), main = paste("pm", " ~ c.age"), freq = FALSE)
  lines(density(resid(lmer.1.pm)))                                     # 
  qqnorm(resid(lmer.1.pm), main = paste("qqnorm - "))
  qqline(resid(lmer.1.pm))
#      text(x = Bodywt, y = Brainwt, labels=row.names(primates), pos=4)
#      qqmath(ranef(lmer.alt, postVar = TRUE), strip = FALSE)$c.id
  plot(fitted(lmer.1.pm), resid(lmer.1.pm), main = "fitted x resid", xlab = "fitted", ylab = "resid")
  text(fitted(lmer.1.pm), resid(lmer.1.pm), labels = paste(lmer.1.pm@frame$c.id, lmer.1.pm@frame$i.sample.number.2))      # use this to label the points in the plot
  abline(0,0)
  plot(resid(lmer.1.pm) ~ lmer.1.pm@frame[,2], main = " Values x Residuals")  # lmer.rank@frame$i.rank gets the input independent variables (i.e. i.rank)
  abline(0,0)



    
  par(mfrow=c(1,1))
  plot(x = 1, y = 1)
  d.end.time <- Sys.time()
  print(d.end.time - d.start.time)
}