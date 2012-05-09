# source("f.ggplot.multi.variable.working.r")

# print(df.data[df.data$c.id=="mls", 1:10])
# df.temp <- subset(df.data, !(c.id == "mls" & c.age == "subadult"))
# print(df.temp[df.temp$c.id=="mls",])

f.ggplot.multi.variable <- function()
{
  f.theme <- function(base_size = 12) {
    structure(list(
					axis.line =         theme_blank(),
					axis.text.x =       theme_text(size = base_size * 1.75, lineheight = 0.9, colour = "black", hjust = 0.5, vjust = 1, angle = 0, face = "bold"),
					axis.text.y =       theme_text(size = base_size * 1.75, lineheight = 0.9, colour = "black", hjust = 0.5, vjust = 0.5, face = "bold"),
					axis.ticks =        theme_segment(colour = "black", size = 0.5, linetype = 1),
#					axis.ticks =        theme_blank(),
					axis.title.x =      theme_text(size = base_size * 2.25, vjust = 1, face = "bold"),
					axis.title.y =      theme_text(size = base_size * 2.25, angle = 90, vjust = 0, face = "bold"),
#					axis.ticks.length = unit(0.3, "lines"),
#					axis.ticks.margin = unit(0, "lines"),
#          axis.ticks =        theme.blank(),
        					
					legend.background = theme_rect(colour = NA), 
					legend.key =        theme_rect(fill = NA, colour = NA),
					legend.key.size =   unit(1.2, "lines"),
					legend.text =       theme_text(size = base_size * 0.8),
					legend.title =      theme_text(size = base_size * 1, hjust = 0, face = "bold"),
					legend.position =   "right",
					
					panel.background =  theme_rect(fill = NA, colour = "black"), 
					panel.border =      theme_blank(), #theme_rect(fill = NA, colour = "black"), # 
					panel.grid.major =  theme_line(colour = NA, linetype = 2),
					panel.grid.minor =  theme_line(colour = NA, size = 0.25),
					panel.margin =      unit(1, "lines"),
					
					strip.background =  theme_rect(fill = "grey", colour = "black"), 
					strip.label =       function(variable, value) value, 
					strip.text.x =      theme_text(size = base_size * 2.5, face = "bold", vjust = 1),
					strip.text.y =      theme_text(size = base_size * 2.5, angle = -90),
					
					plot.background =   theme_rect(colour = NA),
					plot.title =        theme_text(size = base_size * 2, face = "bold"), #, just = c(0.5, 0.5)),
					plot.margin =       unit(c(1, 10, 1, 10), "lines")      # 1 = top, 2, side, 3 = bottom, 4 = side
			), class = "options")
  }

  df.temp.full <- subset(df.data.flat, c.sex == "f")  
#  df.temp.full <- subset(df.data.flat, c.sex == "f" & c.age == "adult")
#  df.temp.full <- subset(df.data.flat, d.age.months >= 24 )
#  df.temp.full <- subset(df.temp.full, !(c.id == "mp" & i.sample.number.2 == 2))
#  df.temp.full <- subset(df.temp.full, !(c.id == "gol" & i.sample.number.2 == 3))
#  df.temp.full <- subset(df.temp.full, !(c.id == "cr" & i.sample.number.2 == 1))  
#  df.temp.full <- subset(df.temp.full, c.reproductive.status != "bado")  
  df.temp.full <- subset(df.temp.full, c.reproductive.status != "neither")  
  df.temp.full <- subset(df.temp.full, !(is.na(i.rank)))  
  df.temp.full <- subset(df.temp.full, !(is.na(d.prey.low.density.prior)))  
  df.temp.full <- subset(df.temp.full, !(is.na(d.prey.low.density.current)))  

  print("length(df.temp.full$c.id)")
  print(length(df.temp.full$c.id))

#  df.temp <- subset(df.temp, !(c.id == "cr" & i.sample.number.2 == 1))
#  df.temp <- subset(df.temp, !(c.id == "ger" & i.sample.number.2 == 1))
#  df.temp <- subset(df.temp, !(c.id == "jj" & i.sample.number.2 == 1))

  df.ec <- df.temp.full[, c("c.id", "i.rank", "d.age.months", "c.reproductive.status", "d.sample.date", "c.strain.bka.ec", "d.mic.90.bka.ec", 
    "d.percent.inhibition.ec", "d.sintegral.mic.ec", "d.mic.90.median.bka.ec", "d.prey.low.density.prior", "d.prey.low.density.current")]
  df.ec$c.strain <- "Escherichia coli"
  df.ec$d.mic.90 <- (df.ec$d.mic.90.bka.ec)
  df.ec$d.mic.90.median <- (df.ec$d.mic.90.median.bka.ec)
  df.ec$d.percent.inhibition <- (df.ec$d.percent.inhibition.ec)
  df.ec$d.sintegral.mic <- (df.ec$d.sintegral.mic)
  df.ec$d.prey.density <- ((df.ec$d.prey.low.density.prior + df.ec$d.prey.low.density.current) / 2) 
  df.ec <- subset(df.ec, select = c("c.id", "i.rank", "d.age.months", "c.reproductive.status", "d.sample.date", "c.strain", "d.mic.90", 
    "d.percent.inhibition", "d.sintegral.mic", "d.mic.90.median", "d.prey.density"))

  df.pm <- df.temp.full[, c("c.id", "i.rank", "d.age.months", "c.reproductive.status", "d.sample.date", "c.strain.bka.pm", "d.mic.90.bka.pm", 
    "d.percent.inhibition.pm", "d.sintegral.mic.pm", "d.mic.90.median.bka.pm", "d.prey.low.density.prior", "d.prey.low.density.current")]
  df.pm$c.strain <- "Proteus mirabilis"
  df.pm$d.mic.90 <- (df.pm$d.mic.90.bka.pm)
  df.pm$d.mic.90.median <- (df.pm$d.mic.90.median.bka.pm)
  df.pm$d.percent.inhibition <- (df.pm$d.percent.inhibition.pm)
  df.pm$d.sintegral.mic <- (df.pm$d.sintegral.mic)
  df.pm$d.prey.density <- ((df.pm$d.prey.low.density.prior + df.pm$d.prey.low.density.current) / 2) 
  df.pm <- subset(df.pm, select = c("c.id", "i.rank", "d.age.months", "c.reproductive.status", "d.sample.date", "c.strain", "d.mic.90", 
    "d.percent.inhibition", "d.sintegral.mic", "d.mic.90.median", "d.prey.density"))

  df.igg <- df.temp.full[, c("c.id", "i.rank", "d.age.months", "c.reproductive.status", "d.sample.date", 
    "c.isotype.target.total.igg", "d.blank.abs.total.igg", "d.prey.low.density.prior", "d.prey.low.density.current")]
  df.igg$c.isotype <- "IgG"
  df.igg$d.absorbance <- df.igg$d.blank.abs.total.igg
  df.igg$d.prey.density <- ((df.igg$d.prey.low.density.prior + df.igg$d.prey.low.density.current)  / 2)  
  df.igg <- subset(df.igg, select = c("c.id", "i.rank", "d.age.months", "c.reproductive.status", "d.sample.date", "c.isotype", "d.absorbance",
    "d.prey.density"))

  df.igm <- df.temp.full[, c("c.id", "i.rank", "d.age.months", "c.reproductive.status", "d.sample.date", 
    "c.isotype.target.total.igm", "d.blank.abs.total.igm", "d.prey.low.density.prior", "d.prey.low.density.current")]
  df.igm$c.isotype <- "IgM"
  df.igm$d.absorbance <- df.igm$d.blank.abs.total.igm
  df.igm$d.prey.density <- ((df.igm$d.prey.low.density.prior + df.igm$d.prey.low.density.current) / 2)  
  df.igm <- subset(df.igm, select = c("c.id", "i.rank", "d.age.months", "c.reproductive.status", "d.sample.date", "c.isotype", "d.absorbance", 
    "d.prey.density"))

  df.ec$d.dependent <- log(df.ec$d.percent.inhibition)
  df.pm$d.dependent <- log(df.pm$d.percent.inhibition)
  df.ec$d.dependent <- log2(df.ec$d.mic.90)
  df.pm$d.dependent <- log2(df.pm$d.mic.90)
#  df.ec$d.dependent <- log2(df.ec$d.mic.90.median)
#  df.pm$d.dependent <- log2(df.pm$d.mic.90.median)
#  df.ec$d.dependent <- (df.ec$d.sintegral.mic)
#  df.pm$d.dependent <- (df.pm$d.sintegral.mic)
#  df.ec$d.dependent <- (df.ec$d.sintegral.mic) / 8  #  8 is the maximum area under the curve. A values of 8 for a sample would indicate complete inhibition
#  df.pm$d.dependent <- (df.pm$d.sintegral.mic) / 8
  df.ec$d.prey.density <- log(df.ec$d.prey.density)
  df.pm$d.prey.density <- log(df.pm$d.prey.density)
  df.igg$d.prey.density <- log(df.igg$d.prey.density)
  df.igm$d.prey.density <- log(df.igm$d.prey.density)

  print(data.frame(cbind(factor(df.pm$c.id), df.pm$i.rank, df.pm$d.dependent, rank(df.pm$d.dependent))))
  print(df.pm) 

  print("******************************** EC *******************************")
  print("******************************** EC *******************************")
  print("******************************** model 1 *******************************")

  lmer.0.ec <- lmer(d.dependent ~ 1 + (1 | c.id), data = df.ec, REML = FALSE)
  lmer.1.ec <- lmer(d.dependent ~ i.rank  + (1 | c.id), data = df.ec, REML = FALSE)
  print("cftest(lmer.1.ec)") 
  print(cftest(lmer.1.ec)) 
  print("summary(lmer.1.ec)") 
  print(summary(lmer.1.ec)) 
  print(lillie.test(resid(lmer.1.ec)))
  print(ad.test(resid(lmer.1.ec)))
  print(cvm.test(resid(lmer.1.ec)))

  par(mfrow = c(2,3))
  plot(lmer.1.ec@frame[,1] ~ lmer.1.ec@frame[,2], 
    main = paste((dimnames(lmer.1.ec@frame[1])[[2]]), " ~ ", (dimnames(lmer.1.ec@frame[2])[[2]]), 
      "\ncftest pval: ", signif(((cftest(lmer.1.ec))$test$pvalues)[2], digits = 4),   # ["d.independent"], digits = 4), 
      "\nanova pval: ", signif((anova(lmer.0.ec, lmer.1.ec))$"Pr(>Chisq)"[2], digits = 4), 
      sep = ""), 
    xlab = "Rank", ylab = "log ( MIC )") 
  text(lmer.1.ec@frame[,1] ~ lmer.1.ec@frame[,2], labels = paste(lmer.1.ec@frame$c.id, lmer.1.ec@frame$i.sample.number.2), pos = 4, cex = 1)
    abline(a = (fixef(lmer.1.ec))[[1]], b = (fixef(lmer.1.ec))[[2]])    # a = intercept, b = slope (coefficient)
  hist(resid(lmer.1.ec), main = paste("ec", " ~ i.rank"))
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


  print("******************************** model 2 *******************************")

  lmer.0.ec <- lmer(d.dependent ~ 1 + (1 | c.id), data = df.ec, REML = FALSE)
  lmer.1.ec <- lmer(d.dependent ~ i.rank + d.age.months + c.reproductive.status + (1 | c.id), data = df.ec, REML = FALSE)
  print("cftest(lmer.1.ec)") 
  print(cftest(lmer.1.ec)) 
  print("summary(lmer.1.ec)") 
  print(summary(lmer.1.ec)) 
  print(lillie.test(resid(lmer.1.ec)))
  print(ad.test(resid(lmer.1.ec)))
  print(cvm.test(resid(lmer.1.ec)))
  
  par(mfrow = c(2,3))
  plot(lmer.1.ec@frame[,1] ~ lmer.1.ec@frame[,2], 
    main = paste((dimnames(lmer.1.ec@frame[1])[[2]]), " ~ ", (dimnames(lmer.1.ec@frame[2])[[2]]), 
      "\ncftest pval: ", signif(((cftest(lmer.1.ec))$test$pvalues)[2], digits = 4),   # ["d.independent"], digits = 4), 
      "\nanova pval: ", signif((anova(lmer.0.ec, lmer.1.ec))$"Pr(>Chisq)"[2], digits = 4), 
      sep = ""), 
    xlab = "Rank", ylab = "log ( MIC )") 
  text(lmer.1.ec@frame[,1] ~ lmer.1.ec@frame[,2], labels = paste(lmer.1.ec@frame$c.id, lmer.1.ec@frame$i.sample.number.2), pos = 4, cex = 1)
    abline(a = (fixef(lmer.1.ec))[[1]], b = (fixef(lmer.1.ec))[[2]])    # a = intercept, b = slope (coefficient)
  hist(resid(lmer.1.ec), main = paste("ec", " ~ i.rank"))
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

  print("******************************** model 3 *******************************")

  lmer.0.ec <- lmer(d.dependent ~ 1 + (1 | c.id), data = df.ec, REML = FALSE)
  lmer.1.ec <- lmer(d.dependent ~ i.rank*c.reproductive.status + d.age.months + (1 | c.id), data = df.ec, REML = FALSE)
  print("cftest(lmer.1.ec)") 
  print(cftest(lmer.1.ec)) 
  print("summary(lmer.1.ec)") 
  print(summary(lmer.1.ec)) 
  print(lillie.test(resid(lmer.1.ec)))
  print(ad.test(resid(lmer.1.ec)))
  print(cvm.test(resid(lmer.1.ec)))

  par(mfrow = c(2,3))
  plot(lmer.1.ec@frame[,1] ~ lmer.1.ec@frame[,2], 
    main = paste((dimnames(lmer.1.ec@frame[1])[[2]]), " ~ ", (dimnames(lmer.1.ec@frame[2])[[2]]), 
      "\ncftest pval: ", signif(((cftest(lmer.1.ec))$test$pvalues)[2], digits = 4),   # ["d.independent"], digits = 4), 
      "\nanova pval: ", signif((anova(lmer.0.ec, lmer.1.ec))$"Pr(>Chisq)"[2], digits = 4), 
      sep = ""), 
    xlab = "Rank", ylab = "log ( MIC )") 
  text(lmer.1.ec@frame[,1] ~ lmer.1.ec@frame[,2], labels = paste(lmer.1.ec@frame$c.id, lmer.1.ec@frame$i.sample.number.2), pos = 4, cex = 1)
    abline(a = (fixef(lmer.1.ec))[[1]], b = (fixef(lmer.1.ec))[[2]])    # a = intercept, b = slope (coefficient)
  hist(resid(lmer.1.ec), main = paste("ec", " ~ i.rank"))
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

  print("******************************** model prey *******************************")

  lmer.0.ec <- lmer(d.dependent ~ 1 + (1 | c.id), data = df.ec, REML = FALSE)
  lmer.1.ec <- lmer(d.dependent ~ d.prey.density + (1 | c.id), data = df.ec, REML = FALSE)
  print("cftest(lmer.1.ec)") 
  print(cftest(lmer.1.ec)) 
  print("summary(lmer.1.ec)") 
  print(summary(lmer.1.ec)) 
  print(lillie.test(resid(lmer.1.ec)))
  print(ad.test(resid(lmer.1.ec)))
  print(cvm.test(resid(lmer.1.ec)))

  par(mfrow = c(2,3))
  plot(lmer.1.ec@frame[,1] ~ lmer.1.ec@frame[,2], 
    main = paste((dimnames(lmer.1.ec@frame[1])[[2]]), " ~ ", (dimnames(lmer.1.ec@frame[2])[[2]]), 
      "\ncftest pval: ", signif(((cftest(lmer.1.ec))$test$pvalues)[2], digits = 4),   # ["d.independent"], digits = 4), 
      "\nanova pval: ", signif((anova(lmer.0.ec, lmer.1.ec))$"Pr(>Chisq)"[2], digits = 4), 
      sep = ""), 
    xlab = "Prey Density", ylab = "log ( MIC )") 
  text(lmer.1.ec@frame[,1] ~ lmer.1.ec@frame[,2], labels = paste(lmer.1.ec@frame$c.id, lmer.1.ec@frame$i.sample.number.2), pos = 4, cex = 1)
    abline(a = (fixef(lmer.1.ec))[[1]], b = (fixef(lmer.1.ec))[[2]])    # a = intercept, b = slope (coefficient)
  hist(resid(lmer.1.ec), main = paste("ec", " ~ d.prey.density"))
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

  lmer.0.pm <- lmer((d.dependent) ~ 1 + (1 | c.id), data = df.pm, REML = FALSE)
  lmer.1.pm <- lmer((d.dependent) ~ i.rank  + (1 | c.id), data = df.pm, REML = FALSE)
  print("cftest(lmer.1.pm)") 
  print(cftest(lmer.1.pm)) 
  print("summary(lmer.1.pm)") 
  print(summary(lmer.1.pm)) 
  print(lillie.test(resid(lmer.1.pm)))
  print(ad.test(resid(lmer.1.pm)))
  print(cvm.test(resid(lmer.1.pm)))

  par(mfrow = c(2,3))
  plot(lmer.1.pm@frame[,1] ~ lmer.1.pm@frame[,2], 
    main = paste((dimnames(lmer.1.pm@frame[1])[[2]]), " ~ ", (dimnames(lmer.1.pm@frame[2])[[2]]), 
      "\ncftest pval: ", signif(((cftest(lmer.1.pm))$test$pvalues)[2], digits = 4),   # ["d.independent"], digits = 4), 
      "\nanova pval: ", signif((anova(lmer.0.pm, lmer.1.pm))$"Pr(>Chisq)"[2], digits = 4), 
      sep = ""), 
    xlab = "Rank", ylab = "log ( MIC )") 
  text(lmer.1.pm@frame[,1] ~ lmer.1.pm@frame[,2], labels = paste(lmer.1.pm@frame$c.id, lmer.1.pm@frame$i.sample.number.2), pos = 4, cex = 1)
    abline(a = (fixef(lmer.1.pm))[[1]], b = (fixef(lmer.1.pm))[[2]])    # a = intercept, b = slope (coefficient)
  hist(resid(lmer.1.pm), main = paste("pm", " ~ i.rank"))
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

  print("******************************** model 2 *******************************")

  lmer.0.pm <- lmer((d.dependent) ~ 1 + (1 | c.id), data = df.pm, REML = FALSE)
  lmer.1.pm <- lmer((d.dependent) ~ i.rank + d.age.months + c.reproductive.status + (1 | c.id), data = df.pm, REML = FALSE)
  print("cftest(lmer.1.pm)") 
  print(cftest(lmer.1.pm)) 
  print("summary(lmer.1.pm)") 
  print(summary(lmer.1.pm)) 
  print(lillie.test(resid(lmer.1.pm)))
  print(ad.test(resid(lmer.1.pm)))
  print(cvm.test(resid(lmer.1.pm)))

  par(mfrow = c(2,3))
  plot(lmer.1.pm@frame[,1] ~ lmer.1.pm@frame[,2], 
    main = paste((dimnames(lmer.1.pm@frame[1])[[2]]), " ~ ", (dimnames(lmer.1.pm@frame[2])[[2]]), 
      "\ncftest pval: ", signif(((cftest(lmer.1.pm))$test$pvalues)[2], digits = 4),   # ["d.independent"], digits = 4), 
      "\nanova pval: ", signif((anova(lmer.0.pm, lmer.1.pm))$"Pr(>Chisq)"[2], digits = 4), 
      sep = ""), 
    xlab = "Rank", ylab = "log ( MIC )") 
  text(lmer.1.pm@frame[,1] ~ lmer.1.pm@frame[,2], labels = paste(lmer.1.pm@frame$c.id, lmer.1.pm@frame$i.sample.number.2), pos = 4, cex = 1)
    abline(a = (fixef(lmer.1.pm))[[1]], b = (fixef(lmer.1.pm))[[2]])    # a = intercept, b = slope (coefficient)
  hist(resid(lmer.1.pm), main = paste("pm", " ~ i.rank"))
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

  print("******************************** model 3 *******************************")

  lmer.0.pm <- lmer((d.dependent) ~ 1 + (1 | c.id), data = df.pm, REML = FALSE)
  lmer.1.pm <- lmer((d.dependent) ~ i.rank*c.reproductive.status + d.age.months + (1 | c.id), data = df.pm, REML = FALSE)
  print("cftest(lmer.1.pm)") 
  print(cftest(lmer.1.pm)) 
  print("summary(lmer.1.pm)") 
  print(summary(lmer.1.pm)) 
  print(lillie.test(resid(lmer.1.pm)))
  print(ad.test(resid(lmer.1.pm)))
  print(cvm.test(resid(lmer.1.pm)))

  par(mfrow = c(2,3))
  plot(lmer.1.pm@frame[,1] ~ lmer.1.pm@frame[,2], 
    main = paste((dimnames(lmer.1.pm@frame[1])[[2]]), " ~ ", (dimnames(lmer.1.pm@frame[2])[[2]]), 
      "\ncftest pval: ", signif(((cftest(lmer.1.pm))$test$pvalues)[2], digits = 4),   # ["d.independent"], digits = 4), 
      "\nanova pval: ", signif((anova(lmer.0.pm, lmer.1.pm))$"Pr(>Chisq)"[2], digits = 4), 
      sep = ""), 
    xlab = "Rank", ylab = "log ( MIC )") 
  text(lmer.1.pm@frame[,1] ~ lmer.1.pm@frame[,2], labels = paste(lmer.1.pm@frame$c.id, lmer.1.pm@frame$i.sample.number.2), pos = 4, cex = 1)
    abline(a = (fixef(lmer.1.pm))[[1]], b = (fixef(lmer.1.pm))[[2]])    # a = intercept, b = slope (coefficient)
  hist(resid(lmer.1.pm), main = paste("pm", " ~ i.rank"))
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

  print("******************************** model prey *******************************")

  lmer.0.pm <- lmer((d.dependent) ~ 1 + (1 | c.id), data = df.pm, REML = FALSE)
  lmer.1.pm <- lmer((d.dependent) ~ d.prey.density + (1 | c.id), data = df.pm, REML = FALSE)
  print("cftest(lmer.1.pm)") 
  print(cftest(lmer.1.pm)) 
  print("summary(lmer.1.pm)") 
  print(summary(lmer.1.pm)) 
  print(lillie.test(resid(lmer.1.pm)))
  print(ad.test(resid(lmer.1.pm)))
  print(cvm.test(resid(lmer.1.pm)))

  par(mfrow = c(2,3))
  plot(lmer.1.pm@frame[,1] ~ lmer.1.pm@frame[,2], 
    main = paste((dimnames(lmer.1.pm@frame[1])[[2]]), " ~ ", (dimnames(lmer.1.pm@frame[2])[[2]]), 
      "\ncftest pval: ", signif(((cftest(lmer.1.pm))$test$pvalues)[2], digits = 4),   # ["d.independent"], digits = 4), 
      "\nanova pval: ", signif((anova(lmer.0.pm, lmer.1.pm))$"Pr(>Chisq)"[2], digits = 4), 
      sep = ""), 
    xlab = "Prey Density", ylab = "log ( MIC )") 
  text(lmer.1.pm@frame[,1] ~ lmer.1.pm@frame[,2], labels = paste(lmer.1.pm@frame$c.id, lmer.1.pm@frame$i.sample.number.2), pos = 4, cex = 1)
    abline(a = (fixef(lmer.1.pm))[[1]], b = (fixef(lmer.1.pm))[[2]])    # a = intercept, b = slope (coefficient)
  hist(resid(lmer.1.pm), main = paste("pm", " ~ i.rank"))
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

  print("******************************** IgG *******************************")
  print("******************************** IgG *******************************")
  print("******************************** model 1 *******************************")
  par(mfrow = c(4, 3))

  lmer.0.igg <- lmer(d.absorbance ~ 1 + (1 | c.id), data = df.igg, REML = FALSE)
  lmer.1.igg <- lmer(d.absorbance ~ i.rank + (1 | c.id), data = df.igg, REML = FALSE)
  print(hist(resid(lmer.1.igg)))
  print("cftest(lmer.1.igg))") 
  print(cftest(lmer.1.igg))
  print("summary(lmer.1.igg)") 
  print(summary(lmer.1.igg)) 
  print(lillie.test(resid(lmer.1.igg)))
  print(ad.test(resid(lmer.1.igg)))
  print(cvm.test(resid(lmer.1.igg)))


  print("******************************** model 2 *******************************")
  lmer.0.igg <- lmer(d.absorbance ~ 1 + (1 | c.id), data = df.igg, REML = FALSE)
  lmer.1.igg <- lmer(d.absorbance ~ i.rank + d.age.months + c.reproductive.status + (1 | c.id), data = df.igg, REML = FALSE)
  print(hist(resid(lmer.1.igg)))
  print("cftest(lmer.1.igg))") 
  print(cftest(lmer.1.igg))
  print("summary(lmer.1.igg)") 
  print(summary(lmer.1.igg)) 
  print(lillie.test(resid(lmer.1.igg)))
  print(ad.test(resid(lmer.1.igg)))
  print(cvm.test(resid(lmer.1.igg)))


  print("******************************** model 3 *******************************")
  lmer.0.igg <- lmer(d.absorbance ~ 1 + (1 | c.id), data = df.igg, REML = FALSE)
  lmer.1.igg <- lmer(d.absorbance ~ i.rank * c.reproductive.status + d.age.months + (1 | c.id), data = df.igg, REML = FALSE)
  print(hist(resid(lmer.1.igg)))
  print("cftest(lmer.1.igg))") 
  print(cftest(lmer.1.igg))
  print("summary(lmer.1.igg)") 
  print(summary(lmer.1.igg)) 
  print(lillie.test(resid(lmer.1.igg)))
  print(ad.test(resid(lmer.1.igg)))
  print(cvm.test(resid(lmer.1.igg)))

  print("******************************** model prey *******************************")
  par(mfrow = c(2, 3))
  lmer.0.igg <- lmer(d.absorbance ~ 1 + (1 | c.id), data = df.igg, REML = FALSE)
  lmer.1.igg <- lmer(d.absorbance ~ d.prey.density + (1 | c.id), data = df.igg, REML = FALSE)
  print("cftest(lmer.1.igg)") 
  print(cftest(lmer.1.igg)) 
  print("summary(lmer.1.igg)") 
  print(summary(lmer.1.igg)) 
  print(lillie.test(resid(lmer.1.igg)))
  print(ad.test(resid(lmer.1.igg)))
  print(cvm.test(resid(lmer.1.igg)))

  par(mfrow = c(2,3))
  plot(lmer.1.igg@frame[,1] ~ lmer.1.igg@frame[,2], 
    main = paste((dimnames(lmer.1.igg@frame[1])[[2]]), " ~ ", (dimnames(lmer.1.igg@frame[2])[[2]]), 
      "\ncftest pval: ", signif(((cftest(lmer.1.igg))$test$pvalues)[2], digits = 4),   # ["d.independent"], digits = 4), 
      "\nanova pval: ", signif((anova(lmer.0.igg, lmer.1.igg))$"Pr(>Chisq)"[2], digits = 4), 
      sep = ""), 
    xlab = "Prey Density", ylab = "log ( MIC )") 
  text(lmer.1.igg@frame[,1] ~ lmer.1.igg@frame[,2], labels = paste(lmer.1.igg@frame$c.id, lmer.1.igg@frame$i.sample.number.2), pos = 4, cex = 1)
    abline(a = (fixef(lmer.1.igg))[[1]], b = (fixef(lmer.1.igg))[[2]])    # a = intercept, b = slope (coefficient)
  hist(resid(lmer.1.igg), main = paste("igg", " ~ d.prey.density"))
  lines(density(resid(lmer.1.igg)))                                     # 
  qqnorm(resid(lmer.1.igg), main = paste("qqnorm - "))
  qqline(resid(lmer.1.igg))
#      text(x = Bodywt, y = Brainwt, labels=row.names(primates), pos=4)
#      qqmath(ranef(lmer.alt, postVar = TRUE), strip = FALSE)$c.id
  plot(fitted(lmer.1.igg), resid(lmer.1.igg), main = "fitted x resid", xlab = "fitted", ylab = "resid")
  text(fitted(lmer.1.igg), resid(lmer.1.igg), labels = paste(lmer.1.igg@frame$c.id, lmer.1.igg@frame$i.sample.number.2))      # use this to label the points in the plot
  abline(0,0)
  plot(resid(lmer.1.igg) ~ lmer.1.igg@frame[,2], main = " Values x Residuals")  # lmer.rank@frame$i.rank gets the input independent variables (i.e. i.rank)
  abline(0,0)

  
  print("******************************** IgM *******************************")
  print("******************************** IgM *******************************")
  print("******************************** model 1 *******************************")
  par(mfrow = c(4, 3))
  lmer.0.igm <- lmer(d.absorbance ~ 1 + (1 | c.id), data = df.igm, REML = FALSE)
  lmer.1.igm <- lmer(d.absorbance ~ i.rank + (1 | c.id), data = df.igm, REML = FALSE)
  print(hist(resid(lmer.1.igm)))
  print("cftest(lmer.1.igm))") 
  print(cftest(lmer.1.igm))
  print("summary(lmer.1.igm)") 
  print(summary(lmer.1.igm)) 
  print(lillie.test(resid(lmer.1.igm)))
  print(ad.test(resid(lmer.1.igm)))
  print(cvm.test(resid(lmer.1.igm)))

  print("******************************** model 2 *******************************")

  lmer.0.igm <- lmer(d.absorbance ~ 1 + (1 | c.id), data = df.igm, REML = FALSE)
  lmer.1.igm <- lmer(d.absorbance ~ i.rank + d.age.months + c.reproductive.status + (1 | c.id), data = df.igm, REML = FALSE)
  print(hist(resid(lmer.1.igm)))
  print("cftest(lmer.1.igm))") 
  print(cftest(lmer.1.igm))
  print("summary(lmer.1.igm)") 
  print(summary(lmer.1.igm)) 
  print(lillie.test(resid(lmer.1.igm)))
  print(ad.test(resid(lmer.1.igm)))
  print(cvm.test(resid(lmer.1.igm)))

  print("******************************** model 3 *******************************")
  lmer.0.igm <- lmer(d.absorbance ~ 1 + (1 | c.id), data = df.igm, REML = FALSE)
  lmer.1.igm <- lmer(d.absorbance ~ i.rank * c.reproductive.status + d.age.months + (1 | c.id), data = df.igm, REML = FALSE)
  print(hist(resid(lmer.1.igm)))
  print("cftest(lmer.1.igm))") 
  print(cftest(lmer.1.igm))
  print("summary(lmer.1.igm)") 
  print(summary(lmer.1.igm)) 
  print(lillie.test(resid(lmer.1.igm)))
  print(ad.test(resid(lmer.1.igm)))
  print(cvm.test(resid(lmer.1.igm)))

  print("******************************** model prey *******************************")
  par(mfrow = c(2, 3))
  lmer.0.igm <- lmer(d.absorbance ~ 1 + (1 | c.id), data = df.igm, REML = FALSE)
  lmer.1.igm <- lmer(d.absorbance ~ d.prey.density + (1 | c.id), data = df.igm, REML = FALSE)
  print("cftest(lmer.1.igm)") 
  print(cftest(lmer.1.igm)) 
  print("summary(lmer.1.igm)") 
  print(summary(lmer.1.igm)) 
  print(lillie.test(resid(lmer.1.igm)))
  print(ad.test(resid(lmer.1.igm)))
  print(cvm.test(resid(lmer.1.igm)))

  par(mfrow = c(2,3))
  plot(lmer.1.igm@frame[,1] ~ lmer.1.igm@frame[,2], 
    main = paste((dimnames(lmer.1.igm@frame[1])[[2]]), " ~ ", (dimnames(lmer.1.igm@frame[2])[[2]]), 
      "\ncftest pval: ", signif(((cftest(lmer.1.igm))$test$pvalues)[2], digits = 4),   # ["d.independent"], digits = 4), 
      "\nanova pval: ", signif((anova(lmer.0.igm, lmer.1.igm))$"Pr(>Chisq)"[2], digits = 4), 
      sep = ""), 
    xlab = "Prey Density", ylab = "log ( MIC )") 
  text(lmer.1.igm@frame[,1] ~ lmer.1.igm@frame[,2], labels = paste(lmer.1.igm@frame$c.id, lmer.1.igm@frame$i.sample.number.2), pos = 4, cex = 1)
    abline(a = (fixef(lmer.1.igm))[[1]], b = (fixef(lmer.1.igm))[[2]])    # a = intercept, b = slope (coefficient)
  hist(resid(lmer.1.igm), main = paste("igm", " ~ d.prey.density"))
  lines(density(resid(lmer.1.igm)))                                     # 
  qqnorm(resid(lmer.1.igm), main = paste("qqnorm - "))
  qqline(resid(lmer.1.igm))
#      text(x = Bodywt, y = Brainwt, labels=row.names(primates), pos=4)
#      qqmath(ranef(lmer.alt, postVar = TRUE), strip = FALSE)$c.id
  plot(fitted(lmer.1.igm), resid(lmer.1.igm), main = "fitted x resid", xlab = "fitted", ylab = "resid")
  text(fitted(lmer.1.igm), resid(lmer.1.igm), labels = paste(lmer.1.igm@frame$c.id, lmer.1.igm@frame$i.sample.number.2))      # use this to label the points in the plot
  abline(0,0)
  plot(resid(lmer.1.igm) ~ lmer.1.igm@frame[,2], main = " Values x Residuals")  # lmer.rank@frame$i.rank gets the input independent variables (i.e. i.rank)
  abline(0,0)

  df.ec$d.intercept <- fixef(lmer.1.ec)[[1]]
  df.ec$d.slope <- fixef(lmer.1.ec)[[2]]
  df.pm$d.intercept <- fixef(lmer.1.pm)[[1]]
  df.pm$d.slope <- fixef(lmer.1.pm)[[2]]

  df.igg$d.intercept <- fixef(lmer.1.igg)[[1]]
  df.igg$d.slope <- fixef(lmer.1.igg)[[2]]
  df.igm$d.intercept <- fixef(lmer.1.igm)[[1]]
  df.igm$d.slope <- fixef(lmer.1.igm)[[2]]

  df.temp.1 <- rbind(df.ec, df.pm)
  df.temp.2 <- rbind(df.igg, df.igm)
  print(min(df.temp.1$d.dependent))
  print(max(df.temp.1$d.dependent))

  gg.rank <- ggplot(data = df.temp.1, 
    aes(x = i.rank, 
        y = d.dependent)
    )
     
  print(gg.rank
    + geom_point(aes(x = i.rank))
#    + stat_abline(intercept = d.intercept, slope = d.slope)
    + geom_abline(aes(intercept = d.intercept, slope = d.slope), size = 1.5) #, data = df.temp)
#    + scale_y_continuous(limits = c(min(df.temp.1$d.dependent), max(df.temp.1$d.dependent)))     
    + xlab("Rank")
    + ylab("log ( MIC )")   
    + facet_wrap(~ c.strain, nrow = 2, scales = "free")
    + f.theme() 
  )  

  gg.repro <- ggplot(data = df.temp.1, 
    aes(x = c.reproductive.status, 
        y = d.dependent)
  )

  print(gg.repro
    + geom_boxplot(aes(x = c.reproductive.status))
    + geom_jitter(aes(x = c.reproductive.status), position = position_jitter(w = 0.1, h = 0.05))
    + scale_y_continuous(limits = c(min(df.temp.1$d.dependent), max(df.temp.1$d.dependent)))     
    + xlab("Reproductive status")
    + ylab("log ( MIC )")   
    + facet_wrap(~ c.strain, nrow = 2, scales = "free")
    + f.theme() 
  )  

  gg.rank <- ggplot(data = df.temp.2, 
    aes(x = i.rank, 
        y = d.absorbance)
    )
     
  print(gg.rank
    + geom_point(aes(x = i.rank))
#    + stat_abline(intercept = d.intercept, slope = d.slope)
    + geom_abline(aes(intercept = d.intercept, slope = d.slope), size = 1.5) #, data = df.temp)
#    + scale_y_continuous(limits = c(min(df.temp.2$d.absorbance), max(df.temp.2$d.absorbance)))     
    + xlab("Rank")
    + ylab("Absorbance (450nm)")   
    + facet_wrap(~ c.isotype, nrow = 2, scales = "free")
    + f.theme() 
  )  

  gg.repro <- ggplot(data = df.temp.2, 
    aes(x = c.reproductive.status, 
        y = d.absorbance)
  )

  print(gg.repro
    + geom_boxplot(aes(x = c.reproductive.status))
    + geom_jitter(aes(x = c.reproductive.status), position = position_jitter(w = 0.1, h = 0.05))
#    + scale_y_continuous(limits = c(min(df.temp.2$d.absorbance), max(df.temp.2$d.absorbance)))     
    + xlab("Reproductive status")
    + ylab("Absorbance (450nm)")   
    + facet_wrap(~ c.isotype, nrow = 2, scales = "free")
    + f.theme() 
  )  
    
   
  par(mfrow=c(1,1))
  plot(x = 1, y = 1)
  
}