# source("f.ggplot.sex.r")

f.ggplot.sex <- function()
{

  df.temp.full <- subset(df.data.flat, c.age == "adult")
#  df.temp.full <- subset(df.data.flat, c.sex == "f" & c.age == "adult")
  df.temp.full <- subset(df.temp.full, !(c.id == "mp" & i.sample.number.2 == 2))
  df.temp.full <- subset(df.temp.full, !(c.id == "gol" & i.sample.number.2 == 3))
  df.temp.full <- subset(df.temp.full, !(c.id == "cr" & i.sample.number.2 == 1))
#  df.temp.full <- subset(df.temp.full, c.reproductive.status != "bado")
  df.temp.full <- subset(df.temp.full, !(is.na(c.sex)))
  df.temp.full <- subset(df.temp.full, !(is.na(d.total.wbc)))
  df.temp.full <- subset(df.temp.full, (c.sex == "m" & c.natal.immigrant == "immigrant") | (c.sex == "f"))

  # relevel to remove "bado"
  df.temp.full$c.reproductive.status <- factor(as.character(df.temp.full$c.reproductive.status))
  df.temp.full$c.sex <- factor(as.character(df.temp.full$c.sex))
  
#  c.dependent <- c("d.lymphocytes")
  c.dependent <- c("d.blank.abs.total.igm")
  c.dependent.1 <- c("d.neutrophils")
  c.dependent.2 <- c("d.lymphocytes")
  print(c.dependent)
#  print(c.dependent.1)
#  print(c.dependent.1)
  d.dependent <- df.temp.full[[c.dependent.1]] / df.temp.full[[c.dependent.2]]
  d.dependent <- (df.temp.full[[c.dependent]])
  c.independent.1 <- "c.sex"
  d.independent.1 <- df.temp.full$c.sex
  c.independent.2 <- "d.age.months"
  d.independent.2 <- df.temp.full$d.age.months
  
  print(df.temp.full[,c("c.id", "i.sample.number", "d.age.months", "d.mic.90.ec", "d.mic.90.pm", "d.blank.abs.total.igg", "d.blank.abs.total.igm", 
    "d.lymphocytes")])

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
#  print(df.temp.full[,c("c.id", "i.sample.number", "d.age.months", "d.lymphocytes")])
#  df.temp.full <- subset(df.temp.full, !(c.id == "mig" & i.sample.number.2 == 3))
#  print(df.temp.full[,c("c.id", "i.sample.number", "i.sample.number.2", "d.age.months", "d.lymphocytes")])
  d.dependent <- df.temp.full[[c.dependent.1]] / df.temp.full[[c.dependent.2]]
  d.dependent <- (df.temp.full[[c.dependent]])
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
  print(paste("full model,  interactions"))
  lmer.0 <- lmer(d.dependent ~ 1 + (1 | c.id), data = df.temp.full, REML = FALSE)
  lmer.1 <- lmer(d.dependent ~ d.independent.1  * d.independent.2  + 
    (1 | c.id), data = df.temp.full, REML = FALSE)
  print(cftest(lmer.1))
  print(summary(lmer.1))
#  print(str(lmer.1))
  plot(1,1)
  plot(lmer.1@frame$"d.dependent" ~ lmer.1@frame$"d.independent.1", main = paste(c.dependent, " ~ ", c.independent.1),
   xlab = "Sex", ylab = "sqrt ( MIC )", , cex.lab = 1.5, cex.axis = 1.5, font.lab = 2, font.axis = 2)
##  abline(a = fixef(lmer.1)[1][[1]], b = fixef(lmer.1)[2][[1]])
  plot(lmer.1@frame$"d.dependent" ~ lmer.1@frame$"d.independent.2", main = paste(c.dependent, " ~ ", c.independent.2),
   xlab = "Age (months)", ylab = "sqrt ( MIC )", col = "black", pch = 19, cex.lab = 1.5, cex.axis = 1.5, font.lab = 2, font.axis = 2, las = 2)
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
  print(paste("full model, no interactions"))
  lmer.0 <- lmer(d.dependent ~ 1 + (1 | c.id), data = df.temp.full, REML = FALSE)
  lmer.1 <- lmer(d.dependent ~ d.independent.1  + d.independent.2  + 
    (1 | c.id), data = df.temp.full, REML = FALSE)
  print(cftest(lmer.1))
  print(summary(lmer.1))
#  print(str(lmer.1))
  plot(1,1)
  plot(lmer.1@frame$"d.dependent" ~ lmer.1@frame$"d.independent.1", main = paste(c.dependent, " ~ ", c.independent.1),
   xlab = "Sex", ylab = "sqrt ( MIC )", , cex.lab = 1.5, cex.axis = 1.5, font.lab = 2, font.axis = 2)
##  abline(a = fixef(lmer.1)[1][[1]], b = fixef(lmer.1)[2][[1]])
  plot(lmer.1@frame$"d.dependent" ~ lmer.1@frame$"d.independent.2", main = paste(c.dependent, " ~ ", c.independent.2),
   xlab = "Age (months)", ylab = "sqrt ( MIC )", col = "black", pch = 19, cex.lab = 1.5, cex.axis = 1.5, font.lab = 2, font.axis = 2, las = 2)
  abline(a = fixef(lmer.1)["(Intercept)"], b = fixef(lmer.1)["d.independent.2"])
  hist(resid(lmer.1), main = paste("Histogram of residuals - ", (dimnames(lmer.1@frame[1])[[2]]),
    "\nr.squared: ", signif(sqrt((vcov(lmer.1)@factors$Cholesky)[1,1]), digits = 5)))
  lines(density(resid(lmer.1)))                                     #
  qqnorm(resid(lmer.1), main = paste("qqnorm\n", (dimnames(lmer.1@frame[1])[[2]]), " ~ ", (dimnames(lmer.1@frame[2])[[2]])))
  qqline(resid(lmer.1))
  plot(fitted(lmer.1), resid(lmer.1), main = "model 3, fitted x resid", xlab = "fitted", ylab = "resid")

  print(ad.test(resid(lmer.1)))
  print(cvm.test(resid(lmer.1)))

  df.lmer.1 <- data.frame(cbind(lmer.1@frame$"d.dependent", lmer.1@frame$"d.independent.1", lmer.1@frame$"d.independent.2"))
  df.lmer.1$d.intercept <- fixef(lmer.1)["(Intercept)"]
  df.lmer.1$d.slope <- fixef(lmer.1)["d.independent.2"]
  colnames(df.lmer.1) <- c("d.dependent", "d.independent.1", "d.independent.2", "d.intercept", "d.slope")
  df.lmer.1$d.independent.1[df.lmer.1$d.independent.1 == 1] <- "Female" 
  df.lmer.1$d.independent.1[df.lmer.1$d.independent.1 == 2] <- "Male" 
  print(df.lmer.1)


  f.theme <- function(base_size = 12) {
    structure(list(
					axis.line =         theme_blank(),
					axis.text.x =       theme_text(size = base_size, lineheight = 0.9, colour = "black", hjust = 0.5, vjust = 1, angle = 0, face = "bold"),
					axis.text.y =       theme_text(size = base_size, lineheight = 0.9, colour = "black", hjust = 0.5, vjust = 0.5, face = "bold"),
					axis.ticks =        theme_segment(colour = "black", size = 0.5, linetype = 1),
#					axis.ticks =        theme_blank(),
					axis.title.x =      theme_text(size = base_size, vjust = 0, face = "bold"),
					axis.title.y =      theme_text(size = base_size, angle = 90, vjust = 0, face = "bold"),
#					axis.ticks.length = unit(0.3, "lines"),
#					axis.ticks.margin = unit(0, "lines"),
#          axis.ticks =        theme.blank(),
        					
					legend.background = theme_rect(colour = NA), 
					legend.key =        theme_rect(fill = NA, colour = NA),
					legend.key.size =   unit(1.2, "lines"),
					legend.text =       theme_text(size = base_size),
					legend.title =      theme_text(size = base_size * 1, hjust = 0, face = "bold"),
					legend.position =   "right",
					
					panel.background =  theme_rect(fill = NA, colour = "black"), 
					panel.border =      theme_blank(), #theme_rect(fill = NA, colour = "black"), # 
					panel.grid.major =  theme_line(colour = NA, linetype = 2),
					panel.grid.minor =  theme_line(colour = NA, size = 0.25),
					panel.margin =      unit(1, "lines"),
					
					strip.background =  theme_rect(fill = "grey", colour = "black"), 
					strip.label =       function(variable, value) value, 
					strip.text.x =      theme_text(size = base_size, face = "bold"),
					strip.text.y =      theme_text(size = base_size, angle = -90),
					
					plot.background =   theme_rect(colour = NA),
					plot.title =        theme_text(size = base_size, face = "bold", vjust = 1),
					plot.margin =       unit(c(10, 25, 10, 25), "lines")      # 1 = top, 2, side, 3 = bottom, 4 = side
			), class = "options")
  }

  gg.wbc <- ggplot(data = df.lmer.1, 
    aes(x = factor(d.independent.1), 
        y = d.dependent)   #df.temp.full[["c.dependent"]])
    )
     
  print(gg.wbc
#    + geom_point()
    + geom_boxplot()
#    + geom_jitter(width = 0.1, height = 0.05)
#    + geom_abline(intercept = fixef(lmer.2)[1][[1]], slope = fixef(lmer.2)[2][[1]])
  #    + stat_smooth(method = "lm", se = TRUE, size = 2)
#    + geom_point(aes(x = d.blank.abs, y = d.blank.abs.total.ig), size = 3.5, legend = FALSE)
#    + scale_colour_manual(name = "Adjuvant", values = c("NO" = "grey", "YES" = "black"), legend = TRUE)
#    + geom_errorbar(aes(x = d.age.months, 
#      ymax = d.mic.mean + d.stderr, 
#      ymin = d.mic.mean - d.stderr), width = 0) 
    + opts(title = "Total IgG") #, color = "Andy")
    + xlab("        Sex")
    + ylab("Absorbance (450nm)")   
#    + facet_wrap(~ c.strain, nrow = 2, scales = "free")
    + f.theme() 
  )  

 
gg.wbc <- ggplot(data = df.lmer.1, 
    aes(x = d.independent.2, 
        y = d.dependent)   #df.temp.full[["c.dependent"]])
    )
     
  print(gg.wbc
#    + geom_point()
    + geom_point()
    + geom_abline(intercept = 1.857241, slope = -0.0007490781)
  #    + stat_smooth(method = "lm", se = TRUE, size = 2)
#    + geom_point(aes(x = d.blank.abs, y = d.blank.abs.total.ig), size = 3.5, legend = FALSE)
#    + scale_colour_manual(name = "Adjuvant", values = c("NO" = "grey", "YES" = "black"), legend = TRUE)
#    + geom_errorbar(aes(x = d.age.months, 
#      ymax = d.mic.mean + d.stderr, 
#      ymin = d.mic.mean - d.stderr), width = 0) 
    + opts(title = "Total IgG") #, color = "Andy")
    + xlab("Age (months)")
    + ylab("Absorbance (450nm)")   
#    + facet_wrap(~ c.strain, nrow = 2, scales = "free")
    + f.theme() 
  )  


  par(mfrow = c(1,1))
  plot(1,1)
}
