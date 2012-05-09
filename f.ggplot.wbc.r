# source("f.ggplot.wbc.r")

f.ggplot.wbc <- function()
{

  df.temp.full <- subset(df.data.flat, c.sex == "f" & c.age == "adult")
#  df.temp.full <- subset(df.temp.full, !(c.id == "mp" & i.sample.number.2 == 2))
#  df.temp.full <- subset(df.temp.full, !(c.id == "gol" & i.sample.number.2 == 3))
#  df.temp.full <- subset(df.temp.full, !(c.id == "cr" & i.sample.number.2 == 1))
  df.temp.full <- subset(df.temp.full, c.reproductive.status != "bado")
  df.temp.full <- subset(df.temp.full, !(is.na(i.rank)))
  df.temp.full <- subset(df.temp.full, !(is.na(d.total.wbc)))

  # relevel to remove "bado"
  df.temp.full$c.reproductive.status <- factor(as.character(df.temp.full$c.reproductive.status))

  c.dependent <- c("d.lymphocytes")
  c.dependent.1 <- c("d.neutrophils")
  c.dependent.2 <- c("d.lymphocytes")
  print(c.dependent)
  print(c.dependent.1)
  print(c.dependent.1)
  d.dependent <- df.temp.full[[c.dependent.1]] / df.temp.full[[c.dependent.2]]
#  d.dependent <- (df.temp.full[[c.dependent]])
  c.independent.1 <- "i.rank"
  d.independent.1 <- df.temp.full$i.rank
  c.independent.2 <- "d.age.months"
  d.independent.2 <- df.temp.full$d.age.months
  c.independent.3 <- "c.reproductive.status"
  d.independent.3 <- df.temp.full$c.reproductive.status
  
  df.plot.data <- data.frame(cbind(d.dependent, df.temp.full[[c.independent.1]], df.temp.full[[c.independent.2]], 
    df.temp.full[[c.independent.3]]))
  colnames(df.plot.data) <- c("d.dependent", c.independent.1, c.independent.2, c.independent.3)  
  print(df.plot.data)  

  par(mfrow = c(2,2))
  print("*************************")
  print(c.independent.1)
  lmer.0 <- lmer(d.dependent ~ 1 + (1 | c.id), data = df.temp.full, REML = FALSE)
  lmer.1 <- lmer(d.dependent ~ d.independent.1 + (1 | c.id), data = df.temp.full, REML = FALSE)
  print(cftest(lmer.1))
  print(anova(lmer.1))
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

  print("*************************")
  print(c.independent.2)
  lmer.0 <- lmer(d.dependent ~ 1 + (1 | c.id), data = df.temp.full, REML = FALSE)
  lmer.2 <- lmer(d.dependent ~ d.independent.2 + (1 | c.id), data = df.temp.full, REML = FALSE)
  print(cftest(lmer.2))
  print(anova(lmer.2))
  plot(d.dependent ~ d.independent.2, data = df.temp.full, main = paste(c.dependent, " ~ ", c.independent.2))
  abline(a = fixef(lmer.2)[1][[1]], b = fixef(lmer.2)[2][[1]])
  hist(resid(lmer.2), main = paste("Histogram of residuals - ", (dimnames(lmer.2@frame[1])[[2]]),
    "\nr.squared: ", signif(sqrt((vcov(lmer.2)@factors$Cholesky)[1,1]), digits = 5)))
  lines(density(resid(lmer.2)))                                     #
  qqnorm(resid(lmer.2), main = paste("qqnorm\n", (dimnames(lmer.2@frame[1])[[2]]), " ~ ", (dimnames(lmer.2@frame[2])[[2]])))
  qqline(resid(lmer.2))
  plot(fitted(lmer.2), resid(lmer.2), main = "fitted x resid", xlab = "fitted", ylab = "resid")

  print(ad.test(resid(lmer.2)))
  print(cvm.test(resid(lmer.2)))


  print("*************************")
  print(c.independent.3)
  lmer.0 <- lmer(d.dependent ~ 1 + (1 | c.id), data = df.temp.full, REML = FALSE)
  lmer.3 <- lmer(d.dependent ~ d.independent.3 + (1 | c.id), data = df.temp.full, REML = FALSE)
  print(cftest(lmer.3))
  print(anova(lmer.3))
  plot(d.dependent ~ d.independent.3, data = df.temp.full, main = paste(c.dependent, " ~ ", c.independent.3))
  abline(a = fixef(lmer.3)[1][[1]], b = fixef(lmer.3)[2][[1]])
  hist(resid(lmer.3), main = paste("Histogram of residuals - ", (dimnames(lmer.3@frame[1])[[2]]),
    "\nr.squared: ", signif(sqrt((vcov(lmer.3)@factors$Cholesky)[1,1]), digits = 5)))
  lines(density(resid(lmer.3)))                                     #
  qqnorm(resid(lmer.3), main = paste("qqnorm\n", (dimnames(lmer.3@frame[1])[[2]]), " ~ ", (dimnames(lmer.3@frame[2])[[2]])))
  qqline(resid(lmer.3))
  plot(fitted(lmer.3), resid(lmer.3), main = "fitted x resid", xlab = "fitted", ylab = "resid")

  print(ad.test(resid(lmer.3)))
  print(cvm.test(resid(lmer.3)))


  f.theme <- function(base_size = 12) {
    structure(list(
					axis.line =         theme_blank(),
					axis.text.x =       theme_text(size = base_size, lineheight = 0.9, colour = "black", hjust = 0.5, vjust = 1, angle = 0),
					axis.text.y =       theme_text(size = base_size, lineheight = 0.9, colour = "black", hjust = 0.5, vjust = 0.5),
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

  gg.wbc <- ggplot(data = df.plot.data, 
    aes(x = i.rank, 
        y = d.dependent)   #df.temp.full[["c.dependent"]])
    )
     
  print(gg.wbc
#    + geom_point()
    + geom_point()
#    + geom_abline(intercept = 50, slope = 0)
    + geom_abline(intercept = fixef(lmer.1)[1][[1]], slope = fixef(lmer.1)[2][[1]])
  #    + stat_smooth(method = "lm", se = TRUE, size = 2)
#    + geom_point(aes(x = d.blank.abs, y = d.blank.abs.total.ig), size = 3.5, legend = FALSE)
#    + scale_colour_manual(name = "Adjuvant", values = c("NO" = "grey", "YES" = "black"), legend = TRUE)
#    + geom_errorbar(aes(x = d.age.months, 
#      ymax = d.mic.mean + d.stderr, 
#      ymin = d.mic.mean - d.stderr), width = 0) 
    + opts(title = "White Blood Cell counts") #, color = "Andy")
    + xlab("Rank")
    + ylab("Neutrophil / Lymphocyte Ratio")   
#    + facet_wrap(~ c.strain, nrow = 2, scales = "free")
    + f.theme() 
  )  
 
  gg.wbc <- ggplot(data = df.plot.data, 
    aes(x = d.age.months, 
        y = d.dependent)   #df.temp.full[["c.dependent"]])
    )
     
  print(gg.wbc
#    + geom_point()
    + geom_point()
    + geom_abline(intercept = fixef(lmer.2)[1][[1]], slope = fixef(lmer.2)[2][[1]])
  #    + stat_smooth(method = "lm", se = TRUE, size = 2)
#    + geom_point(aes(x = d.blank.abs, y = d.blank.abs.total.ig), size = 3.5, legend = FALSE)
#    + scale_colour_manual(name = "Adjuvant", values = c("NO" = "grey", "YES" = "black"), legend = TRUE)
#    + geom_errorbar(aes(x = d.age.months, 
#      ymax = d.mic.mean + d.stderr, 
#      ymin = d.mic.mean - d.stderr), width = 0) 
    + opts(title = "White Blood Cell counts") #, color = "Andy")
    + xlab("Age (months)")
    + ylab("Neutrophil / Lymphocyte Ratio")   
#    + facet_wrap(~ c.strain, nrow = 2, scales = "free")
    + f.theme() 
  )  

  gg.wbc <- ggplot(data = df.plot.data, 
    aes(x = factor(c.reproductive.status), 
        y = d.dependent)   #df.temp.full[["c.dependent"]])
    )
     
  print(gg.wbc
#    + geom_point()
    + geom_boxplot()
#    + geom_abline(intercept = fixef(lmer.2)[1][[1]], slope = fixef(lmer.2)[2][[1]])
  #    + stat_smooth(method = "lm", se = TRUE, size = 2)
#    + geom_point(aes(x = d.blank.abs, y = d.blank.abs.total.ig), size = 3.5, legend = FALSE)
#    + scale_colour_manual(name = "Adjuvant", values = c("NO" = "grey", "YES" = "black"), legend = TRUE)
#    + geom_errorbar(aes(x = d.age.months, 
#      ymax = d.mic.mean + d.stderr, 
#      ymin = d.mic.mean - d.stderr), width = 0) 
    + opts(title = "White Blood Cell counts") #, color = "Andy")
    + xlab("Reproductive status")
    + ylab("Neutrophil / Lymphocyte Ratio")   
#    + facet_wrap(~ c.strain, nrow = 2, scales = "free")
    + f.theme() 
  )  


  par(mfrow = c(1,1))
  plot(1,1)
}
