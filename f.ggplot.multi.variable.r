# source("f.ggplot.multi.variable.r")

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
  
  df.temp.full <- subset(df.data.flat, c.sex == "f" & c.age == "adult")
  df.temp.full <- subset(df.temp.full, !(c.id == "mp" & i.sample.number.2 == 2))
  df.temp.full <- subset(df.temp.full, !(c.id == "gol" & i.sample.number.2 == 3))
  df.temp.full <- subset(df.temp.full, !(c.id == "cr" & i.sample.number.2 == 1))  
  df.temp.full <- subset(df.temp.full, c.reproductive.status != "bado")  
  df.temp.full <- subset(df.temp.full, !(is.na(i.rank)))  

#  df.temp <- subset(df.temp, !(c.id == "cr" & i.sample.number.2 == 1))
#  df.temp <- subset(df.temp, !(c.id == "ger" & i.sample.number.2 == 1))
#  df.temp <- subset(df.temp, !(c.id == "jj" & i.sample.number.2 == 1))

  df.ec <- df.temp.full[, c("c.id", "i.rank", "d.age.months", "c.reproductive.status", "d.sample.date", "c.strain.bka.ec", "d.mic.90.bka.ec")]
  df.ec$c.strain <- "Escherichia coli"
  df.ec$d.mic.90 <- log(df.ec$d.mic.90.bka.ec)
  df.ec <- subset(df.ec, select = c("c.id", "i.rank", "d.age.months", "c.reproductive.status", "d.sample.date", "c.strain", "d.mic.90"))

  df.pm <- df.temp.full[, c("c.id", "i.rank", "d.age.months", "c.reproductive.status", "d.sample.date", "c.strain.bka.pm", "d.mic.90.bka.pm")]
  df.pm$c.strain <- "Proteus mirabilis"
  df.pm$d.mic.90 <- log(df.pm$d.mic.90.bka.pm)
  df.pm <- subset(df.pm, select = c("c.id", "i.rank", "d.age.months", "c.reproductive.status", "d.sample.date", "c.strain", "d.mic.90"))

  df.igg <- df.temp.full[, c("c.id", "i.rank", "d.age.months", "c.reproductive.status", "d.sample.date", 
    "c.isotype.target.total.igg", "d.blank.abs.total.igg")]
  df.igg$c.isotype <- "IgG"
  df.igg$d.absorbance <- df.igg$d.blank.abs.total.igg
  df.igg <- subset(df.igg, select = c("c.id", "i.rank", "d.age.months", "c.reproductive.status", "d.sample.date", "c.isotype", "d.absorbance"))

  df.igm <- df.temp.full[, c("c.id", "i.rank", "d.age.months", "c.reproductive.status", "d.sample.date", 
    "c.isotype.target.total.igm", "d.blank.abs.total.igm")]
  df.igm$c.isotype <- "IgM"
  df.igm$d.absorbance <- df.igm$d.blank.abs.total.igm
  df.igm <- subset(df.igm, select = c("c.id", "i.rank", "d.age.months", "c.reproductive.status", "d.sample.date", "c.isotype", "d.absorbance"))

  lmer.null.ec <- lmer(d.mic.90 ~ 1 + (1 | c.id), data = df.ec, REML = FALSE)
  lmer.alt.1.ec <- lmer(d.mic.90 ~ i.rank + d.age.months + c.reproductive.status + (1 | c.id), data = df.ec, REML = FALSE)
  print(cftest(lmer.alt.1.ec)) 
  hist(resid(lmer.alt.1.ec))

  lmer.null.ec <- lmer(d.mic.90 ~ 1 + (1 | c.id), data = df.ec, REML = FALSE)
  lmer.alt.1.ec <- lmer(d.mic.90 ~ i.rank*c.reproductive.status + d.age.months + (1 | c.id), data = df.ec, REML = FALSE)
  print(cftest(lmer.alt.1.ec)) 
  hist(resid(lmer.alt.1.ec))
  print("summary(lmer.alt.1.ec))") 
  print(summary(lmer.alt.1.ec))
  print("anova(lmer.alt.1.ec")
  print(anova(lmer.alt.1.ec))

  lmer.null.pm <- lmer(d.mic.90 ~ 1 + (1 | c.id), data = df.pm, REML = FALSE)
  lmer.alt.1.pm <- lmer(d.mic.90 ~ i.rank  + (1 | c.id), data = df.pm, REML = FALSE)
  print(cftest(lmer.alt.1.pm)) 
  hist(resid(lmer.alt.1.pm))
  print("summary(lmer.alt.1.pm))") 
  print(summary(lmer.alt.1.pm))
  print("anova(lmer.alt.1.pm")
  print(anova(lmer.alt.1.pm))

  lmer.null.pm <- lmer(d.mic.90 ~ 1 + (1 | c.id), data = df.pm, REML = FALSE)
  lmer.alt.1.pm <- lmer(d.mic.90 ~ i.rank + d.age.months + c.reproductive.status + (1 | c.id), data = df.pm, REML = FALSE)
  print(cftest(lmer.alt.1.pm)) 
  hist(resid(lmer.alt.1.pm))
  print("summary(lmer.alt.1.pm))") 
  print(summary(lmer.alt.1.pm))
  print("anova(lmer.alt.1.pm")
  print(anova(lmer.alt.1.pm))

  lmer.null.pm <- lmer(d.mic.90 ~ 1 + (1 | c.id), data = df.pm, REML = FALSE)
  lmer.alt.1.pm <- lmer(d.mic.90 ~ i.rank*c.reproductive.status + d.age.months + (1 | c.id), data = df.pm, REML = FALSE)
  print(cftest(lmer.alt.1.pm)) 
  hist(resid(lmer.alt.1.pm))
  print("summary(lmer.alt.1.pm))") 
  print(summary(lmer.alt.1.pm))
  print("anova(lmer.alt.1.pm")
  print(anova(lmer.alt.1.pm))

  lmer.null.igg <- lmer(d.absorbance ~ 1 + (1 | c.id), data = df.igg, REML = FALSE)
  lmer.alt.1.igg <- lmer(d.absorbance ~ i.rank + d.age.months + c.reproductive.status + (1 | c.id), data = df.igg, REML = FALSE)
  print(cftest(lmer.alt.1.igg)) 
  print(hist(resid(lmer.alt.1.igg)))
  print("summary(lmer.alt.1.igg))") 
  print(summary(lmer.alt.1.igg))
  print("anova(lmer.alt.1.igg")
  print(anova(lmer.alt.1.igg))

  lmer.null.igm <- lmer(d.absorbance ~ 1 + (1 | c.id), data = df.igm, REML = FALSE)
  lmer.alt.1.igm <- lmer(d.absorbance ~ i.rank + d.age.months + c.reproductive.status + (1 | c.id), data = df.igm, REML = FALSE)
  print(hist(resid(lmer.alt.1.igm)))
  print(cftest(lmer.alt.1.igm)) 
  print("summary(lmer.alt.1.igm))") 
  print(summary(lmer.alt.1.igm))
  print("anova(lmer.alt.1.igm")
  print(anova(lmer.alt.1.igm))

  par(mfrow = c(3, 3))
  plot(lmer.alt.1.ec@frame[,1] ~ lmer.alt.1.ec@frame[,2], 
    main = paste((dimnames(lmer.alt.1.ec@frame[1])[[2]]), " ~ ", (dimnames(lmer.alt.1.ec@frame[2])[[2]]), 
      "\ncftest pval: ", signif(((cftest(lmer.alt.1.ec))$test$pvalues)[2], digits = 4),   # ["d.independent"], digits = 4), 
      "\nanova pval: ", signif((anova(lmer.null.ec, lmer.alt.1.ec))$"Pr(>Chisq)"[2], digits = 4), 
      sep = ""), 
    xlab = "Rank", ylab = "log ( MIC )") 
  text(lmer.alt.1.ec@frame[,1] ~ lmer.alt.1.ec@frame[,2], labels = paste(df.ec$c.id, df.ec$i.sample.number.2), pos = 4, cex = 1)
    abline(a = (fixef(lmer.alt.1.ec))[[1]], b = (fixef(lmer.alt.1.ec))[[2]])    # a = intercept, b = slope (coefficient)

  df.ec$d.intercept <- fixef(lmer.alt.1.ec)[[1]]
  df.ec$d.slope <- fixef(lmer.alt.1.ec)[[2]]
  df.pm$d.intercept <- fixef(lmer.alt.1.pm)[[1]]
  df.pm$d.slope <- fixef(lmer.alt.1.pm)[[2]]

  df.igg$d.intercept <- fixef(lmer.alt.1.igg)[[1]]
  df.igg$d.slope <- fixef(lmer.alt.1.igg)[[2]]
  df.igm$d.intercept <- fixef(lmer.alt.1.igm)[[1]]
  df.igm$d.slope <- fixef(lmer.alt.1.igm)[[2]]

  df.temp.1 <- rbind(df.ec, df.pm)
  df.temp.2 <- rbind(df.igg, df.igm)

  gg.rank <- ggplot(data = df.temp.1, 
    aes(x = i.rank, 
        y = d.mic.90)
    )
     
  print(gg.rank
    + geom_point(aes(x = i.rank))
#    + stat_abline(intercept = d.intercept, slope = d.slope)
    + geom_abline(aes(intercept = d.intercept, slope = d.slope), size = 1.5) #, data = df.temp)
    + xlab("Rank")
    + ylab("log ( MIC )")   
    + facet_wrap(~ c.strain, nrow = 2, scales = "free")
    + f.theme() 
  )  

  gg.repro <- ggplot(data = df.temp.1, 
    aes(x = c.reproductive.status, 
        y = d.mic.90)
  )

  print(gg.repro
    + geom_boxplot(aes(x = c.reproductive.status))
    + geom_jitter(aes(x = c.reproductive.status), position = position_jitter(w = 0.1, h = 0.05))
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
    + xlab("Reproductive status")
    + ylab("Absorbance (450nm)")   
    + facet_wrap(~ c.isotype, nrow = 2, scales = "free")
    + f.theme() 
  )  
    
   
  par(mfrow=c(1,1))
  plot(x = 1, y = 1)
  
}