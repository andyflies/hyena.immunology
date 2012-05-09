# source("f.ggplot.single.variable.r")

# print(df.data[df.data$c.id=="mls", 1:10])
# df.temp <- subset(df.data, !(c.id == "mls" & c.age == "subadult"))
# print(df.temp[df.temp$c.id=="mls",])

f.ggplot.single.variable <- function()
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
					strip.text.x =      theme_text(size = base_size * 2.5, face = "bold"),
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
  df.temp.full <- subset(df.temp.full, !(is.na(i.rank)))  

#  df.temp <- subset(df.temp, !(c.id == "cr" & i.sample.number.2 == 1))
#  df.temp <- subset(df.temp, !(c.id == "ger" & i.sample.number.2 == 1))
#  df.temp <- subset(df.temp, !(c.id == "jj" & i.sample.number.2 == 1))

  df.ec <- df.temp.full[, c("c.id", "i.rank", "d.sample.date", "c.strain.bka.ec", "d.mic.90.bka.ec")]
  df.ec$c.strain <- "Escherichia coli"
  df.ec$d.mic.90 <- log(df.ec$d.mic.90.bka.ec)
  df.ec <- subset(df.ec, select = c("c.id", "i.rank", "d.sample.date", "c.strain", "d.mic.90"))

  df.pm <- df.temp.full[, c("c.id", "i.rank", "d.sample.date", "c.strain.bka.pm", "d.mic.90.bka.pm")]
  df.pm$c.strain <- "Proteus mirabilis"
  df.pm$d.mic.90 <- log(df.pm$d.mic.90.bka.pm)
  df.pm <- subset(df.pm, select = c("c.id", "i.rank", "d.sample.date", "c.strain", "d.mic.90"))

  lmer.null.ec <- lmer(d.mic.90 ~ 1 + (1 | c.id), data = df.ec, REML = FALSE)
  lmer.alt.1.ec <- lmer(d.mic.90 ~ i.rank + (1 | c.id), data = df.ec, REML = FALSE)
  print(cftest(lmer.alt.1.ec)) 
  print("summary(lmer.alt.1.ec))") 
  print(summary(lmer.alt.1.ec))

  lmer.null.pm <- lmer(d.mic.90 ~ 1 + (1 | c.id), data = df.pm, REML = FALSE)
  lmer.alt.1.pm <- lmer(d.mic.90 ~ i.rank + (1 | c.id), data = df.pm, REML = FALSE)
  print(cftest(lmer.alt.1.pm)) 
  print("summary(lmer.alt.1.pm))") 
  print(summary(lmer.alt.1.pm))

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

  df.temp <- rbind(df.ec, df.pm)

  gg.rank <- ggplot(data = df.temp, 
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
   
   
  par(mfrow=c(1,1))
  plot(x = 1, y = 1)
  
}