# source("f.ggplot.single.variable.total.ig.r")

f.ggplot.single.variable.total.ig <- function()
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

c.independent.1 <- "d.blank.abs.total.igg"
c.independent.2 <- "d.blank.abs.total.igm"
  
  df.temp.full <- subset(df.data.flat, c.sex == "f" & c.age == "adult")
  df.temp.full <- subset(df.temp.full, !(c.id == "mp" & i.sample.number.2 == 2))
  df.temp.full <- subset(df.temp.full, !(c.id == "gol" & i.sample.number.2 == 3))
  df.temp.full <- subset(df.temp.full, !(c.id == "cr" & i.sample.number.2 == 1))  
  df.temp.full <- subset(df.temp.full, !(is.na(i.rank)))  

  df.1 <- df.temp.full[, c("c.id", "i.rank", "d.sample.date", c.independent.1)]
  df.1$c.facet <- "Total IgG"
  # If transformation is needed, do it here
  df.1$d.response <- log(df.1[[c.independent.1]])
  df.1$d.response <- df.1[[c.independent.1]]
  df.1 <- subset(df.1, select = c("c.id", "i.rank", "d.sample.date", "c.facet", "d.response"))

  df.2 <- df.temp.full[, c("c.id", "i.rank", "d.sample.date", c.independent.2)]
  df.2$c.facet <- "Total IgM"
  # If transformation is needed, do it here
  df.2$d.response <- log(df.2[[c.independent.2]])
  df.2$d.response <- df.2[[c.independent.2]]
  df.2 <- subset(df.2, select = c("c.id", "i.rank", "d.sample.date", "c.facet", "d.response"))

  lmer.null.set.1 <- lmer(d.response ~ 1 + (1 | c.id), data = df.1, REML = FALSE)
  lmer.alt.1.set.1 <- lmer(d.response ~ i.rank + (1 | c.id), data = df.1, REML = FALSE)
  print(cftest(lmer.alt.1.set.1)) 
  print("summary(lmer.alt.1.set.1))") 
  print(summary(lmer.alt.1.set.1))

  lmer.null.set.2 <- lmer(d.response ~ 1 + (1 | c.id), data = df.2, REML = FALSE)
  lmer.alt.1.set.2 <- lmer(d.response ~ i.rank + (1 | c.id), data = df.2, REML = FALSE)
  print(cftest(lmer.alt.1.set.2)) 
  print("summary(lmer.alt.1.set.2))") 
  print(summary(lmer.alt.1.set.2))

  par(mfrow = c(3, 3))
  plot(lmer.alt.1.set.1@frame[,1] ~ lmer.alt.1.set.1@frame[,2], 
    main = paste((dimnames(lmer.alt.1.set.1@frame[1])[[2]]), " ~ ", (dimnames(lmer.alt.1.set.1@frame[2])[[2]]), 
      "\ncftest pval: ", signif(((cftest(lmer.alt.1.set.1))$test$pvalues)[2], digits = 4),   # ["d.independent"], digits = 4), 
      "\nanova pval: ", signif((anova(lmer.null.set.1, lmer.alt.1.set.1))$"Pr(>Chisq)"[2], digits = 4), 
      sep = ""), 
    xlab = "Rank", ylab = "log ( MIC )") 
  text(lmer.alt.1.set.1@frame[,1] ~ lmer.alt.1.set.1@frame[,2], labels = paste(df.1$c.id, df.1$i.sample.number.2), pos = 4, cex = 1)
    abline(a = (fixef(lmer.alt.1.set.1))[[1]], b = (fixef(lmer.alt.1.set.1))[[2]])    # a = intercept, b = slope (coefficient)

  df.1$d.intercept <- fixef(lmer.alt.1.set.1)[[1]]
  df.1$d.slope <- fixef(lmer.alt.1.set.1)[[2]]
  df.2$d.intercept <- fixef(lmer.alt.1.set.2)[[1]]
  df.2$d.slope <- fixef(lmer.alt.1.set.2)[[2]]

  df.temp <- rbind(df.1, df.2)

  gg.rank <- ggplot(data = df.temp, 
    aes(x = i.rank, 
        y = d.response)
    )
     
  print(gg.rank
    + geom_point(aes(x = i.rank))
    + geom_abline(aes(intercept = d.intercept, slope = d.slope), size = 1.5) #, data = df.temp)
    + xlab("Rank")
    + ylab("Absorbance (450nm)")   
    + facet_wrap(~ c.facet, nrow = 2, scales = "free")
    + f.theme() 
  )  
   
   
  par(mfrow=c(1,1))
  plot(x = 1, y = 1)
  
}