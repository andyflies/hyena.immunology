# source("f.lmer.adult.female.r")

f.lmer.adult.female <- function()
{
  f.theme <- function(base_size = 12) {
    structure(list(
					axis.line =         theme_blank(),
					axis.text.x =       theme_text(size = base_size * 1.2, lineheight = 0.9, colour = "black", hjust = 0.5, vjust = 1, angle = 0),
					axis.text.y =       theme_text(size = base_size * 1.2, lineheight = 0.9, colour = "black", hjust = 0.5, vjust = 0.5),
					axis.ticks =        theme_segment(colour = "black", size = 0.5, linetype = 1),
#					axis.ticks =        theme_blank(),
					axis.title.x =      theme_text(size = base_size * 1.25, vjust = 1, face = "bold"),
					axis.title.y =      theme_text(size = base_size * 1.25, angle = 90, vjust = 0.35, face = "bold"),
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
					strip.text.x =      theme_text(size = base_size * 1.25, face = "bold"),
					strip.text.y =      theme_text(size = base_size * 1.25, angle = -90),
					
					plot.background =   theme_rect(colour = NA),
					plot.title =        theme_text(size = base_size * 2, face = "bold"), #, just = c(0.5, 0.5)),
					plot.margin =       unit(c(0.5, 0.25, 0.5, 0.25), "lines")      # 1 = top, 2, side, 3 = bottom, 4 = side
			), class = "options")
  }

  df.ec.data <- subset(df.master.bka, c.strain == "ec" & c.sex == "f" & c.age == "adult" & i.sample.number.2 == 2)   # Get only the first sample from each individual for the E. coli data
  df.pm.data <- subset(df.master.bka, c.strain == "pm" & c.sex == "f" & c.age == "adult" & i.sample.number.2 == 2)   # Get only the first sample from each individual for the P. mirabilis data

  # Testing to see if removing individual "gol" changes the outcome. "gol" is a potential outlier
#  df.ec.data <- subset(df.master.bka, c.strain == "ec" & c.sex == "f" & c.age == "adult" & i.sample.number.2 == 2 & c.id != "mp")   # Get only the first sample from each individual for the E. coli data
#  df.pm.data <- subset(df.master.bka, c.strain == "pm" & c.sex == "f" & c.age == "adult" & i.sample.number.2 == 2 & c.id != "mp")   # Get only the first sample from each individual for the P. mirabilis data

  print("df.ec.data$c.id")
  print(df.ec.data$c.id)
  print("df.pm.data$c.id")
  print(df.pm.data$c.id)
                                
#########################################################################
   
  df.ec.data <- with(df.ec.data, f.sort.frame(df.ec.data, i.rank, c.id))
  df.pm.data <- with(df.pm.data, f.sort.frame(df.pm.data, i.rank, c.id))
  
  df.mic.data <- rbind(df.ec.data, df.pm.data)
  print(summary(df.mic.data))
  
  df.mic.data$c.strain <- as.character(df.mic.data$c.strain)
  df.mic.data$c.strain[df.mic.data$c.strain == "ec"] <- "Escherichia coli"
  df.mic.data$c.strain[df.mic.data$c.strain == "pm"] <- "Proteus mirabilis"
  print(summary(df.mic.data))
 
  print(df.mic.data[,c("c.id", "c.sex", "c.age", "i.sample.number", "d.mic.90", "d.mic.90.level")])
   
  t.ec.xtabs <- xtabs(~ df.ec.data$c.sex + df.ec.data$c.age)
  t.pm.xtabs <- xtabs(~ + df.pm.data$c.sex + df.pm.data$c.age)
  print(t.ec.xtabs)
  print(t.pm.xtabs)
  
#  m.bacteria.mic.correlation <- cor(df.ec.data[,"d.mic.90"], df.pm.data[,"d.mic.90"])
#  print(paste("correlation between d.mic.90 for E. coli and P. mirabilis: ", m.bacteria.mic.correlation), quote = FALSE)

  m.bacteria.mic.correlation <- cor(df.mic.data[,"d.mic.90"], df.mic.data[,"d.mic.90.level"])
  print(paste("correlation between d.mic.90.level : ", m.bacteria.mic.correlation), quote = FALSE)

  m.bacteria.mic.correlation <- cor(df.mic.data[,"d.mic.90.level"], df.mic.data[,"d.minutes.blood"])
  print(paste("correlation between d.mic.90.level and d.minutes.blood : ", m.bacteria.mic.correlation), quote = FALSE)
    
  #####################
  # Plots and lmer tests

  print("lmer on e.c.") 
  par(mfrow=c(2,3))
  lmer.rank <- lmer(d.mic.90.level ~ i.rank + (1 | c.id), data = df.ec.data)
  print(summary(lmer.rank))#  plot(lmer.rank)
  hist(resid(lmer.rank))

#  lmer.rank.date <- lmer(d.mic.90.level ~ i.rank + (d.sample.date | c.id), data = df.ec.data)
#  print(summary(lmer.rank))
#  hist(resid(lmer.rank))
#  print(anova(lmer.rank, lmer.rank.date))

                       
  print("")
  print("lmer on p.m.")  
  par(mfrow=c(2,3))
  lmer.rank <- lmer(d.mic.90.level ~ i.rank + (1 | c.id), data = df.pm.data, REML = FALSE)
  print(summary(lmer.rank))
  hist(resid(lmer.rank))  
  
  plot(d.mic.90.level ~ d.sample.date, data = df.pm.data)

  print(summary(df.pm.data$i.rank))  
  print("summary(df.pm.data$d.sample.date)")
  print(summary(df.pm.data$d.sample.date))
  print(summary(as.integer(df.pm.data$d.sample.date)))
  print("summary(as.integer(df.pm.data$d.sample.date))")
  print(summary(as.integer(df.pm.data$d.sample.date)))
  lmer.rank.date <- lmer(d.mic.90.level ~ i.rank + as.integer(d.sample.date) + (1 | c.id), data = df.pm.data, , REML = FALSE)
  print(summary(lmer.rank.date))
  hist(resid(lmer.rank.date))
  print(anova(lmer.rank, lmer.rank.date))  

  lmer.rank.age.months <- lmer(d.mic.90.level ~ i.rank + d.age.months + (1 | c.id), data = df.pm.data, , REML = FALSE)
  print(summary(lmer.rank.age.months))
  hist(resid(lmer.rank.age.months))
  print(anova(lmer.rank, lmer.rank.age.months))  

  lmer.rank.mass <- lmer(d.mic.90.level ~ i.rank + d.mass + (1 | c.id), data = df.pm.data, REML = FALSE)
  print(summary(lmer.rank.mass))
  hist(resid(lmer.rank.mass))
  print(anova(lmer.rank, lmer.rank.mass))  

  lmer.rank.minuntes.blood <- lmer(d.mic.90.level ~ i.rank + d.minutes.blood + (1 | c.id), data = df.pm.data, REML = FALSE)
  print(summary(lmer.rank.minuntes.blood))
  hist(resid(lmer.rank.minuntes.blood))
  print(anova(lmer.rank, lmer.rank.minuntes.blood))  
 
  lmer.rank.glucose.green <- lmer(d.mic.90.level ~ i.rank + i.glucose.green + (1 | c.id), data = df.pm.data, REML = FALSE)
  print(summary(lmer.rank.glucose.green))
  hist(resid(lmer.rank.glucose.green))
  print(anova(lmer.rank, lmer.rank.glucose.green))  

  lmer.rank.glucose.red <- lmer(d.mic.90.level ~ i.rank + i.glucose.red + (1 | c.id), data = df.pm.data, REML = FALSE)
  print(summary(lmer.rank.glucose.red))
  hist(resid(lmer.rank.glucose.red))
  print(anova(lmer.rank, lmer.rank.glucose.red))  

  lmer.rank.pcv <- lmer(d.mic.90.level ~ i.rank + d.pcv + (1 | c.id), data = df.pm.data, REML = FALSE)
  print(summary(lmer.rank.pcv))
  hist(resid(lmer.rank.pcv))
  print(anova(lmer.rank, lmer.rank.pcv))  

  lmer.rank.months.to.grad <- lmer(d.mic.90.level ~ i.rank + d.months.to.grad + (1 | c.id), data = df.pm.data, REML = FALSE)
  print(summary(lmer.rank.months.to.grad))
  hist(resid(lmer.rank.months.to.grad))
  print(anova(lmer.rank, lmer.rank.months.to.grad))

  lmer.rank.months.to.wean <- lmer(d.mic.90.level ~ i.rank + d.months.to.wean + (1 | c.id), data = df.pm.data, REML = FALSE)
  print(summary(lmer.rank.months.to.wean))
  hist(resid(lmer.rank.months.to.wean))
  print(anova(lmer.rank, lmer.rank.months.to.wean))

  par(mfrow=c(2,3))
  print(lm.rank <- lm(d.mic.90.level ~ i.rank, data = df.data))
  print(summary(lm.rank))
  plot(lm.rank)
  hist(resid(lm.rank))

  par(mfrow=c(2,3))
  print(lm.months.to.wean <- lm(d.mic.90.level ~ d.months.to.wean, data = df.data))
  print(summary(lm.months.to.wean))
  plot(lm.months.to.wean)
  hist(resid(lm.months.to.wean))

#  lmer.rank.cdv.titer <- lmer(d.mic.90.level ~ i.rank + i.cdv.titer + (1 | c.id), data = df.pm.data, REML = FALSE)
#  print(summary(lmer.rank.cdv.titer))
#  hist(resid(lmer.rank.cdv.titer))
#  print(anova(lmer.rank, lmer.rank.cdv.titer))

   
  par(mfrow=c(1,1))
  plot(x = 1, y = 1)
  
}