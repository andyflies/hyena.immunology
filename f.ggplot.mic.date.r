# source("f.ggplot.date.r")

# print(df.data[df.data$c.id=="mls", 1:10])
# df.temp <- subset(df.data, !(c.id == "mls" & c.age == "subadult"))
# print(df.temp[df.temp$c.id=="mls",])

f.ggplot.mic.date <- function()
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
					plot.margin =       unit(c(1, 10, 1, 10), "lines")      # 1 = top, 2, side, 3 = bottom, 4 = side
			), class = "options")
  }

  df.ec.data <- subset(df.master.bka, c.strain == "ec")   
  df.pm.data <- subset(df.master.bka, c.strain == "pm")   
  
  # convert i.sample.number to a factor and use it as a categorical variable for keeping the mean mic of each sample separate
  
#  df.ec.data <- df.ec.data[!is.na(df.ec.data$i.sample.number),]
#  df.pm.data <- df.ec.data[!is.na(df.pm.data$i.sample.number),]
  
  df.ec.data$i.sample.number <- factor(df.ec.data$i.sample.number)   
  df.pm.data$i.sample.number <- factor(df.pm.data$i.sample.number)   
  df.ec.data$c.sample.date <- as.character(df.ec.data$d.sample.date)   
  df.pm.data$c.sample.date <- as.character(df.pm.data$d.sample.date)  
  

  # Testing to see if removing individual "gol" changes the outcome. "gol" is a potential outlier
#  df.ec.data <- subset(df.master.bka, c.strain == "ec" & c.sex == "f" & c.age == "adult" & c.id != "gol")   # Get only the first sample from each individual for the E. coli data
#  df.pm.data <- subset(df.master.bka, c.strain == "pm" & c.sex == "f" & c.age == "adult" & c.id != "gol")   # Get only the first sample from each individual for the P. mirabilis data

 
  ########################### aggregate and get the means #################
  df.ec.mean <- aggregate(cbind(d.mic.90.level, i.rank, d.months.to.wean, d.sample.date, d.dob, d.age.months, d.mass,
    i.maternal.rank.at.birth, i.maternal.rank.at.sample, i.litter.rank, i.number.in.litter,d.minutes.blood, i.glucose.green,i.glucose.red,
    d.pcv, d.total.solids  
    ) 
    ~ c.id 
    + c.sex 
    + c.strain
    + c.age
#    + c.mom
  #  + c.clan
    + c.natal.immigrant
    + i.sample.number
    + c.sample.date
  #  + c.body.condition
    , 
    data = df.ec.data, mean, na.action = na.pass)
  
  df.pm.mean <- aggregate(cbind(d.mic.90.level, i.rank, d.months.to.wean, d.sample.date, d.dob, d.age.months, d.mass,
    i.maternal.rank.at.birth, i.maternal.rank.at.sample, i.litter.rank, i.number.in.litter,d.minutes.blood, i.glucose.green,i.glucose.red,
    d.pcv, d.total.solids  
    ) 
    ~ c.id 
    + c.sex 
    + c.strain
    + c.age
#    + c.mom
  #  + c.clan
    + c.natal.immigrant
    + i.sample.number
    + c.sample.date
  #  + c.body.condition
    ,
    data = df.pm.data, mean, na.action = na.pass)

  ########################### aggregate and get the standard errors #################
  df.ec.stderr <- aggregate(cbind(d.mic.90.level, i.rank, d.months.to.wean, d.sample.date, d.dob, d.age.months, d.mass,
    i.maternal.rank.at.birth, i.maternal.rank.at.sample, i.litter.rank, i.number.in.litter,d.minutes.blood, i.glucose.green,i.glucose.red,
    d.pcv, d.total.solids  
    ) 
    ~ c.id 
    + c.sex 
    + c.strain
    + c.age
#    + c.mom
  #  + c.clan
    + c.natal.immigrant
    + i.sample.number
    + c.sample.date
  #  + c.body.condition
    , 
    data = df.ec.data, std.error, na.action = na.pass)
  
  df.pm.stderr <- aggregate(cbind(d.mic.90.level, i.rank, d.months.to.wean, d.sample.date, d.dob, d.age.months, d.mass,
    i.maternal.rank.at.birth, i.maternal.rank.at.sample, i.litter.rank, i.number.in.litter,d.minutes.blood, i.glucose.green,i.glucose.red,
    d.pcv, d.total.solids  
    ) 
    ~ c.id 
    + c.sex 
    + c.strain
    + c.age
#    + c.mom
  #  + c.clan
    + c.natal.immigrant
    + i.sample.number
    + c.sample.date
  #  + c.body.condition
    ,
    data = df.pm.data, std.error, na.action = na.pass)
 
  d.stderr <- df.ec.stderr$d.mic.90.level
  df.ec.mean <- cbind(df.ec.mean, d.stderr)
#  print(df.ec.mean)  

  d.stderr <- df.pm.stderr$d.mic.90.level
  df.pm.mean <- cbind(df.pm.mean, d.stderr)
#  print(df.pm.mean)  

  l.v.check.names <- names(df.ec.mean) == "d.mic.90.level"
  colnames(df.ec.mean)[l.v.check.names]<-"d.mic.mean"  
  l.v.check.names <- names(df.pm.mean) == "d.mic.90.level"
  colnames(df.pm.mean)[l.v.check.names]<-"d.mic.mean"  
  
#  print(df.ec.mean)
#  print(df.pm.mean)
                                
#########################################################################
  df.ec.mean$date.sample.date <- as.Date((df.ec.mean$c.sample.date), "%d-%b-%Y")
  df.pm.mean$date.sample.date <- as.Date((df.pm.mean$c.sample.date), "%d-%b-%Y")
 
  df.ec.mean <- with(df.ec.mean, f.sort.frame(df.ec.mean, date.sample.date, i.rank, c.id))
  df.pm.mean <- with(df.pm.mean, f.sort.frame(df.pm.mean, date.sample.date, i.rank, c.id))
    
#  print(df.ec.mean)
#  print(df.pm.mean)  
  
  df.mic.means <- rbind(df.ec.mean, df.pm.mean)
  print(df.mic.means)
  
  df.mic.means$c.strain <- as.character(df.mic.means$c.strain)
#  print(df.mic.means)
  df.mic.means$c.strain[df.mic.means$c.strain == "ec"] <- "Escherichia coli"
  df.mic.means$c.strain[df.mic.means$c.strain == "pm"] <- "Proteus mirabilis"
#  print(df.mic.means)
  
  #df.temp.names <- data.frame(df.ec.median$c.id, df.ec.mean$c.id, df.ec.sd$c.id)
  #print(df.temp.names)
  
#  t.ec.xtabs <- xtabs(~ df.ec.mean$c.sex + df.ec.mean$c.age)
#  t.pm.xtabs <- xtabs(~ + df.pm.mean$c.sex + df.pm.mean$c.age)
#  print(t.ec.xtabs)
#  print(t.pm.xtabs)
   print(cbind(df.ec.mean$d.mass, df.ec.mean$d.mass, df.pm.mean$d.mass))
  
  m.bacteria.mic.correlation <- cor(df.ec.mean[,"d.mic.mean"], df.pm.mean[,"d.mic.mean"])
  print(paste("correlation between MICs for E. coli and P. mirabilis: ", m.bacteria.mic.correlation), quote = FALSE)

  m.bacteria.mic.correlation <- cor(df.ec.mean[,"d.mass"], df.ec.mean[,"d.mass"], use = "complete.obs")
  print(paste("correlation between d.mass for E. coli and E. coli: ", m.bacteria.mic.correlation), quote = FALSE)

  m.bacteria.mic.correlation <- cor(df.ec.mean[,"d.mass"], df.pm.mean[,"d.mass"], use = "complete.obs")
  print(paste("correlation between d.mass for E. coli and P. mirabilis: ", m.bacteria.mic.correlation), quote = FALSE)

  m.bacteria.mic.correlation <- cor(df.pm.mean[,"i.rank"], df.pm.mean[,"d.months.to.wean"], use = "complete.obs")
  print(paste("correlation for P. mirabilis rank and months.to.wean: ", m.bacteria.mic.correlation), quote = FALSE)

  m.bacteria.mic.correlation <- cor(df.pm.mean[,"i.rank"], df.pm.mean[,"d.mic.mean"], use = "complete.obs")
  print(paste("correlation for P. mirabilis rank and months.to.wean: ", m.bacteria.mic.correlation), quote = FALSE)

  
  # This is just the test to make sure the data sets match
  #   This tests for a correlation between the minutes to blood collection of each individual
  #   in the data set for the different bacteria tested.  The correrlation should be equal to 1.
#  m.bacteria.minutes.blood.correlation <- cor(df.ec.mean[,"d.minutes.blood"], df.pm.mean[,"d.minutes.blood"])
#  print(paste("correlation between minutes from blood for E. coli and P. mirabilis: ", m.bacteria.minutes.blood.correlation), quote = FALSE)
  
  # This is just the test to make sure the data sets match
  #   This tests for a correlation between the rank of each individual
  #   in the data set for the different bacteria tested.  The correrlation should be equal to 1.
  m.bacteria.rank.correlation <- cor(df.ec.mean[,"i.rank"], df.pm.mean[,"i.rank"], use = "complete.obs")
  print(paste("correlation between rank for E. coli and P. mirabilis: ", m.bacteria.rank.correlation), quote = FALSE)
  
  
  #####################
  # Plots and lm tests
 
  par(mfrow=c(2,3))
 print("1")
 plot(d.mic.mean ~ date.sample.date, data = df.ec.mean)
  lm.date <- lm(d.mic.mean ~ date.sample.date, data = df.ec.mean)
 print("2")
  print(summary(lm.date))
  plot(lm.date)
  hist(resid(lm.date), main = "Escherichia coli")
  plot(d.mic.mean ~ date.sample.date, data = df.ec.mean, main = "Escherichia coli")
  
  par(mfrow=c(2,3))
  lm.date <- lm(d.mic.mean ~ date.sample.date, data = df.pm.mean)
  print(summary(lm.date))
  plot(lm.date)
  hist(resid(lm.date), main = "Proteus mirablis")  
  plot(d.mic.mean ~ date.sample.date, data = df.pm.mean, main = "Proteus mirablis")
  
  gg.mic.means <- ggplot(data = df.mic.means, 
    aes(x = date.sample.date, 
        y = d.mic.mean)
    )
     
  print(gg.mic.means
    + geom_point(aes(x = date.sample.date))
    + stat_smooth(method = "lm", se = TRUE, size = 2)
#    + geom_point(aes(x = d.blank.abs, y = d.blank.abs.total.ig), size = 3.5, legend = FALSE)
#    + scale_colour_manual(name = "Adjuvant", values = c("NO" = "grey", "YES" = "black"), legend = TRUE)
    + geom_errorbar(aes(x = date.sample.date, 
      ymax = d.mic.mean + d.stderr, 
      ymin = d.mic.mean - d.stderr), width = 0) 
    + opts(title = "Bacterial killing assays") #, color = "Andy")
    + xlab("Date")
    + ylab("MIC level")   
    + facet_wrap(~ c.strain, nrow = 2, scales = "free")
    + f.theme() 
  )  
   
  gg.mic.means <- ggplot(data = df.mic.means, 
    aes(x = d.age.months, 
        y = d.mic.mean)
    )
     
  print(gg.mic.means
    + geom_point(aes(x = d.age.months))
    + stat_smooth(method = "lm", se = TRUE, size = 2)
#    + geom_point(aes(x = d.blank.abs, y = d.blank.abs.total.ig), size = 3.5, legend = FALSE)
#    + scale_colour_manual(name = "Adjuvant", values = c("NO" = "grey", "YES" = "black"), legend = TRUE)
    + geom_errorbar(aes(x = d.age.months, 
      ymax = d.mic.mean + d.stderr, 
      ymin = d.mic.mean - d.stderr), width = 0) 
    + opts(title = "Bacterial killing assays") #, color = "Andy")
    + xlab("Age (months)")
    + ylab("MIC level")   
    + facet_wrap(~ c.strain, nrow = 2, scales = "free")
    + f.theme() 
  )  
   
   
  par(mfrow=c(1,1))
  plot(x = 1, y = 1)
  
}