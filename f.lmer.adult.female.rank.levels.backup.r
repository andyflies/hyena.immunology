# source("f.lmer.adult.female.rank.levels.r")

# print(df.data[df.data$c.id=="mls", 1:10])
# df.temp <- subset(df.data, !(c.id == "mls" & c.age == "subadult"))
# print(df.temp[df.temp$c.id=="mls",])

f.lmer.adult.female.rank.levels <- function()
{

  df.ec.data <- subset(df.data, c.strain == "ec" & c.sex == "f" & c.age == "adult")   # Get only the first sample from each individual for the E. coli data
  df.pm.data <- subset(df.data, c.strain == "pm" & c.sex == "f" & c.age == "adult")   # Get only the first sample from each individual for the P. mirabilis data
  
  ########################### aggregate and get the means for all continuous variables. #################
  ### include i.plate.id if we want to use this model to get a measure of the within assays variation in individuals. ###
  ## d.mic.90 is the original dilution that inhibited, log2(d.mic.90) is the base 2 log of the original dilution inhibition level and
  ##   d.mic.90.level is the re-code level of inhibition (i.e. dilution 5x = 1, 10x = 2, 20x = 3 ...   ##
  df.ec.data <- aggregate(cbind(d.mic.90, log2(d.mic.90), d.mic.90.level,
    i.rank, 
    d.age.months, 
    d.mass,
    i.maternal.rank.at.birth,
    i.maternal.rank.at.sample,
    i.rank,
    i.litter.rank,
    i.number.in.litter,
    d.minutes.blood,
    i.glucose.green,
    i.glucose.red,
    d.pcv,
    d.total.solids,
    d.months.to.grad,
    d.months.to.wean,
    i.cdv.titer
    ) 
    ~ c.id 
    + i.sample.number 
    + c.sex 
    + d.sample.date
#    + i.plate.id    
    + c.mom
    + d.dob
    + c.age
    + c.clan
    + c.natal.immigrant
    + c.body.condition
    + c.matched.subadult.adult
    + c.one.adult.samples
    + c.two.adult.samples
    + c.three.adult.samples
    , 
    data = df.ec.data, mean, na.action = na.pass)
  
  df.pm.data <- aggregate(cbind(d.mic.90, log2(d.mic.90), d.mic.90.level, 
    i.rank, 
    d.age.months, 
    d.mass,
    i.maternal.rank.at.birth,
    i.maternal.rank.at.sample,
    i.rank,
    i.litter.rank,
    i.number.in.litter,
    d.minutes.blood,
    i.glucose.green,
    i.glucose.red,
    d.pcv,
    d.total.solids,
    d.months.to.grad,
    d.months.to.wean,
    i.cdv.titer
    ) 
    ~ c.id 
    + i.sample.number 
    + c.sex 
    + d.sample.date
#    + i.plate.id    
    + c.mom
    + d.dob
    + c.age
    + c.clan
    + c.natal.immigrant
    + c.body.condition
    + c.matched.subadult.adult
    + c.one.adult.samples
    + c.two.adult.samples
    + c.three.adult.samples
    , 
    data = df.pm.data, mean, na.action = na.pass)
        
#########################################################################
   
  df.ec.data <- with(df.ec.data, f.sort.frame(df.ec.data, i.rank, c.id))
  df.pm.data <- with(df.pm.data, f.sort.frame(df.pm.data, i.rank, c.id))
 
  print(df.ec.data[,1:5])
  print(df.pm.data)
  
  
#  print(df.ec.data[,c("c.id", "c.sex", "c.age", "i.sample.number", "d.mic.90", "d.mic.90.level")])
#  print(df.pm.data[,c("c.id", "c.sex", "c.age", "i.sample.number", "d.mic.90", "d.mic.90.level")])
  
  #df.temp.names <- data.frame(df.ec.median$c.id, df.ec.data$c.id, df.ec.sd$c.id)
  #print(df.temp.names)
  
  t.ec.xtabs <- xtabs(~ df.ec.data$c.sex + df.ec.data$c.age)
  t.pm.xtabs <- xtabs(~ + df.pm.data$c.sex + df.pm.data$c.age)
  print(t.ec.xtabs)
  print(t.pm.xtabs)
  
  m.bacteria.mic.correlation <- cor(df.ec.data[,"d.mic.90"], df.pm.data[,"d.mic.90"])
  print(paste("correlation between d.mic.90 for E. coli and P. mirabilis: ", m.bacteria.mic.correlation), quote = FALSE)

  m.bacteria.mic.correlation <- cor(df.ec.data[,"log2(d.mic.90)"], df.pm.data[,"log2(d.mic.90)"])
  print(paste("correlation between log2(d.mic.90) for E. coli and P. mirabilis: ", m.bacteria.mic.correlation), quote = FALSE)

  m.bacteria.mic.correlation <- cor(df.ec.data[,"d.mic.90.level"], df.pm.data[,"d.mic.90.level"])
  print(paste("correlation between d.mic.90.level for E. coli and P. mirabilis: ", m.bacteria.mic.correlation), quote = FALSE)
  
  # This is just the test to make sure the data sets match
  #   This tests for a correlation between the minutes to blood collection of each individual
  #   in the data set for the different bacteria tested.  The correrlation should be equal to 1.
  m.bacteria.minutes.blood.correlation <- cor(df.ec.data[,"d.minutes.blood"], df.pm.data[,"d.minutes.blood"])
  print(paste("correlation between minutes from blood for E. coli and P. mirabilis: ", m.bacteria.minutes.blood.correlation), quote = FALSE)
  
  # This is just the test to make sure the data sets match
  #   This tests for a correlation between the rank of each individual
  #   in the data set for the different bacteria tested.  The correrlation should be equal to 1.
  m.bacteria.rank.correlation <- cor(df.ec.data[,"i.rank"], df.pm.data[,"i.rank"])
  print(paste("correlation between rank for E. coli and P. mirabilis: ", m.bacteria.rank.correlation), quote = FALSE)
  
  
  #####################
  # Plots and lmer tests

#  print("lmer on e.c.") 
#  par(mfrow=c(2,3))
#  lmer.rank <- lmer(d.mic.90.level ~ i.rank + (1 | c.id), data = df.ec.data)
#  print(summary(lmer.rank))#  plot(lmer.rank)
#  hist(resid(lmer.rank))

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