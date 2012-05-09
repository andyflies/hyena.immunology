# source("f.lm.matched.pairs.levels.r")

# print(df.data[df.data$c.id=="mls", 1:10])
df.temp <- subset(df.data, !(c.id == "mls"))
# print(df.temp[df.temp$c.id=="mls",])

f.lm.matched.pairs.levels <- function()
{

  df.ec.data <- subset(df.temp, c.strain == "ec" & c.matched.subadult.adult == "yes")   # E. coli data
  df.pm.data <- subset(df.temp, c.strain == "pm" & c.matched.subadult.adult == "yes")   # P. mirabilis data
#  df.ec.adult.data <- subset(df.data, c.strain == "ec" & c.age == "adult")   # E. coli data
  
#  c.v.subadult.matches <- df.data$c.id[df.data$c.id %in% df.ec.subadult.data$c.id]   # This match will return all subadults and only adults with matched subadults
#  c.v.matches <- c.v.subadult.matches[c.v.subadult.matches %in% df.ec.adult.data$c.id] # This match will return only matched pairs
#  print(c.v.matches)
  
  ########################### aggregate and get the means #################
  df.ec.mean <- aggregate(cbind(d.mic.90.level, i.rank) ~ c.id 
 #   + i.sample.number 
    + c.sex 
#    + d.sample.date
  #  + c.mom
  #  + d.dob
    + c.age
  #  + d.age.months 
  #  + d.mass
  #  + c.clan
 #   + c.natal.immigrant
  #  + i.maternal.rank.at.birth
  #  + i.maternal.rank.at.sample
#    + i.rank
  #  + i.litter.rank
  #  + i.number.in.litter
#    + d.minutes.blood
  #  + i.glucose.green
  #  + i.glucose.red
  #  + d.pcv
  #  + d.total.solids
  #  + c.body.condition
  #  + c.matched.subadult.adult
  #  + c.one.adult.samples
  #  + c.two.adult.samples
  #  + c.three.adult.samples
    , 
    data = df.ec.data, mean, na.action = na.pass)
  
  df.pm.mean <- aggregate(cbind(d.mic.90.level, i.rank) ~ c.id 
#    + i.sample.number 
    + c.sex 
#    + d.sample.date
  #  + c.mom
  #  + d.dob
    + c.age
  #  + d.age.months 
  #  + d.mass
  #  + c.clan
#    + c.natal.immigrant
  #  + i.maternal.rank.at.birth
  #  + i.maternal.rank.at.sample
#    + i.rank
  #  + i.litter.rank
  #  + i.number.in.litter
#    + d.minutes.blood
  #  + i.glucose.green
  #  + i.glucose.red
  #  + d.pcv
  #  + d.total.solids
  #  + c.body.condition
  #  + c.matched.subadult.adult
  #  + c.one.adult.samples
  #  + c.two.adult.samples
  #  + c.three.adult.samples
    , 
    data = df.pm.data, mean, na.action = na.pass)
     
  l.v.check.names <- names(df.ec.mean) == "d.mic.90.level"
  colnames(df.ec.mean)[l.v.check.names]<-"d.mic.mean"  
  l.v.check.names <- names(df.pm.mean) == "d.mic.90.level"
  colnames(df.pm.mean)[l.v.check.names]<-"d.mic.mean"  
  
  print(df.ec.mean)
  print(df.pm.mean)
  
#########################################################################
   
  df.ec.mean <- with(df.ec.mean, f.sort.frame(df.ec.mean, i.rank, c.id))
  df.pm.mean <- with(df.pm.mean, f.sort.frame(df.pm.mean, i.rank, c.id))
  
  print(df.ec.mean)
  print(df.pm.mean)
  
  boxplot(d.mic.mean ~ c.age, data = df.ec.mean, 
    main = "E. coli MIC matched pairs", xlab = "age", ylab = "d.mic.mean")
  boxplot(d.mic.mean ~ c.age, data = df.pm.mean, 
    main = "P. mirabilis MIC matched pairs", xlab = "age", ylab = "d.mic.mean")
  
  #df.temp.names <- data.frame(df.ec.median$c.id, df.ec.mean$c.id, df.ec.sd$c.id)
  #print(df.temp.names)
  
  t.ec.xtabs <- xtabs(~ df.ec.mean$c.sex + df.ec.mean$c.age)
  t.pm.xtabs <- xtabs(~ + df.pm.mean$c.sex + df.pm.mean$c.age)
  print(t.ec.xtabs)
  print(t.pm.xtabs)
  
  m.bacteria.mic.correlation <- cor(df.ec.mean[,"d.mic.mean"], df.pm.mean[,"d.mic.mean"])
  print(paste("correlation between MICs for E. coli and P. mirabilis: ", m.bacteria.mic.correlation), quote = FALSE)
  
  # This is just the test to make sure the data sets match
  #   This tests for a correlation between the minutes to blood collection of each individual
  #   in the data set for the different bacteria tested.  The correrlation should be equal to 1.
#  m.bacteria.minutes.blood.correlation <- cor(df.ec.mean[,"d.minutes.blood"], df.pm.mean[,"d.minutes.blood"])
#  print(paste("correlation between minutes from blood for E. coli and P. mirabilis: ", m.bacteria.minutes.blood.correlation), quote = FALSE)
  
  # This is just the test to make sure the data sets match
  #   This tests for a correlation between the rank of each individual
  #   in the data set for the different bacteria tested.  The correrlation should be equal to 1.
  m.bacteria.rank.correlation <- cor(df.ec.mean[,"i.rank"], df.pm.mean[,"i.rank"])
  print(paste("correlation between rank for E. coli and P. mirabilis: ", m.bacteria.rank.correlation), quote = FALSE)
  
  
  #####################
  # Plots and lm tests
 
  par(mfrow=c(2,3))
  lm.rank <- lm(d.mic.mean ~ c.age, data = df.ec.mean)
  print(summary(lm.rank))
  plot(lm.rank)
  hist(resid(lm.rank))
  
  par(mfrow=c(2,3))
  lm.rank <- lm(d.mic.mean ~ c.age, data = df.pm.mean)
  print(summary(lm.rank))
  plot(lm.rank)
  hist(resid(lm.rank))  
  
  tt.ec.matched <- t.test(d.mic.mean ~ c.age, data = df.ec.mean, paired = TRUE)
  tt.pm.matched <- t.test(d.mic.mean ~ c.age, data = df.pm.mean, paired = TRUE)

  print(tt.ec.matched)
  print(tt.pm.matched)  
  
  par(mfrow=c(1,1))
  plot(x = 1, y = 1)
  
}