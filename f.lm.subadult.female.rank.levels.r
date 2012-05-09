# source("f.lm.subadult.female.rank.levels.r")

# print(df.data[df.data$c.id=="mls", 1:10])
# df.temp <- subset(df.data, !(c.id == "mls" & c.age == "subadult"))
# print(df.temp[df.temp$c.id=="mls",])

f.lm.subadult.female.rank.levels <- function()
{

  df.ec.data <- subset(df.data, c.strain == "ec" & c.sex == "f" & c.age == "subadult")   # Get only the first sample from each individual for the E. coli data
  df.pm.data <- subset(df.data, c.strain == "pm" & c.sex == "f" & c.age == "subadult")   # Get only the first sample from each individual for the P. mirabilis data
  
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
  lm.rank <- lm(d.mic.mean ~ i.rank, data = df.ec.mean)
  print(summary(lm.rank))
  plot(lm.rank)
  hist(resid(lm.rank))
  
  par(mfrow=c(2,3))
  lm.rank <- lm(d.mic.mean ~ i.rank, data = df.pm.mean)
  print(summary(lm.rank))
  plot(lm.rank)
  hist(resid(lm.rank))  
  
  par(mfrow=c(1,1))
  plot(x = 1, y = 1)
  
}