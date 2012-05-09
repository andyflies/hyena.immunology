# source("f.lrm.adult.female.rank.median.levels.r")

# Ordinal logistic regression of i.rank on d.mic.median for adult females
# Uses lrm from Design package

# print(df.data[df.data$c.id=="mls", 1:10])
# df.temp <- subset(df.data, !(c.id == "mls" & c.age == "subadult"))
# print(df.temp[df.temp$c.id=="mls",])

f.lrm.adult.female.rank.median.levels <- function()
{

  df.ec.data <- subset(df.data, c.strain == "ec" & c.sex == "f" & c.age == "adult")   # Get only the first sample from each individual for the E. coli data
  df.pm.data <- subset(df.data, c.strain == "pm" & c.sex == "f" & c.age == "adult")   # Get only the first sample from each individual for the P. mirabilis data
  
  ########################### aggregate and get the medians #################
  df.ec.median <- aggregate(cbind(d.mic.90.level, i.rank) ~ c.id 
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
    data = df.ec.data, median, na.action = na.pass)
  
  df.pm.median <- aggregate(cbind(d.mic.90.level, i.rank) ~ c.id 
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
    data = df.pm.data, median, na.action = na.pass)
     
  l.v.check.names <- names(df.ec.median) == "d.mic.90.level"
  colnames(df.ec.median)[l.v.check.names]<-"d.mic.median"  
  l.v.check.names <- names(df.pm.median) == "d.mic.90.level"
  colnames(df.pm.median)[l.v.check.names]<-"d.mic.median"  
  
  print(df.ec.median)
  print(df.pm.median)
  
#########################################################################
   
  df.ec.median <- with(df.ec.median, f.sort.frame(df.ec.median, i.rank, c.id))
  df.pm.median <- with(df.pm.median, f.sort.frame(df.pm.median, i.rank, c.id))
  
  print(df.ec.median)
  print(df.pm.median)
  
  #df.temp.names <- data.frame(df.ec.median$c.id, df.ec.median$c.id, df.ec.sd$c.id)
  #print(df.temp.names)
  
  t.ec.xtabs <- xtabs(~ df.ec.median$c.sex + df.ec.median$c.age)
  t.pm.xtabs <- xtabs(~ + df.pm.median$c.sex + df.pm.median$c.age)
  print(t.ec.xtabs)
  print(t.pm.xtabs)
  
  m.bacteria.mic.correlation <- cor(df.ec.median[,"d.mic.median"], df.pm.median[,"d.mic.median"])
  print(paste("correlation between MICs for E. coli and P. mirabilis: ", m.bacteria.mic.correlation), quote = FALSE)
  
  # This is just the test to make sure the data sets match
  #   This tests for a correlation between the minutes to blood collection of each individual
  #   in the data set for the different bacteria tested.  The correrlation should be equal to 1.
#  m.bacteria.minutes.blood.correlation <- cor(df.ec.median[,"d.minutes.blood"], df.pm.median[,"d.minutes.blood"])
#  print(paste("correlation between minutes from blood for E. coli and P. mirabilis: ", m.bacteria.minutes.blood.correlation), quote = FALSE)
  
  # This is just the test to make sure the data sets match
  #   This tests for a correlation between the rank of each individual
  #   in the data set for the different bacteria tested.  The correrlation should be equal to 1.
  m.bacteria.rank.correlation <- cor(df.ec.median[,"i.rank"], df.pm.median[,"i.rank"])
  print(paste("correlation between rank for E. coli and P. mirabilis: ", m.bacteria.rank.correlation), quote = FALSE)
  
  
  #####################
  # Plots and lm tests
 
  par(mfrow=c(2,3))
  lrm.rank <- lrm(d.mic.median ~ i.rank, data = df.ec.median)
#  print(summary(lrm.rank))
  print(lrm.rank)
#  plot(lrm.rank)
#  hist(resid(lrm.rank))

  ols.rank <- ols(d.mic.median ~ i.rank, data = df.ec.median)
  print(ols.rank)
  print(ols.rank$coef[1])
  print(ols.rank$coef[2])
  print(coef(ols.rank))
  plot(d.mic.median ~ i.rank, data = df.ec.median) 
  abline(coef = coef(ols.rank), col = "red")
  hist(residuals(ols.rank))
    
  lrm.rank <- lrm(d.mic.median ~ i.rank, data = df.pm.median)
#  print(summary(lrm.rank))
  print(lrm.rank)
  print("coef(lrm.rank)")
  print(coef(lrm.rank))
#  plot(lrm.rank)
#  hist(resid(lrm.rank)) 
  ols.rank <- ols(d.mic.median ~ i.rank, data = df.pm.median)
  print(ols.rank)
  plot(d.mic.median ~ i.rank, data = df.pm.median) 
  abline(coef = coef(ols.rank), col = "red")
  hist(residuals(ols.rank))

  
  par(mfrow=c(1,1))
  plot(x = 1, y = 1)
  
}