# source("f.lm.total.ig.adult.sex.r")

# print(df.data[df.data$c.id=="mls", 1:10])
# df.temp <- subset(df.data, !(c.id == "mls" & c.age == "subadult"))
# print(df.temp[df.temp$c.id=="mls",])

f.lm.total.ig.adult.sex <- function()
{

  df.igg.data <- subset(df.data, c.isotype.target == "IgG" & c.age == "adult")   # Get only the first sample from each individual for the E. coli data
  df.igm.data <- subset(df.data, c.isotype.target == "IgG" & c.age == "adult")   # Get only the first sample from each individual for the P. mirabilis data

  ## Testing the data with c.id = "gol" removed because it is a potential outlier
#  df.igg.data <- subset(df.data, c.strain == "ec" & c.sex == "f" & c.age == "adult" & c.id != "gol")   # Get only the first sample from each individual for the E. coli data
#  df.igm.data <- subset(df.data, c.strain == "pm" & c.sex == "f" & c.age == "adult" & c.id != "gol")   # Get only the first sample from each individual for the P. mirabilis data
 

  
  ########################### aggregate and get the means #################
  df.igg.mean <- aggregate(cbind(d.mic.90.level, i.rank, d.months.to.wean, d.blank.abs.total.ig) ~ c.id 
    + c.isotype.target
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
    data = df.igg.data, mean, na.action = na.pass)
  
  df.igm.mean <- aggregate(cbind(d.mic.90.level, i.rank, d.months.to.wean, d.blank.abs.total.ig) ~ c.id 
    + c.isotype.target
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
    data = df.igm.data, mean, na.action = na.pass)
     
  l.v.check.names <- names(df.igg.mean) == "d.mic.90.level"
  colnames(df.igg.mean)[l.v.check.names]<-"d.mic.mean"  
  l.v.check.names <- names(df.igm.mean) == "d.mic.90.level"
  colnames(df.igm.mean)[l.v.check.names]<-"d.mic.mean"  
  
  print(df.igg.mean)
  print(df.igm.mean)
                                
#########################################################################
   
  df.igg.mean <- with(df.igg.mean, f.sort.frame(df.igg.mean, i.rank, c.id))
  df.igm.mean <- with(df.igm.mean, f.sort.frame(df.igm.mean, i.rank, c.id))
  
  print(df.igg.mean)
  print(df.igm.mean)
  
  #df.temp.names <- data.frame(df.igg.median$c.id, df.igg.mean$c.id, df.igg.sd$c.id)
  #print(df.temp.names)
  
  t.igg.xtabs <- xtabs(~ df.igg.mean$c.sex + df.igg.mean$c.age)
  t.igm.xtabs <- xtabs(~ + df.igm.mean$c.sex + df.igm.mean$c.age)
  print(t.igg.xtabs)
  print(t.igm.xtabs)
  
  m.isotype.correlation <- cor(df.igg.mean[,"d.blank.abs.total.ig"], df.igm.mean[,"d.blank.abs.total.ig"])
  print(paste("correlation between blank absorbance for IgG and IgM: ", m.isotype.correlation), quote = FALSE)

  m.isotype.correlation <- cor(df.igg.mean[,"i.rank"], df.igg.mean[,"d.months.to.wean"], use = "complete.obs")
  print(paste("correlation for IgG rank and months.to.wean: ", m.isotype.correlation), quote = FALSE)

  m.isotype.correlation <- cor(df.igg.mean[,"i.rank"], df.igg.mean[,"d.blank.abs.total.ig"], use = "complete.obs")
  print(paste("correlation for IgG rank and blank absorbance: ", m.isotype.correlation), quote = FALSE)

  
  # This is just the test to make sure the data sets match
  #   This tests for a correlation between the rank of each individual
  #   in the data set for the different bacteria tested.  The correrlation should be equal to 1.
  m.isotype.correlation <- cor(df.igg.mean[,"i.rank"], df.igm.mean[,"i.rank"])
  print(paste("correlation between rank for IgG and IgM: ", m.isotype.correlation), quote = FALSE)
  
  
  #####################
  # Plots and lm tests
 
  par(mfrow=c(2,3))
  lm.rank <- lm(d.blank.abs.total.ig ~ i.rank, data = df.igg.mean)
  print(summary(lm.rank))
  plot(lm.rank)
  hist(resid(lm.rank))
  plot(d.blank.abs.total.ig ~ i.rank, data = df.igg.mean)
  
  par(mfrow=c(2,3))
  lm.rank <- lm(d.blank.abs.total.ig ~ i.rank, data = df.igm.mean)
  print(summary(lm.rank))
  plot(lm.rank)
  hist(resid(lm.rank))  
  plot(d.blank.abs.total.ig ~ i.rank, data = df.igm.mean)

  par(mfrow=c(2,3))
  lm.sex <- lm(d.blank.abs.total.ig ~ c.sex, data = df.igg.mean)
  print(summary(lm.sex))
  plot(lm.sex)
  hist(resid(lm.sex))  
  
  par(mfrow=c(2,3))
  lm.sex <- lm(d.blank.abs.total.ig ~ c.sex, data = df.igm.mean)
  print(summary(lm.sex))
  plot(lm.sex)
  hist(resid(lm.sex))  
  
  par(mfrow=c(1,1))
  plot(x = 1, y = 1)
  
}