# source("s.single.median.mean.sd.levels.r")

# print(df.data[df.data$c.id=="mls", 1:10])
# df.temp <- subset(df.data, !(c.id == "mls" & c.age == "subadult"))
# print(df.temp[df.temp$c.id=="mls",])


df.ec.data <- subset(df.data, c.strain == "ec" & i.sample.number == 1)   # Get only the first sample from each individual for the E. coli data
df.pm.data <- subset(df.data, c.strain == "pm" & i.sample.number == 1)   # Get only the first sample from each individual for the P. mirabilis data

df.ec.median <- aggregate((d.mic.90.level) ~ c.id 
  + i.sample.number 
  + c.sex 
  + d.sample.date
#  + c.mom
#  + d.dob
  + c.age
#  + d.age.months 
#  + d.mass
#  + c.clan
  + c.natal.immigrant
#  + i.maternal.rank.at.birth
#  + i.maternal.rank.at.sample
#  + i.rank
#  + i.litter.rank
#  + i.number.in.litter
#  + d.minutes.blood
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

df.pm.median <- aggregate((d.mic.90.level) ~ c.id 
  + i.sample.number 
  + c.sex 
  + d.sample.date
  + c.mom
  + d.dob
  + c.age
  + d.age.months 
  + d.mass
  + c.clan
  + c.natal.immigrant
  + i.maternal.rank.at.birth
  + i.maternal.rank.at.sample
  + i.rank
  + i.litter.rank
  + i.number.in.litter
  + d.minutes.blood
  + i.glucose.green
  + i.glucose.red
  + d.pcv
  + d.total.solids
  + c.body.condition
  + c.matched.subadult.adult
  + c.one.adult.samples
  + c.two.adult.samples
  + c.three.adult.samples
  , 
  data = df.pm.data, median, na.action = na.pass)
   
l.v.check.names <- names(df.ec.median) == "(d.mic.90.level)"
colnames(df.ec.median)[l.v.check.names]<-"d.mic.median"  
l.v.check.names <- names(df.pm.median) == "(d.mic.90.level)"
colnames(df.pm.median)[l.v.check.names]<-"d.mic.median"  

########################### aggregate and get the means #################
df.ec.mean <- aggregate((d.mic.90.level) ~ c.id 
  + i.sample.number 
  + c.sex 
  + d.sample.date
  + c.mom
  + d.dob
  + c.age
  + d.age.months 
  + d.mass
  + c.clan
  + c.natal.immigrant
  + i.maternal.rank.at.birth
  + i.maternal.rank.at.sample
  + i.rank
  + i.litter.rank
  + i.number.in.litter
  + d.minutes.blood
  + i.glucose.green
  + i.glucose.red
  + d.pcv
  + d.total.solids
  + c.body.condition
  + c.matched.subadult.adult
  + c.one.adult.samples
  + c.two.adult.samples
  + c.three.adult.samples
  , 
  data = df.ec.data, mean, na.action = na.pass)

df.pm.mean <- aggregate((d.mic.90.level) ~ c.id 
  + i.sample.number 
  + c.sex 
  + d.sample.date
  + c.mom
  + d.dob
  + c.age
  + d.age.months 
  + d.mass
  + c.clan
  + c.natal.immigrant
  + i.maternal.rank.at.birth
  + i.maternal.rank.at.sample
  + i.rank
  + i.litter.rank
  + i.number.in.litter
  + d.minutes.blood
  + i.glucose.green
  + i.glucose.red
  + d.pcv
  + d.total.solids
  + c.body.condition
  + c.matched.subadult.adult
  + c.one.adult.samples
  + c.two.adult.samples
  + c.three.adult.samples
  , 
  data = df.pm.data, mean, na.action = na.pass)
   
l.v.check.names <- names(df.ec.mean) == "(d.mic.90.level)"
colnames(df.ec.mean)[l.v.check.names]<-"d.mic.mean"  
l.v.check.names <- names(df.pm.mean) == "(d.mic.90.level)"
colnames(df.pm.mean)[l.v.check.names]<-"d.mic.mean"  

#########################################################################

########################### aggregate and get the standard deviation #################
df.ec.sd <- aggregate((d.mic.90.level) ~ c.id 
  + i.sample.number 
  + c.sex 
  + d.sample.date
  + c.mom
  + d.dob
  + c.age
  + d.age.months 
  + d.mass
  + c.clan
  + c.natal.immigrant
  + i.maternal.rank.at.birth
  + i.maternal.rank.at.sample
  + i.rank
  + i.litter.rank
  + i.number.in.litter
  + d.minutes.blood
  + i.glucose.green
  + i.glucose.red
  + d.pcv
  + d.total.solids
  + c.body.condition
  + c.matched.subadult.adult
  + c.one.adult.samples
  + c.two.adult.samples
  + c.three.adult.samples
  , 
  data = df.ec.data, sd, na.action = na.pass)

df.pm.sd <- aggregate((d.mic.90.level) ~ c.id 
  + i.sample.number 
  + c.sex 
  + d.sample.date
  + c.mom
  + d.dob
  + c.age
  + d.age.months 
  + d.mass
  + c.clan
  + c.natal.immigrant
  + i.maternal.rank.at.birth
  + i.maternal.rank.at.sample
  + i.rank
  + i.litter.rank
  + i.number.in.litter
  + d.minutes.blood
  + i.glucose.green
  + i.glucose.red
  + d.pcv
  + d.total.solids
  + c.body.condition
  + c.matched.subadult.adult
  + c.one.adult.samples
  + c.two.adult.samples
  + c.three.adult.samples
  , 
  data = df.pm.data, sd, na.action = na.pass)
   
l.v.check.names <- names(df.ec.sd) == "(d.mic.90.level)"
colnames(df.ec.sd)[l.v.check.names]<-"d.mic.sd"  
l.v.check.names <- names(df.pm.sd) == "(d.mic.90.level)"
colnames(df.pm.sd)[l.v.check.names]<-"d.mic.sd"  

#########################################################################

df.ec.median <- with(df.ec.median, f.sort.frame(df.ec.median, d.sample.date, c.id, i.sample.number))  
df.ec.mean <- with(df.ec.mean, f.sort.frame(df.ec.mean, d.sample.date, c.id, i.sample.number))
df.ec.sd <- with(df.ec.sd, f.sort.frame(df.ec.sd, d.sample.date, c.id, i.sample.number))

df.pm.median <- with(df.pm.median, f.sort.frame(df.pm.median, d.sample.date, c.id, i.sample.number))  
df.pm.mean <- with(df.pm.mean, f.sort.frame(df.pm.mean, d.sample.date, c.id, i.sample.number))
df.pm.sd <- with(df.pm.sd, f.sort.frame(df.pm.sd, d.sample.date, c.id, i.sample.number))

#df.temp.names <- data.frame(df.ec.median$c.id, df.ec.mean$c.id, df.ec.sd$c.id)
#print(df.temp.names)

# Combine the data frames so the median, mean, and sd are all in the same data frame
df.ec.median.mean <- merge(df.ec.median, df.ec.mean)
df.ec.median.mean.sd <- merge(df.ec.median.mean, df.ec.sd)
print(df.ec.median.mean.sd[,1:10])

df.pm.median.mean <- merge(df.pm.median, df.pm.mean)
df.pm.median.mean.sd <- merge(df.pm.median.mean, df.pm.sd)
print(df.pm.median.mean.sd[,1:10])


t.ec.xtabs <- xtabs(~ df.ec.mean$c.sex + df.ec.mean$c.age)
t.pm.xtabs <- xtabs(~ + df.pm.mean$c.sex + df.pm.mean$c.age)
print(t.ec.xtabs)
print(t.pm.xtabs)

m.bacteria.mic.correlation <- cor(df.ec.mean[,"d.mic.mean"], df.pm.mean[,"d.mic.mean"])
print(paste("correlation between MICs for E. coli and P. mirabilis: ", m.bacteria.mic.correlation), quote = FALSE)

# This is just the test to make sure the data sets match
#   This tests for a correlation between the minutes to blood collection of each individual
#   in the data set for the different bacteria tested.  The correrlation should be equal to 1.
m.bacteria.minutes.blood.correlation <- cor(df.ec.mean[,"d.minutes.blood"], df.pm.mean[,"d.minutes.blood"])
print(paste("correlation between minutes from blood for E. coli and P. mirabilis: ", m.bacteria.minutes.blood.correlation), quote = FALSE)

# This is just the test to make sure the data sets match
#   This tests for a correlation between the rank of each individual
#   in the data set for the different bacteria tested.  The correrlation should be equal to 1.
m.bacteria.rank.correlation <- cor(df.ec.mean[,"i.rank"], df.pm.mean[,"i.rank"])
print(paste("correlation between rank for E. coli and P. mirabilis: ", m.bacteria.rank.correlation), quote = FALSE)


#####################
# Plots and lm tests
par(mfrow=c(2,1))
boxplot(d.mic.mean ~ c.age, data = df.ec.mean, 
  main = "E. coli MIC full set", xlab = "age", ylab = "(mic)")
boxplot(d.mic.mean ~ c.age, data = df.pm.mean, 
  main = "P. mirabilis MIC full set", xlab = "age", ylab = "(mic)")

boxplot(d.mic.mean ~ c.age + c.sex, data = df.ec.mean, 
  main = "E. coli MIC full set", xlab = "age, sex", ylab = "(mic)")
boxplot(d.mic.mean ~ c.age + c.sex, data = df.pm.mean, 
  main = "P. mirabilis MIC full set", xlab = "age, sex", ylab = "(mic)")

par(mfrow=c(2,3))  
lm.age <- lm(d.mic.mean ~ c.age, data = df.ec.mean)
print(summary(lm.age))
plot(lm.age)
hist(resid(lm.age))

par(mfrow=c(2,3))
lm.age <- lm(d.mic.mean ~ c.age, data = df.pm.mean)
print(summary(lm.age))
plot(lm.age)
hist(resid(lm.age))  

par(mfrow=c(2,3))
lm.sex <- lm(d.mic.mean ~ c.sex, data = df.ec.mean)
print(summary(lm.sex))
plot(lm.sex)
hist(resid(lm.sex))

par(mfrow=c(2,3))
lm.sex <- lm(d.mic.mean ~ c.sex, data = df.pm.mean)
print(summary(lm.sex))
plot(lm.sex)
hist(resid(lm.sex))  

par(mfrow=c(2,3))
lm.age.sex <- lm(d.mic.mean ~ c.age + c.sex, data = df.ec.mean)
print(summary(lm.age.sex))
plot(lm.age.sex)
hist(resid(lm.age.sex))

par(mfrow=c(2,3))
lm.age.sex<- lm(d.mic.mean ~ c.age + c.sex, data = df.pm.mean)
print(summary(lm.age.sex))
plot(lm.age.sex)
hist(resid(lm.age.sex))  

par(mfrow=c(2,3))

###########################################################
# Need to separate males and females before comparing rank
# plot(d.mic.mean ~ i.rank, data = df.ec.mean, 
#   main = "P. mirabilis MIC full set", xlab = "rank", ylab = "(mic)")
# abline(lm(df.ec.mean$d.mic.mean ~ df.ec.mean$i.rank), col = "red") # regression line (y~x) 
# lines(lowess(df.ec.mean$d.mic.mean ~ df.ec.mean$i.rank), col = "blue") # lowess line (x,y)

# plot(d.mic.mean ~ i.rank, data = df.pm.mean, 
#   main = "P. mirabilis MIC full set", xlab = "rank", ylab = "(mic)")
# abline(lm(df.pm.mean$d.mic.mean ~ df.pm.mean$i.rank), col = "red") # regression line (y~x) 
# lines(lowess(df.pm.mean$d.mic.mean ~ df.pm.mean$i.rank), col = "blue") # lowess line (x,y)
###########################################################

###########################################################
# Remove individual "mls" from the data set.  This sample shows no inhibition and I suspect there may be something wrong with the sample (e.g. collection procedure)
# df.ec.mean <- df.ec.mean[grep("mls", df.ec.mean[,"c.id"], invert = TRUE), ]
# df.pm.mean <- df.pm.mean[grep("mls", df.pm.mean[,"c.id"], invert = TRUE), ]
# print(df.ec.mean)
# print(df.pm.mean)

# lm.age <- lm(d.mic.mean ~ c.age, data = df.ec.mean)
# print(summary(lm.age))
# plot(lm.age)
# hist(resid(lm.age))

# lm.age <- lm(d.mic.mean ~ c.age, data = df.pm.mean)
# print(summary(lm.age))
# plot(lm.age)
# hist(resid(lm.age))

###########################################################



par(mfrow=c(1,1))
plot(x = 1, y = 1)
