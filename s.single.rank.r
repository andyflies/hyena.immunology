# source("s.single.adult.rank.r")

par(mfrow=c(2,3))

# df.ec.data <- df.data[grep("ec", df.data$c.strain), ]        # Get only the E. coli data
# df.pm.data <- df.data[grep("pm", df.data$c.strain), ]        # Get only the P. mirabilis data

df.ec.data <- subset(df.data, c.strain == "ec" & i.sample.number == 1)   # Get only the first sample from each individual for the E. coli data
df.pm.data <- subset(df.data, c.strain == "pm" & i.sample.number == 1)   # Get only the first sample from each individual for the P. mirabilis data


df.ec.means <- aggregate(log2(d.mic.90) ~ c.id 
  + i.sample.number 
  + c.sex 
  + d.sample.date
#  + c.mom
#  + d.dob
  + c.age
#  + d.age.months 
#  + d.mass
#  + c.clan
#  + c.natal.immigrant
#  + i.maternal.rank.at.birth
#  + i.maternal.rank.at.sample
  + i.rank
#  + i.litter.rank
#  + i.number.in.litter
  + d.minutes.blood
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
  data = df.ec.data, mean)

df.pm.means <- aggregate(log2(d.mic.90) ~ c.id 
  + i.sample.number 
  + c.sex 
  + d.sample.date
#  + c.mom
#  + d.dob
  + c.age
#  + d.age.months 
#  + d.mass
#  + c.clan
#  + c.natal.immigrant
#  + i.maternal.rank.at.birth
#  + i.maternal.rank.at.sample
  + i.rank
#  + i.litter.rank
#  + i.number.in.litter
  + d.minutes.blood
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
  data = df.pm.data, mean)
   
l.v.check.names <- names(df.ec.means) == "log2(d.mic.90)"
colnames(df.ec.means)[l.v.check.names]<-"d.mic"  
l.v.check.names <- names(df.pm.means) == "log2(d.mic.90)"
colnames(df.pm.means)[l.v.check.names]<-"d.mic"  
  
df.ec.means <- with(df.ec.means, f.sort.frame(df.ec.means, d.mic, d.sample.date, c.id, i.sample.number))
df.pm.means <- with(df.pm.means, f.sort.frame(df.pm.means, d.mic, d.sample.date, c.id, i.sample.number))

print(df.ec.means)
print(df.pm.means)

m.bacteria.mic.correlation <- cor(df.ec.means[,"d.mic"], df.pm.means[,"d.mic"])
print(paste("correlation between MICs for E. coli and P. mirabilis: ", m.bacteria.mic.correlation), quote = FALSE)

# This is just the test to make sure the data sets match
#   This tests for a correlation between the minutes to blood collection of each individual
#   in the data set for the different bacteria tested.  The correrlation should be equal to 1.
m.bacteria.minutes.blood.correlation <- cor(df.ec.means[,"d.minutes.blood"], df.pm.means[,"d.minutes.blood"])
print(paste("correlation between minutes from blood for E. coli and P. mirabilis: ", m.bacteria.minutes.blood.correlation), quote = FALSE)

# This is just the test to make sure the data sets match
#   This tests for a correlation between the rank of each individual
#   in the data set for the different bacteria tested.  The correrlation should be equal to 1.
m.bacteria.rank.correlation <- cor(df.ec.means[,"i.rank"], df.pm.means[,"i.rank"])
print(paste("correlation between rank for E. coli and P. mirabilis: ", m.bacteria.rank.correlation), quote = FALSE)


#####################
# Plots and lm tests for rank
boxplot(d.mic ~ i.rank, data = df.ec.means, 
  main = "E. coli MIC full set", xlab = "rank", ylab = "log2(mic)")
boxplot(d.mic ~ i.rank, data = df.pm.means, 
  main = "P. mirabilis MIC full set", xlab = "rank", ylab = "log2(mic)")

boxplot(d.mic ~ i.rank + c.age, data = df.ec.means, 
  main = "E. coli MIC full set", xlab = "rank and age", ylab = "log2(mic)")
boxplot(d.mic ~ i.rank + c.age, data = df.pm.means, 
  main = "P. mirabilis MIC full set", xlab = "rank and age", ylab = "log2(mic)")

boxplot(d.mic ~ c.age + c.sex, data = df.ec.means, 
  main = "E. coli MIC full set", xlab = "age, sex", ylab = "log2(mic)")
boxplot(d.mic ~ c.age + c.sex, data = df.pm.means, 
  main = "P. mirabilis MIC full set", xlab = "age, sex", ylab = "log2(mic)")
  
lm.rank.age <- lm(d.mic ~ i.rank + c.age, data = df.ec.means)
print(summary(lm.rank.age))
plot(lm.rank.age)
hist(resid(lm.rank.age))

lm.rank.age <- lm(d.mic ~ i.rank + c.age, data = df.pm.means)
print(summary(lm.rank.age))
plot(lm.rank.age)
hist(resid(lm.rank.age))  

lm.rank.age.sex <- lm(d.mic ~ i.rank + c.age + c.sex, data = df.ec.means)
print(summary(lm.rank.age.sex))
plot(lm.rank.age.sex)
hist(resid(lm.rank.age.sex))

lm.rank.age.sex<- lm(d.mic ~ i.rank + c.age + c.sex, data = df.pm.means)
print(summary(lm.rank.age.sex))
plot(lm.rank.age.sex)
hist(resid(lm.rank.age.sex))  

par(mfrow=c(2,3))

plot(d.mic ~ i.rank, data = df.ec.means, 
  main = "P. mirabilis MIC full set", xlab = "rank", ylab = "log2(mic)")
abline(lm(df.ec.means$d.mic ~ df.ec.means$i.rank), col = "red") # regression line (y~x) 
lines(lowess(df.ec.means$d.mic ~ df.ec.means$i.rank), col = "blue") # lowess line (x,y)

plot(d.mic ~ i.rank, data = df.pm.means, 
  main = "P. mirabilis MIC full set", xlab = "rank", ylab = "log2(mic)")
abline(lm(df.pm.means$d.mic ~ df.pm.means$i.rank), col = "red") # regression line (y~x) 
lines(lowess(df.pm.means$d.mic ~ df.pm.means$i.rank), col = "blue") # lowess line (x,y)

###########################################################
# Remove individual "mls" from the data set.  This sample shows no inhibition and I suspect there may be something wrong with the sample (e.g. collection procedure)
# df.ec.means <- df.ec.means[grep("mls", df.ec.means[,"c.id"], invert = TRUE), ]
# df.pm.means <- df.pm.means[grep("mls", df.pm.means[,"c.id"], invert = TRUE), ]
# print(df.ec.means)
# print(df.pm.means)

# lm.age <- lm(d.mic ~ c.age, data = df.ec.means)
# print(summary(lm.age))
# plot(lm.age)
# hist(resid(lm.age))

# lm.age <- lm(d.mic ~ c.age, data = df.pm.means)
# print(summary(lm.age))
# plot(lm.age)
# hist(resid(lm.age))

###########################################################



par(mfrow=c(1,1))
plot(x = 1, y = 1)
