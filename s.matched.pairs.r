
par(mfrow=c(2,3))

df.ec.data <- df.data[grep("ec", df.data$c.strain), ]        # Get only the E. coli data
df.pm.data <- df.data[grep("pm", df.data$c.strain), ]        # Get only the P. mirabilis data

df.ec.matched.pairs <- df.ec.data[grep("yes", df.ec.data$c.matched.subadult.adult), ]
df.ec.matched.pairs <- df.ec.matched.pairs[grep(3, df.ec.matched.pairs$i.sample.number, invert = TRUE), ]

df.pm.matched.pairs <- df.pm.data[grep("yes", df.pm.data$c.matched.subadult.adult), ]
df.pm.matched.pairs <- df.pm.matched.pairs[grep(3, df.pm.matched.pairs$i.sample.number, invert = TRUE), ]

df.ec.means <- aggregate(log2(d.mic.90) ~ c.id 
  + i.sample.number 
  + c.sex 
  + d.sample.date
#  + c.mom
#  + d.dob
  + c.age
#  + d.age.months 
  + d.mass
#  + c.clan
#  + c.natal.immigrant
#  + i.maternal.rank.at.birth
#  + i.maternal.rank.at.sample
#  + i.rank
#  + i.litter.rank
#  + i.number.in.litter
  + d.minutes.blood
#  + i.glucose.green
#  + i.glucose.red
#  + d.pcv
#  + d.total.solids
#  + c.body.condition
  + c.matched.subadult.adult
#  + c.one.adult.samples
#  + c.two.adult.samples
#  + c.three.adult.samples
  , 
  data = df.ec.matched.pairs, mean)

df.pm.means <- aggregate(log2(d.mic.90) ~ c.id 
  + i.sample.number 
  + c.sex 
  + d.sample.date
#  + c.mom
#  + d.dob
  + c.age
#  + d.age.months 
  + d.mass
#  + c.clan
#  + c.natal.immigrant
#  + i.maternal.rank.at.birth
#  + i.maternal.rank.at.sample
#  + i.rank
#  + i.litter.rank
#  + i.number.in.litter
  + d.minutes.blood
#  + i.glucose.green
#  + i.glucose.red
#  + d.pcv
#  + d.total.solids
#  + c.body.condition
  + c.matched.subadult.adult
#  + c.one.adult.samples
#  + c.two.adult.samples
#  + c.three.adult.samples
  , 
  data = df.pm.matched.pairs, mean)
   
l.v.check.names <- names(df.ec.means) == "log2(d.mic.90)"
colnames(df.ec.means)[l.v.check.names]<-"d.mic"  
l.v.check.names <- names(df.pm.means) == "log2(d.mic.90)"
colnames(df.pm.means)[l.v.check.names]<-"d.mic"  
  
df.ec.means <- with(df.ec.means, f.sort.frame(df.ec.means, c.id, i.sample.number))
df.pm.means <- with(df.pm.means, f.sort.frame(df.pm.means, c.id, i.sample.number))

print(df.ec.means)
print(df.pm.means)

m.mic.correlation <- cor(df.ec.means[,"d.mic"], df.pm.means[,"d.mic"])
print(m.mic.correlation)

boxplot(d.mic ~ c.age, data = df.ec.means, 
  main = "E. coli MIC matched pairs", xlab = "age", ylab = "log2(mic)")
boxplot(d.mic ~ c.age, data = df.pm.means, 
  main = "P. mirabilis MIC matched pairs", xlab = "age", ylab = "log2(mic)")

tt.ec.matched <- t.test(d.mic ~ c.age, data = df.ec.means, paired = TRUE)
tt.pm.matched <- t.test(d.mic ~ c.age, data = df.pm.means, paired = TRUE)

print(tt.ec.matched)
print(tt.pm.matched)

par(mfrow=c(2,3))

# lm.age <- lm(d.mic ~ c.age, data = df.ec.means)
# print(summary(lm.age))
# plot(lm.age)
# hist(resid(lm.age))

# par(mfrow=c(2,3))
# lm.mass <- lm(d.mic.90 ~ as.double(d.mass), data = df.ec.means)
# print(summary(lm.mass))
# plot(lm.mass)
# hist(resid(lm.mass))

# par(mfrow=c(2,3))
# lm.sample.date <- lm(d.mic.90 ~ as.Date(d.sample.date), data = df.ec.means)
# print(summary(lm.sample.date))
# plot(lm.sample.date)
# hist(resid(lm.sample.date))


