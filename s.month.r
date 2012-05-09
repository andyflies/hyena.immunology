# source("s.month.r")
# df.serology.raw <- subset(df.data.flat, !(c.id == "bail" & i.sample.number == 2))

df.adult.female.flat <- subset(df.data.flat, c.age == "adult" & c.sex == "f")
df.adult.female.flat <- subset(df.data.flat, c.age == "adult" & c.sex == "f" & i.sample.number.2 == 2)


print(plot(df.adult.female.flat[[cv.immune.short[1]]] ~ i.rank, df.adult.female.flat,  
  main = paste(cv.immune.short[1], " by rank"), ylab = cv.immune.short[1]))
lm.rank <- lm(df.adult.female.flat[[cv.immune.short[1]]] ~ i.rank, data = df.adult.female.flat)
print(summary(lm.rank))  
print(abline(lm.rank))































