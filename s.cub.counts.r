# source("s.cub.counts.r")

set.seed(123)

df.temp <- subset(df.data)

df.cub.counts <- aggregate(
  cbind(i.survive.parturition, i.survive.grad, i.survive.wean, i.survive.24.months) 
  ~ c.id + i.kay.code,
  data = df.temp, sum, na.action = na.pass)    # na.pass includes the na values, na.action = na.omit  removes all records that contain an NA             

df.cub.counts <- df.cub.counts[order(df.cub.counts$c.id),]

print(df.cub.counts)
write.csv(df.cub.counts, file = "df.cub.counts.csv") 


