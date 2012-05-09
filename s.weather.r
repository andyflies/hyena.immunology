# source("s.weather.r")

set.seed(123)

df.temp <- subset(df.data)

df.weather <- aggregate(
  cbind(d.temp.min, d.temp.max, d.precip) 
  ~ i.year + i.month,
  data = df.temp, sum, na.action = na.pass)    # na.pass includes the na values, na.action = na.omit  removes all records that contain an NA             

# df.weather <- df.weather[order(df.cub.weather$c.id),]

print(df.weather)
write.csv(df.weather, file = "df.weather.csv") 


