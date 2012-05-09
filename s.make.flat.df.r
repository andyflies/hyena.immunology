# source("s.make.flat.df.r")

list.dfs <- list(df.bka.flat, df.total.ig.flat, df.bacteria.ig.flat, df.serology.flat, df.wbc.flat, df.darting.flat, df.hormones.flat) #, df.fitness)

df.data.flat <- merge.rec(list.dfs, all = T, suffixes=c("", ""))
print(paste("length df.data.flat = ", length(df.data.flat$c.id)))

write.csv(x = df.data.flat, file = "df.data.flat.csv")
