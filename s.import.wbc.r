# source("s.import.wbc.r")

print("Use df.wbc.sum.separate.counters for analysis of consistency between the WBC counters")
print("Use df.wbc for all other analysis")

df.wbc.raw <- read.table(file = "wbc.csv", sep = ",", header = TRUE, blank.lines.skip = TRUE)
df.wbc.raw <- df.wbc.raw[!is.na(df.wbc.raw$i.blind.number),]  # This grep command removes the blank lines
print(paste("length df.wbc.raw = ", length(df.wbc.raw$i.blind.number)))
# df.wbc <- df.wbc[!is.na(df.wbc$i.blind.number),]  # This grep command removes the blank lines

########################### aggregate and get the sums of the WBC counts with the counts from CK and BY separated #################
df.wbc.separate.counters <- aggregate(cbind(d.basophils,	d.eosinophils, d.lymphocytes, d.monocytes, d.neutrophils, d.total.wbc) ~
  i.blind.number
  + c.counter
  ,
  data = df.wbc.raw, sum, na.action = na.pass)    # Note that this aggregate function calls "sum" to get the totals of the 50 field of views counted on the slides

df.wbc.separate.counters$d.neutrophil.lymphocyte.ratio <- df.wbc.separate.counters$d.neutrophils / df.wbc.separate.counters$d.lymphocytes 
df.wbc.separate.counters$d.relative.basophils <- (df.wbc.separate.counters$d.basophils / df.wbc.separate.counters$d.total.wbc)
df.wbc.separate.counters$d.relative.eosinophils <- (df.wbc.separate.counters$d.eosinophils / df.wbc.separate.counters$d.total.wbc)
df.wbc.separate.counters$d.relative.lymphocytes <- (df.wbc.separate.counters$d.lymphocytes / df.wbc.separate.counters$d.total.wbc)
df.wbc.separate.counters$d.relative.monocytes <- (df.wbc.separate.counters$d.monocytes / df.wbc.separate.counters$d.total.wbc)
df.wbc.separate.counters$d.relative.neutrophils <- (df.wbc.separate.counters$d.neutrophils / df.wbc.separate.counters$d.total.wbc)
df.wbc.separate.counters$d.relative.total <- (df.wbc.separate.counters$d.relative.basophils
  + df.wbc.separate.counters$d.relative.eosinophils
  + df.wbc.separate.counters$d.relative.lymphocytes
  + df.wbc.separate.counters$d.relative.monocytes
  + df.wbc.separate.counters$d.relative.neutrophils)

df.master.wbc.separate.counters <- merge(df.master, df.wbc.separate.counters,                          # merge the two data frames. Keep all rows
  by = c("i.blind.number"), # use these columns as keys for merging
  all = TRUE)
df.master.wbc.separate.counters <- subset(df.master.wbc.separate.counters, !(c.id == "mls" & i.sample.number == 1))
df.master.wbc.separate.counters$d.sample.date <- as.Date(as.character(df.master.wbc.separate.counters$d.sample.date), "%d-%b-%Y")
df.master.wbc.separate.counters <- with(df.master.wbc.separate.counters, f.sort.frame(df.master.wbc.separate.counters, c.id, i.sample.number))
print(paste("length df.master.wbc.separate.counters = ", length(df.master.wbc.separate.counters$i.blind.number)))

########################### pool the WBC counts of the two different WBC counters (BY and CK) #################
df.wbc <- aggregate(cbind(d.basophils,	d.eosinophils, d.lymphocytes, d.monocytes, d.neutrophils, d.total.wbc) ~
  i.blind.number
  ,
  data = df.wbc.separate.counters, mean, na.action = na.pass) # This aggregate function calls "mean" to get the mean counts from the 2 different WBC counters

df.wbc$d.neutrophil.lymphocyte.ratio <- df.wbc$d.neutrophils / df.wbc$d.lymphocytes 
df.wbc$d.relative.basophils <- (df.wbc$d.basophils / df.wbc$d.total.wbc)
df.wbc$d.relative.eosinophils <- (df.wbc$d.eosinophils / df.wbc$d.total.wbc)
df.wbc$d.relative.lymphocytes <- (df.wbc$d.lymphocytes / df.wbc$d.total.wbc)
df.wbc$d.relative.monocytes <- (df.wbc$d.monocytes / df.wbc$d.total.wbc)
df.wbc$d.relative.neutrophils <- (df.wbc$d.neutrophils / df.wbc$d.total.wbc)
df.wbc$d.relative.total <- (df.wbc$d.relative.basophils
  + df.wbc$d.relative.eosinophils
  + df.wbc$d.relative.lymphocytes
  + df.wbc$d.relative.monocytes
  + df.wbc$d.relative.neutrophils)

df.master.wbc <- merge(df.master, df.wbc,                          # merge the two data frames. Keep all rows
  by = c("i.blind.number"), # use these columns as keys for merging
  all = TRUE,
  suffixes = c("", ""))

df.master.wbc <- subset(df.master.wbc, !(c.id == "mls" & i.sample.number == 1))                                                 # keep all the records, we can go back after this and remove the NA rows (we need to do it after merging, otherwise the blank and ampicillin and B3T rows get left out.
df.master.wbc <- with(df.master.wbc, f.sort.frame(df.master.wbc, c.id, i.sample.number))
print(paste("length df.master.wbc = ", length(df.master.wbc$c.id)))

df.wbc <- df.master.wbc
print(paste("length df.wbc = ", length(df.wbc$c.id)))
df.wbc.flat <- df.master.wbc
print(paste("length df.wbc.flat = ", length(df.wbc.flat$c.id)))






