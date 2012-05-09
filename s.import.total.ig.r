# source("s.import.total.ig.r")

df.total.ig.raw <- read.table("total.ig.csv", sep=",", header=TRUE, blank.lines.skip = TRUE)
df.total.ig.raw <- subset(df.total.ig.raw, !(c.id == "mls" & i.sample.number == 1))
df.total.ig.raw$d.sample.date <- as.Date(as.character(df.total.ig.raw$d.sample.date), "%d-%b-%Y")
print(paste("length df.total.ig.raw = ", length(df.total.ig.raw$c.id)))
df.total.ig.raw$c.secondary.antibody <- factor(as.character(df.total.ig.raw$c.secondary.antibody))


df.master.total.ig <- merge(df.master, df.total.ig.raw,                           # merge the two data frames. Keep all rows in df.bka.raw
  by = c("c.id", "i.kay.code", "i.sample.number", "d.sample.date"), # use these columns as keys for merging
  suffixes = c("",".total.ig"), all = TRUE)                                  # rename the non-key columns with the suffix .total.ig when there are conflicts
print(paste("length df.master.total.ig = ", length(df.master.total.ig$c.id)))

################# Total IgG ######################################################################################################
########################### aggregate and get the means #################
df.total.igg.mean <- aggregate(cbind(d.raw.abs,
  d.blank.abs,
  d.blank.abs.corrected
  )
  ~ c.id
  + i.kay.code
  + d.sample.date
  + i.sample.number
#  + c.sex
  + c.isotype.target
  + c.primary.antibody
  + i.primary.dilution
  + c.secondary.antibody
  + i.secondary.dilution
  + i.sample.dilution
  ,
  data = df.master.total.ig[df.master.total.ig$c.isotype.target == "IgG",], mean, na.action = na.pass
)

########################### aggregate and get the standard error #################
df.total.igg.std.error <- aggregate(cbind(d.raw.abs,
  d.blank.abs,
  d.blank.abs.corrected
  )
  ~ c.id
  + i.kay.code
  + d.sample.date
  + i.sample.number
  ,
  data = df.master.total.ig[df.master.total.ig$c.isotype.target == "IgG",], std.error, na.action = na.pass
)

df.total.igg <- merge(df.total.igg.mean, df.total.igg.std.error,                       # merge the two data frames. Keep all rows in df.master
  by = c("c.id", "i.kay.code", "i.sample.number", "d.sample.date"),                         # use these columns as keys for merging
  suffixes = c("",".std.error"), all = TRUE)                                  # rename the non-key columns when there are conflicts

df.total.igg.temp <- subset(df.total.igg, select = -c(c.id, i.kay.code, i.sample.number, d.sample.date))      # make data frame without the key columns
df.total.igg.temp <- df.total.igg.temp[,sort(names(df.total.igg.temp))]                              # sort the new data frame alphabetically by column names
df.total.igg <- cbind(df.total.igg[, c("c.id", "i.kay.code", "i.sample.number", "d.sample.date")], df.total.igg.temp)  # combine the key columns and the sorted columns
rm(df.total.igg.mean)
rm(df.total.igg.std.error)
rm(df.total.igg.temp)
df.total.igg <- with(df.total.igg, f.sort.frame(df.total.igg, c.id, i.sample.number))
print(paste("length df.total.igg = ", length(df.total.igg$c.id)))


################# Total IgM ######################################################################################################
########################### aggregate and get the means #################
df.total.igm.mean <- aggregate(cbind(d.raw.abs,
  d.blank.abs,
  d.blank.abs.corrected
  )
  ~ c.id
  + i.kay.code
  + d.sample.date
  + i.sample.number
#  + c.sex
  + c.isotype.target
  + c.primary.antibody
  + i.primary.dilution
  + c.secondary.antibody
  + i.secondary.dilution
  + i.sample.dilution
  ,
  data = df.master.total.ig[df.master.total.ig$c.isotype.target == "IgM",], mean, na.action = na.pass
)

########################### aggregate and get the standard error #################
df.total.igm.std.error <- aggregate(cbind(d.raw.abs,
  d.blank.abs,
  d.blank.abs.corrected
  )
  ~ c.id
  + i.kay.code
  + d.sample.date
  + i.sample.number
  ,
  data = df.master.total.ig[df.master.total.ig$c.isotype.target == "IgM",], std.error, na.action = na.pass
)

df.total.igm <- merge(df.total.igm.mean, df.total.igm.std.error,                       # merge the two data frames. Keep all rows in df.master
  by = c("c.id", "i.kay.code", "i.sample.number", "d.sample.date"),                         # use these columns as keys for merging
  suffixes = c("",".std.error"), all = TRUE)                                  # rename the non-key columns when there are conflicts

df.total.igm.temp <- subset(df.total.igm, select = -c(c.id, i.kay.code, i.sample.number, d.sample.date))      # make data frame without the key columns
df.total.igm.temp <- df.total.igm.temp[,sort(names(df.total.igm.temp))]                              # sort the new data frame alphabetically by column names
df.total.igm <- cbind(df.total.igm[, c("c.id", "i.kay.code", "i.sample.number", "d.sample.date")], df.total.igm.temp)  # combine the key columns and the sorted columns
rm(df.total.igm.mean)
rm(df.total.igm.std.error)
rm(df.total.igm.temp)
df.total.igm <- with(df.total.igm, f.sort.frame(df.total.igm, c.id, i.sample.number))
print(paste("length df.total.igm = ", length(df.total.igm$c.id)))

#### make flat data frames with one entry for each sample
df.total.ig.flat <- merge(x = df.total.igg, y = df.total.igm,
  by = c("c.id", "i.kay.code", "d.sample.date", "i.sample.number"),
#  by.y = c("c.id", "i.kay.code", "c.sex", "d.sample.date", "i.sample.number", "c.age"),
  all = TRUE,
#  all.y = T,
  suffixes = c(".total.igg", ".total.igm"))
print(paste("length df.total.ig.flat = ", length(df.total.ig.flat$c.id)))








