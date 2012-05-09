# source("s.import.bacteria.ig.r")

df.bacteria.ig.raw <- read.table("bacteria.elisa.csv", sep=",", header=TRUE, blank.lines.skip = TRUE)
df.bacteria.ig.raw <- subset(df.bacteria.ig.raw, select = -c.sex) # remove c.sex column from this input data frame because we already have the data it will create a duplicate variable when merging
df.bacteria.ig.raw <- subset(df.bacteria.ig.raw, !(c.id == "mls" & i.sample.number == 1))
df.bacteria.ig.raw <- subset(df.bacteria.ig.raw, !(c.id == "nav" & i.sample.number == 3 & i.plate.id == 4))
df.bacteria.ig.raw <- subset(df.bacteria.ig.raw, !(c.id == "pnc" & i.sample.number == 1 & i.plate.id == 4))
df.bacteria.ig.raw <- subset(df.bacteria.ig.raw, !(c.id == "sim" & i.sample.number == 1 & i.plate.id == 4))
df.bacteria.ig.raw <- subset(df.bacteria.ig.raw, !(c.id == "stl" & i.sample.number == 1 & i.plate.id == 4))
df.bacteria.ig.raw <- subset(df.bacteria.ig.raw, !(c.id == "tia" & i.sample.number == 1 & i.plate.id == 4))
df.bacteria.ig.raw <- subset(df.bacteria.ig.raw, !(c.id == "nav" & i.sample.number == 3 & i.plate.id == 4))
df.bacteria.ig.raw <- subset(df.bacteria.ig.raw, !(c.id == "vgs" & i.sample.number == 1 & i.plate.id == 4))
df.bacteria.ig.raw <- subset(df.bacteria.ig.raw, !(c.id == "zc" & i.sample.number == 1 & i.plate.id == 4))

df.bacteria.ig.raw$d.sample.date <- as.Date(as.character(df.bacteria.ig.raw$d.sample.date), "%d-%b-%Y")
print(paste("length df.bacteria.ig.raw = ", length(df.bacteria.ig.raw$c.id)))
df.bacteria.ig.raw$c.secondary.antibody <- factor(as.character(df.bacteria.ig.raw$c.secondary.antibody))


df.master.bacteria.ig <- merge(df.master, df.bacteria.ig.raw,              #
  by = c("c.id", "i.kay.code", "i.sample.number", "d.sample.date"), # use these columns as keys for merging
  suffixes = c("",".bacteria.ig"), all = TRUE)                                  # rename the non-key columns with the suffix .total.ig when there are conflicts
print(paste("length df.master.bacteria.ig = ", length(df.master.bacteria.ig$c.id)))

################# Bacteria ELISA for E. coli IgG ######################################################################################################
########################### aggregate and get the means #################
df.ec.igg.mean <- aggregate(cbind(d.raw.abs,
  d.blank.abs
  )
  ~ c.id
  + i.kay.code
  + d.sample.date
  + i.sample.number
#  + c.sex
  + c.isotype.target
  + c.secondary.antibody
  + i.secondary.dilution
  + i.sample.dilution
  ,
  data = df.master.bacteria.ig[df.master.bacteria.ig$c.isotype.target == "IgG" & df.master.bacteria.ig$c.target == "ec.8739", ], 
  mean, na.action = na.pass
)

########################### aggregate and get the standard error #################
df.ec.igg.std.error <- aggregate(cbind(d.raw.abs,
  d.blank.abs
  )
  ~ c.id
  + i.kay.code
  + d.sample.date
  + i.sample.number
  ,
  data = df.master.bacteria.ig[df.master.bacteria.ig$c.isotype.target == "IgG" & df.master.bacteria.ig$c.target == "ec.8739", ], 
  std.error, na.action = na.pass
)

df.ec.igg <- merge(df.ec.igg.mean, df.ec.igg.std.error,                       # merge the two data frames. Keep all rows in df.master
  by = c("c.id", "i.kay.code", "i.sample.number", "d.sample.date"),                         # use these columns as keys for merging
  suffixes = c("",".std.error"), all = TRUE)                                  # rename the non-key columns when there are conflicts

df.ec.igg.temp <- subset(df.ec.igg, select = -c(c.id, i.kay.code, i.sample.number, d.sample.date))      # make data frame without the key columns
df.ec.igg.temp <- df.ec.igg.temp[,sort(names(df.ec.igg.temp))]                              # sort the new data frame alphabetically by column names
df.ec.igg <- cbind(df.ec.igg[, c("c.id", "i.kay.code", "i.sample.number", "d.sample.date")], df.ec.igg.temp)  # combine the key columns and the sorted columns
rm(df.ec.igg.mean)
rm(df.ec.igg.std.error)
rm(df.ec.igg.temp)
df.ec.igg <- with(df.ec.igg, f.sort.frame(df.ec.igg, c.id, i.sample.number))
print(paste("length df.ec.igg = ", length(df.ec.igg$c.id)))


################# Bacteria ELISA for E. coli IgM ######################################################################################################
########################### aggregate and get the means #################
df.ec.igm.mean <- aggregate(cbind(d.raw.abs,
  d.blank.abs
  )
  ~ c.id
  + i.kay.code
  + d.sample.date
  + i.sample.number
#  + c.sex
  + c.isotype.target
  + c.secondary.antibody
  + i.secondary.dilution
  + i.sample.dilution
  ,
  data = df.master.bacteria.ig[df.master.bacteria.ig$c.isotype.target == "IgM" & df.master.bacteria.ig$c.target == "ec.8739", ], 
  mean, na.action = na.pass
)

########################### aggregate and get the standard error #################
df.ec.igm.std.error <- aggregate(cbind(d.raw.abs,
  d.blank.abs
  )
  ~ c.id
  + i.kay.code
  + d.sample.date
  + i.sample.number
  ,
  data = df.master.bacteria.ig[df.master.bacteria.ig$c.isotype.target == "IgM" & df.master.bacteria.ig$c.target == "ec.8739", ], 
  std.error, na.action = na.pass
)

df.ec.igm <- merge(df.ec.igm.mean, df.ec.igm.std.error,                       # merge the two data frames. Keep all rows in df.master
  by = c("c.id", "i.kay.code", "i.sample.number", "d.sample.date"),                         # use these columns as keys for merging
  suffixes = c("",".std.error"), all = TRUE)                                  # rename the non-key columns when there are conflicts

df.ec.igm.temp <- subset(df.ec.igm, select = -c(c.id, i.kay.code, i.sample.number, d.sample.date))      # make data frame without the key columns
df.ec.igm.temp <- df.ec.igm.temp[,sort(names(df.ec.igm.temp))]                              # sort the new data frame alphabetically by column names
df.ec.igm <- cbind(df.ec.igm[, c("c.id", "i.kay.code", "i.sample.number", "d.sample.date")], df.ec.igm.temp)  # combine the key columns and the sorted columns
rm(df.ec.igm.mean)
rm(df.ec.igm.std.error)
rm(df.ec.igm.temp)
df.ec.igm <- with(df.ec.igm, f.sort.frame(df.ec.igm, c.id, i.sample.number))
print(paste("length df.ec.igm = ", length(df.ec.igm$c.id)))


################# Bacteria ELISA for P. mirabilis IgG ######################################################################################################
########################### aggregate and get the means #################
df.pm.igg.mean <- aggregate(cbind(d.raw.abs,
  d.blank.abs
  )
  ~ c.id
  + i.kay.code
  + d.sample.date
  + i.sample.number
#  + c.sex
  + c.isotype.target
  + c.secondary.antibody
  + i.secondary.dilution
  + i.sample.dilution
  ,
  data = df.master.bacteria.ig[df.master.bacteria.ig$c.isotype.target == "IgG" & df.master.bacteria.ig$c.target == "pm.35659", ], 
  mean, na.action = na.pass
)

########################### aggregate and get the standard error #################
df.pm.igg.std.error <- aggregate(cbind(d.raw.abs,
  d.blank.abs
  )
  ~ c.id
  + i.kay.code
  + d.sample.date
  + i.sample.number
  ,
  data = df.master.bacteria.ig[df.master.bacteria.ig$c.isotype.target == "IgG" & df.master.bacteria.ig$c.target == "pm.35659", ], 
  std.error, na.action = na.pass
)

df.pm.igg <- merge(df.pm.igg.mean, df.pm.igg.std.error,                       # merge the two data frames. Keep all rows in df.master
  by = c("c.id", "i.kay.code", "i.sample.number", "d.sample.date"),                         # use these columns as keys for merging
  suffixes = c("",".std.error"), all = TRUE)                                  # rename the non-key columns when there are conflicts

df.pm.igg.temp <- subset(df.pm.igg, select = -c(c.id, i.kay.code, i.sample.number, d.sample.date))      # make data frame without the key columns
df.pm.igg.temp <- df.pm.igg.temp[,sort(names(df.pm.igg.temp))]                              # sort the new data frame alphabetically by column names
df.pm.igg <- cbind(df.pm.igg[, c("c.id", "i.kay.code", "i.sample.number", "d.sample.date")], df.pm.igg.temp)  # combine the key columns and the sorted columns
rm(df.pm.igg.mean)
rm(df.pm.igg.std.error)
rm(df.pm.igg.temp)
df.pm.igg <- with(df.pm.igg, f.sort.frame(df.pm.igg, c.id, i.sample.number))
print(paste("length df.pm.igg = ", length(df.pm.igg$c.id)))



################# Bacteria ELISA for P. mirabilis IgM ######################################################################################################
########################### aggregate and get the means #################
df.pm.igm.mean <- aggregate(cbind(d.raw.abs,
  d.blank.abs
  )
  ~ c.id
  + i.kay.code
  + d.sample.date
  + i.sample.number
#  + c.sex
  + c.isotype.target
  + c.secondary.antibody
  + i.secondary.dilution
  + i.sample.dilution
  ,
  data = df.master.bacteria.ig[df.master.bacteria.ig$c.isotype.target == "IgM" & df.master.bacteria.ig$c.target == "pm.35659", ], 
  mean, na.action = na.pass
)

########################### aggregate and get the standard error #################
df.pm.igm.std.error <- aggregate(cbind(d.raw.abs,
  d.blank.abs
  )
  ~ c.id
  + i.kay.code
  + d.sample.date
  + i.sample.number
  ,
  data = df.master.bacteria.ig[df.master.bacteria.ig$c.isotype.target == "IgM" & df.master.bacteria.ig$c.target == "pm.35659", ], 
  std.error, na.action = na.pass
)

df.pm.igm <- merge(df.pm.igm.mean, df.pm.igm.std.error,                       # merge the two data frames. Keep all rows in df.master
  by = c("c.id", "i.kay.code", "i.sample.number", "d.sample.date"),                         # use these columns as keys for merging
  suffixes = c("",".std.error"), all = TRUE)                                  # rename the non-key columns when there are conflicts

df.pm.igm.temp <- subset(df.pm.igm, select = -c(c.id, i.kay.code, i.sample.number, d.sample.date))      # make data frame without the key columns
df.pm.igm.temp <- df.pm.igm.temp[, sort(names(df.pm.igm.temp))]                              # sort the new data frame alphabetically by column names
df.pm.igm <- cbind(df.pm.igm[, c("c.id", "i.kay.code", "i.sample.number", "d.sample.date")], df.pm.igm.temp)  # combine the key columns and the sorted columns
rm(df.pm.igm.mean)
rm(df.pm.igm.std.error)
rm(df.pm.igm.temp)
df.pm.igm <- with(df.pm.igm, f.sort.frame(df.pm.igm, c.id, i.sample.number))
print(paste("length df.pm.igm = ", length(df.pm.igm$c.id)))

#### make flat data frames with one entry for each sample. Need to iteratively combine 4 data frames for bacteria ELISA data
df.bacteria.ig.flat.ec <- merge(x = df.ec.igg, y = df.ec.igm,
  by = c("c.id", "i.kay.code", "d.sample.date", "i.sample.number"),
#  by.y = c("c.id", "i.kay.code", "c.sex", "d.sample.date", "i.sample.number", "c.age"),
  all = TRUE,
#  all.y = T,
  suffixes = c(".ec.igg", ".ec.igm"))
print(paste("length df.bacteria.ig.flat.ec = ", length(df.bacteria.ig.flat.ec$c.id)))

df.bacteria.ig.flat.pm <- merge(x = df.pm.igg, y = df.pm.igm,
  by = c("c.id", "i.kay.code", "d.sample.date", "i.sample.number"),
#  by.y = c("c.id", "i.kay.code", "c.sex", "d.sample.date", "i.sample.number", "c.age"),
  all = TRUE,
#  all.y = T,
  suffixes = c(".pm.igg", ".pm.igm"))
print(paste("length df.bacteria.ig.flat.pm = ", length(df.bacteria.ig.flat.pm$c.id)))

df.bacteria.ig.flat <- merge(x = df.bacteria.ig.flat.ec, y = df.bacteria.ig.flat.pm,
  by = c("c.id", "i.kay.code", "d.sample.date", "i.sample.number"),
#  by.y = c("c.id", "i.kay.code", "c.sex", "d.sample.date", "i.sample.number", "c.age"),
  all = TRUE,
#  all.y = T,
  suffixes = c("", ""))
print(paste("length df.bacteria.ig.flat = ", length(df.bacteria.ig.flat$c.id)))








