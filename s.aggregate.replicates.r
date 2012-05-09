# source("s.aggregate.replicates.r")

################# BKA data for E. coli ######################################################################################################
########################### aggregate and get the means #################
df.bka.ec.mean <- aggregate(cbind(i.sample.dilution,
  d.antibiotic.concentration,
  d.raw.abs,
  d.blank.abs,
  d.percent.control.abs,
  c.50.percent.inhibit,
  c.90.percent.inhibit,
  d.mic.50,
  d.mic.90,
  d.mic.50.level,
  d.mic.90.level
  )
  ~ c.id
  + i.kay.code
  + d.sample.date
  + i.sample.number
  + c.sex
  + c.age
  + d.current.date
  + d.current.time
  + d.start.date
  + d.start.time
  + i.time.point
  + d.elapsed.time
  + c.strain
  + c.reference.number
  ,
  data = df.master.bka[df.master.bka$c.strain == "ec",], mean, na.action = na.pass
)
  df.bka.ec.mean <- df.bka.ec.mean[, sort(names(df.bka.ec.mean))]

########################### aggregate and get the standard error #################
df.bka.ec.std.error <- aggregate(cbind(d.raw.abs,
  d.blank.abs,
  d.mic.50,
  d.mic.90,
  d.mic.50.level,
  d.mic.90.level
  )
  ~ c.id
  + i.kay.code
  + d.sample.date
  + i.sample.number
  ,
  data = df.master.bka[df.master.bka$c.strain == "ec",], std.error, na.action = na.pass
)

df.bka.ec <- merge(df.bka.ec.mean, df.bka.ec.std.error,                       # merge the two data frames. Keep all rows in df.master
  by = c("c.id", "i.sample.number", "d.sample.date"),                         # use these columns as keys for merging
  suffixes = c("",".std.error"), all = TRUE)                                  # rename the non-key columns when there are conflicts

df.bka.ec.temp <- subset(df.bka.ec, select = -c(c.id, i.kay.code, i.sample.number, d.sample.date))      # make data frame without the key columns
df.bka.ec.temp <- df.bka.ec.temp[,sort(names(df.bka.ec.temp))]                              # sort the new data frame alphabetically by column names
df.bka.ec <- cbind(df.bka.ec[, c("c.id", "i.kay.code", "i.sample.number", "d.sample.date")], df.bka.ec.temp)  # combine the key columns and the sorted columns
rm(df.bka.ec.mean)
rm(df.bka.ec.std.error)
rm(df.bka.ec.temp)

################# BKA data for P. mirabilis ######################################################################################################
########################### aggregate and get the means #################
df.bka.pm.mean <- aggregate(cbind(i.sample.dilution,
  d.antibiotic.concentration,
  d.raw.abs,
  d.blank.abs,
  d.percent.control.abs,
  c.50.percent.inhibit,
  c.90.percent.inhibit,
  d.mic.50,
  d.mic.90,
  d.mic.50.level,
  d.mic.90.level
  )
  ~ c.id
  + i.kay.code
  + d.sample.date
  + i.sample.number
  + c.sex
  + c.age
  + d.current.date
  + d.current.time
  + d.start.date
  + d.start.time
  + i.time.point
  + d.elapsed.time
  + c.strain
  + c.reference.number
  ,
  data = df.master.bka[df.master.bka$c.strain == "pm",], mean, na.action = na.pass
)

########################### aggregate and get the standard error #################
df.bka.pm.std.error <- aggregate(cbind(d.raw.abs,
  d.blank.abs,
  d.mic.50,
  d.mic.90,
  d.mic.50.level,
  d.mic.90.level
  )
  ~ c.id
  + i.kay.code
  + d.sample.date
  + i.sample.number
  ,
  data = df.master.bka[df.master.bka$c.strain == "pm",], std.error, na.action = na.pass
)

df.bka.pm <- merge(df.bka.pm.mean, df.bka.pm.std.error,                       # merge the two data frames. Keep all rows in df.master
  by = c("c.id", "i.sample.number", "d.sample.date"),                         # use these columns as keys for merging
  suffixes = c("",".std.error"), all = TRUE)                                  # rename the non-key columns when there are conflicts

df.bka.pm.temp <- subset(df.bka.pm, select = -c(c.id, i.kay.code, i.sample.number, d.sample.date))      # make data frame without the key columns
df.bka.pm.temp <- df.bka.pm.temp[,sort(names(df.bka.pm.temp))]                              # sort the new data frame alphabetically by column names
df.bka.pm <- cbind(df.bka.pm[, c("c.id", "i.kay.code", "i.sample.number", "d.sample.date")], df.bka.pm.temp)  # combine the key columns and the sorted columns
rm(df.bka.pm.mean)
rm(df.bka.pm.std.error)
rm(df.bka.pm.temp)

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
  + c.sex
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
  + c.sex
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


################# Bacteria ELISA for E. coli IgG ######################################################################################################
########################### aggregate and get the means #################
df.ec.igg.mean <- aggregate(cbind(d.raw.abs,
  d.blank.abs
  )
  ~ c.id
  + i.kay.code
  + d.sample.date
  + i.sample.number
  + c.sex
  + c.isotype.target
  + c.secondary.antibody
  + i.secondary.dilution
  + i.sample.dilution
  ,
  data = df.bacteria.elisa[df.bacteria.elisa$c.isotype.target == "IgG" & df.bacteria.elisa$c.target == "ec.8739", ], mean, na.action = na.pass
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
  data = df.bacteria.elisa[df.bacteria.elisa$c.isotype.target == "IgG" & df.bacteria.elisa$c.target == "ec.8739", ], std.error, na.action = na.pass
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


################# Bacteria ELISA for E. coli IgM ######################################################################################################
########################### aggregate and get the means #################
df.ec.igm.mean <- aggregate(cbind(d.raw.abs,
  d.blank.abs
  )
  ~ c.id
  + i.kay.code
  + d.sample.date
  + i.sample.number
  + c.sex
  + c.isotype.target
  + c.secondary.antibody
  + i.secondary.dilution
  + i.sample.dilution
  ,
  data = df.bacteria.elisa[df.bacteria.elisa$c.isotype.target == "IgM" & df.bacteria.elisa$c.target == "ec.8739", ], mean, na.action = na.pass
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
  data = df.bacteria.elisa[df.bacteria.elisa$c.isotype.target == "IgM" & df.bacteria.elisa$c.target == "ec.8739", ], std.error, na.action = na.pass
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


################# Bacteria ELISA for P. mirabilis IgG ######################################################################################################
########################### aggregate and get the means #################
df.pm.igg.mean <- aggregate(cbind(d.raw.abs,
  d.blank.abs
  )
  ~ c.id
  + i.kay.code
  + d.sample.date
  + i.sample.number
  + c.sex
  + c.isotype.target
  + c.secondary.antibody
  + i.secondary.dilution
  + i.sample.dilution
  ,
  data = df.bacteria.elisa[df.bacteria.elisa$c.isotype.target == "IgG" & df.bacteria.elisa$c.target == "pm.35659", ], mean, na.action = na.pass
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
  data = df.bacteria.elisa[df.bacteria.elisa$c.isotype.target == "IgG" & df.bacteria.elisa$c.target == "pm.35659", ], std.error, na.action = na.pass
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


################# Bacteria ELISA for P. mirabilis IgM ######################################################################################################
########################### aggregate and get the means #################
df.pm.igm.mean <- aggregate(cbind(d.raw.abs,
  d.blank.abs
  )
  ~ c.id
  + i.kay.code
  + d.sample.date
  + i.sample.number
  + c.sex
  + c.isotype.target
  + c.secondary.antibody
  + i.secondary.dilution
  + i.sample.dilution
  ,
  data = df.bacteria.elisa[df.bacteria.elisa$c.isotype.target == "IgM" & df.bacteria.elisa$c.target == "pm.35659", ], mean, na.action = na.pass
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
  data = df.bacteria.elisa[df.bacteria.elisa$c.isotype.target == "IgM" & df.bacteria.elisa$c.target == "pm.35659", ], std.error, na.action = na.pass
)

df.pm.igm <- merge(df.pm.igm.mean, df.pm.igm.std.error,                       # merge the two data frames. Keep all rows in df.master
  by = c("c.id", "i.kay.code", "i.sample.number", "d.sample.date"),                         # use these columns as keys for merging
  suffixes = c("",".std.error"), all = TRUE)                                  # rename the non-key columns when there are conflicts

df.pm.igm.temp <- subset(df.pm.igm, select = -c(c.id, i.kay.code, i.sample.number, d.sample.date))      # make data frame without the key columns
df.pm.igm.temp <- df.pm.igm.temp[,sort(names(df.pm.igm.temp))]                              # sort the new data frame alphabetically by column names
df.pm.igm <- cbind(df.pm.igm[, c("c.id", "i.kay.code", "i.sample.number", "d.sample.date")], df.pm.igm.temp)  # combine the key columns and the sorted columns
rm(df.pm.igm.mean)
rm(df.pm.igm.std.error)
rm(df.pm.igm.temp)
