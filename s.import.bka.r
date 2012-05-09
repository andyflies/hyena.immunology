# source("s.import.bka.r")

df.bka.raw <- read.table("hyena.bka.data.set.csv", sep=",", header=TRUE, blank.lines.skip = TRUE)
df.bka.raw <- df.bka.raw[grep("", df.bka.raw$i.well.number),]  # This grep command removes the blank lines

# make a new variable called d.percent.inhibition by substracting d.percent.control.abs from 1. 
#  This actually leads to some negative values because some samples and dilutions increase growth, rather than inhibit
#  Also this variable is currently setup to allow greater than 100% inhibition.
#  df.function.data$d.percent.control.abs[df.function.data$d.percent.control.abs < 0] <- 0   # use this to eliminate inhibition greater than 100%
#  df.function.data$d.percent.control.abs[df.function.data$d.percent.control.abs > 1] <- 1   # use this to eliminate inhibition less than 0%
df.bka.raw$d.percent.inhibition <- ((1 - df.bka.raw$d.percent.control.abs) * 100)

# Removing MLS subadult from the data set because there is no inhibition from this sample
df.bka.raw <- subset(df.bka.raw, !(c.id == "mls" & i.sample.number == 1))
# df.bka.raw <- subset(df.bka.raw, !(c.id == "juba" & i.sample.number == 1))
# df.bka.raw <- subset(df.bka.raw, !(c.id == "jj" & i.sample.number == 1))
# df.bka.raw <- subset(df.bka.raw, !(c.id == "ger" & i.sample.number == 1))

# Removing measurements from e. coli plate 3 because I missed a row on the serum dilutions. There should still be duplicate measurements for each individual
# df.bka.raw <- subset(df.bka.raw, !(c.id == "ccp" & i.plate.id == 3 & c.strain == "ec"))
# df.bka.raw <- subset(df.bka.raw, !(c.id == "chac" & i.plate.id == 3 & c.strain == "ec"))
# df.bka.raw <- subset(df.bka.raw, !(c.id == "chip" & i.plate.id == 3 & c.strain == "ec"))
# df.bka.raw <- subset(df.bka.raw, !(c.id == "chob" & i.plate.id == 3 & c.strain == "ec"))
# df.bka.raw <- subset(df.bka.raw, !(c.id == "com" & i.plate.id == 3 & c.strain == "ec"))
# df.bka.raw <- subset(df.bka.raw, !(c.id == "coo" & i.plate.id == 3 & c.strain == "ec"))
# df.bka.raw <- subset(df.bka.raw, !(c.id == "cr" & i.plate.id == 3 & c.strain == "ec"))

df.bka.raw$d.sample.date <- as.Date(as.character(df.bka.raw$d.sample.date), "%d-%b-%Y")
print("df.bka.raw$d.sample.date[1:100]")
print(df.bka.raw$d.sample.date[1:100])
df.bka.raw$d.current.date <- as.Date(as.character(df.bka.raw$d.current.date), "%d-%b-%Y")
print("df.bka.raw$d.current.date[1:100]")
print(df.bka.raw$d.current.date[1:100])
df.bka.raw$d.start.date <- as.Date(as.character(df.bka.raw$d.start.date), "%d-%b-%Y")


df.bka.raw.single.dilution <- df.bka.raw[(df.bka.raw$i.sample.dilution == 5 | df.bka.raw$d.antibiotic.concentration == 0.5),]     # The data for dilutions 5:640 are all the same, so we only need to keep the first dilution (i.e. d.mic.90.level for dilution 5 = dilution 10 = dilution 20...
df.bka.raw.single.dilution <- df.bka.raw.single.dilution[!is.na(df.bka.raw.single.dilution$c.id),]  # This grep command removes the NAs
print(paste("length df.bka.raw.single.dilution = ", length(df.bka.raw.single.dilution$c.id)))


df.master.bka <- merge(df.master, df.bka.raw.single.dilution,                           # merge the two data frames. Keep all rows in df.master
#  by = c("c.id", "i.kay.code", "i.sample.number", "d.sample.date"), # use these columns as keys for merging
  all.x = TRUE,
  suffixes = c("",".bka"), all = TRUE)                                  # rename the non-key columns when there are conflicts
print(paste("length df.master.bka = ", length(df.master.bka$c.id)))
print("df.master.bka$d.sample.date")
print(df.master.bka$d.sample.date)


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
#  + c.sex
#  + c.age
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

########################### aggregate and get the medians #################
df.bka.ec.median <- aggregate(cbind(i.sample.dilution,
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
  data = df.master.bka[df.master.bka$c.strain == "ec",], median, na.action = na.pass
)
  df.bka.ec.median <- df.bka.ec.median[, sort(names(df.bka.ec.median))]
  print("df.bka.ec.median")
  print(df.bka.ec.median)

df.bka.ec <- merge(df.bka.ec.mean, df.bka.ec.std.error,                       # merge the two data frames. Keep all rows in df.master
  by = c("c.id", "i.kay.code", "d.sample.date", "i.sample.number"),             # use these columns as keys for merging
  all = TRUE,
  suffixes = c("",".std.error"))                                  # rename the non-key columns when there are conflicts

df.bka.ec.temp <- subset(df.bka.ec, select = -c(c.id, i.kay.code, d.sample.date, i.sample.number))      # make data frame without the key columns
# df.bka.ec.temp <- df.bka.ec.temp[, sort(names(df.bka.ec.temp))]                              # sort the new data frame alphabetically by column names
df.bka.ec <- cbind(df.bka.ec[, c("c.id", "i.kay.code", "d.sample.date", "i.sample.number")], df.bka.ec.temp)  # combine the key columns and the sorted columns
rm(df.bka.ec.mean)
rm(df.bka.ec.std.error)
rm(df.bka.ec.temp)
df.bka.ec <- with(df.bka.ec, f.sort.frame(df.bka.ec, c.id, d.sample.date))
print(paste("length df.bka.ec = ", length(df.bka.ec$c.id)))

df.bka.ec <- merge(df.bka.ec, df.bka.ec.median,                       # merge the two data frames. Keep all rows in df.master
  by = c("c.id", "i.kay.code", "d.sample.date", "i.sample.number"),             # use these columns as keys for merging
  all = TRUE,
  suffixes = c("",".median"))                                  # rename the non-key columns when there are conflicts

df.bka.ec.temp <- subset(df.bka.ec, select = -c(c.id, i.kay.code, d.sample.date, i.sample.number))      # make data frame without the key columns
# df.bka.ec.temp <- df.bka.ec.temp[, sort(names(df.bka.ec.temp))]                              # sort the new data frame alphabetically by column names
df.bka.ec <- cbind(df.bka.ec[, c("c.id", "i.kay.code", "d.sample.date", "i.sample.number")], df.bka.ec.temp)  # combine the key columns and the sorted columns
rm(df.bka.ec.median)
rm(df.bka.ec.temp)
df.bka.ec <- with(df.bka.ec, f.sort.frame(df.bka.ec, c.id, d.sample.date))
print(paste("length df.bka.ec = ", length(df.bka.ec$c.id)))

df.bka.raw.ec.temp <- subset(df.bka.raw, c.strain == "ec")
d.sintegral.mic.ec <- f.run.calc.inhibition.area.under.curve(df.bka.ec, df.bka.raw.ec.temp)
df.bka.ec <- cbind(df.bka.ec, d.sintegral.mic.ec)
rm(df.bka.raw.ec.temp)
rm(d.sintegral.mic.ec)


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
#  + c.sex
#  + c.age
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

########################### aggregate and get the medians #################
df.bka.pm.median <- aggregate(cbind(i.sample.dilution,
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
  data = df.master.bka[df.master.bka$c.strain == "pm",], median, na.action = na.pass
)

df.bka.pm <- merge(df.bka.pm.mean, df.bka.pm.std.error,                       # merge the two data frames. Keep all rows in df.master
  by = c("c.id", "i.kay.code", "d.sample.date", "i.sample.number"),             # use these columns as keys for merging
  all = TRUE,
  suffixes = c("",".std.error"))                                  # rename the non-key columns when there are conflicts

df.bka.pm.temp <- subset(df.bka.pm, select = -c(c.id, i.kay.code, d.sample.date, i.sample.number))      # make data frame without the key columns
# df.bka.pm.temp <- df.bka.pm.temp[, sort(names(df.bka.pm.temp))]                              # sort the new data frame alphabetically by column names
df.bka.pm <- cbind(df.bka.pm[, c("c.id", "i.kay.code", "d.sample.date", "i.sample.number")], df.bka.pm.temp)  # combine the key columns and the sorted columns
rm(df.bka.pm.mean)
rm(df.bka.pm.std.error)
rm(df.bka.pm.temp)
df.bka.pm <- with(df.bka.pm, f.sort.frame(df.bka.pm, c.id, d.sample.date))
print(paste("length df.bka.pm = ", length(df.bka.pm$c.id)))

df.bka.pm <- merge(df.bka.pm, df.bka.pm.median,                       # merge the two data frames. Keep all rows in df.master
  by = c("c.id", "i.kay.code", "d.sample.date", "i.sample.number"),             # use these columns as keys for merging
  all = TRUE,
  suffixes = c("",".median"))                                  # rename the non-key columns when there are conflicts

df.bka.pm.temp <- subset(df.bka.pm, select = -c(c.id, i.kay.code, d.sample.date, i.sample.number))      # make data frame without the key columns
# df.bka.pm.temp <- df.bka.pm.temp[, sort(names(df.bka.pm.temp))]                              # sort the new data frame alphabetically by column names
df.bka.pm <- cbind(df.bka.pm[, c("c.id", "i.kay.code", "d.sample.date", "i.sample.number")], df.bka.pm.temp)  # combine the key columns and the sorted columns
rm(df.bka.pm.median)
rm(df.bka.pm.temp)
df.bka.pm <- with(df.bka.pm, f.sort.frame(df.bka.pm, c.id, d.sample.date))
print(paste("length df.bka.pm = ", length(df.bka.pm$c.id)))


df.bka.raw.pm.temp <- subset(df.bka.raw, c.strain == "pm")
d.sintegral.mic.pm <- f.run.calc.inhibition.area.under.curve(df.bka.pm, df.bka.raw.pm.temp)
df.bka.pm <- cbind(df.bka.pm, d.sintegral.mic.pm)
rm(df.bka.raw.pm.temp)
rm(d.sintegral.mic.pm)

#### make flat data frames with one entry for each sample
df.bka.flat <- merge(x = df.bka.ec, y = df.bka.pm,
  by = c("c.id", "i.kay.code", "d.sample.date", "i.sample.number"),
#  by.y = c("c.id", "i.kay.code", "d.sample.date", "i.sample.number"),
  all = TRUE,
#  all.y = T,
  suffixes = c(".bka.ec", ".bka.pm"))
print(paste("length df.bka.flat = ", length(df.bka.flat$c.id)))


df.percent.inhibition <- f.calculate.percent.inhibition()
#### make flat data frames with one entry for each sample
df.bka.flat <- merge(x = df.bka.flat, df.percent.inhibition,
  by = c("c.id", "i.kay.code", "d.sample.date", "i.sample.number"),
#  #  by.y = c("c.id", "i.kay.code", "d.sample.date", "i.sample.number"),
  all = TRUE,
#  #  all.y = T,
  suffixes = c("", ""))
print(paste("length df.bka.flat = ", length(df.bka.flat$c.id)))


df.bka.full.dilution <- merge(df.master, df.bka.raw,                           # merge the two data frames. Keep all rows in df.master
  by = c("c.id", "i.kay.code", "i.sample.number", "d.sample.date"), # use these columns as keys for merging
  all.x = TRUE,
  suffixes = c("",".bka")) #, all = TRUE)                                  # rename the non-key columns when there are conflicts
# df.bka.full.dilution$d.sample.date <- as.Date(as.character(df.bka.full.dilution$d.sample.date), "%d-%b-%Y")
df.bka.full.dilution <- subset(df.bka.full.dilution, !(c.id == "ampicillin" | c.id == "bacteria"))
df.bka.full.dilution <- with(df.bka.full.dilution, f.sort.frame(df.bka.full.dilution, c.strain, c.id, d.sample.date, i.sample.number, i.sample.dilution))
print("df.bka.full.dilution$d.sample.date[1:100]")
print(df.bka.full.dilution$d.sample.date[1:100])

  
print(paste("length df.bka.full.dilution = ", length(df.bka.full.dilution$c.id)))
# write.xls(df.bka.full.dilution, file = "df.bka.full.dilution.xls",
#          colNames = TRUE,
#          sheet = 1,
#          from = 1,
#          rowNames = NA,
#          naStrings = NA)
 
write.csv(x = df.bka.full.dilution, file = "df.bka.full.dilution.csv")          

rm(df.bka.raw.single.dilution)


