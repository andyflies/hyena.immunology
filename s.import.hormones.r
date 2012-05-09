# source("s.import.hormones.r")

# import the hormone spreadsheet
df.hormones.raw <- read.table(file = "tblHormones2005.csv", # header = TRUE, sep=",") #,
  sep = ",", header = TRUE, blank.lines.skip = TRUE, strip.white = TRUE, fill = FALSE)   # na.strings = "NA",
df.hormones.raw <- subset(df.hormones.raw, select = -d.igf) # remove d.igf column because d.igf06 is more complete and is highly correlated (0.87) with d.igf
df.hormones.raw$d.sample.date <- as.Date(as.character(df.hormones.raw$d.sample.date), "%d-%b-%Y")
df.hormones.subset <- df.hormones.raw[df.hormones.raw$c.id %in% df.master$c.id,]  # Get all the records in the hormone table that match the kay codes in the master hyena list
df.hormones <- df.hormones.subset[df.hormones.subset$d.sample.date %in% df.master$d.sample.date,] # Get all of the records in the hormone subset that match the dates in the serology data frame
df.hormones <- with(df.hormones, f.sort.frame(df.hormones, c.id, i.kay.code, d.sample.date)) # Sort the data frame
df.hormones <- df.hormones[,c("c.id", "d.sample.date", "i.kay.code", "CodeNumberOnSamples", "d.tucb", "d.a4ucb", "d.t", "d.p",
  "d.c", "d.e", "d.a", "d.lh", "STRESSCA", "NIPS", "d.igf06", "TESTES", "Notes", "WT", "PM3", "MOTOBABE",
  "i.rank.hormone", "TENURE", "d.age.months.hormone", "c.sex", "MO", "YR", "c.reproductive.status.hormone", "REPCAT", "SOCCAT")]
print(paste("length df.hormones = ", length(df.hormones$c.id)))
df.hormones$d.cortisol <- df.hormones$d.c
df.hormones$d.cortisol[df.hormones$d.cortisol < 0.01] <- NA
df.hormones$d.testosterone <- df.hormones$d.t
df.hormones$d.testosterone[df.hormones$d.testosterone < 0.01] <- NA



rm(df.hormones.raw)
rm(df.hormones.subset)

df.master.hormones <- merge(df.master, df.hormones,                          # merge the two data frames. Keep all rows
#  by = c("c.id", "i.kay.code", "d.sample.date"), # use these columns as keys for merging
  all = TRUE)
df.master.hormones <- subset(df.master.hormones, !(c.id == "mls" & i.sample.number == 1))                                                           # keep all the records, we can go back after this and remove the NA rows (we need to do it after merging, otherwise the blank and ampicillin and B3T rows get left out.
df.master.hormones <- with(df.master.hormones, f.sort.frame(df.master.hormones, c.id, i.sample.number))
print(paste("length df.master.hormones = ", length(df.master.hormones$c.id)))

df.hormones <- df.master.hormones
print(paste("length df.hormones = ", length(df.hormones$c.id)))
df.hormones.flat <- df.master.hormones
print(paste("length df.hormones.flat = ", length(df.hormones.flat$c.id)))

