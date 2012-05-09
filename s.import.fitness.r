# source("s.import.fitness.r")

# import the fitness spreadsheet from Eli
df.fitness.raw <- read.table(file = "fitness.csv", # header = TRUE, sep=",") #,
  sep = ",", header = TRUE, blank.lines.skip = TRUE, strip.white = TRUE, fill = FALSE)   # na.strings = "NA",
df.fitness.subset <- df.fitness.raw[df.fitness.raw$c.id %in% df.master$c.id,]  # Get all the records in the hormone table that match the kay codes in the master hyena list
# df.fitness <- df.fitness.kay.code.subset[df.fitness.kay.code.subset$d.sample.date %in% df.master$d.sample.date,] # Get all of the records in the hormone subset that match the dates in the serology data frame
df.fitness.subset <- with(df.fitness.subset, f.sort.frame(df.fitness.subset, c.id)) # Sort the data frame
# df.fitness <- df.fitness[,c("c.id", "d.sample.date", "i.kay.code", "CodeNumberOnSamples", "d.tucb", "d.a4ucb", "d.t", "d.p",
#  "d.c", "d.e", "d.a", "d.lh", "STRESSCA", "NIPS", "d.igf", "d.igf06", "TESTES", "Notes", "WT", "PM3", "MOTOBABE",
#  "i.rank.hormone", "TENURE", "d.age.months.hormone", "c.sex", "MO", "YR", "c.reproductive.status.hormone", "REPCAT", "SOCCAT")]
print(paste("length df.fitness.subset = ", length(df.fitness.subset$c.id)))
# rm(df.fitness.raw)
# rm(df.fitness.subset)

df.master.fitness <- merge(df.master, df.fitness.subset,                          # merge the two data frames. Keep all rows
#  by = c("c.id", "i.kay.code", "d.sample.date"), # use these columns as keys for merging
  all = TRUE)
df.master.fitness <- subset(df.master.fitness, !(c.id == "mls" & i.sample.number == 1))                                                           # keep all the records, we can go back after this and remove the NA rows (we need to do it after merging, otherwise the blank and ampicillin and B3T rows get left out.
df.master.fitness <- with(df.master.fitness, f.sort.frame(df.master.fitness, c.id, i.sample.number))
print(paste("length df.master.fitness = ", length(df.master.fitness$c.id)))

df.fitness <- df.master.fitness
print(paste("length df.fitness = ", length(df.fitness$c.id)))
df.fitness.flat <- df.master.fitness
print(paste("length df.fitness.flat = ", length(df.fitness.flat$c.id)))

