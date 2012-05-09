# source("s.import.darting.r")

# import the entire darting spreadsheet
df.darting.raw <- read.table(file = "tblDarting.csv", # header = TRUE, sep=",") #,
  sep = ",", header = TRUE, blank.lines.skip = TRUE, strip.white = TRUE, fill = FALSE)   # na.strings = "NA",
df.darting.raw$d.sample.date <- as.Date(as.character(df.darting.raw$d.sample.date), "%d-%b-%Y")
df.darting.subset <- df.darting.raw[df.darting.raw$c.id %in% df.master$c.id,]  # Get all the records in the darting table that match the kay codes in the master list
df.darting <- df.darting.subset[df.darting.subset$d.sample.date %in% df.master$d.sample.date,] # Get all of the records in the darting subset that match the dates in the master list
df.darting <- with(df.darting, f.sort.frame(df.darting, c.id, i.kay.code, d.sample.date)) # Sort the data frame by date, then kay code, then c.id
df.darting <- df.darting[,c("c.id", "i.kay.code", "d.sample.date", "c.knotts.microfilaria", "c.scars.wounds", "c.dart.sheet.comments")]
print(paste("length df.darting = ", length(df.darting$c.id)))
rm(df.darting.subset)

df.master.darting <- merge(df.master, df.darting,                          # merge the two data frames. Keep all rows
  by = c("c.id", "i.kay.code", "d.sample.date"), # use these columns as keys for merging
  all = TRUE)                                                               # keep all the records, we can go back after this and remove the NA rows (we need to do it after merging, otherwise the blank and ampicillin and B3T rows get left out.
df.master.darting <- subset(df.master.darting, !(c.id == "mls" & i.sample.number == 1))
df.master.darting <- with(df.master.darting, f.sort.frame(df.master.darting, c.id, i.sample.number))
print(paste("length df.master.darting = ", length(df.master.darting$c.id)))

df.darting <- df.master.darting
print(paste("length df.darting = ", length(df.darting$c.id)))
df.darting.flat <- df.master.darting
print(paste("length df.darting.flat = ", length(df.darting.flat$c.id)))







