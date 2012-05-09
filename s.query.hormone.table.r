# source("s.query.hormone.table.r")

print("")
print("Need to rename the columns if importing directly from access/excel because some column names have '/'")
print("Need to replace all apostrophes and pound signs in the xlsx/csv file with semicolons or the read.table function will fail due to commas in data cells")
print("  Use replace function in excel with 's or 't ' or #")
print("  Format the dates in excel and don't both with it in R")
print("")

# import the entire hormones spreadsheet
df.hormones <- read.table(file = "C:\\Users\\AF\\Documents\\My Dropbox\\msu.stuff\\access.fisi\\tblHormones2005.csv", # header = TRUE, sep=",") #,
  sep = ",", header=TRUE, blank.lines.skip = TRUE, strip.white = TRUE, fill = FALSE)   # na.strings = "NA",      
# print(paste("length of d.sample.date: ", length(df.darting$d.sample.date)))  
print(summary(df.hormones))

df.hormones.id.subset <- df.hormones[df.hormones$c.id %in% df.serology$c.id,]  # Get all the records in the darting table that match the c.id in the serology table
print("print(summary(df.hormones.id.subset))")
print(summary(df.hormones.id.subset))

df.hormones.subset <- df.hormones.id.subset[df.hormones.id.subset$d.sample.date %in% df.serology$d.sample.date,] # Get all of the records in the darting subset that match the dates in the serology data frame
df.hormones.subset <- with(df.hormones.subset, f.sort.frame(df.hormones.subset, c.id, i.kay.code, d.sample.date)) # Sort the data frame by date, then kay code, then c.id
# df.hormones.subset <- df.hormones.subset[,c("c.id")] #, 
#  "i.kay.code", "d.sample.date", "c.sex", "d.age.months.hormone", "i.rank.hormone", 
#  "c.reproductive.status.hormone", "d.age.months.hormone", 
#  "d.tucb",	"d.a4ucb", "d.t", "d.p", "d.c", "d.e", "d.a", "d.lh", "d.igf", "d.igf06")] #, 
#  "TENURE", "MO", "YR", , "REPCAT", "SOCCAT")]

print("print(df.hormones.subset[,1:4])")    
print(df.hormones.subset[,1:10])

# c.knotts.microfilaria