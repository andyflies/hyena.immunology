# source("s.query.darting.table.r")    

print("")
print("Need to rename the columns if importing directly from access/excel because some column names have '/'")
print("Need to replace all apostrophes and pound signs in the xlsx/csv file with semicolons or the read.table function will fail due to commas in data cells")
print("  Use replace function in excel with 's or 't ' or #")
print("  Format the dates in excel and don't both with it in R")
print("")

# import the entire darting spreadsheet
df.darting <- read.table(file = "C:\\Users\\AF\\Documents\\My Dropbox\\msu.stuff\\access.fisi\\tblDarting.csv", # header = TRUE, sep=",") #,
  sep = ",", header=TRUE, blank.lines.skip = TRUE, strip.white = TRUE, fill = FALSE)   # na.strings = "NA",      
# print(paste("length of d.sample.date: ", length(df.darting$d.sample.date)))  

df.darting.kay.code.subset <- df.darting[df.darting$c.id %in% df.serology$c.id,]  # Get all the records in the darting table that match the kay codes in the serology table

df.darting.subset <- df.darting.kay.code.subset[df.darting.kay.code.subset$d.sample.date %in% df.serology$d.sample.date,] # Get all of the records in the darting subset that match the dates in the serology data frame
df.darting.subset <- with(df.darting.subset, f.sort.frame(df.darting.subset, c.id, i.kay.code, d.sample.date)) # Sort the data frame by date, then kay code, then c.id
df.darting.subset <- df.darting.subset[,c("c.id", "i.kay.code", "d.sample.date", "c.sex", "c.knotts.microfilaria", "c.scars.wounds", "c.dart.sheet.comments")]
# print(df.darting.subset[,1:4]) 

# print(length(df.darting.subset[,1]))

# c.knotts.microfilaria