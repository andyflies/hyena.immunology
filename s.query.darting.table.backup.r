# source("s.query.darting.table.r")

print("")
print("Need to rename the columns if importing directly from access/excel because some column names have '/'")
print("Need to replace all commas in the xlsx/csv file with semicolons or the read.table function will fail due to commas in data cells")
print("")

# import the entire darting spreadsheet
# df.darting <- read.table(file = "C:\\Users\\AF\\Documents\\My Dropbox\\msu.stuff\\access.fisi\\tblDarting.csv",
#   sep=",", header=TRUE, strip.white = TRUE, fill = TRUE, na.strings = "NA")  # ,  blank.lines.skip = TRUE, 
# print(paste("length of DartingDate: ", length(df.darting$DartingDate)))  
# df.darting <- df.darting[na.omit(df.darting$DartingDate),]
# print(summary(df.darting))  
# print(paste("length of DartingDate: ", length(df.darting$DartingDate)))

# import the entire darting spreadsheet
df.darting <- read.table(file = "C:\\Users\\AF\\Documents\\My Dropbox\\msu.stuff\\access.fisi\\tblDarting.csv", # header = TRUE, sep=",") #,
  sep = ",", header=TRUE, blank.lines.skip = TRUE, strip.white = TRUE, fill = FALSE)   # na.strings = "NA",      
print(paste("length of DartingDate: ", length(df.darting$DartingDate)))  
df.darting <- df.darting[na.omit(df.darting$DartingDate),]
# print(summary(df.darting))  
print(paste("length of DartingDate: ", length(df.darting$DartingDate)))

# c.v.id <- as.character(na.omit(df.serology$c.id))
# l.index.matches <- df.data$c.id %in% c.v.id
# print("summary(df.data[l.index.matches,1:8])   from c.v.id <- as.character(na.omit(df.serology$c.id))")
# print(summary(df.data[l.index.matches,1:8]))

# l.index.matches <- df.darting$ID %in% c.v.id
# print("summary(df.darting[l.index.matches,1:8])   from df.darting$ID %in% c.v.id")
# print(summary(df.darting[l.index.matches,1:8]))

# l.index.matches <- df.darting$Hyena %in% c.v.id
# print(l.index.matches)
# print("summary(df.darting[l.index.matches,1:8])    from df.darting$Hyena %in% c.v.id")
# print(summary(df.darting[l.index.matches,1:8]))

c.v.kay.code <- as.character(na.omit(df.serology$i.kay.code))
l.index.matches <- df.darting$KayCode %in% c.v.kay.code
# print(l.index.matches)
print("summary(df.darting[l.index.matches,1:3])    from df.darting$KayCode %in% c.v.kay.code")
print(summary(df.darting[l.index.matches,1:3]))

# print("c.v.sample.date.serology.as.date     from c.v.sample.date.serology.as.date <- as.Date(na.omit(df.serology$d.sample.date), %d-%b-%Y)")
# c.v.sample.date.serology.as.date <- as.Date(na.omit(df.serology$d.sample.date), "%d-%b-%Y")
# print(c.v.sample.date.serology.as.date)
# print(typeof(c.v.sample.date.serology.as.date))

# print("c.v.darting.date.as.date     from c.v.darting.date.as.date <- as.Date(na.omit(df.darting$DartingDate), %d-%b-%y)")
# c.v.darting.date.as.date <- as.Date(na.omit(df.darting$DartingDate), "%d-%b-%Y")
# print(c.v.darting.date.as.date)
# print(typeof(c.v.darting.date.as.date))

l.index.matches <- c.v.darting.date.as.date %in% c.v.sample.date.serology.as.date
print("l.index.matches <- c.v.darting.date.as.date %in% c.v.sample.date.serology.as.date")
# print(l.index.matches)
print(df.darting[l.index.matches, c("Hyena","ID","KayCode","DartingDate","FoundDead","Sex", "Age", "i.knotts.microfilaria")])
#print(df.darting[l.index.matches, "i.knotts.microfilaria"])
print(length(df.darting[l.index.matches,1]))


# knotts.microfilaria