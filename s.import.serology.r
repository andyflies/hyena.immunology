# source("s.import.serology.r")

df.serology.raw <- read.table(file = "ckg.serology.csv", sep = ",", header = TRUE, blank.lines.skip = TRUE)
df.serology.raw <- subset(df.serology.raw, select = -c.sex) # remove c.sex column from this input data frame because we already have the data it will create a duplicate variable when merging
df.serology.raw <- subset(df.serology.raw, !(c.id == "mls" & i.sample.number == 1))
print(paste("length df.serology.raw = ", length(df.serology.raw$c.id)))
# Removing measurement that had wrong serum dilution due to running out of serum
df.serology.raw <- subset(df.serology.raw, !(c.id == "ali" & i.sample.number == 1))
df.serology.raw <- subset(df.serology.raw, !(c.id == "bail" & i.sample.number == 2))
df.serology.raw <- subset(df.serology.raw, !(c.id == "sim" & i.sample.number == 1))
df.serology.raw <- subset(df.serology.raw, !(c.id == "stl" & i.sample.number == 1))
df.serology.raw$d.sample.date <- as.Date(as.character(df.serology.raw$d.sample.date), "%d-%b-%Y")
print(paste("length df.serology.raw after remomving incomplete samples = ", length(df.serology.raw$c.id)))

df.master.serology <- merge(df.master, df.serology.raw,                          # merge the two data frames. Keep all rows
  by = c("c.id", "i.kay.code", "i.sample.number", "d.sample.date"), # use these columns as keys for merging
  all = TRUE)                                                               # keep all the records, we can go back after this and remove the NA rows (we need to do it after merging, otherwise the blank and ampicillin and B3T rows get left out.

df.master.serology <- with(df.master.serology, f.sort.frame(df.master.serology, c.id, i.sample.number))
df.master.serology <- subset(df.master.serology, !(c.id == "mls" & i.sample.number == 1))

# List of pathogen names used for creating the pathogen exposure index
v.serology.names.igg <- c("i.cmi.bsa.igg", "i.cmi.cav2.igg", "i.cmi.ccv.igg", "i.cmi.cdv.igg", "i.cmi.dirofilaria.igg", "i.cmi.fiv.p24.igg") 	
v.serology.names.igm <- c("i.cmi.bsa.igm","i.cmi.cav2.igm",	"i.cmi.ccv.igm", "i.cmi.cdv.igm",	"i.cmi.dirofilaria.igm", "i.cmi.fiv.p24.igm")

# Call function to get the ranks of the titers for the pathogens and then append to the data frame
df.master.serology <- f.convert.to.ranks(df.master.serology, v.serology.names.igg)  
df.master.serology <- f.convert.to.ranks(df.master.serology, v.serology.names.igm)  
print(paste("length df.master.serology = ", length(df.master.serology$c.id)))

df.master.serology <- f.make.serology.index(df.master.serology, v.serology.names.igg)  
df.master.serology <- f.make.serology.index(df.master.serology, v.serology.names.igm)  

# Paste the suffic .rank onto the variable names. Then make a new df and merge it to the master frame
# v.serology.rank.igg <- paste(v.serology.names.igg, ".rank", sep="") 	
# v.serology.rank.igm <- paste(v.serology.names.igm, ".rank", sep="")

# df.serology.ranks.igg <- f.convert.to.ranks(df.serology.flat[, v.serology.names.igg])  
# df.serology.ranks.igm <- f.convert.to.ranks(df.serology.flat[, v.serology.names.igm])  
# colnames(df.serology.ranks.igg) <- v.serology.rank.igg
# colnames(df.serology.ranks.igm) <- v.serology.rank.igm


df.serology <- df.master.serology
print(paste("length df.serology = ", length(df.serology$c.id)))

df.serology.flat <- df.master.serology
print(paste("length df.serology.flat = ", length(df.serology.flat$c.id)))


i.cmi.cav2.igg <- c(400, 200)
i.cmi.cav2.igg.hl <- c(900, 400)
i.cmi.cav2.igm <- c(400, 200)

i.cmi.ccv.igg <- c(150, 100)
i.cmi.ccv.igg.hl <- c(300, 150)
i.cmi.ccv.igm <- c(400, 200)

i.cmi.cdv.igg <- c(150, 100)
i.cmi.cdv.igg.hl <- c(300, 150)
i.cmi.cdv.igm <- c(400, 200)

i.cmi.cpv.igg <- c(200, 100)
i.cmi.cpv.igg.hl <- c(200, 100)
i.cmi.cpv.igm <- c(400, 200)

i.cmi.dirofilaria.igg <- c(400, 200)
i.cmi.dirofilaria.igg.hl <- c(1000, 700)
i.cmi.dirofilaria.igm <- c(900, 400)

i.cmi.fcov.igg <- c(200, 100)
i.cmi.fcov.igg.hl <- c(250, 150)
i.cmi.fcov.igm <- c(200, 100)

i.cmi.fcv.sah.igg <- c(400, 200)
i.cmi.fcv.sah.igg.hl <- c(200, 100)
i.cmi.fcv.sah.igm <- c(200, 100)

i.cmi.felv.p27.igg <- c(300, 150)
i.cmi.felv.p27.igg.hl <- c(1000, 700)
i.cmi.felv.p27.igm <- c(900, 400)

i.cmi.fiv.gp95.igg <- c(900, 400)
i.cmi.fiv.gp95.igg.hl <- c(900, 400)
i.cmi.fiv.gp95.igm <- c(900, 400)

i.cmi.fiv.p24.igg <- c(900, 400)
i.cmi.fiv.p24.igg.hl <- c(900, 400)
i.cmi.fiv.p24.igm <- c(900, 250)

m.serology.cutoff.values <- rbind(
  i.cmi.cav2.igg, i.cmi.cav2.igg.hl, i.cmi.cav2.igm,
  i.cmi.ccv.igg, i.cmi.ccv.igg.hl, i.cmi.ccv.igm,
  i.cmi.cdv.igg, i.cmi.cdv.igg.hl, i.cmi.cdv.igm,
  i.cmi.cpv.igg, i.cmi.cpv.igg.hl, i.cmi.cpv.igm,
  i.cmi.dirofilaria.igg, i.cmi.dirofilaria.igg.hl, i.cmi.dirofilaria.igm,
  i.cmi.fcov.igg, i.cmi.fcov.igg.hl, i.cmi.fcov.igm,
  i.cmi.fcv.sah.igg, i.cmi.fcv.sah.igg.hl, i.cmi.fcv.sah.igm,
  i.cmi.felv.p27.igg, i.cmi.felv.p27.igg.hl, i.cmi.felv.p27.igm,
  i.cmi.fiv.gp95.igg, i.cmi.fiv.gp95.igg.hl, i.cmi.fiv.gp95.igm,
  i.cmi.fiv.p24.igg, i.cmi.fiv.p24.igg.hl, i.cmi.fiv.p24.igm
  )

colnames(m.serology.cutoff.values) <- c("high", "low")





