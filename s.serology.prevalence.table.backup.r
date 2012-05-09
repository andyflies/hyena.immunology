# source("s.serology.prevalence.table.r")


df.data.female <- df.data[df.data$c.sex == 'f' & df.data$c.age == "adult",]
df.data.male <- df.data[df.data$c.sex == 'm' & df.data$c.age == "adult",]
# df.temp <- df.data.female[,c("c.id", "i.kay.code", "d.sample.date", "c.sex", "i.sample.number",
# df.temp <- df.data.male[,c("c.id", "i.kay.code", "d.sample.date", "c.sex", "i.sample.number",
df.temp <- df.data[,c("c.id", "i.kay.code", "d.sample.date", "c.sex", "i.sample.number",
  "d.age.months", "d.mass", "i.rank", "i.litter.rank",
  "d.months.to.grad", 
  "i.glucose.green", "i.glucose.red", "d.pcv", 
  "d.total.solids", "d.mic.90.level", 
 
  "i.cdv.titer", "i.calici.titer", "i.corona.titer", "i.panleuk.titer",
  "i.group.number", 
  "d.elisa.igg.broad.date", "d.elisa.igg.specific.date", "d.elisa.igm.date", 
  "i.cmi.bcv.igm", "i.cmi.bcv.igg.broad", 
  "i.cmi.bsa.broad", 
  "i.cmi.cav2.igg.broad", "i.cmi.cav2.igg.specific", "i.cmi.cav2.igm", 
  "i.cmi.ccv.broad", 
  "i.cmi.cdv.igg.broad", "i.cmi.cdv.igg.specific", "i.cmi.cdv.igm", 
  "i.cmi.cpiv.broad", 
  "i.cmi.cpv.broad", 
  "i.cmi.dirofilaria.immitis.igg.broad", "i.cmi.dirofilaria.immitis.igg.specific", "i.cmi.dirofilaria.immitis.igm", 
  "i.cmi.fcov.broad", 
  "i.cmi.fcv.sah.broad", "i.cmi.fcv.sah.igm", "i.cmi.fcv.fd.broad", 
  "i.cmi.felv.igg.broad.1", "i.cmi.felv.igg.broad.8.24", "i.cmi.felv.igg.specific", "i.cmi.felv.igm", 
  "i.cmi.fiv.p24.broad", 
  "i.cmi.herpes.sah.broad", 
  "i.cmi.fhv.fvr.kz.igm", "i.cmi.herpes.solvay.broad", 
  "i.cmi.hiv.gp120.broad", "i.cmi.siv.broad")]
    
df.temp.aggregate <- aggregate(
  cbind(d.age.months, d.mass, i.rank, i.litter.rank, d.months.to.grad, i.glucose.green, i.glucose.red, d.pcv, d.total.solids, d.mic.90.level, 
    i.cdv.titer, i.calici.titer, i.corona.titer, i.panleuk.titer,
    i.cmi.bcv.igm, i.cmi.bcv.igg.broad, 
    i.cmi.bsa.broad, 
    i.cmi.cav2.igg.broad, i.cmi.cav2.igg.specific, i.cmi.cav2.igm, 
    i.cmi.ccv.broad, 
    i.cmi.cdv.igg.broad, i.cmi.cdv.igg.specific, i.cmi.cdv.igm, 
    i.cmi.cpiv.broad, 
    i.cmi.cpv.broad, 
    i.cmi.dirofilaria.immitis.igg.broad, i.cmi.dirofilaria.immitis.igg.specific, i.cmi.dirofilaria.immitis.igm, 
    i.cmi.fcov.broad, 
    i.cmi.fcv.sah.broad, i.cmi.fcv.sah.igm, i.cmi.fcv.fd.broad, 
    i.cmi.felv.igg.broad.1, i.cmi.felv.igg.broad.8.24, i.cmi.felv.igg.specific, i.cmi.felv.igm, 
    i.cmi.fiv.p24.broad, 
    i.cmi.herpes.sah.broad, 
    i.cmi.fhv.fvr.kz.igm, i.cmi.herpes.solvay.broad, 
    i.cmi.hiv.gp120.broad, i.cmi.siv.broad     
  ) 
  ~ c.id + i.kay.code + c.sex + i.sample.number + d.sample.date,
  data = df.temp, mean, na.action = na.pass)    # na.pass includes the na values, na.action = na.omit  removes all records that contain an NA             
# print(summary(df.temp.aggregate))


c.v.pathogens <- c("cav2", "cdv", "dirofilaria.immitis")  # List of pathogens to calculate prevalence for
i.v.pathogen.igg.broad.high.limit <- c(1000, 500, 900)    # High limits for each of the pathogens in c.v.pathogens
i.v.pathogen.igg.broad.low.limit <- c(500, 250, 450)      # Low limits for each of the pathogens in c.v.pathogens
i.v.pathogen.igg.specific.high.limit <- c(500, 200, 450)
i.v.pathogen.igg.specific.low.limit <- c(250, 100, 225)
i.v.pathogen.igm.high.limit <- c(1000, 500, 900)
i.v.pathogen.igm.low.limit <- c(500, 250, 450)

i.pathogen.counter <- 1
for(i.pathogen.counter in i.pathogen.counter:length(c.v.pathogens))
{
  # c.temp.pathogens <- "dirofilaria.immitis"
  c.temp.pathogen <- c.v.pathogens[i.pathogen.counter]
  i.igg.broad.high <- i.v.pathogen.igg.broad.high.limit[i.pathogen.counter]
  i.igg.broad.low <- i.v.pathogen.igg.broad.low.limit[i.pathogen.counter]
  i.igg.specific.high <- i.v.pathogen.igg.specific.high.limit[i.pathogen.counter]
  i.igg.specific.low <- i.v.pathogen.igg.specific.low.limit[i.pathogen.counter]
  i.igm.high <- i.v.pathogen.igm.high.limit[i.pathogen.counter]
  i.igm.low <- i.v.pathogen.igm.high.limit[i.pathogen.counter]

  # Create temp variables. Use these to access the colnames we want to extract while looping through the pathogens 
  c.igg.broad <- paste("i.cmi.", c.temp.pathogen, ".igg.broad", sep = "")
  print(c.igg.broad)
  c.igg.specific <- paste("i.cmi.", c.temp.pathogen, ".igg.specific", sep = "")
  c.igm <- paste("i.cmi.", c.temp.pathogen, ".igm", sep = "")

  # Create logical vectors of positive, low positive, and negative for IgG broad antibody 
  # c.v.igg.broad.positive <- (df.temp.aggregate$i.cmi.dirofilaria.immitis.igg.broad > 899)
  c.v.igg.broad.positive <- (df.temp.aggregate[,c.igg.broad] >= i.igg.broad.high)
  c.v.igg.broad.low.positive <- (df.temp.aggregate[,c.igg.broad] < i.igg.broad.high  
    & df.temp.aggregate[,c.igg.broad] >= i.igg.broad.low)
  c.v.igg.broad.negative <- (df.temp.aggregate[,c.igg.broad] < i.igg.broad.low)

  # Create logical vectors of positive, low positive, and negative for IgG specific antibody 
  c.v.igg.specific.positive <- (df.temp.aggregate[,c.igg.specific] >= i.igg.specific.high)
  c.v.igg.specific.low.positive <- (df.temp.aggregate[,c.igg.specific] < i.igg.specific.high  
    & df.temp.aggregate[,c.igg.specific] >= i.igg.specific.low)
  c.v.igg.specific.negative <- (df.temp.aggregate[,c.igg.specific] < i.igg.specific.low)

  # Create logical vectors of positive, low positive, and negative for IgM antibody 
  c.v.igm.positive <- (df.temp.aggregate[,c.igm] >= i.igm.high)
  c.v.igm.low.positive <- (df.temp.aggregate[,c.igm] < i.igm.high 
  & df.temp.aggregate[,c.igm] >= i.igm.low)
  c.v.igm.negative <- (df.temp.aggregate[,c.igm] < i.igm.low)

  df.prevalence <- data.frame(df.temp.aggregate$c.id, 
    df.temp.aggregate[,c.igg.broad],
    df.temp.aggregate[,c.igg.specific], 
    df.temp.aggregate[,c.igm],                     
    c.v.igg.broad.positive,
    c.v.igg.broad.low.positive,
    c.v.igg.broad.negative,
    c.v.igg.specific.positive,
    c.v.igg.specific.low.positive,
    c.v.igg.specific.negative,
    c.v.igm.positive,
    c.v.igm.low.positive,
    c.v.igm.negative)
    
#  if(i.pathogen.counter == 1)
#  {
    colnames(df.prevalence) <- c("individual", "igg.broad.titer", "igg.specific.titer", "igm.titer", 
      "igg.broad.pos", "igg.broad.low.pos", "igg.broad.neg",
      "igg.specific.pos", "igg.specific.low.pos", "igg.specific.neg",
      "igm.pos", "igm.low.pos", "igm.neg")
#  }  
# print(df.prevalence)

  df.prevalence.sorted <- with(df.prevalence, f.sort.frame(df.prevalence, individual))
#  print(df.prevalence.sorted)

#  a.igg.broad <- paste("i.cmi.", c.temp.pathogen, ".igg.broad", sep = "")
  a.column.sums <- colSums(df.prevalence.sorted[,5:13], na.rm = TRUE)
  print(a.column.sums)

  if(i.pathogen.counter == 1)
  {
    df.prevalence.full <- data.frame(rbind(a.column.sums, NULL))
  }
  else
  {
    df.prevalence.full <- data.frame(rbind(df.prevalence.full, a.column.sums))    
  }
  
}
rownames(df.prevalence.full) <- c.v.pathogens
print(df.prevalence.full)

# print(xtabs(~ igg.broad.pos
#  + igg.specific.pos # + igg.specific.low.pos + igg.specific.neg 
#  + igm.pos # + igm.low.pos + igm.neg, 
#  , df.prevalence, na.action = na.pass))


# write.csv(m.cov, file = "serology.covariance.csv") 
# write.csv(m.cor, file = "serology.correlations.csv") 

