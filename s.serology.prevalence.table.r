# source("s.serology.prevalence.table.r")


df.data.female <- df.data.flat[df.data.flat$c.sex == 'f' & df.data.flat$c.age == "adult",]
df.data.male <- df.data.flat[df.data.flat$c.sex == 'm' & df.data.flat$c.age == "adult",]
# df.temp <- df.data.female[,c("c.id", "i.kay.code", "d.sample.date", "c.sex", "i.sample.number",
# df.temp <- df.data.male[,c("c.id", "i.kay.code", "d.sample.date", "c.sex", "i.sample.number",
df.temp <- df.data.flat[,c("c.id", "i.kay.code", "d.sample.date", "c.sex", "i.sample.number",
  "d.age.months", "d.mass", "i.rank", "i.litter.rank",
  "d.months.to.grad",
  "i.glucose.green", "i.glucose.red", "d.pcv",
  "d.total.solids", "d.mic.90.bka.ec", "d.mic.90.bka.pm", 
  "i.cdv.titer", "i.calici.titer", "i.corona.titer", "i.panleuk.titer",
  "i.group.number",
#  "d.elisa.igg.date", "d.elisa.igm.date", 
#  "i.cmi.bcv.igg", "i.cmi.bcv.igm", 
  "i.cmi.cav2.igg", "i.cmi.cav2.igm", 
  "i.cmi.ccv.igg", "i.cmi.ccv.igm", 
  "i.cmi.cdv.igg", "i.cmi.cdv.igm", 
#  "i.cmi.cpiv.igg", "i.cmi.cpiv.igm",
#  "i.cmi.cpv.igg", "i.cmi.cpv.igm", 
  "i.cmi.dirofilaria.igg", "i.cmi.dirofilaria.igm", 
#  "i.cmi.fcov.igg", "i.cmi.fcov.igm",
#  "i.cmi.fcv.sah.igg", "i.cmi.fcv.sah.igm", 
#  "i.cmi.fcv.fd.igg", "i.cmi.fcv.fd.igm",
#  "i.cmi.felv.gp70.igg", "i.cmi.felv.gp70.igm", 
#  "i.cmi.felv.p27.igg", "i.cmi.felv.p27.igm",
#  "i.cmi.fiv.34TF10.igg", "i.cmi.fiv.34TF10.igm",	
#  "i.cmi.fiv.gp95.igg", "i.cmi.fiv.gp95.igm",	
  "i.cmi.fiv.p24.igg", "i.cmi.fiv.p24.igm"
  )]
  
df.temp.aggregate <- aggregate(
  cbind(d.age.months, d.mass, i.rank, i.litter.rank, d.months.to.grad, i.glucose.green, i.glucose.red, d.pcv, d.total.solids, 
    d.mic.90.bka.ec, d.mic.90.bka.pm,
    i.cdv.titer, i.calici.titer, i.corona.titer, i.panleuk.titer,
#    i.cmi.bcv.igg, i.cmi.bcv.igm, 
#    i.cmi.bsa.igg, 
    i.cmi.cav2.igg, i.cmi.cav2.igm, 
    i.cmi.ccv.igg, i.cmi.ccv.igm, 
    i.cmi.cdv.igg, i.cmi.cdv.igm, 
#    i.cmi.cpiv.igg, i.cmi.cpiv.igm,
#    i.cmi.cpv.igg, i.cmi.cpv.igm, 
    i.cmi.dirofilaria.igg, i.cmi.dirofilaria.igm, 
#    i.cmi.fcov.igg, i.cmi.fcov.igm,
#    i.cmi.fcv.sah.igg, i.cmi.fcv.sah.igm, 
#    i.cmi.fcv.fd.igg, i.cmi.fcv.fd.igm,
#    i.cmi.felv.gp70.igg, i.cmi.felv.gp70.igm, 
#    i.cmi.felv.p27.igg, i.cmi.felv.p27.igm,
#    i.cmi.fiv.34TF10.igg, i.cmi.fiv.34TF10.igm,	
#    i.cmi.fiv.gp95.igg, i.cmi.fiv.gp95.igm,	
    i.cmi.fiv.p24.igg, i.cmi.fiv.p24.igm	
  ) 
  ~ c.id + i.kay.code + c.sex + i.sample.number + d.sample.date,
  data = df.temp, mean, na.action = na.pass)    # na.pass includes the na values, na.action = na.omit  removes all records that contain an NA             
# print(summary(df.temp.aggregate))

c.v.pathogens <- c("cav2", "ccv", "cdv", "dirofilaria", "fiv.p24")  # List of pathogens to calculate prevalence for
# i.v.pathogen.igg.hl.high.limit <- c(1000, 500, 900, 500)    # High limits for each of the pathogens in c.v.pathogens
# i.v.pathogen.igg.hl.low.limit <- c(500, 250, 450, 200)      # Low limits for each of the pathogens in c.v.pathogens
#i.v.pathogen.igg.high.limit <- c(500, 200, 450, 250)
i.v.pathogen.igg.high.limit <- c(200, 200, 200, 200, 200)
i.v.pathogen.igg.low.limit <- c(100, 100, 100, 100, 100)
i.v.pathogen.igm.high.limit <- c(150, 150, 150, 150, 150)
i.v.pathogen.igm.low.limit <- c(75, 75, 75, 75, 75)

# i.v.pathogen.igg.high.limit <- c(400, 400, 400, 400, 400)
# i.v.pathogen.igg.low.limit <- c(200, 200, 200, 200, 200)
# i.v.pathogen.igm.high.limit <- c(400, 400, 400, 400, 400)
# i.v.pathogen.igm.low.limit <- c(200, 200, 200, 200, 200)

i.pathogen.counter <- 1
for(i.pathogen.counter in i.pathogen.counter:length(c.v.pathogens))
{
  # c.temp.pathogens <- "dirofilaria.immitis"
  c.temp.pathogen <- c.v.pathogens[i.pathogen.counter]
#  i.igg.hl.high <- i.v.pathogen.igg.hl.high.limit[i.pathogen.counter]
#  i.igg.hl.low <- i.v.pathogen.igg.hl.low.limit[i.pathogen.counter]
  i.igg.high <- i.v.pathogen.igg.high.limit[i.pathogen.counter]
  i.igg.low <- i.v.pathogen.igg.low.limit[i.pathogen.counter]
  i.igm.high <- i.v.pathogen.igm.high.limit[i.pathogen.counter]
  i.igm.low <- i.v.pathogen.igm.low.limit[i.pathogen.counter]

  # Create temp variables. Use these to access the colnames we want to extract while looping through the pathogens 
#  c.igg.hl <- paste("i.cmi.", c.temp.pathogen, ".igg.hl", sep = "")
#  print(c.igg.hl)
  c.igg <- paste("i.cmi.", c.temp.pathogen, ".igg", sep = "")
  c.igm <- paste("i.cmi.", c.temp.pathogen, ".igm", sep = "")

  # Create logical vectors of positive, low positive, and negative for IgG specific antibody 
  c.v.igg.positive <- (df.temp.aggregate[,c.igg] >= i.igg.high)
  c.v.igg.low.positive <- (df.temp.aggregate[,c.igg] < i.igg.high  
    & df.temp.aggregate[,c.igg] >= i.igg.low)
  c.v.igg.negative <- (df.temp.aggregate[,c.igg] < i.igg.low)

  # Create logical vectors of positive, low positive, and negative for IgM antibody 
  c.v.igm.positive <- (df.temp.aggregate[,c.igm] >= i.igm.high)
  c.v.igm.low.positive <- (df.temp.aggregate[,c.igm] < i.igm.high 
  & df.temp.aggregate[,c.igm] >= i.igm.low)
  c.v.igm.negative <- (df.temp.aggregate[,c.igm] < i.igm.low)

  df.prevalence <- data.frame(df.temp.aggregate$c.id, 
#    df.temp.aggregate[,c.igg.hl],
    df.temp.aggregate[,c.igg], 
    df.temp.aggregate[,c.igm],                     
#    c.v.igg.hl.positive,
#    c.v.igg.hl.low.positive,
#    c.v.igg.hl.negative,
    c.v.igg.positive,
    c.v.igg.low.positive,
    c.v.igg.negative,
    c.v.igm.positive,
    c.v.igm.low.positive,
    c.v.igm.negative)
    
#  if(i.pathogen.counter == 1)
#  {
    colnames(df.prevalence) <- c("individual", "igg.titer", "igm.titer", 
#      "igg.hl.pos", "igg.hl.low.pos", "igg.hl.neg",
      "igg.pos", "igg.low.pos", "igg.neg",
      "igm.pos", "igm.low.pos", "igm.neg")
#  }  

  df.prevalence.sorted <- with(df.prevalence, f.sort.frame(df.prevalence, individual))
#  print(df.prevalence.sorted)

#  a.igg.hl <- paste("i.cmi.", c.temp.pathogen, ".igg.hl", sep = "")
  a.column.sums <- colSums(df.prevalence.sorted[,4:9], na.rm = TRUE)

  if(i.pathogen.counter == 1)
  {
    df.prevalence.full <- data.frame(rbind(a.column.sums, NULL))
#    df.proportion.full <- data.frame(rbind((a.column.sums / length(df.prevalence.sorted[,1]), NULL))
  }
  else
  {
    df.prevalence.full <- data.frame(rbind(df.prevalence.full, a.column.sums))    
  }
  
}
rownames(df.prevalence.full) <- c.v.pathogens
print(df.prevalence.full)
cv.samples <- df.prevalence.sorted[(!(is.na(df.prevalence.sorted[,4]))),4]  # Get all the rows that are not NA, which should be 92 for this analysis
i.number.samples <- length(cv.samples)
print(i.number.samples)

m.prevalence <- df.prevalence.full / i.number.samples
print(m.prevalence)

df.combined <- data.frame(df.prevalence.full, m.prevalence)
print(df.combined)
# print(xtabs(~ igg.hl.pos
#  + igg.pos # + igg.low.pos + igg.neg 
#  + igm.pos # + igm.low.pos + igm.neg, 
#  , df.prevalence, na.action = na.pass))


write.csv(df.combined, file = "cmi.serology.prevalence.csv") 

