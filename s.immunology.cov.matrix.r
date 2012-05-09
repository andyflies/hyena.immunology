# source("s.immunology.cov.matrix.r")

#df.temp <- df.data[1:5,11:35]
# df.temp <- df.data[,11:35]
df.data.female <- df.data[df.data$c.sex == 'f' & df.data$c.age == "adult",]
df.data.male <- df.data[df.data$c.sex == 'm' & df.data$c.age == "adult",]
# df.temp <- df.data.female[,c("c.id", "i.kay.code", "d.sample.date", "c.sex", "i.sample.number",
# df.temp <- df.data.male[,c("c.id", "i.kay.code", "d.sample.date", "c.sex", "i.sample.number",

df.temp <- df.data.female

df.temp <- df.data[,c("c.id", "i.kay.code", "d.sample.date", "c.sex", "i.sample.number",
  "d.age.months", "d.mass", "i.rank", "i.litter.rank",
  "d.months.to.grad", 
  "i.glucose.green", "i.glucose.red", "d.pcv", 
  "d.total.solids", "d.mic.90.level", 
 
  "i.cdv.titer", "i.calici.titer", "i.corona.titer", "i.panleuk.titer",
  
  "i.group.number", 
  "d.elisa.igg.hl.date", "d.elisa.igg.date", "d.elisa.igm.date", 
  "i.cmi.bcv.igg.hl", "i.cmi.bcv.igg", "i.cmi.bcv.igm", 
  "i.cmi.bsa.igg.hl", 
  "i.cmi.cav2.igg.hl", "i.cmi.cav2.igg", "i.cmi.cav2.igm", 
  "i.cmi.ccv.igg.hl", "i.cmi.ccv.igg", "i.cmi.ccv.igm", 
  "i.cmi.cdv.igg.hl", "i.cmi.cdv.igg", "i.cmi.cdv.igm", 
  "i.cmi.cpiv.igg.hl", "i.cmi.cpiv.igg", "i.cmi.cpiv.igm",
  "i.cmi.cpv.igg.hl", "i.cmi.cpv.igg", "i.cmi.cpv.igm", 
  "i.cmi.dirofilaria.igg.hl", "i.cmi.dirofilaria.igg", "i.cmi.dirofilaria.igm", 
  "i.cmi.fcov.igg.hl", "i.cmi.fcov.igg", "i.cmi.fcov.igm",
  "i.cmi.fcv.sah.igg.hl", "i.cmi.fcv.sah.igg", "i.cmi.fcv.sah.igm", 
  "i.cmi.fcv.fd.igg.hl", "i.cmi.fcv.fd.igg", "i.cmi.fcv.fd.igm",
  "i.cmi.felv.gp70.igg.hl", "i.cmi.felv.gp70.igg", "i.cmi.felv.gp70.igm", 
  "i.cmi.felv.p27.igg.hl",	"i.cmi.felv.p27.igg", "i.cmi.felv.p27.igm",
  "i.cmi.fiv.34TF10.igg.hl", "i.cmi.fiv.34TF10.igg", "i.cmi.fiv.34TF10.igm",	
  "i.cmi.fiv.gp95.igg.hl",	"i.cmi.fiv.gp95.igg", "i.cmi.fiv.gp95.igm",	
  "i.cmi.fiv.p24.igg.hl", "i.cmi.fiv.p24.igg", "i.cmi.fiv.p24.igm",	
  "i.cmi.herpes.sah.igg.hl",	               # No igg or igm
  "i.cmi.fhv.fvr.kz.igm",	                   # No igg or igm
  "i.cmi.herpes.solvay.igg.hl",              # No igg or igm
  "i.cmi.hiv.gp120.igg.hl", "i.cmi.hiv.gp120.igg", "i.cmi.hiv.gp120.igm",	
  "i.cmi.siv.igg.hl", "i.cmi.siv.igg", "i.cmi.siv.igm")] 
    
print(summary(df.temp))
# df.temp.aggregate <- aggregate(i.cdv.titer + i.calici.titer + i.corona.titer + i.panleuk.titer ~ 
#  c.id + d.sample.date,
#  data = df.temp, mean)

#print("na.pass")
# df.temp.aggregate <- aggregate(cbind(i.cdv.titer, i.calici.titer, i.corona.titer, i.panleuk.titer) ~ 
#  c.id + i.kay.code + c.sex + i.sample.number + d.sample.date,
#  data = df.temp, mean, na.action = na.pass)    # na.pass includes the na values, na.action = na.omit  removes all records that contain an NA             
# print(summary(df.temp.aggregate))  

print("na.pass")
df.temp.aggregate <- aggregate(
  cbind(d.age.months, d.mass, i.rank, i.litter.rank, d.months.to.grad, i.glucose.green, i.glucose.red, d.pcv, d.total.solids, d.mic.90.level, 
    i.cdv.titer, i.calici.titer, i.corona.titer, i.panleuk.titer,
    d.elisa.igg.hl.date, d.elisa.igg.date, d.elisa.igm.date, 
    i.cmi.bcv.igg.hl, i.cmi.bcv.igg, i.cmi.bcv.igm, 
    i.cmi.bsa.igg.hl, 
    i.cmi.cav2.igg.hl, i.cmi.cav2.igg, i.cmi.cav2.igm, 
    i.cmi.ccv.igg.hl, i.cmi.ccv.igg, i.cmi.ccv.igm, 
    i.cmi.cdv.igg.hl, i.cmi.cdv.igg, i.cmi.cdv.igm, 
    i.cmi.cpiv.igg.hl, i.cmi.cpiv.igg, i.cmi.cpiv.igm,
    i.cmi.cpv.igg.hl, i.cmi.cpv.igg, i.cmi.cpv.igm, 
    i.cmi.dirofilaria.igg.hl, i.cmi.dirofilaria.igg, i.cmi.dirofilaria.igm, 
    i.cmi.fcov.igg.hl, i.cmi.fcov.igg, i.cmi.fcov.igm,
    i.cmi.fcv.sah.igg.hl, i.cmi.fcv.sah.igg, i.cmi.fcv.sah.igm, 
    i.cmi.fcv.fd.igg.hl, i.cmi.fcv.fd.igg, i.cmi.fcv.fd.igm,
    i.cmi.felv.gp70.igg.hl, i.cmi.felv.gp70.igg, i.cmi.felv.gp70.igm, 
    i.cmi.felv.p27.igg.hl,	i.cmi.felv.p27.igg, i.cmi.felv.p27.igm,
    i.cmi.fiv.34TF10.igg.hl, i.cmi.fiv.34TF10.igg, i.cmi.fiv.34TF10.igm,	
    i.cmi.fiv.gp95.igg.hl,	i.cmi.fiv.gp95.igg, i.cmi.fiv.gp95.igm,	
    i.cmi.fiv.p24.igg.hl, i.cmi.fiv.p24.igg, i.cmi.fiv.p24.igm,	
    i.cmi.herpes.sah.igg.hl,	               # No igg or igm
    i.cmi.fhv.fvr.kz.igm,	                   # No igg or igm
    i.cmi.herpes.solvay.igg.hl,              # No igg or igm
    i.cmi.hiv.gp120.igg.hl, i.cmi.hiv.gp120.igg, i.cmi.hiv.gp120.igm,	
    i.cmi.siv.igg.hl, i.cmi.siv.igg, i.cmi.siv.igm   
  ) 
  ~ c.id + i.kay.code + c.sex + i.sample.number + d.sample.date,
  data = df.temp, mean, na.action = na.pass)    # na.pass includes the na values, na.action = na.omit  removes all records that contain an NA             
print(summary(df.temp.aggregate))


m.cov <- cov(df.temp.aggregate[, -c(1:5)], df.temp.aggregate[, -c(1:5)],  use = "pairwise.complete.obs")
# m.cor <- cor(df.temp.aggregate[, c(-1,-2,-3,-4,-5)], df.temp.aggregate[, c(-1,-2,-3,-4,-5)],  use = "pairwise.complete.obs")
m.cor <- cor(df.temp.aggregate[, -c(1:5)], df.temp.aggregate[, -c(1:5)],  use = "pairwise.complete.obs")
#m.cov2cor <- cov2cor(m.cor)

write.csv(m.cov, file = "serology.covariance.csv") 
write.csv(m.cor, file = "serology.correlations.csv") 
# write.csv(m.cov2cor, file = "IgM.IgG.cov2cor.csv")

# write.xlsx(m.cov, file = "IgM.IgG.covariance.xlsx", sheetName="Sheet 1")
# write.xlsx(m.cor, file = "IgM.IgG.correlations.xlsx", sheetName="Sheet 1")
# write.xlsx(m.cov2cor, file = "IgM.IgG.cov2cor.xlsx", sheetName="Sheet 1")

rm(df.temp)