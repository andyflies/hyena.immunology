# source("s.cov.matrix.r")

#df.temp <- df.data[1:5,11:35]
# df.temp <- df.data[,11:35]
df.temp <- df.data[,c("c.id", "i.kay.code", "d.sample.date", "c.sex", "i.sample.number", 
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
    
print(summary(df.temp))
# df.temp.aggregate <- aggregate(i.cdv.titer + i.calici.titer + i.corona.titer + i.panleuk.titer ~ 
#  c.id + d.sample.date,
#  data = df.temp, mean)

print("na.pass")
df.temp.aggregate <- aggregate(cbind(i.cdv.titer, i.calici.titer, i.corona.titer, i.panleuk.titer) ~ 
  c.id + i.kay.code + c.sex + i.sample.number + d.sample.date,
  data = df.temp, mean, na.action = na.pass)    # na.pass includes the na values, na.action = na.omit  removes all records that contain an NA             
print(summary(df.temp.aggregate))  

print("na.pass")
df.temp.aggregate <- aggregate(
  cbind(i.cdv.titer, i.calici.titer, i.corona.titer, i.panleuk.titer,
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