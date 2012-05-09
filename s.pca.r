# source("s.pca.r")


df.pca.immune.short <- df.data.flat[, c("d.mic.90.level.bka.ec", "d.mic.90.level.bka.pm", "d.blank.abs.total.igg",	"d.blank.abs.total.igm",
  "d.blank.abs.ec.igg", "d.blank.abs.ec.igm", "d.blank.abs.pm.igg", "d.blank.abs.pm.igm")]
  
pca.immune.short <- princomp(df.pca.immune.short, cor = TRUE)

print(summary(pca.immune.short))
print(loadings(pca.immune.short))

df.pca.immune.long <- df.data.flat[, c("d.mic.90.level.bka.ec", "d.mic.90.level.bka.pm", "d.blank.abs.total.igg", "d.blank.abs.total.igm", "d.blank.abs.ec.igg", "d.blank.abs.ec.igm", "d.blank.abs.pm.igg", "d.blank.abs.pm.igm",
  "i.cmi.bsa.igg", "i.cmi.bsa.igm", "i.cmi.cav2.igg", "i.cmi.cav2.igm", "i.cmi.ccv.igg", "i.cmi.ccv.igm", "i.cmi.cdv.igg", "i.cmi.cdv.igm",
  "i.cmi.dirofilaria.igg", "i.cmi.dirofilaria.igm", "i.cmi.fiv.p24.igg", "i.cmi.fiv.p24.igm")]

df.pca.immune.long <- na.omit(df.pca.immune.long)

pca.immune.long <- princomp(df.pca.immune.long, cor = TRUE, na.action = na.omit)
print(summary(pca.immune.long))
print(loadings(pca.immune.long))



# df.pca.immune.long <- df.data.flat[, c("i.glucose.green",	"i.glucose.red", "d.pcv",	"d.total.solids",	"d.mic.90.level.bka.ec", "d.mic.90.level.bka.pm",
#  "d.blank.abs.total.igg", "d.blank.abs.total.igm", "d.blank.abs.ec.igg", "d.blank.abs.ec.igm", "d.blank.abs.pm.igg", "d.blank.abs.pm.igm",
#  "i.cmi.bsa.igg", "i.cmi.bsa.igm", "i.cmi.cav2.igg", "i.cmi.cav2.igm", "i.cmi.ccv.igg", "i.cmi.ccv.igm", "i.cmi.cdv.igg", "i.cmi.cdv.igm",
#  "i.cmi.dirofilaria.igg", "i.cmi.dirofilaria.igm", "i.cmi.fiv.p24.igg", "i.cmi.fiv.p24.igm", "d.basophils", "d.eosinophils", "d.lymphocytes",
#  "d.monocytes", "d.neutrophils", "d.total.wbc")]














