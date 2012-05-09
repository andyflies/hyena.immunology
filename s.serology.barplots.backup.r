# source("s.serology.barplots.r")

  f.theme <- function(base_size = 12) {
    structure(list(
					axis.line =         theme_blank(),
					axis.text.x =       theme_text(size = base_size * 0.9 , lineheight = 0.9, colour = "black", hjust = 0.5, vjust = 1, angle = 0),
					axis.text.y =       theme_text(size = base_size * 0.9, lineheight = 0.9, colour = "black", hjust = 0.5, vjust = 0.5),
					axis.ticks =        theme_segment(colour = "black", size = 0.5, linetype = 1),
#					axis.ticks =        theme_blank(),
					axis.title.x =      theme_text(size = base_size, vjust = 1, face = "bold"),
					axis.title.y =      theme_text(size = base_size, angle = 90, vjust = 0.35, face = "bold"),
#					axis.ticks.length = unit(0.3, "lines"),
#					axis.ticks.margin = unit(0, "lines"),
#          axis.ticks =        theme.blank(),
        					
					legend.background = theme_rect(colour = NA), 
					legend.key =        theme_rect(fill = NA, colour = NA),
					legend.key.size =   unit(1.2, "lines"),
					legend.text =       theme_text(size = base_size * 0.8),
					legend.shape =      theme_text(size = base_size * 0.8, face = "bold"),
					legend.title =      theme_text(size = base_size * 1, hjust = 0, face = "bold"),
					legend.position =   "right",
					
					panel.background =  theme_rect(fill = "white", colour = "black"), 
					panel.border =      theme_blank(), #theme_rect(fill = NA, colour = "black"), # 
					panel.grid.major =  theme_line(colour = NA, linetype = 2),
					panel.grid.minor =  theme_line(colour = NA, size = 0.25),
					panel.margin =      unit(1, "lines"),
					
					strip.background =  theme_rect(fill = "grey", colour = "black"), 
					strip.label =       function(variable, value) value, 
					strip.text.x =      theme_text(size = base_size * 1),
					strip.text.y =      theme_text(size = base_size * 1, angle = -90),
					
					plot.background =   theme_rect(fill = NA, colour = NA),
					plot.title =        theme_text(size = base_size * 1.2), #, just = c(0.5, 0.5)),
					plot.margin =       unit(c(0.5, 0.25, 0.5, 0.25), "lines")      # 1 = top, 2, side, 3 = bottom, 4 = side
			), class = "options")
  }



df.data.female <- df.data[df.data$c.sex == 'f' & df.data$c.age == "adult",]
df.data.male <- df.data[df.data$c.sex == 'm' & df.data$c.age == "adult",]
df.temp <- df.data[,c("c.id", "i.kay.code", "d.sample.date", "c.sex", "i.sample.number", 
# df.temp <- df.data.male[,c("c.id", "i.kay.code", "d.sample.date", "c.sex", "i.sample.number",

#df.temp <- df.data[,c("c.id", "i.kay.code", "d.sample.date", "c.sex", "i.sample.number",
  "d.age.months", "d.mass", "i.rank", "i.litter.rank",
  "d.months.to.grad", 
  "i.glucose.green", "i.glucose.red", "d.pcv", 
  "d.total.solids", "d.mic.90", "d.mic.90.level", 
 
  "i.cdv.titer", "i.calici.titer", "i.corona.titer", "i.panleuk.titer",
  "i.group.number", 
  "d.elisa.igg.hl.date", "d.elisa.igg.date", "d.elisa.igm.date", 
  "i.cmi.bcv.igm",
  "i.cmi.bcv.igg.hl",
  "i.cmi.bsa.igg.hl",
  "i.cmi.cav2.igg.hl", "i.cmi.cav2.igg", "i.cmi.cav2.igm", 
  "i.cmi.ccv.igg.hl",
  "i.cmi.cdv.igg.hl", "i.cmi.cdv.igg", "i.cmi.cdv.igm",
  "i.cmi.cpiv.igg.hl", 
  "i.cmi.cpv.igg.hl", 
  "i.cmi.dirofilaria.igg.hl", "i.cmi.dirofilaria.igg", "i.cmi.dirofilaria.igm", 
  "i.cmi.fcov.igg.hl", 
  "i.cmi.fcv.sah.igg.hl", "i.cmi.fcv.sah.igm", "i.cmi.fcv.fd.igg.hl", 
  "i.cmi.felv.gp70.igg.hl", "i.cmi.felv.gp70.igg", "i.cmi.felv.gp70.igm",
  "i.cmi.felv.p27.igg.hl", "i.cmi.felv.p27.igg", "i.cmi.felv.p27.igm",
  "i.cmi.fiv.34TF10.igg.hl", "i.cmi.fiv.34TF10.igg", "i.cmi.fiv.34TF10.igm",
  "i.cmi.fiv.gp95.igg.hl", "i.cmi.fiv.gp95.igg", "i.cmi.fiv.gp95.igm",
  "i.cmi.fiv.p24.igg.hl", "i.cmi.fiv.p24.igg", "i.cmi.fiv.p24.igm",
  "i.cmi.herpes.sah.igg.hl",
  "i.cmi.fhv.fvr.kz.igm",
  "i.cmi.herpes.solvay.igg.hl",
  "i.cmi.hiv.gp120.igg.hl", "i.cmi.hiv.gp120.igg", "i.cmi.hiv.gp120.igm",
  "i.cmi.siv.igg.hl", "i.cmi.siv.igg", "i.cmi.siv.igm"
  )]
    
# print(summary(df.temp))

print("na.pass")
df.temp.aggregate <- aggregate(
  cbind(d.age.months, d.mass, i.rank, i.litter.rank, d.months.to.grad, i.glucose.green, i.glucose.red, d.pcv, d.total.solids, 
    d.mic.90, d.mic.90.level, 
    i.cdv.titer, i.calici.titer, i.corona.titer, i.panleuk.titer,
    i.cmi.bcv.igm, i.cmi.bcv.igg.hl, 
    i.cmi.bsa.igg.hl, 
    i.cmi.cav2.igg.hl, i.cmi.cav2.igg, i.cmi.cav2.igm, 
    i.cmi.ccv.igg.hl, 
    i.cmi.cdv.igg.hl, i.cmi.cdv.igg, i.cmi.cdv.igm, 
    i.cmi.cpiv.igg.hl, 
    i.cmi.cpv.igg.hl, 
  i.cmi.dirofilaria.igg.hl, i.cmi.dirofilaria.igg, i.cmi.dirofilaria.igm, 
  i.cmi.fcov.igg.hl, 
  i.cmi.fcv.sah.igg.hl, i.cmi.fcv.sah.igm, i.cmi.fcv.fd.igg.hl, 
  i.cmi.felv.gp70.igg.hl, i.cmi.felv.gp70.igg, i.cmi.felv.gp70.igm,
  i.cmi.felv.p27.igg.hl, i.cmi.felv.p27.igg, i.cmi.felv.p27.igm,
  i.cmi.fiv.34TF10.igg.hl, i.cmi.fiv.34TF10.igg, i.cmi.fiv.34TF10.igm,
  i.cmi.fiv.gp95.igg.hl, i.cmi.fiv.gp95.igg, i.cmi.fiv.gp95.igm,
  i.cmi.fiv.p24.igg.hl, i.cmi.fiv.p24.igg, i.cmi.fiv.p24.igm,
  i.cmi.herpes.sah.igg.hl,
  i.cmi.fhv.fvr.kz.igm,
  i.cmi.herpes.solvay.igg.hl,
  i.cmi.hiv.gp120.igg.hl, i.cmi.hiv.gp120.igg, i.cmi.hiv.gp120.igm,
  i.cmi.siv.igg.hl, i.cmi.siv.igg, i.cmi.siv.igm   
  ) 
  ~ c.id + i.kay.code + c.sex + i.sample.number + d.sample.date,
  data = df.temp, mean, na.action = na.pass)    # na.pass includes the na values, na.action = na.omit  removes all records that contain an NA             

c.targets <- c("cav2", "cdv", "cpv", "fcov", "dirofilaria", "fcov", "fcv", "felv", "fiv")

c.temp <- paste("i.cmi.", c.targets[5], ".igg", sep = "")
print(c.temp)
c.temp.igg.status <- paste("c.cmi.", c.targets[5], ".igg.status", sep = "")
print(c.temp.igg.status)
c.temp.igg.hl.status <- paste("c.cmi.", c.targets[5], ".igg.hl.status", sep = "")
print(c.temp.igg.hl.status)
c.temp.igm.status <- paste("c.cmi.", c.targets[5], ".igm.status", sep = "")
print(c.temp.igm.status)


c.cmi.dirofilaria.igg.status <- as.character(df.temp.aggregate[, c.temp]) 
# c.cmi.dirofilaria.igg.status <- as.character(df.temp.aggregate[, "i.cmi.dirofilaria.igg"]) 
#c.cmi.dirofilaria.igg.status <- as.character(df.temp.aggregate$i.cmi.dirofilaria.igg) 
c.cmi.dirofilaria.igg.status[df.temp.aggregate$i.cmi.dirofilaria.igg >= m.serology.cutoff.values[c.temp, "high"]] <- "positive"
c.cmi.dirofilaria.igg.status[df.temp.aggregate$i.cmi.dirofilaria.igg < m.serology.cutoff.values[c.temp, "high"]
 & df.temp.aggregate$i.cmi.dirofilaria.igg > m.serology.cutoff.values[c.temp, "low"]] <- "low positive"
c.cmi.dirofilaria.igg.status[df.temp.aggregate$i.cmi.dirofilaria.igg <= m.serology.cutoff.values[c.temp, "low"]] <- "negative"

# c.temp <- paste("i.", "dirofilaria", ".igg.hl", sep = "")
c.temp <- paste("i.cmi.", c.targets[5], ".igg", sep = "")

c.cmi.dirofilaria.igg.hl.status <- as.character(df.temp.aggregate$i.cmi.dirofilaria.igg.hl) 
c.cmi.dirofilaria.igg.hl.status[df.temp.aggregate$i.cmi.dirofilaria.igg.hl >= m.serology.cutoff.values[c.temp, "high"]] <- "positive"
c.cmi.dirofilaria.igg.hl.status[df.temp.aggregate$i.cmi.dirofilaria.igg.hl < m.serology.cutoff.values[c.temp, "high"]
 & df.temp.aggregate$i.cmi.dirofilaria.igg.hl > m.serology.cutoff.values[c.temp, "low"]] <- "low positive"
c.cmi.dirofilaria.igg.hl.status[df.temp.aggregate$i.cmi.dirofilaria.igg.hl <= m.serology.cutoff.values[c.temp, "low"]] <- "negative"

c.temp <- paste("i.cmi.", c.targets[5], ".igg", sep = "")

c.cmi.dirofilaria.igm.status <- as.character(df.temp.aggregate$i.cmi.dirofilaria.igm) 
c.cmi.dirofilaria.igm.status[df.temp.aggregate$i.cmi.dirofilaria.igm >= m.serology.cutoff.values[c.temp, "high"]] <- "positive"
c.cmi.dirofilaria.igm.status[df.temp.aggregate$i.cmi.dirofilaria.igm < m.serology.cutoff.values[c.temp, "high"]
 & df.temp.aggregate$i.cmi.dirofilaria.igm > m.serology.cutoff.values[c.temp, "low"]] <- "low positive"
c.cmi.dirofilaria.igm.status[df.temp.aggregate$i.cmi.dirofilaria.igm <= m.serology.cutoff.values[c.temp, "low"]] <- "negative"


df.temp.aggregate <- cbind(df.temp.aggregate, c.cmi.dirofilaria.igg.status, c.cmi.dirofilaria.igg.hl.status, c.cmi.dirofilaria.igm.status)
print(summary(df.temp.aggregate))

df.igg.temp <- cbind("D. immitis", "IgG", c.cmi.dirofilaria.igg.status)
df.igg.hl.temp <- cbind("D. immitis", "IgG(H + L)", c.cmi.dirofilaria.igg.hl.status)
df.igm.temp <- cbind("D. immitis", "IgM", c.cmi.dirofilaria.igm.status)
df.counts <- data.frame(rbind(df.igg.temp, df.igg.hl.temp, df.igm.temp))
colnames(df.counts) <- c("target", "isotype", "status")
df.counts <- df.counts[!is.na(df.counts$status), ] 
print(df.counts)

df.counts$status <- factor(df.counts$status, levels = c("negative", "low positive", "positive"))# rev(levels(df.dirofilaria$status)))

# df.cmi.dirofilaria.igm.count <- count(c.cmi.dirofilaria.igm.status)
# print(c.cmi.dirofilaria.igm.count$freq[df.cmi.dirofilaria.igm.count$x == "high" & !is.na(df.cmi.dirofilaria.igm.count$x)])
# print(c.cmi.dirofilaria.igm.count$freq[df.cmi.dirofilaria.igm.count$x == "low" & !is.na(df.cmi.dirofilaria.igm.count$x)])
# print(c.cmi.dirofilaria.igm.count$freq[df.cmi.dirofilaria.igm.count$x == "negative" & !is.na(df.cmi.dirofilaria.igm.count$x)])


  gg.serology <- ggplot(data = df.counts, 
    aes(factor(isotype),
      fill = factor(status),
      order = -as.numeric(status),
#      weight = count(status),
#    aes(x = i.day,
#      y = d.mean,
#      group = factor(d.starting.age),
#      color = factor(d.starting.age),    
#      shape = factor(d.starting.age),
    legend = FALSE
    )
  )  

  print(gg.serology
    + geom_bar()  
    + scale_fill_manual(name = "Serology status", values = c("positive" = "dark grey", "low positive" = "light grey", "negative" = "black"), legend = TRUE) 
#    + opts(title = "Anti-DNP time course response") #
    + xlab("Isotype")
    + ylab("Count")   
    + facet_wrap(~ target, ncol = 2, scales = "free")
    + f.theme()    
#    + scale_x_continuous(limits=c(0.5,5.5), breaks = c(1,2,3,4,5), label = c("0", "14", "28", "180", "365"))    
  )




#m.cov <- cov(df.temp.aggregate[, -c(1:5)], df.temp.aggregate[, -c(1:5)],  use = "pairwise.complete.obs")  # using -c(...) to exclude the non-numerical columns
#m.cor <- cor(df.temp.aggregate[, -c(1:5)], df.temp.aggregate[, -c(1:5)],  use = "pairwise.complete.obs")  # using -c(...) to exclude the non-numerical columns

#write.csv(m.cov, file = "serology.covariance.csv") 
#write.csv(m.cor, file = "serology.correlations.csv") 

