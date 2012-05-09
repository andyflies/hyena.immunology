# source("s.serology.barplots.year.r")


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
  
  df.temp$d.sample.year <- as.numeric(format(df.temp$d.sample.date, "%Y"))  # extract the year from the sample date and create a new variable to store the year
  df.temp <- df.temp[df.temp$d.sample.year > 1996,]                 # removing samples from earlier than 1997 becaues there are only 2 samples from 1993, no samples from 1994-1996
  
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
  ~ c.id + i.kay.code + c.sex + i.sample.number + d.sample.date + d.sample.year,
  data = df.temp, mean, na.action = na.pass)    # na.pass includes the na values, na.action = na.omit  removes all records that contain an NA             

i.cmi.cav2.igg <- c(200, 100)
i.cmi.cav2.igm <- c(200, 100)

i.cmi.ccv.igg <- c(200, 100)
i.cmi.ccv.igm <- c(200, 100)

i.cmi.cdv.igg <- c(200, 100)
i.cmi.cdv.igm <- c(200, 100)

i.cmi.dirofilaria.igg <- c(200, 100)
i.cmi.dirofilaria.igm <- c(200, 100)

i.cmi.fiv.p24.igg <- c(200, 100)
i.cmi.fiv.p24.igm <- c(200, 100)

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

c.target.antigens <- c("cav2", "ccv", "cdv", "fiv.p24", "dirofilaria")  # List of pathogens to calculate prevalence for
#c.target.antigens <- c("cav2", "ccv", "cdv", "cpv", "dirofilaria", "fcov", "fcv", "felv.p27", "fiv.gp95", "fiv.p24")  # List of pathogens to calculate prevalence for
# c.target.antigens <- c("cav2", "cdv", "cpv", "dirofilaria", "fcov", "fcv", "felv", "fiv")
# c.target.antigens <- c("cav2", "cdv", "cpv", "dirofilaria", "fcov", "fcv.sah", "felv.p27", "fiv.p24")
#c.target.antigens <- c("cav2", "cdv", "dirofilaria", "felv.p27", "fiv.p24")
c.target.names <- c("Canine adenovirus", "Canine coronavirus", "Canine distemper virus", "Feline immunodeficiency virus", "Dirofilaria")
# c.target.names <- c(expression(italic("Canine adenovirus")), expression(italic("Canine coronavirus")), 
#  expression(italic("Canine distemper virus")), expression(italic("Dirofilaria immitis")), expression(italic("Feline immunodeficiency virus")))

#c.target.names <- c("Canine adenovirus", "Canine coronavirus", "Canine distemper virus", "cpv",
#  "Dirofilaria immitis", "fcov", "fcv", "FeLV p27", "Feline immunodeficiency virus gp95", "Feline immunodeficiency virus p24")
  
# Loop to assign postive, low postive, negative status to each pathogen. A new variable is created for each pathogen  
i.counter <- 1
i.stop.counter <- length(c.target.antigens)
for(i.counter in i.counter:i.stop.counter)
{
  c.temp <- paste("i.cmi.", c.target.antigens[i.counter], ".igg", sep = "")
  print(c.temp)
#  c.temp.igg.status <- paste("c.cmi.", c.target.antigens[i.counter], ".igg.status", sep = "")
#  c.temp.igm.status <- paste("c.cmi.", c.target.antigens[i.counter], ".igm.status", sep = "")

  c.temp.igg.status <- as.character(df.temp.aggregate[, c.temp]) 
#  print(c.temp.igg.status)
#  print(c.temp.igm.status)
  # c.temp.igg.status <- as.character(df.temp.aggregate[, "i.cmi.dirofilaria.igg"]) 
  #c.temp.igg.status <- as.character(df.temp.aggregate$i.cmi.dirofilaria.igg) 
  c.temp.igg.status[df.temp.aggregate[, c.temp] >= m.serology.cutoff.values[c.temp, "high"]] <- "High positive"
  c.temp.igg.status[df.temp.aggregate[, c.temp] < m.serology.cutoff.values[c.temp, "high"]
   & df.temp.aggregate[, c.temp] > m.serology.cutoff.values[c.temp, "low"]] <- "Low positive"
  c.temp.igg.status[df.temp.aggregate[, c.temp] <= m.serology.cutoff.values[c.temp, "low"]] <- "Negative"
#  print(c.temp.igg.status)
  # c.temp <- paste("i.", "dirofilaria", ".igg.hl", sep = "")
#  c.temp <- paste("i.cmi.", c.target.antigens[i.counter], ".igg.hl", sep = "")

  
  print("")
  
  c.temp <- paste("i.cmi.", c.target.antigens[i.counter], ".igm", sep = "")
  print(c.temp)
  c.temp.igm.status <- as.character(df.temp.aggregate[, c.temp]) 
#  print(c.temp.igm.status)
  c.temp.igm.status[df.temp.aggregate[, c.temp] >= m.serology.cutoff.values[c.temp, "high"]] <- "High positive"
  c.temp.igm.status[df.temp.aggregate[, c.temp] < m.serology.cutoff.values[c.temp, "high"]
   & df.temp.aggregate[, c.temp] > m.serology.cutoff.values[c.temp, "low"]] <- "Low positive"
  c.temp.igm.status[df.temp.aggregate[, c.temp] <= m.serology.cutoff.values[c.temp, "low"]] <- "Negative"
#  print(c.temp.igm.status)    
#  df.temp.aggregate <- cbind(df.temp.aggregate, c.temp.igg.status, c.temp.igg.hl.status, c.temp.igm.status)
#  print(summary(df.temp.aggregate))

  if(i.counter == 1)
  {  
    df.igg.temp <- cbind(c.target.names[i.counter], "IgG", c.temp.igg.status, df.temp.aggregate$d.sample.year, as.character(df.temp.aggregate$c.id))
    df.igm.temp <- cbind(c.target.names[i.counter], "IgM", c.temp.igm.status, df.temp.aggregate$d.sample.year, as.character(df.temp.aggregate$c.id))
    df.counts <- data.frame(rbind(df.igg.temp, df.igm.temp))
    df.year <- 0
    colnames(df.counts) <- c("c.target", "c.isotype", "c.status", "d.year", "c.id")
#    colnames(df.counts) <- c("target", "isotype", "status")
#    df.counts <- df.counts[!is.na(df.counts$status), ] 
#    print(df.counts)
  }
  else
  {
    df.igg.temp <- cbind(c.target.names[i.counter], "IgG", c.temp.igg.status, df.temp.aggregate$d.sample.year, as.character(df.temp.aggregate$c.id))
    df.igm.temp <- cbind(c.target.names[i.counter], "IgM", c.temp.igm.status, df.temp.aggregate$d.sample.year, as.character(df.temp.aggregate$c.id))
    df.counts.temp <- data.frame(rbind(df.igg.temp, df.igm.temp))  
    colnames(df.counts.temp) <- c("c.target", "c.isotype", "c.status", "d.year", "c.id")
    df.counts <- rbind(df.counts, df.counts.temp)     
  }
}  

  df.counts <- df.counts[!is.na(df.counts$c.status), ] 
#  print(df.counts)
  df.counts$c.status <- factor(df.counts$c.status, levels = c("Negative", "Low positive", "High positive"))


  f.theme <- function(base_size = 12) {
    structure(list(
					axis.line =         theme_blank(),
					axis.text.x =       theme_text(size = base_size * 1, lineheight = 0.9, colour = "black", hjust = 0.7, vjust = 0.7, angle = 45, face = "bold"),
					axis.text.y =       theme_text(size = base_size * 1, lineheight = 0.9, colour = "black", hjust = 0.5, vjust = 0.5, face = "bold"),
					axis.ticks =        theme_segment(colour = "black", size = 0.5, linetype = 1),
#					axis.ticks =        theme_blank(),
					axis.title.x =      theme_text(size = base_size * 1.5, vjust = 1, face = "bold"),
					axis.title.y =      theme_text(size = base_size * 1.5, angle = 90, vjust = 0.35, face = "bold"),
#					axis.ticks.length = unit(0.3, "lines"),
#					axis.ticks.margin = unit(0, "lines"),
#          axis.ticks =        theme.blank(),
        					
					legend.background = theme_rect(colour = NA), 
					legend.key =        theme_rect(fill = NA, colour = NA),
					legend.key.size =   unit(1.2, "lines"),
					legend.text =       theme_text(size = base_size * 1.25, face = "bold"),
					legend.shape =      theme_text(size = base_size * 1, face = "bold"),
					legend.title =      theme_text(size = base_size * 1.5, hjust = 0, vjust = 0, face = "bold"),
					legend.position =   c(0.8, 0.35),
#					legend.position =   "right",
					
					panel.background =  theme_rect(fill = "white", colour = "black"), 
					panel.border =      theme_blank(), #theme_rect(fill = NA, colour = "black"), # 
					panel.grid.major =  theme_line(colour = NA, linetype = 2),
					panel.grid.minor =  theme_line(colour = NA, size = 0.25),
					panel.margin =      unit(1, "lines"),
					
					strip.background =  theme_rect(fill = "grey", colour = "black"), 
#					strip.label =       function(variable, value) expression(italic(value)), 
#					strip.label =       function(variable, value), 
					strip.text.x =      theme_text(size = base_size * 1.4, face = "bold.italic"),
					strip.text.y =      theme_text(size = base_size * 1, angle = -90),
					
					plot.background =   theme_rect(fill = NA, colour = NA),
					plot.title =        theme_text(size = base_size * 1), #, just = c(0.5, 0.5)),
					plot.margin =       unit(c(0.5, 0.25, 0.5, 0.25), "lines")      # 1 = top, 2, side, 3 = bottom, 4 = side
			), class = "options")
  }

  gg.serology <- ggplot(data = subset(df.counts, c.isotype == "IgG"), 
    aes(x = d.year,
      fill = factor(c.status),
      color = factor(c.status),
      legend = FALSE
    )
  )  

  print(gg.serology
    + geom_bar(position = "stack") 
    + scale_color_manual(name = "IgG serology status", values = c("High positive" = "black", "Low positive" = "black", "Negative" = "black"), legend = FALSE) 
    + scale_fill_manual(name = "IgG serology status", values = c("High positive" = "darkred", "Low positive" = "orangered2", "Negative" = "orange"), # legend = TRUE) 
      breaks = c("High positive", "Low positive", "Negative"), legend = TRUE) 
#    + opts(title = "Anti-DNP time course response") #
    + xlab("Year")
    + ylab("Number of Individuals")   
    + facet_wrap(~ c.target, ncol = 3, scales = "free")
    + f.theme()    
#    + geom_text(label = "n = 93", size = 5, x = 0.5, y = 0.5)
    + scale_x_discrete(breaks = 1997:2009, labels = c("1997", "", "1999", " ", "2001", "  ", "2003", "   ", "2005", "    ", "2007", "     ", "2009"))    
  )

#  gg.serology <- ggplot(data = subset(df.counts, c.isotype == "IgG"), 
#    aes(factor(d.year),
#      fill = factor(c.status),
#      color = factor(c.status),
#      legend = FALSE
#    )
#  )  

#  print(gg.serology
#    + geom_bar(position = "fill") 
#    + scale_color_manual(name = "Serology status", values = c("High positive" = "black", "Low positive" = "black", "Negative" = "black"), legend = FALSE) 
#    + scale_fill_manual(name = "Serology status", values = c("High positive" = "darkred", "Low positive" = "orangered2", "Negative" = "orange"), # legend = TRUE) 
#      breaks = c("High positive", "Low positive", "Negative"), legend = TRUE) 
##    + opts(title = "Anti-DNP time course response") #
#    + xlab("Year")
#    + ylab("Proportion")   
#    + facet_wrap(~ c.target, ncol = 3, scales = "free")
#    + f.theme()    
##    + geom_text(label = "n = 93", size = 5, x = 0.5, y = 0.5)
##    + scale_x_continuous(limits=c(0.5,5.5), breaks = c(1,2,3,4,5), label = c("0", "14", "28", "180", "365"))    
#  )

  f.theme <- function(base_size = 12) {
    structure(list(
					axis.line =         theme_blank(),
					axis.text.x =       theme_text(size = base_size * 1.5, lineheight = 0.9, colour = "black", hjust = 0.5, vjust = 1, angle = 0),
					axis.text.y =       theme_text(size = base_size * 1.4, lineheight = 0.9, colour = "black", hjust = 0.5, vjust = 0.5),
					axis.ticks =        theme_segment(colour = "black", size = 0.5, linetype = 1),
#					axis.ticks =        theme_blank(),
					axis.title.x =      theme_text(size = base_size * 1.5, vjust = 1, face = "bold"),
					axis.title.y =      theme_text(size = base_size * 1.5, angle = 90, vjust = 0, face = "bold"),
#					axis.ticks.length = unit(0.3, "lines"),
#					axis.ticks.margin = unit(0, "lines"),
#          axis.ticks =        theme.blank(),
        					
					legend.background = theme_rect(colour = NA), 
					legend.key =        theme_rect(fill = NA, colour = NA),
					legend.key.size =   unit(1.2, "lines"),
					legend.text =       theme_text(size = base_size * 1.2),
					legend.shape =      theme_text(size = base_size * 1.5, face = "bold"),
					legend.title =      theme_text(size = base_size * 1.5, hjust = 0, face = "bold"),
					legend.position =   c(0.67, 0.25),
#					legend.position =   "right",
					
					panel.background =  theme_rect(fill = "white", colour = "black"), 
					panel.border =      theme_blank(), #theme_rect(fill = NA, colour = "black"), # 
					panel.grid.major =  theme_line(colour = NA, linetype = 2),
					panel.grid.minor =  theme_line(colour = NA, size = 0.25),
					panel.margin =      unit(1, "lines"),
					
					strip.background =  theme_rect(fill = "grey", colour = "black"), 
#					strip.label =       function(variable, value) expression(italic(value)), 
#					strip.label =       function(variable, value), 
					strip.text.x =      theme_text(size = base_size * 1.4, face = "bold.italic"),
					strip.text.y =      theme_text(size = base_size * 1, angle = -90),
					
					plot.background =   theme_rect(fill = NA, colour = NA),
					plot.title =        theme_text(size = base_size * 1), #, just = c(0.5, 0.5)),
					plot.margin =       unit(c(0.5, 10, 0.5, 10), "lines")      # 1 = top, 2, side, 3 = bottom, 4 = side
			), class = "options")
  }

#  gg.serology <- ggplot(data = df.counts, 
#    aes(factor(c.isotype),
#      fill = factor(c.status),
#      color = factor(c.status),
#      legend = FALSE
#    )
#  )  

#  print(gg.serology
#    + geom_bar(position = "fill", width = 0.75) 
#    + scale_color_manual(name = "Serology status", values = c("High positive" = "black", "Low positive" = "black", "Negative" = "black"), legend = FALSE) 
#    + scale_fill_manual(name = "Serology status", values = c("High positive" = "darkred", "Low positive" = "orangered2", "Negative" = "orange"), # legend = TRUE) 
#      breaks = c("High positive", "Low positive", "Negative"), legend = TRUE) 
#    + opts(title = "Anti-DNP time course response") #
#    + xlab("Isotype")
#    + ylab("Proportion")   
#    + facet_wrap(~ c.target, ncol = 2, scales = "free")
#    + f.theme()    
##    + geom_text(label = "n = 93", size = 5, x = 0.5, y = 0.5)
##    + scale_x_continuous(limits=c(0.5,5.5), breaks = c(1,2,3,4,5), label = c("0", "14", "28", "180", "365"))    
#  )

#m.cov <- cov(df.temp.aggregate[, -c(1:5)], df.temp.aggregate[, -c(1:5)],  use = "pairwise.complete.obs")  # using -c(...) to exclude the non-numerical columns
#m.cor <- cor(df.temp.aggregate[, -c(1:5)], df.temp.aggregate[, -c(1:5)],  use = "pairwise.complete.obs")  # using -c(...) to exclude the non-numerical columns

#write.csv(m.cov, file = "serology.covariance.csv") 
#write.csv(m.cor, file = "serology.correlations.csv") 

