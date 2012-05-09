# source("f.ggplot.ig.blank.abs.one.sample.r")
                   
f.ggplot.ig.blank.abs.one.sample <- function(df.input.data.frame)
{
  f.theme <- function(base_size = 12) {
    structure(list(
					axis.line =         theme_blank(),
					axis.text.x =       theme_text(size = base_size * 1.2, lineheight = 0.9, colour = "black", hjust = 0.5, vjust = 1, angle = 0),
					axis.text.y =       theme_text(size = base_size * 1.2, lineheight = 0.9, colour = "black", hjust = 0.5, vjust = 0.5),
					axis.ticks =        theme_segment(colour = "black", size = 0.5, linetype = 1),
#					axis.ticks =        theme_blank(),
					axis.title.x =      theme_text(size = base_size * 1.25, vjust = 1, face = "bold"),
					axis.title.y =      theme_text(size = base_size * 1.25, angle = 90, vjust = 0.35, face = "bold"),
#					axis.ticks.length = unit(0.3, "lines"),
#					axis.ticks.margin = unit(0, "lines"),
#          axis.ticks =        theme.blank(),
        					
					legend.background = theme_rect(colour = NA), 
					legend.key =        theme_rect(fill = NA, colour = NA),
					legend.key.size =   unit(1.2, "lines"),
					legend.text =       theme_text(size = base_size * 0.8),
					legend.title =      theme_text(size = base_size * 1, hjust = 0, face = "bold"),
					legend.position =   "right",
					
					panel.background =  theme_rect(fill = NA, colour = "black"), 
					panel.border =      theme_blank(), #theme_rect(fill = NA, colour = "black"), # 
					panel.grid.major =  theme_line(colour = NA, linetype = 2),
					panel.grid.minor =  theme_line(colour = NA, size = 0.25),
					panel.margin =      unit(1, "lines"),
					
					strip.background =  theme_rect(fill = "grey", colour = "black"), 
					strip.label =       function(variable, value) value, 
					strip.text.x =      theme_text(size = base_size * 1.25, face = "bold"),
					strip.text.y =      theme_text(size = base_size * 1.25, angle = -90),
					
					plot.background =   theme_rect(colour = NA),
					plot.title =        theme_text(size = base_size * 2, face = "bold"), #, just = c(0.5, 0.5)),
					plot.margin =       unit(c(0.5, 0.25, 0.5, 0.25), "lines")      # 1 = top, 2, side, 3 = bottom, 4 = side
			), class = "options")
  }
   
#  df.igg.temp <- subset(df.master.total.ig, c.isotype.target == "IgG" & (c.id == "com" | c.id == "cr" | c.id == "jj"),
#    select = c(c.id, c.isotype.target, c.sex, c.age, i.rank, d.months.to.wean,  i.plate.id, i.row, i.column, d.blank.abs.corrected))   
#  df.igm.temp <- subset(df.master.total.ig, c.isotype.target == "IgM" &  (c.id == "art" | c.id == "coo" | c.id == "hml"),
#    select = c(c.id, c.isotype.target, c.sex, c.age, i.rank, d.months.to.wean, i.plate.id, i.row, i.column, d.blank.abs.corrected))
#  print(df.igg.temp)
#  print(df.igm.temp)  

#  df.igg.data <- subset(df.igg.data, !(c.id == "cr" & i.plate.id == 4 & i.row == 6 & i.column == 4 ))
#  df.igg.data <- subset(df.igg.data, !(c.id == "jj" & i.plate.id == 3 & i.row == 2 & i.column == 2 ))
#  df.igm.data <- subset(df.igm.data, !(c.id == "cr" & i.plate.id == 4 & i.row == 6 & i.column == 4 ))
#  df.igm.data <- subset(df.igm.data, !(c.id == "coo" & i.plate.id == 4 & i.row == 1 & i.column == 1 ))
#  print(df.igg.temp)
#  print(df.igm.temp)  
  
  df.igg.data <- subset(df.master.total.ig, c.isotype.target == "IgG" & c.sex == "f" & c.age == "adult" & i.sample.number.2 == 2)   # Get only the first sample from each individual for the E. coli data
  df.igm.data <- subset(df.master.total.ig, c.isotype.target == "IgM" &  c.sex == "f" & c.age == "adult" & i.sample.number.2 == 2)   # Get only the first sample from each individual for the P. mirabilis data

  
  ########################### aggregate and get the means #################
  df.igg.mean <- aggregate(cbind(i.rank, d.months.to.wean, d.blank.abs.corrected) ~ c.id 
    + c.isotype.target
 #   + i.sample.number 
    + c.sex 
#    + d.sample.date
  #  + c.mom
  #  + d.dob
    + c.age
  #  + d.age.months 
  #  + d.mass
  #  + c.clan
 #   + c.natal.immigrant
  #  + i.maternal.rank.at.birth
  #  + i.maternal.rank.at.sample
#    + i.rank
  #  + i.litter.rank
  #  + i.number.in.litter
#    + d.minutes.blood
  #  + i.glucose.green
  #  + i.glucose.red
  #  + d.pcv
  #  + d.total.solids
  #  + c.body.condition
  #  + c.matched.subadult.adult
  #  + c.one.adult.samples
  #  + c.two.adult.samples
  #  + c.three.adult.samples
    , 
    data = df.igg.data, mean, na.action = na.pass)
      
  df.igm.mean <- aggregate(cbind(i.rank, d.months.to.wean, d.blank.abs.corrected) ~ c.id 
    + c.isotype.target
#    + i.sample.number 
    + c.sex 
#    + d.sample.date
  #  + c.mom
  #  + d.dob
    + c.age
  #  + d.age.months 
  #  + d.mass
  #  + c.clan
#    + c.natal.immigrant
  #  + i.maternal.rank.at.birth
  #  + i.maternal.rank.at.sample
#    + i.rank
  #  + i.litter.rank
  #  + i.number.in.litter
#    + d.minutes.blood
  #  + i.glucose.green
  #  + i.glucose.red
  #  + d.pcv
  #  + d.total.solids
  #  + c.body.condition
  #  + c.matched.subadult.adult
  #  + c.one.adult.samples
  #  + c.two.adult.samples
  #  + c.three.adult.samples
    , 
    data = df.igm.data, mean, na.action = na.pass)
     
  ########################### aggregate and get the stderror #################
  df.igg.stderr <- aggregate(cbind(i.rank, d.months.to.wean, d.blank.abs.corrected) ~ c.id 
    + c.isotype.target
 #   + i.sample.number 
    + c.sex 
#    + d.sample.date
  #  + c.mom
  #  + d.dob
    + c.age
  #  + d.age.months 
  #  + d.mass
  #  + c.clan
 #   + c.natal.immigrant
  #  + i.maternal.rank.at.birth
  #  + i.maternal.rank.at.sample
#    + i.rank
  #  + i.litter.rank
  #  + i.number.in.litter
#    + d.minutes.blood
  #  + i.glucose.green
  #  + i.glucose.red
  #  + d.pcv
  #  + d.total.solids
  #  + c.body.condition
  #  + c.matched.subadult.adult
  #  + c.one.adult.samples
  #  + c.two.adult.samples
  #  + c.three.adult.samples
    , 
    data = df.igg.data, std.error, na.action = na.pass)
  
  df.igm.stderr <- aggregate(cbind(i.rank, d.months.to.wean, d.blank.abs.corrected) ~ c.id 
    + c.isotype.target
#    + i.sample.number 
    + c.sex 
#    + d.sample.date
  #  + c.mom
  #  + d.dob
    + c.age
  #  + d.age.months 
  #  + d.mass
  #  + c.clan
#    + c.natal.immigrant
  #  + i.maternal.rank.at.birth
  #  + i.maternal.rank.at.sample
#    + i.rank
  #  + i.litter.rank
  #  + i.number.in.litter
#    + d.minutes.blood
  #  + i.glucose.green
  #  + i.glucose.red
  #  + d.pcv
  #  + d.total.solids
  #  + c.body.condition
  #  + c.matched.subadult.adult
  #  + c.one.adult.samples
  #  + c.two.adult.samples
  #  + c.three.adult.samples
    , 
    data = df.igm.data, std.error, na.action = na.pass) 
    
  d.stderr <- df.igg.stderr$d.blank.abs.corrected
  df.igg <- cbind(df.igg.mean, d.stderr)
  print(df.igg)  

  d.stderr <- df.igm.stderr$d.blank.abs.corrected
  df.igm <- cbind(df.igm.mean, d.stderr)
  print(df.igm)

  df.igg <- with(df.igg, f.sort.frame(df.igg, i.rank, c.id))
  df.igm <- with(df.igm, f.sort.frame(df.igm, i.rank, c.id))
  df.ig <- rbind(df.igg, df.igm)

  print((df.igg)) 
  print((df.igm)) 
  print(df.ig)
    
  gg.means <- ggplot(data = df.ig, 
    aes(x = i.rank, 
        y = d.blank.abs.corrected)
#      color = factor(c.adjuvant),
#      fill = factor(c.adjuvant), 
#      y = d.blank.abs.total.ig, 
#      y.max = max(d.blank.abs.corrected)), 
#    size = 0.5) 
    )
     
  print(gg.means
    + geom_point(aes(x = i.rank))
    + stat_smooth(method = "lm", se = TRUE, size = 2)
#    + geom_point(aes(x = d.blank.abs.corrected, y = d.blank.abs.total.ig), size = 3.5, legend = FALSE)
#    + scale_colour_manual(name = "Adjuvant", values = c("NO" = "grey", "YES" = "black"), legend = TRUE)
    + geom_errorbar(aes(x = i.rank, 
      ymax = d.blank.abs.corrected + d.stderr, 
      ymin = d.blank.abs.corrected - d.stderr), width = 0) 
    + opts(title = "Total immunoglobulins") #, color = "Andy")
    + xlab("Rank")
    + ylab("Absorbance (450nm)")   
    + facet_wrap(~ c.isotype.target, nrow = 2, scales = "free")
    + f.theme() 
  )  
  
  par(mfrow=c(2,3))
  lm.rank <- lm(d.blank.abs.corrected ~ i.rank, data = df.igg)
  print(summary(lm.rank))
  plot(lm.rank)
  hist(resid(lm.rank))
  plot(d.blank.abs.corrected ~ i.rank, data = df.igg)
  ols.rank <- ols(d.blank.abs.corrected ~ i.rank, data = df.igg)
  print(ols.rank)
  abline(ols.rank)
  
  par(mfrow=c(2,3))
  lm.rank <- lm(d.blank.abs.corrected ~ i.rank, data = df.igm)
  print(summary(lm.rank))
  plot(lm.rank)
  hist(resid(lm.rank))
  plot(d.blank.abs.corrected ~ i.rank, data = df.igm)
  ols.rank <- ols(d.blank.abs.corrected ~ i.rank, data = df.igm)
  print(ols.rank)
  abline(ols.rank)
   
  par(mfrow=c(2,1))
#  plot(d.blank.abs.corrected ~ i.rank, data = df.igg)
#  abline(h = 1)
#  plot(d.blank.abs.corrected ~ i.rank, data = df.igm)
#  abline(a = 1)
  plot(x=1)
}  
