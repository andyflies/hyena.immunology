# source("f.ggplot.bacteria.elisa.blank.abs.mean.sample.r")
                   
f.ggplot.bacteria.elisa.blank.abs.mean.sample <- function(df.input.data.frame)
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
   
#  df.igg.temp <- subset(df.master.total.ig, c.target == "IgG" & (c.id == "com" | c.id == "cr" | c.id == "jj"),
#    select = c(c.id, c.target, c.sex, c.age, i.rank, d.months.to.wean,  i.plate.id, i.row, i.column, d.blank.abs))   
#  df.igm.temp <- subset(df.master.total.ig, c.target == "IgM" &  (c.id == "art" | c.id == "coo" | c.id == "hml"),
#    select = c(c.id, c.target, c.sex, c.age, i.rank, d.months.to.wean, i.plate.id, i.row, i.column, d.blank.abs))
#  print(df.igg.temp)
#  print(df.igm.temp)  

#  df.igg.data <- subset(df.igg.data, !(c.id == "cr" & i.plate.id == 4 & i.row == 6 & i.column == 4 ))
#  df.igg.data <- subset(df.igg.data, !(c.id == "jj" & i.plate.id == 3 & i.row == 2 & i.column == 2 ))
#  df.igm.data <- subset(df.igm.data, !(c.id == "cr" & i.plate.id == 4 & i.row == 6 & i.column == 4 ))
#  df.igm.data <- subset(df.igm.data, !(c.id == "coo" & i.plate.id == 4 & i.row == 1 & i.column == 1 ))
#  print(df.igg.temp)
#  print(df.igm.temp)  
  
  df.ec.igg.data <- subset(df.master.bacteria.elisa, c.target == "ec.8739" & c.sex == "f" & c.age == "adult")
  df.pm.igg.data <- subset(df.master.bacteria.elisa, c.target == "pm.35659" &  c.sex == "f" & c.age == "adult")

  
  ########################### aggregate and get the means #################
  df.ec.igg.mean <- aggregate(cbind(i.rank, d.months.to.wean, d.blank.abs, d.age.months) ~ c.id 
    + c.target
#    + i.sample.number 
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
    data = df.ec.igg.data, mean, na.action = na.pass)
      
  df.pm.igg.mean <- aggregate(cbind(i.rank, d.months.to.wean, d.blank.abs, d.age.months) ~ c.id 
    + c.target
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
    data = df.pm.igg.data, mean, na.action = na.pass)
     
  ########################### aggregate and get the stderror #################
  df.ec.igg.stderr <- aggregate(cbind(i.rank, d.months.to.wean, d.blank.abs, d.age.months) ~ c.id 
    + c.target
#    + i.sample.number 
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
    data = df.ec.igg.data, std.error, na.action = na.pass)
  
  df.pm.igg.stderr <- aggregate(cbind(i.rank, d.months.to.wean, d.blank.abs, d.age.months) ~ c.id 
    + c.target
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
    data = df.pm.igg.data, std.error, na.action = na.pass) 
    
  d.stderr <- df.ec.igg.stderr$d.blank.abs
  df.ec.igg <- cbind(df.ec.igg.mean, d.stderr)
  print(df.ec.igg)  

  d.stderr <- df.pm.igg.stderr$d.blank.abs
  df.pm.igg <- cbind(df.pm.igg.mean, d.stderr)
  print(df.pm.igg)

  df.ec.igg <- with(df.ec.igg, f.sort.frame(df.ec.igg, i.rank, c.id))
  df.pm.igg <- with(df.pm.igg, f.sort.frame(df.pm.igg, i.rank, c.id))
  df.ig <- rbind(df.ec.igg, df.pm.igg)

  print((df.ec.igg)) 
  print((df.pm.igg)) 
  print(df.ig)
    
  gg.means <- ggplot(data = df.ig, 
    aes(x = i.rank, 
        y = d.blank.abs
#      color = factor(i.sample.number),
#      fill = factor(c.adjuvant), 
#      y = d.blank.abs.total.ig, 
#      y.max = max(d.blank.abs)), 
#    size = 0.5) 
    ),  
  )
     
  print(gg.means
    + geom_point() # , position = "jitter")
#    + geom_jitter(aes(x = i.rank))
    + stat_smooth(method = "lm", se = TRUE, size = 2)
#    + geom_point(aes(x = d.blank.abs, y = d.blank.abs.total.ig), size = 3.5, legend = FALSE)
#    + scale_colour_manual(name = "sample number", values = c("1" = "black", "2" = "red", "3" = "blue"), legend = TRUE)
    + geom_errorbar(aes(x = i.rank,
      ymax = d.blank.abs + d.stderr, 
      ymin = d.blank.abs - d.stderr), width = 0) 
    + opts(title = "Anti-bacterial IgG") #, color = "Andy")
    + xlab("Rank")
    + ylab("Absorbance (450nm)")   
    + facet_wrap(~ c.target, nrow = 2, scales = "free")
    + f.theme() 
  )  
  
  par(mfrow=c(2,3))
  lm.rank <- lm(d.blank.abs ~ i.rank, data = df.ec.igg)
  print(summary(lm.rank))
  plot(lm.rank)
  hist(resid(lm.rank))
  plot(d.blank.abs ~ i.rank, data = df.ec.igg)
  ols.rank <- ols(d.blank.abs ~ i.rank, data = df.ec.igg)
  print(ols.rank)
  abline(ols.rank)
  
  par(mfrow=c(2,3))
  lm.rank <- lm(d.blank.abs ~ i.rank, data = df.pm.igg)
  print(summary(lm.rank))
  plot(lm.rank)
  hist(resid(lm.rank))
  plot(d.blank.abs ~ i.rank, data = df.pm.igg)
  ols.rank <- ols(d.blank.abs ~ i.rank, data = df.pm.igg)
  print(ols.rank)
  abline(ols.rank)

       
  par(mfrow=c(2,1))
#  plot(d.blank.abs ~ i.rank, data = df.igg)
#  abline(h = 1)
#  plot(d.blank.abs ~ i.rank, data = df.igm)
#  abline(a = 1)
  plot(x = 1, y = 1)
}  
