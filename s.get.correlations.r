# source("s.get.correlations.r")

set.seed(123)

df.temp <- subset(df.data.flat, select = c("c.id", "i.sample.number", "i.cmi.bsa.igg", "i.cmi.bsa.igm", "i.cmi.cav2.igg", 
  "i.cmi.cav2.igm", "i.corona.titer", "i.cmi.ccv.igg", "i.cmi.ccv.igm", "c.cdv", "i.cdv.titer", "i.cmi.cdv.igg", "i.cmi.cdv.igm", 
  "i.cmi.dirofilaria.igg", "i.cmi.dirofilaria.igm", 
  "i.cmi.fiv.p24.igg", "i.cmi.fiv.p24.igm"))
  
# df.temp <- subset(df.temp, !(c.id == "zc" & i.sample.number == 2)) 

df.temp$c.cdv <- as.character(df.temp$c.cdv)
df.temp$c.cdv[df.temp$c.cdv == "pos"] <- 1   # 1 = TRUE = positive
df.temp$c.cdv[df.temp$c.cdv == "neg"] <- 0   # 0 = FALSE = negative
df.temp$c.cdv <- as.integer(df.temp$c.cdv)

df.temp.2 <- subset(df.temp, select = c("i.cmi.bsa.igg", "i.cmi.bsa.igm", "i.cmi.cav2.igg", 
  "i.cmi.cav2.igm", "i.corona.titer", "i.cmi.ccv.igg", "i.cmi.ccv.igm", "i.cdv.titer", "i.cmi.cdv.igg", "i.cmi.cdv.igm", 
  "i.cmi.dirofilaria.igg", "i.cmi.dirofilaria.igm", 
  "i.cmi.fiv.p24.igg", "i.cmi.fiv.p24.igm"))
   
df.temp.3 <- df.temp.2
df.temp.3$i.cmi.cav2.igg <- df.temp.2$i.cmi.cav2.igg + df.temp.2$i.cmi.bsa.igg 
df.temp.3$i.cmi.cav2.igm <- df.temp.2$i.cmi.cav2.igg + df.temp.2$i.cmi.bsa.igm 
df.temp.3$i.cmi.ccv.igg <- df.temp.2$i.cmi.ccv.igg + df.temp.2$i.cmi.bsa.igg 
df.temp.3$i.cmi.ccv.igm <- df.temp.2$i.cmi.ccv.igg + df.temp.2$i.cmi.bsa.igm 
df.temp.3$i.cmi.cdv.igg <- df.temp.2$i.cmi.cdv.igg + df.temp.2$i.cmi.bsa.igg 
df.temp.3$i.cmi.cdv.igm <- df.temp.2$i.cmi.cdv.igg + df.temp.2$i.cmi.bsa.igm 
df.temp.3$i.cmi.dirofilaria.igg <- df.temp.2$i.cmi.dirofilaria.igg + df.temp.2$i.cmi.bsa.igg 
df.temp.3$i.cmi.dirofilaria.igm <- df.temp.2$i.cmi.dirofilaria.igg + df.temp.2$i.cmi.bsa.igm 
df.temp.3$i.cmi.fiv.p24.igg <- df.temp.2$i.cmi.fiv.p24.igg + df.temp.2$i.cmi.bsa.igg 
df.temp.3$i.cmi.fiv.p24.igm <- df.temp.2$i.cmi.fiv.p24.igg + df.temp.2$i.cmi.bsa.igm 
df.temp.3$c.cdv <- as.integer(df.temp$c.cdv) 

print(length(!is.na(df.temp.3$i.cmi.cdv.igg))) 
  
df.temp$c.cmi.cdv.igg <- df.temp$i.cmi.cdv.igg
df.temp$c.cmi.cdv <- NA   # 0 = FALSE = negative
df.temp$c.cmi.cdv[df.temp$i.cmi.cdv.igg >= 200 | df.temp$i.cmi.cdv.igm >= 150] <- 1   # 1 = TRUE = positive
df.temp$c.cmi.cdv[df.temp$i.cmi.cdv.igg < 200 & df.temp$i.cmi.cdv.igm < 150] <- 0   # 1 = TRUE = positive
df.temp$c.cmi.cdv.igg[df.temp$i.cmi.cdv.igg >= 100] <- 1   # 1 = TRUE = positive
df.temp$c.cmi.cdv.igg[df.temp$i.cmi.cdv.igg < 100] <- 0   # 0 = FALSE = negative
df.temp$c.cmi.cdv.igm[df.temp$i.cmi.cdv.igm >= 75] <- 1   # 1 = TRUE = positive
df.temp$c.cmi.cdv.igm[df.temp$i.cmi.cdv.igm < 75] <- 0   # 0 = FALSE = negative

df.temp.2$c.cdv <- as.integer(df.temp$c.cdv)  
df.temp.2$c.cmi.cdv.igg <- as.integer(df.temp$c.cmi.cdv.igg)  
df.temp.2$c.cmi.cdv <- as.integer(df.temp$c.cmi.cdv)  

print(df.temp)

print("cor(df.temp.2, df.temp.2)")
m.cor <- cor(df.temp.2[], df.temp.2[],  use = "pairwise.complete.obs")
print(m.cor)
print("cor(df.temp.3, df.temp.3)")
m.cor <- cor(df.temp.3[,2:10], df.temp.3[,2:10],  use = "pairwise.complete.obs")
print(m.cor)
# write.csv(m.cor, file = "correlations.csv") 


# par(mfrow = c(2,2))
# print("**************** ccv ************************")
# lm.1 <- lm(log(i.corona.titer) ~ (i.cmi.ccv.igg), data = df.temp)
# print(summary(lm.1))
# plot(lm.1)
# hist(resid(lm.1))
# print("***************** lillie.test *************************************")
# print(lillie.test(resid(lm.1)))
# print(ad.test(resid(lm.1)))
# print(cvm.test(resid(lm.1)))

# lmer.0 <- lmer(log(i.corona.titer) ~ (1 | c.id), data = df.temp, na.action = "na.omit", REML = FALSE)
# print(summary(lmer.0))
# print("cftest(lmer.0)")
# print(cftest(lmer.0))
# hist(resid(lmer.0))
# qqnorm(resid(lmer.0)) #, main = paste("qqnorm\n", (dimnames(lmer.alt.1@frame[1])[[2]]), " ~ ", (dimnames(lmer.alt.1@frame[2])[[2]])))
# qqline(resid(lmer.0))

# lmer.1 <- lmer(log(i.corona.titer) ~ (i.cmi.ccv.igg) + (1 | c.id), data = df.temp, na.action = "na.omit", REML = FALSE)
# print(summary(lmer.1))
# print("cftest(lmer.1)")
# print(cftest(lmer.1))
# hist(resid(lmer.1))
# qqnorm(resid(lmer.1)) #, main = paste("qqnorm\n", (dimnames(lmer.alt.1@frame[1])[[2]]), " ~ ", (dimnames(lmer.alt.1@frame[2])[[2]])))
# qqline(resid(lmer.1))

# print(length(resid(lmer.1)))

# print(anova(lmer.0, lmer.1))

# print("***************** lillie.test *************************************")
# print(lillie.test(resid(lmer.1)))
# print(ad.test(resid(lmer.1)))
# print(cvm.test(resid(lmer.1)))

# print(cor.test(df.temp$i.corona.titer, df.temp$i.cmi.ccv.igg, use = "pairwise.complete.obs", method = "pearson"))
# print(cor.test(df.temp$i.corona.titer, df.temp$i.cmi.ccv.igg, use = "pairwise.complete.obs", method = "kendall"))
# print(cor.test(df.temp$i.corona.titer, df.temp$i.cmi.ccv.igg, use = "pairwise.complete.obs", method = "spearman"))


par(mfrow = c(4,4))
print("**************** cdv ************************")
lm.1 <- lm((i.cmi.cdv.igg) ~ log(i.cdv.titer), data = df.temp)
print(summary(lm.1))
plot(lm.1)
hist(resid(lm.1))
print("***************** lillie.test *************************************")
print(lillie.test(resid(lm.1)))
print(ad.test(resid(lm.1)))
print(cvm.test(resid(lm.1)))

lmer.1 <- lmer((i.cmi.cdv.igg) ~ log(i.cdv.titer) + (1 | c.id), data = df.temp, na.action = "na.omit", REML = FALSE)
print(summary(lmer.1))
print(lmer.1)
print("cftest(lmer.1)")
print(cftest(lmer.1))
hist(resid(lmer.1))
qqnorm(resid(lmer.1)) #, main = paste("qqnorm\n", (dimnames(lmer.alt.1@frame[1])[[2]]), " ~ ", (dimnames(lmer.alt.1@frame[2])[[2]])))
qqline(resid(lmer.1))
str(lmer.1)

print("***************** lillie.test *************************************")
print(lillie.test(resid(lmer.1)))
print(ad.test(resid(lmer.1)))
print(cvm.test(resid(lmer.1)))

par(mfrow = c(1,1))
plot(y = df.temp$i.cmi.cdv.igg, x = df.temp$i.cdv.titer)
plot(y = df.temp$i.cmi.cdv.igg, x = log(df.temp$i.cdv.titer))
abline(a = fixef(lmer.1)[[1]], b = fixef(lmer.1)[[2]])
plot(y = log(df.temp$i.cmi.cdv.igg), x = log(df.temp$i.cdv.titer), )
plot(y = log(df.temp$i.cmi.cdv.igg), x = (df.temp$i.cdv.titer))


# l.ct.cdv <- (cor.test(log(df.temp$i.cdv.titer), df.temp$i.cmi.cdv.igg, use = "pairwise.complete.obs", method = "pearson"))
# print(l.ct.cdv)
# l.ct.cdv <- (cor.test((df.temp$i.cdv.titer), df.temp$i.cmi.cdv.igg, use = "pairwise.complete.obs", method = "pearson"))
# print(l.ct.cdv)
print(str(cor.test(log(df.temp$i.cdv.titer), df.temp$i.cmi.cdv.igg, use = "pairwise.complete.obs", method = "pearson")))
print(cor.test(log(df.temp$i.cdv.titer), df.temp$i.cmi.cdv.igg, use = "pairwise.complete.obs", method = "kendall"))
print(cor.test(log(df.temp$i.cdv.titer), df.temp$i.cmi.cdv.igg, use = "pairwise.complete.obs", method = "spearman"))

f.boot.replace <- function(i.input.1, i.input.2) {
  df.input <- data.frame(i.input.1, i.input.2)
  df.input <- subset(df.input, !is.na(df.input[,1]) & !is.na(df.input[,2]))
  i.indices <- 1:length(df.input[,1])
  i.sample.indices <- sample(i.indices, replace = TRUE)
#  print(cbind(df.input[i.sample.indices,1], df.input[i.sample.indices,2]))
  l.cor.test <- cor.test(df.input[i.sample.indices,1], df.input[i.sample.indices,2], use = "pairwise.complete.obs", method = "pearson")
  d.cor.stat <- l.cor.test$estimate
}


l.ct <- cor.test(log(df.temp$i.cdv.titer), df.temp$i.cmi.cdv.igg, use = "pairwise.complete.obs", method = "pearson")
print(l.ct)
i.replicates <- 1000                      
d.test.stats <- replicate(i.replicates, f.boot.replace(log(df.temp$i.cdv.titer), df.temp$i.cmi.cdv.igg))
# print(d.test.stats)
d.boot.ci.lower <- quantile(sort(d.test.stats), probs = 0.05 / 2 )   # compute the lower CI bound by finding the lenght of the vector and then returning the vector at 2.5 percentile
d.boot.ci.upper <- quantile(sort(d.test.stats), probs = 1 - (0.05 / 2) )   # compute the lower CI bound by finding the lenght of the vector and then returning the vector at 2.5 percentile
print(c(d.boot.ci.lower, d.boot.ci.upper))
hist(d.test.stats, main = "boot of cor.test for i.cdv.titer and i.cmi.cdv.igg")
points(1 ~ l.ct$estimate, pch = 8)

f.theme <- function(base_size = 12) {
  structure(list(
				axis.line =         theme_blank(),
				axis.text.x =       theme_text(size = base_size * 1.2, lineheight = 0.9, colour = "black", angle = 0, face = "bold"),
				axis.text.y =       theme_text(size = base_size * 1.2, lineheight = 0.9, colour = "black", face = "bold"),
				axis.ticks =        theme_segment(colour = "black", size = 0.5, linetype = 1),
#					axis.ticks =        theme_blank(),
				axis.title.x =      theme_text(size = base_size * 1.4, vjust = 0, face = "bold"),
				axis.title.y =      theme_text(size = base_size * 1.4, angle = 90, vjust = 0.25, face = "bold"),
#					axis.ticks.length = unit(0.3, "lines"),
#					axis.ticks.margin = unit(0, "lines"),
#          axis.ticks =        theme.blank(),
      					
				legend.background = theme_rect(colour = NA), 
				legend.key =        theme_rect(fill = NA, colour = NA),
				legend.key.size =   unit(1.2, "lines"),
				legend.text =       theme_text(size = base_size * 1.25, face = "bold"),
				legend.shape =      theme_text(size = base_size * 1, face = "bold"),
				legend.title =      theme_text(size = base_size * 1.5, hjust = 0, vjust = 0, face = "bold"),
				legend.position =   c(0.85, 0.55),
#					legend.position =   "right",
				
				panel.background =  theme_rect(fill = "white", colour = "black"), 
				panel.border =      theme_blank(), #theme_rect(fill = NA, colour = "black"), # 
				panel.grid.major =  theme_line(colour = NA, linetype = 2),
				panel.grid.minor =  theme_line(colour = NA, size = 0.25),
				panel.margin =      unit(1, "lines"),
				
				strip.background =  theme_rect(fill = "grey", colour = "black"), 
#					strip.label =       function(variable, value) expression(italic(value)), 
#					strip.label =       function(variable, value), 
				strip.text.x =      theme_text(size = base_size * 1.25, face = "bold.italic"),
				strip.text.y =      theme_text(size = base_size * 1, angle = -90),
				
				plot.background =   theme_rect(fill = NA, colour = NA),
				plot.title =        theme_text(size = base_size * 1.5, face = "bold", vjust = 1), #, just = c(0.5, 0.5)),
				plot.margin =       unit(c(7.5, 14, 7.5, 14), "lines")      # 1 = top, 2, side, 3 = bottom, 4 = side
		), class = "options")
}

df.plot <- subset(df.temp, !is.na(i.cdv.titer) & !is.na(i.cmi.cdv.igg), select = c(i.cdv.titer, i.cmi.cdv.igg))
# a = fixef(lmer.1)[[1]], b = fixef(lmer.1)[[2]])
l.cor.test <- cor.test(log(df.temp$i.cdv.titer), df.temp$i.cmi.cdv.igg, use = "pairwise.complete.obs", method = "pearson")
print(l.cor.test)
print(l.cor.test$estimate)
print(l.cor.test$p.value)
print(l.cor.test$conf.int)
print(l.cor.test$conf.int[1])
print(l.cor.test$conf.int[2])

df.plot$estimate <- paste("Pearson's correlation (r) = ", signif(l.cor.test$estimate, digits = 3), sep = "")
df.plot$p.value <- paste("p value = ", signif(l.cor.test$p.value, digits = 3), sep = "")
df.plot$conf.int <- paste("95% C.I. of r = [ ", signif(l.cor.test$conf.int[1], digits = 3), ", ", signif(l.cor.test$conf.int[2], digits = 3), " ]", sep = "")
#  + scale_color_manual(name = "Serology status", values = c("High positive" = "black",
df.plot$conf.int.low <- signif(l.cor.test$conf.int[1], digits = 4)
df.plot$conf.int.high <- signif(l.cor.test$conf.int[2], digits = 4)

print("df.plot")
print(df.plot)

gg.serology <- ggplot(data = df.plot, 
  aes(x = log(i.cdv.titer),
    y = i.cmi.cdv.igg,
    legend = FALSE
  )
)  

print(gg.serology
  + geom_point(size = 3) 
  + geom_abline(intercept = fixef(lmer.1)[[1]], slope = fixef(lmer.1)[[2]])
  + geom_text(aes(x2, y2, label = text.here), data.frame(x2 = 3, y2 = 550, text.here = df.plot$estimate), hjust = 0, size = 6, face = "bold")
  + geom_text(aes(x2, y2, label = text.here), data.frame(x2 = 3, y2 = 485, text.here = df.plot$p.value), hjust = 0, size = 6)
  + geom_text(aes(x2, y2, label = text.here), data.frame(x2 = 3, y2 = 420, text.here = df.plot$conf.int), hjust = 0, size = 6)
#  + scale_color_manual(name = "Serology status", values = c("High positive" = "black", "Low positive" = "black", "Negative" = "black"), legend = FALSE) 
#  + scale_fill_manual(name = "Serology status", values = c("High positive" = "darkred", "Low positive" = "orangered2", "Negative" = "orange"), # legend = TRUE) 
#    breaks = c("High positive", "Low positive", "Negative"), legend = TRUE) 
  + opts(title = "Assay comparison") #
#  + scale_x_discrete(breaks = 1997:2009, labels = c("1997", "", "1999", " ", "2001", "  ", "2003", "   ", "2005", "    ", "2007", "     ", "2009"))    
  + xlab("Ln (SNT titer)")
  + ylab("ELISA titer")   
#  + facet_wrap(~ c.target + c.isotype, ncol = 2, scales = "free")
  + f.theme()    
#    + geom_text(label = "n = 93", size = 5, x = 0.5, y = 0.5)
)
