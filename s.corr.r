# source("s.corr.r")

df.temp <- subset(df.data.flat, c.age == "adult" & c.sex == "f",
  select = c("c.id", "i.sample.number.2", "i.rank", "d.mic.90.bka.ec", "d.mic.90.bka.pm", "d.blank.abs.total.igg",	"d.blank.abs.total.igm"))
print(length(df.temp$i.rank))
df.temp <- subset(df.temp, !(c.id == "mp" & i.sample.number.2 == 2))
df.temp <- subset(df.temp, !(c.id == "gol" & i.sample.number.2 == 3))
df.temp <- subset(df.temp, !(c.id == "cr" & i.sample.number.2 == 1))
df.temp <- subset(df.temp, !is.na(i.rank))
df.temp <- df.temp[, c("i.rank", "d.mic.90.bka.ec", "d.mic.90.bka.pm", "d.blank.abs.total.igg",	"d.blank.abs.total.igm")]
print(length(df.temp$i.rank))
print(df.temp)
print(cor(df.temp))


print(cor.test(df.temp$d.mic.90.bka.ec, df.temp$d.mic.90.bka.pm, method = "kendall", alternative = "two.sided"))
print(cor.test(df.temp$d.mic.90.bka.ec, df.temp$d.mic.90.bka.pm, method = "spearm", alternative = "two.sided"))
print(cor.test(df.temp$d.mic.90.bka.ec, df.temp$d.mic.90.bka.pm, method = "pearson", alternative = "two.sided"))

print(cor.test(df.temp$d.blank.abs.total.igg, df.temp$d.blank.abs.total.igm, method = "kendall", alternative = "two.sided"))
print(cor.test(df.temp$d.mic.90.bka.ec, df.temp$d.blank.abs.total.igm, method = "spearm", alternative = "two.sided"))
print(cor.test(df.temp$d.blank.abs.total.igg, df.temp$d.blank.abs.total.igm, method = "pearson", alternative = "two.sided"))

f.co.var<-function(x)(
  100*sd(x)/mean(x)
)

print("CV = 100*sd(x)/mean(x)")
print("CV of df.temp$d.mic.90.bka.ec")
print(f.co.var(df.temp$d.mic.90.bka.ec))
print("CV of df.temp$d.mic.90.bka.pm")
print(f.co.var(df.temp$d.mic.90.bka.pm))
print("CV of df.temp$d.blank.abs.total.igg")
print(f.co.var(df.temp$d.blank.abs.total.igg))
print("CV of df.temp$d.blank.abs.total.igm")
print(f.co.var(df.temp$d.blank.abs.total.igm))



