# source("s.plots.r")

par(mfrow=c(2,4))
plot(data = subset(df.bka.ec, c.age = "adult"), d.mic.90.level ~ c.sex, main = "df.bka.ec")
plot(data = subset(df.ec.igg, c.age = "adult"), d.blank.abs ~ c.sex, main = "df.ec.igg")
plot(data = subset(df.pm.igg, c.age = "adult"), d.blank.abs ~ c.sex, main = "df.pm.igg")
plot(data = subset(df.total.igg, c.age = "adult"), d.blank.abs ~ c.sex, main = "df.total.igg")

plot(data = subset(df.bka.pm, c.age = "adult"), d.mic.90.level ~ c.sex, main = "df.bka.pm")
plot(data = subset(df.ec.igm, c.age = "adult"), d.blank.abs ~ c.sex, main = "df.ec.igm")
plot(data = subset(df.pm.igm, c.age = "adult"), d.blank.abs ~ c.sex, main = "df.pm.igm")
plot(data = subset(df.total.igm, c.age = "adult"), d.blank.abs ~ c.sex, main = "df.total.igm")

plot(data = subset(df.master.wbc, c.age = "adult"), d.relative.eosinophils ~ c.sex, main = "df.master.wbc - d.relative.eosinophils")
plot(data = subset(df.master.wbc, c.age = "adult"), d.relative.lymphocytes ~ c.sex, main = "df.master.wbc - d.relative.lymphocytes")
plot(data = subset(df.master.wbc, c.age = "adult"), d.relative.monocytes ~ c.sex, main = "df.master.wbc - d.relative.monocytes")
plot(data = subset(df.master.wbc, c.age = "adult"), d.relative.neutrophils ~ c.sex, main = "df.master.wbc - d.relative.neutrophils")

plot(data = subset(df.master.wbc, c.age = "adult"), d.eosinophils ~ c.sex, main = "df.master.wbc - d.eosinophils")
plot(data = subset(df.master.wbc, c.age = "adult"), d.lymphocytes ~ c.sex, main = "df.master.wbc - d.lymphocytes")
plot(data = subset(df.master.wbc, c.age = "adult"), d.monocytes ~ c.sex, main = "df.master.wbc - d.monocytes")
plot(data = subset(df.master.wbc, c.age = "adult"), d.neutrophils ~ c.sex, main = "df.master.wbc - d.neutrophils")

plot(data = subset(df.master.wbc, c.age = "adult" & c.sex == "f"), d.relative.eosinophils ~ i.rank, main = "df.master.wbc - d.relative.eosinophils")
plot(data = subset(df.master.wbc, c.age = "adult" & c.sex == "f"), d.relative.lymphocytes ~ i.rank, main = "df.master.wbc - d.relative.lymphocytes")
plot(data = subset(df.master.wbc, c.age = "adult" & c.sex == "f"), d.relative.monocytes ~ i.rank, main = "df.master.wbc - d.relative.monocytes")
plot(data = subset(df.master.wbc, c.age = "adult" & c.sex == "f"), d.relative.neutrophils ~ i.rank, main = "df.master.wbc - d.relative.neutrophils")

plot(data = subset(df.master.wbc, c.age = "adult" & c.sex == "f"), d.eosinophils ~ i.rank, main = "df.master.wbc - d.eosinophils")
plot(data = subset(df.master.wbc, c.age = "adult" & c.sex == "f"), d.lymphocytes ~ i.rank, main = "df.master.wbc - d.lymphocytes")
plot(data = subset(df.master.wbc, c.age = "adult" & c.sex == "f"), d.monocytes ~ i.rank, main = "df.master.wbc - d.monocytes")
plot(data = subset(df.master.wbc, c.age = "adult" & c.sex == "f"), d.neutrophils ~ i.rank, main = "df.master.wbc - d.neutrophils")









