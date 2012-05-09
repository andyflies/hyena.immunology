# source("s.convert.variables.r")

df.data$c.id <- as.factor(df.data$c.id)
df.data$i.kay.code <- as.factor(df.data$i.kay.code)
df.data$c.sex <- as.factor(df.data$c.sex)
df.data$i.sample.number <- as.factor(df.data$i.sample.number)
df.data$d.sample.date <- as.Date(as.character(df.data$d.sample.date), "%d-%b-%Y")
# df.data$i.well.number.x <- as.integer(df.data$i.well.number.x)
df.data$i.plate.id <- as.integer(df.data$i.plate.id)
df.data$d.current.date  <- as.Date(as.character(df.data$d.current.date), "%d-%b-%Y")
df.data$d.current.time <- as.double(df.data$d.current.time)   #as.Date(as.character(df.data$d.current.time), "%H:%M")
df.data$d.start.date <- as.Date(as.character(df.data$d.start.date), "%d-%b-%Y")
df.data$d.start.time <- as.double(df.data$d.start.time)
df.data$i.time.point <- as.integer(df.data$i.time.point)
df.data$d.elapsed.time <- as.double(df.data$d.elapsed.time)
df.data$c.strain <- as.factor(df.data$c.strain)
df.data$c.reference.number <- as.factor(df.data$c.reference.number)
df.data$d.cfu.per.ml <- as.double(df.data$d.cfu.per.ml)
df.data$d.actual.cfu.ml <- as.double(df.data$d.actual.cfu.ml)
df.data$i.sample.dilution <- as.integer(df.data$i.sample.dilution)
df.data$d.antibiotic.concentration <- as.double(df.data$d.antibiotic.concentration)
df.data$c.blank <- as.factor(df.data$c.blank)
df.data$i.row <- as.integer(df.data$i.row)
df.data$i.column <- as.integer(df.data$i.column)
df.data$d.raw.abs <- as.double(df.data$d.raw.abs)
df.data$d.blank.abs <- as.double(df.data$d.blank.abs)
df.data$d.diff.abs <- as.double(df.data$d.diff.abs)
df.data$d.plate.control.mean.abs <- as.double(df.data$d.plate.control.mean.abs)
df.data$d.percent.control.abs <- as.double(df.data$d.percent.control.abs)
df.data$c.50.percent.inhibit <- as.factor(df.data$c.50.percent.inhibit)
df.data$c.90.percent.inhibit <- as.factor(df.data$c.90.percent.inhibit)
df.data$d.mic.50 <- as.double(df.data$d.mic.50)
df.data$d.mic.90 <- as.double(df.data$d.mic.90)
df.data$d.mic.50.level <- as.double(df.data$d.mic.50.level)
df.data$d.mic.90.level <- as.double(df.data$d.mic.90.level)
df.data$i.blind.number <- as.integer(df.data$i.blind.number)
df.data$c.mom <- as.factor(df.data$c.mom)
df.data$d.dob <- as.Date(as.character(df.data$d.dob), "%d-%b-%Y")
df.data$c.age <- as.factor(df.data$c.age)
df.data$d.age.months <- as.double(df.data$d.age.months)
df.data$d.estimated.age.months <- as.double(df.data$d.estimated.age.months)
df.data$c.reproductive.status <- as.factor(df.data$c.reproductive.status)
df.data$d.mass <- as.double(df.data$d.mass)
df.data$c.clan <- as.factor(df.data$c.clan)
df.data$c.natal.immigrant <- as.factor(df.data$c.natal.immigrant)
df.data$i.maternal.rank.at.birth <- as.integer(df.data$i.maternal.rank.at.birth)
df.data$i.maternal.rank.at.sample <- as.integer(df.data$i.maternal.rank.at.sample)
df.data$i.rank <- as.integer(df.data$i.rank)
df.data$i.litter.rank <- as.integer(df.data$i.litter.rank)
df.data$i.number.in.litter <- as.integer(df.data$i.number.in.litter)
df.data$c.clan.split.adjusted <- as.factor(df.data$c.clan.split.adjusted)
df.data$d.den.grad <- as.Date(as.character(df.data$d.den.grad), "%d-%b-%Y")
df.data$d.months.to.grad <- as.double(df.data$d.months.to.grad)
df.data$d.disappeared <- as.Date(as.character(df.data$d.disappeared), "%d-%b-%Y")
df.data$c.fate <- as.factor(df.data$c.fate)
df.data$c.mortality.source <- as.factor(df.data$c.mortality.source)
df.data$d.death.date <- as.Date(as.character(df.data$d.death.date), "%d-%b-%Y")
df.data$d.weaned <- as.Date(as.character(df.data$d.weaned), "%d-%b-%Y")
df.data$d.months.to.wean <- as.double(df.data$d.months.to.wean)
df.data$i.thaw.cycles <- as.integer(df.data$i.thaw.cycles)
df.data$d.open.date.1 <- as.Date(as.character(df.data$d.open.date.1), "%d-%b-%Y")
df.data$d.open.date.1 <- as.Date(as.character(df.data$d.open.date.1), "%d-%b-%Y")
df.data$d.open.date.2 <- as.Date(as.character(df.data$d.open.date.2), "%d-%b-%Y")
df.data$d.darting.time <- as.double(df.data$d.darting.time)
df.data$d.time.down <- as.double(df.data$d.time.down)
df.data$d.blood.time <- as.double(df.data$d.blood.time)
df.data$d.minutes.blood <- as.double(df.data$d.minutes.blood)
df.data$c.parasites <- as.factor(df.data$c.parasites)
df.data$i.glucose.green <- as.integer(df.data$i.glucose.green)
df.data$i.glucose.red <- as.integer(df.data$i.glucose.red)
df.data$d.pcv <- as.double(df.data$d.pcv)
df.data$d.total.solids <- as.double(df.data$d.total.solids)
df.data$c.body.condition <- as.factor(df.data$c.body.condition)
df.data$c.matched.subadult.adult <- as.factor(df.data$c.matched.subadult.adult)
df.data$c.one.adult.samples <- as.factor(df.data$c.one.adult.samples)
df.data$c.two.adult.samples <- as.factor(df.data$c.two.adult.samples)
df.data$c.three.adult.samples <- as.factor(df.data$c.three.adult.samples)
df.data$d.time.series.date <- as.Date(as.character(df.data$d.time.series.date), "%d-%b-%Y")
df.data$c.time.series.lps <- as.factor(df.data$c.time.series.lps)
df.data$i.gps.utme <- as.integer(df.data$i.gps.utme)
df.data$i.gps.utmn <- as.integer(df.data$i.gps.utmn)
df.data$c.wbc.slide <- as.factor(df.data$c.wbc.slide)
df.data$d.vol <- as.double(df.data$d.vol)
df.data$c.sample.type <- as.factor(df.data$c.sample.type)
df.data$c.filtered <- as.factor(df.data$c.filtered)
df.data$d.serology.date <- as.Date(as.character(df.data$d.serology.date), "%d-%b-%Y")
df.data$c.cdv <- as.factor(as.character(df.data$c.cdv))
df.data$i.cdv.titer <- as.integer(df.data$i.cdv.titer)
df.data$c.calici <- as.factor(as.character(df.data$c.calici))
df.data$i.calici.titer <- as.integer(df.data$i.calici.titer)
df.data$c.corona <- as.factor(as.character(df.data$c.corona))
df.data$i.corona.titer <- as.integer(df.data$i.corona.titer)
df.data$c.fiv <- as.factor(as.character(df.data$c.fiv))
df.data$c.fiv.neg <- as.factor(as.character(df.data$c.fiv.neg))
df.data$c.fiv.hi.neg <- as.factor(as.character(df.data$c.fiv.hi.neg))
df.data$c.panleuk <- as.factor(as.character(df.data$c.panleuk))
df.data$i.panleuk.titer <- as.integer(df.data$i.panleuk.titer)
df.data$i.group.number <- as.integer(df.data$i.group.number)
df.data$c.id.2 <- as.factor(df.data$c.id.2)
# df.data$i.well.number.y <- as.integer(df.data$i.well.number.y)
# df.data$d.elisa.igg.hl.date <- as.Date(as.character(df.data$d.elisa.igg.hl.date), "%d-%b-%Y")
# df.data$d.elisa.igg.date <- as.Date(as.character(df.data$d.elisa.igg.date), "%d-%b-%Y")
# df.data$d.elisa.igm.date <- as.Date(as.character(df.data$d.elisa.igm.date), "%d-%b-%Y")
# df.data$i.cmi.bcv.igg.hl <- as.integer(df.data$i.cmi.bcv.igg.hl)
# df.data$i.cmi.bcv.igg <- as.integer(df.data$i.cmi.bcv.igg)
# df.data$i.cmi.bcv.igm <- as.integer(df.data$i.cmi.bcv.igm)
# df.data$i.cmi.bsa.igg.hl <- as.integer(df.data$i.cmi.bsa.igg.hl)
# df.data$i.cmi.cav2.igg.hl <- as.integer(df.data$i.cmi.cav2.igg.hl)
df.data$i.cmi.bsa.igg <- as.integer(df.data$i.cmi.bsa.igg)
df.data$i.cmi.bsa.igm <- as.integer(df.data$i.cmi.bsa.igm)
df.data$i.cmi.cav2.igg <- as.integer(df.data$i.cmi.cav2.igg)
df.data$i.cmi.cav2.igm <- as.integer(df.data$i.cmi.cav2.igm)
# df.data$i.cmi.ccv.igg.hl <- as.integer(df.data$i.cmi.ccv.igg.hl)
df.data$i.cmi.ccv.igg <- as.integer(df.data$i.cmi.ccv.igg)
df.data$i.cmi.ccv.igm <- as.integer(df.data$i.cmi.ccv.igm)
# df.data$i.cmi.cdv.igg.hl <- as.integer(df.data$i.cmi.cdv.igg.hl)
df.data$i.cmi.cdv.igg <- as.integer(df.data$i.cmi.cdv.igg)
df.data$i.cmi.cdv.igm <- as.integer(df.data$i.cmi.cdv.igm)
# df.data$i.cmi.cpiv.igg.hl <- as.integer(df.data$i.cmi.cpiv.igg.hl)
# df.data$i.cmi.cpiv.igg <- as.integer(df.data$i.cmi.cpiv.igg)
# df.data$i.cmi.cpiv.igm <- as.integer(df.data$i.cmi.cpiv.igg)
# df.data$i.cmi.cpv.igg.hl <- as.integer(df.data$i.cmi.cpv.igg.hl)
# df.data$i.cmi.cpv.igg <- as.integer(df.data$i.cmi.cpv.igg)
# df.data$i.cmi.cpv.igm <- as.integer(df.data$i.cmi.cpv.igm)
# df.data$i.cmi.dirofilaria.igg.hl <- as.integer(df.data$i.cmi.dirofilaria.igg.hl)
df.data$i.cmi.dirofilaria.igg <- as.integer(df.data$i.cmi.dirofilaria.igg)
df.data$i.cmi.dirofilaria.igm <- as.integer(df.data$i.cmi.dirofilaria.igm)
# df.data$i.cmi.fcov.igg.hl <- as.integer(df.data$i.cmi.fcov.igg.hl)
# df.data$i.cmi.fcov.igg <- as.integer(df.data$i.cmi.fcov.igg)
# df.data$i.cmi.fcov.igm <- as.integer(df.data$i.cmi.fcov.igm)
# df.data$i.cmi.fcv.sah.igg.hl <- as.integer(df.data$i.cmi.fcv.sah.igg.hl)
# df.data$i.cmi.fcv.sah.igg <- as.integer(df.data$i.cmi.fcv.sah.igg)
# df.data$i.cmi.fcv.sah.igm <- as.integer(df.data$i.cmi.fcv.sah.igm)
# df.data$i.cmi.fcv.fd.igg.hl <- as.integer(df.data$i.cmi.fcv.fd.igg.hl)
# df.data$i.cmi.fcv.fd.igg <- as.integer(df.data$i.cmi.fcv.fd.igg)
# df.data$i.cmi.fcv.fd.igm <- as.integer(df.data$i.cmi.fcv.fd.igm)
# df.data$i.cmi.felv.gp70.igg.hl <- as.integer(df.data$i.cmi.felv.gp70.igg.hl)
# df.data$i.cmi.felv.gp70.igg <- as.integer(df.data$i.cmi.felv.gp70.igg)
# df.data$i.cmi.felv.gp70.igm <- as.integer(df.data$i.cmi.felv.gp70.igm)
# df.data$i.cmi.felv.p27.igg.hl <- as.integer(df.data$i.cmi.felv.p27.igg.hl)
# df.data$i.cmi.felv.p27.igg <- as.integer(df.data$i.cmi.felv.p27.igg)
# df.data$i.cmi.felv.p27.igm <- as.integer(df.data$i.cmi.felv.p27.igm)
# df.data$i.cmi.fiv.34TF10.igg.hl <- as.integer(df.data$i.cmi.fiv.34TF10.igg.hl)
# df.data$i.cmi.fiv.34TF10.igg <- as.integer(df.data$i.cmi.fiv.34TF10.igg)
# df.data$i.cmi.fiv.34TF10.igm <- as.integer(df.data$i.cmi.fiv.34TF10.igm)
# df.data$i.cmi.fiv.gp95.igg.hl <- as.integer(df.data$i.cmi.fiv.gp95.igg.hl)
# df.data$i.cmi.fiv.gp95.igg <- as.integer(df.data$i.cmi.fiv.gp95.igg)
# df.data$i.cmi.fiv.gp95.igm <- as.integer(df.data$i.cmi.fiv.gp95.igm)
# df.data$i.cmi.fiv.p24.igg.hl <- as.integer(df.data$i.cmi.fiv.p24.igg.hl)
df.data$i.cmi.fiv.p24.igg <- as.integer(df.data$i.cmi.fiv.p24.igg)
df.data$i.cmi.fiv.p24.igm <- as.integer(df.data$i.cmi.fiv.p24.igm)
# df.data$i.cmi.herpes.sah.igg.hl <- as.integer(df.data$i.cmi.herpes.sah.igg.hl)
# df.data$i.cmi.fhv.fvr.kz.igm <- as.integer(df.data$i.cmi.fhv.fvr.kz.igm)
# df.data$i.cmi.herpes.solvay.igg.hl <- as.integer(df.data$i.cmi.herpes.solvay.igg.hl)
# df.data$i.cmi.hiv.gp120.igg.hl <- as.integer(df.data$i.cmi.hiv.gp120.igg.hl)
# df.data$i.cmi.hiv.gp120.igg <- as.integer(df.data$i.cmi.hiv.gp120.igg)
# df.data$i.cmi.hiv.gp120.igm <- as.integer(df.data$i.cmi.hiv.gp120.igm)
# df.data$i.cmi.siv.igg.hl <- as.integer(df.data$i.cmi.siv.igg.hl)
# df.data$i.cmi.siv.igg <- as.integer(df.data$i.cmi.siv.igg)
# df.data$i.cmi.siv.igm <- as.integer(df.data$i.cmi.siv.igm)
df.data$c.knotts.microfilaria <- as.character(df.data$c.knotts.microfilaria)
df.data$c.scars.wounds <- as.character(df.data$c.scars.wounds)
df.data$c.dart.sheet.comments <- as.character(df.data$c.dart.sheet.comments)

