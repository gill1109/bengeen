setwd("/Users/richard/Desktop/FarewellLecture")

year <- rep(2000:2010, c(rep(12, 11))); year <- c(rep(1999,2), year, rep(2011,5))
month <- c(11:12, rep(1:12, 11), 1:5)
zeros <- c(rep("", 2), rep( c(rep("0",9), rep("",3)), 11), rep("0", 5))
data <- read.csv("1090 CCU.csv", header = FALSE, sep = ";")
data.mat <- t(as.matrix(data))
Resp <- as.integer(data.mat[3:141,17])
Cardio <- as.integer(data.mat[3:141,29])
Hypo <- as.integer(data.mat[3:141,41])
Adm <- as.integer(data.mat[3:141,48])
date <- paste(year, paste(zeros, month, sep = ""), sep="-")
Adm.ts <- ts(Adm, start = c(1999,11), frequency = 12)
# plot(Adm.ts, ylim = c(0, 800))
Cardio.ts <- ts(Cardio, start = c(1999,11), frequency = 12)
Resp.ts <- ts(Resp, start = c(1999,11), frequency = 12)
Hypo.ts <- ts(Hypo, start = c(1999,11), frequency = 12)
All.ts <- Cardio.ts + Resp.ts + Hypo.ts
januaries <- (month == 1)
# plot(All.ts, ylim = c(0,10))
# ts.plot(Adm.ts/1000, januaries.ts)
# ts.plot(All.ts/10, januaries.ts)
Adm.stl <- stl(Adm.ts, s.window = 21)
# plot(Adm.stl)
All <- Cardio + Resp + Hypo
Allper100Adm <- 100 * All / Adm
AllPer100Adm.ts <- ts(Allper100Adm,  start = c(1989,11), frequency = 12)
# plot(AllPer100Adm.ts)
AllPer100Adm.stl <- stl(AllPer100Adm.ts, s.window = 21)
# plot(AllPer100Adm.stl)
januaries.ts <- ts(januaries, start = c(1999,11), frequency = 12)
# ts.plot(Adm.ts/1000, januaries.ts)
# ts.plot(All.ts/10, januaries.ts)


fromCCtoEDwCRorHypo <- c(5, 0, 2, 0, 0, 2, 1, 1, 1, 1, 1, 1, 7, 3, 4)
Months <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"); 
    Months <- c("Dec", Months, "Jan", "Feb")
Year <- c("02", rep("03", 12), "04", "04")
Dates <- paste(Months, Year, sep="\n-")

fromCCtoEDwCRorHypo.FOI <- All[(year == 2002 & month == 12) | (year == 2003) | (year ==2004 & month <= 2) ]



plot(NULL, xlim=c(0.5,22.5), ylim=c(0,8), ylab="", xlab="",
    main = paste("Admissions to critical care from the emergency department, ",
    "with a diagnosis\nof cardio-respiratory arrest and/or hypoglycaemia, data: Head Nurse Block"), 
    axes = FALSE)
abline(h = c(0, 2, 4, 6))
barplot(height = fromCCtoEDwCRorHypo, names.arg = Dates, space = 0.5,
    axes = FALSE, add = TRUE)
axis(side = 2, at = c(0, 2, 4, 6, 8), labels = TRUE, tick = TRUE,
     pos = 0, outer = FALSE, font = NA, lty = "solid")
axis(side = 1, at = 1.5*(-1:20) - 1.25, labels = FALSE, tick = TRUE,
     pos = 0, outer = FALSE, font = NA, lty = "solid")


     
plot(NULL, xlim=c(0.5,22.5), ylim=c(0,8), ylab="", xlab="",
    main = paste("Admissions to critical care from the emergency department, ",
    "with a diagnosis\nof cardio-respiratory arrest and/or hypoglycaemia, data: FOI"), 
    axes = FALSE)
abline(h = c(0, 2, 4, 6))
barplot(height = fromCCtoEDwCRorHypo.FOI, names.arg = Dates, space = 0.5,
    axes = FALSE, add = TRUE)
axis(side = 2, at = c(0, 2, 4, 6, 8), labels = TRUE, tick = TRUE,
     pos = 0, outer = FALSE, font = NA, lty = "solid")
axis(side = 1, at = 1.5*(-1:20) - 1.25, labels = FALSE, tick = TRUE,
     pos = 0, outer = FALSE, font = NA, lty = "solid")
     
     
     
pdf("Barplot_Block.pdf", width = 9, height = 4.5)

graphics.off()



pdf("Barplot_FOI.pdf", width = 9, height = 4.5)

graphics.off()
