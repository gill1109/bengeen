library(Rgraphviz)
library(gRain)
yn <- c("yes", "no")
S <- cptable(~SerialKiller, values=c(1, 999999), levels = yn)
U <- cptable(~UnknownCause, values=c(1, 99), levels = yn)
E.S <- cptable(~EvidenceOneMurder:SerialKiller, values = c(99, 1, 1, 999), levels = yn)
C.SU <- cptable(~ClusterofEvents:SerialKiller:UnknownCause, values = c(999, 1, 1, 99, 999, 1, 1, 99999), levels = yn)
killerNurse <- compileCPT(S, U, C.SU, E.S)
killerNurse
killerNurse$SerialKiller
killerNurse$UnknownCause
killerNurse$EvidenceOneMurder
killerNurse$ClusterofEvents
killerNurse_bn <- grain(killerNurse)
killerNurse_bn <- propagate(killerNurse_bn)
killerNurse_bn

saveHuginNet(killerNurse_bn, "killerNurse.net")


##################################################################################
# No instantiation of evidence
querygrain(killerNurse_bn, nodes = c("SerialKiller", "UnknownCause"), type = "joint")
round(100*querygrain(killerNurse_bn, nodes = c("SerialKiller", "UnknownCause"), type = "joint"))



##################################################################################
# We observe no mysteriouus cluster and no evidence of a murder
killerNurse_bn0 <-  setEvidence(killerNurse_bn, evidence = list(ClusterofEvents="no", EvidenceOneMurder="no"))
querygrain(killerNurse_bn0, nodes = c("SerialKiller", "UnknownCause"), type = "joint")
round(100*querygrain(killerNurse_bn0, nodes = c("SerialKiller", "UnknownCause"), type = "joint"))
# 99% chance nothing going on at all. 1% chance that things have changed but we did not see evidence of it



##################################################################################
# We just observe a suspicious cluster of events involving our nurse
killerNurse_bn1 <-  setEvidence(killerNurse_bn, evidence = list(ClusterofEvents="yes"))
querygrain(killerNurse_bn1, nodes = c("SerialKiller", "UnknownCause"), type = "joint")
round(100*querygrain(killerNurse_bn1, nodes = c("SerialKiller", "UnknownCause"), type = "joint"))
# 90% chance something changed but our nurse is not a serial killer.
# 9% chance that actually nothing changed at all
# 1% chance we have a serial killer on our hands



##################################################################################
# We observe a suspicious cluster and strong evidence of wrong doing by our nurse on one particular patient
killerNurse_bn11 <-  setEvidence(killerNurse_bn, evidence = list(ClusterofEvents="yes", EvidenceOneMurder="yes"))
querygrain(killerNurse_bn11, nodes = c("SerialKiller", "UnknownCause"), type = "joint")
round(100*querygrain(killerNurse_bn11, nodes = c("SerialKiller", "UnknownCause"), type = "joint"))
# 90% chance our nurse is a serial killer
# 9% chance she is not a serial killer but something else caused the cluster of events
# 1% chance nothing is going on at all; 1% chance she's a serial killer and things changed


##################################################################################
# We observe a suspicious cluster but no strong evidence of wrong doing by our nurse on any one patient
killerNurse_bn12 <-  setEvidence(killerNurse_bn, evidence = list(ClusterofEvents="yes", EvidenceOneMurder="no"))
querygrain(killerNurse_bn12, nodes = c("SerialKiller", "UnknownCause"), type = "joint")
round(100*querygrain(killerNurse_bn12, nodes = c("SerialKiller", "UnknownCause"), type = "joint"))
# 91% chance something changed, the nurse is not a serial killer
# 9% chance nothing going on at all


png("barsS.png")
barplot(height = c(0, 100), names =c("Yes", "No"), horiz = "TRUE", col = c("blue", "orange"), xlim = c(0, 100), axes = FALSE, main = "S", ps = 1000)
graphics.off()
PictureS <- readPNG("barsS.png")

png("barsC.png")
barplot(height = c(30, 70), names =c("Yes", "No"), horiz = "TRUE", col = c("blue", "orange"), xlim = c(0, 100), axes = FALSE, main = "C", ps = 1000)
graphics.off()
PictureC <- readPNG("barsC.png")

png("barsU.png")
barplot(height = c(70, 30), names =c("Yes", "No"), horiz = "TRUE", col = c("blue", "orange"), xlim = c(0, 100), axes = FALSE, main = "U", ps = 1000)
graphics.off()
PictureU <- readPNG("barsU.png")

png("barsE.png")
barplot(height = c(100, 0), names =c("Yes", "No"), horiz = "TRUE", col = c("blue", "orange"), xlim = c(0, 100), axes = FALSE, main = "E", ps = 1000)
graphics.off()
PictureE <- readPNG("barsE.png")

g <- graph(c("S", "C", "U", "C", "S", "E"))
l <- matrix(data = c(20, 100, 50, 30, 110, 90, 150, 20), 4, 2, byrow = TRUE, dimnames = list(nodes = c("S", "C", "U", "E"), coord = c("x", "y")))
plot(g, vertex.shape = "rectangle", vertex.size	= 60, vertex.size2 = 30, layout = l, vertex.label = c("S", "C", "U", "E"))
rect(-1, -1, 1, 1)
rasterImage(PictureS, -0.75, -0.65, -0.35, -0.85)
rasterImage(PictureU, 1.2, -0.9, 0.8, -1.1)
rasterImage(PictureC, -1.2, 0.9, -0.8, 1.1)
rasterImage(PictureE, 0.6, 0.65, 0.2, 0.85)
