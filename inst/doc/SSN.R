### R code from vignette source 'SSN.Rnw'

###################################################
### code chunk number 1: SSN.Rnw:143-147
###################################################
## Code to run in the background
set.seed(210112)
options(prompt = "R> ", continue = "+  ", width = 70, useFancyQuotes = FALSE)
Stangle("SSN.Rnw") ## Dump all R code to a file


###################################################
### code chunk number 2: SSN.Rnw:195-196
###################################################
library("SSN")


###################################################
### code chunk number 3: SSN.Rnw:199-202
###################################################
file.copy(system.file("lsndata/MiddleFork04.ssn", package = "SSN"),
  to = tempdir(), recursive = TRUE, copy.mode = FALSE)
setwd(tempdir())


###################################################
### code chunk number 4: SSN.Rnw:724-725 (eval = FALSE)
###################################################
## importSSN(Path, predpts = NULL, o.write = FALSE)


###################################################
### code chunk number 5: SSN.Rnw:733-735
###################################################
mf04p <- importSSN("./MiddleFork04.ssn",
   predpts = "pred1km")


###################################################
### code chunk number 6: SSN.Rnw:751-753
###################################################
mf04p <- importPredpts(mf04p, "Knapp", "ssn")
mf04p <- importPredpts(mf04p, "CapeHorn", "ssn")


###################################################
### code chunk number 7: SSN.Rnw:801-802 (eval = FALSE)
###################################################
## additive.function(mf04p, VarName, afvName)


###################################################
### code chunk number 8: SSN.Rnw:815-816
###################################################
names(mf04p@data)


###################################################
### code chunk number 9: SSN.Rnw:820-821
###################################################
head(mf04p@data[, c("h2oAreaKm2", "afvArea")])


###################################################
### code chunk number 10: SSN.Rnw:825-826
###################################################
mf04p <- additive.function(mf04p, "h2oAreaKm2", "computed.afv")


###################################################
### code chunk number 11: SSN.Rnw:830-831
###################################################
names(mf04p@data)


###################################################
### code chunk number 12: SSN.Rnw:835-838
###################################################
head(mf04p@data[, c("h2oAreaKm2",
   "afvArea", "computed.afv")])
head(getSSNdata.frame(mf04p)[, c("afvArea", "computed.afv")])


###################################################
### code chunk number 13: SSN.Rnw:880-884
###################################################
createDistMat(mf04p, predpts = "Knapp", o.write = TRUE,
	amongpreds = TRUE)
createDistMat(mf04p, predpts = "CapeHorn", o.write = TRUE,
	amongpreds = TRUE)


###################################################
### code chunk number 14: SSN.Rnw:899-902
###################################################
distObs <- getStreamDistMat(mf04p)
str(distObs)
distObs$dist.net1[1:5,1:5]


###################################################
### code chunk number 15: SSN.Rnw:905-907
###################################################
strDistNet2 <- distObs$dist.net2 + t(distObs$dist.net2)
strDistNet2[5:10,5:10]


###################################################
### code chunk number 16: SSN.Rnw:910-913
###################################################
distPred1km <- getStreamDistMat(mf04p, Name = "pred1km")
str(distPred1km)
distPred1km$dist.net1.a[1:5,1:5]


###################################################
### code chunk number 17: SSN.Rnw:916-921
###################################################
createDistMat(mf04p, predpts = "CapeHorn", o.write = TRUE, 
  amongpreds = TRUE)
distCape <- getStreamDistMat(mf04p, Name = "CapeHorn")
str(distCape)
distCape$dist.net2[1:5,1:5]


###################################################
### code chunk number 18: SSN.Rnw:948-949
###################################################
names(mf04p)


###################################################
### code chunk number 19: LoadData
###################################################
plot(mf04p, lwdLineCol = "afvArea", lwdLineEx = 10, lineCol = "blue",
   pch = 19, xlab = "x-coordinate (m)", ylab = "y-coordinate (m)",
   asp = 1)


###################################################
### code chunk number 20: plotSpatialStreamNetwork
###################################################
brks <- plot(mf04p, "Summer_mn", lwdLineCol = "afvArea",
   lwdLineEx = 15, lineCol = "black", xlab =  "x-coordinate" ,
   ylab =  "y-coordinate", asp=1 )


###################################################
### code chunk number 21: plotUsingSP
###################################################
#plot the stream lines
plot(as.SpatialLines(mf04p), col = "blue")
# add the observed locations with size proportional 
# to mean summer temperature
plot(as.SpatialPoints(mf04p), pch = 19, 
  cex = as.SpatialPointsDataFrame(mf04p)$Summer_mn/9 , add = TRUE)
# add the prediction locations on the 1 km spacing
plot(as.SpatialPoints(mf04p, data = "pred1km"), cex = 1.5, add = TRUE)
# add the dense set of points for block prediction on Knapp segment
plot(as.SpatialPoints(mf04p, data = "Knapp"), pch = 19, cex = 0.3, 
  col = "red", add = TRUE)


###################################################
### code chunk number 22: Torgegram
###################################################
mf04.Torg <- Torgegram(mf04p, "Summer_mn", nlag = 20, maxlag = 50000)
plot(mf04.Torg)


###################################################
### code chunk number 23: GauModel0
###################################################
mf04.glmssn0 <- glmssn(Summer_mn ~ ELEV_DEM + SLOPE, mf04p,
   CorModels = NULL, use.nugget = TRUE)
summary(mf04.glmssn0)


###################################################
### code chunk number 24: lm0
###################################################
summary(lm(Summer_mn ~ ELEV_DEM + SLOPE, getSSNdata.frame(mf04p)))


###################################################
### code chunk number 25: GauModel1
###################################################
mf04.glmssn1 <- glmssn(Summer_mn ~ ELEV_DEM + SLOPE, mf04p,
   CorModels = c("Exponential.tailup", "Exponential.taildown",
      "Exponential.Euclid"), addfunccol = "afvArea")
summary(mf04.glmssn1)


###################################################
### code chunk number 26: BinModel1
###################################################
mf04.glmssnBin <- glmssn(MaxOver20 ~ ELEV_DEM + SLOPE, mf04p,
  CorModels = c("Mariah.tailup", "Spherical.taildown"),
  family = "binomial", addfunccol = "afvArea")
summary(mf04.glmssnBin)


###################################################
### code chunk number 27: PoiModel1
###################################################
mf04.glmssnPoi <- glmssn(C16 ~ ELEV_DEM + SLOPE, mf04p,
  CorModels = c("LinearSill.tailup", "LinearSill.taildown"),
  family = "poisson", addfunccol = "afvArea")
summary(mf04.glmssnPoi)


###################################################
### code chunk number 28: Model1
###################################################
mf04.resid1 <- residuals(mf04.glmssn1)
names( getSSNdata.frame(mf04.resid1) )
plot(mf04.resid1)


###################################################
### code chunk number 29: ResidHist
###################################################
par(mfrow = c(1, 2))
hist(mf04.resid1)
hist(mf04p, "Summer_mn")


###################################################
### code chunk number 30: SSN.Rnw:1203-1208
###################################################
ObsDFr <- getSSNdata.frame(mf04.resid1)
ObsDF <- getSSNdata.frame(mf04p)
indOutlier <- ObsDFr["_resid_"] < -3
ObsDF[indOutlier, "Summer_mn"] <- NA
mf04c <- putSSNdata.frame(ObsDF, mf04p)


###################################################
### code chunk number 31: SSN.Rnw:1213-1217
###################################################
mf04c.glmssn0 <- glmssn(Summer_mn ~ ELEV_DEM + SLOPE, mf04c,
   CorModels = c("Exponential.tailup", "Exponential.taildown",
   "Exponential.Euclid"), addfunccol = "afvArea", EstMeth = "ML")
summary(mf04c.glmssn0)


###################################################
### code chunk number 32: SSN.Rnw:1222-1226
###################################################
mf04c.glmssn1 <- glmssn(Summer_mn ~ ELEV_DEM, mf04c,
   CorModels = c("Exponential.tailup", "Exponential.taildown"),
   addfunccol = "afvArea", EstMeth = "ML")
summary(mf04c.glmssn1)


###################################################
### code chunk number 33: LOOCV
###################################################
cv.out <- CrossValidationSSN(mf04c.glmssn1)
par(mfrow = c(1, 2))
plot(mf04c.glmssn1$sampinfo$z,
   cv.out[, "cv.pred"], pch = 19,
   xlab = "Observed Data", ylab = "LOOCV Prediction")
abline(0, 1)
plot( na.omit( getSSNdata.frame(mf04c)[, "Summer_mn"]),
   cv.out[, "cv.se"], pch = 19,
   xlab = "Observed Data", ylab = "LOOCV Prediction SE")


###################################################
### code chunk number 34: LOOCVSummary
###################################################
CrossValidationStatsSSN(mf04c.glmssn1)


###################################################
### code chunk number 35: SSN.Rnw:1280-1282
###################################################
GR2(mf04c.glmssn1)
varcomp(mf04c.glmssn1)


###################################################
### code chunk number 36: SSN.Rnw:1299-1301
###################################################
AIC(mf04c.glmssn0)
AIC(mf04c.glmssn1)


###################################################
### code chunk number 37: SSN.Rnw:1309-1324
###################################################
mf04c.glmssn1 <- glmssn(Summer_mn ~ ELEV_DEM, mf04c,
   CorModels = c("Exponential.tailup", "Exponential.taildown"),
   addfunccol = "afvArea")
mf04c.glmssn2 <- glmssn(Summer_mn ~ ELEV_DEM,  mf04c,
   CorModels = c("LinearSill.tailup", "Mariah.taildown"),
   addfunccol = "afvArea")
mf04c.glmssn3 <- glmssn(Summer_mn ~ ELEV_DEM , mf04c,
   CorModels =  c("Mariah.tailup", "LinearSill.taildown"),
   addfunccol = "afvArea")
mf04c.glmssn4 <- glmssn(Summer_mn ~ ELEV_DEM, mf04c,
   CorModels = c("Spherical.tailup", "Spherical.taildown"),
   addfunccol = "afvArea")
mf04c.glmssn5 <- glmssn(Summer_mn ~ ELEV_DEM, mf04c,
   CorModels = "Exponential.Euclid",
   addfunccol = "afvArea")


###################################################
### code chunk number 38: SSN.Rnw:1332-1336
###################################################
options(digits = 4)
InfoCritCompare(list(mf04c.glmssn1, mf04c.glmssn2,
   mf04c.glmssn3, mf04c.glmssn4, mf04c.glmssn5))
options(digits = 7)


###################################################
### code chunk number 39: SSN.Rnw:1348-1349
###################################################
summary(mf04c.glmssn2)


###################################################
### code chunk number 40: Residuals
###################################################
mf04c.resid2 <- residuals(mf04c.glmssn2)
mf04c.resid2.cv.std <-
    getSSNdata.frame(mf04c.resid2)[, "_resid.crossv_"] /
    getSSNdata.frame(mf04c.resid2)[, "_CrossValStdErr_"]
hist(mf04c.resid2.cv.std)


###################################################
### code chunk number 41: TorgRes
###################################################
plot(Torgegram(mf04c.resid2, "_resid_", nlag = 8, maxlag = 25000))


###################################################
### code chunk number 42: Preds1
###################################################
mf04c.pred1km <- predict(mf04c.glmssn4, "pred1km")
plot(mf04c.pred1km, SEcex.max = 1, SEcex.min = .5/3*2,
     breaktype = "user", brks = brks)


###################################################
### code chunk number 43: Preds2
###################################################
plot(mf04c, "Summer_mn", pch = 1, cex = 3,
   xlab = "x-coordinate", ylab = "y-coordinate",
   xlim = c(-1511000,-1500000), ylim = c(2525000,2535000))
mf04c.glmssn4.Knapp <- predict(mf04c.glmssn4, "Knapp")
plot(mf04c.glmssn4.Knapp, "Summer_mn", add = TRUE,
   xlim = c(-1511000,-1500000), ylim = c(2525000,2535000))


###################################################
### code chunk number 44: SSN.Rnw:1464-1466
###################################################
mf04c.glmssn4.BPKnapp <- BlockPredict(mf04c.glmssn4, "Knapp")
mf04c.glmssn4.BPKnapp


###################################################
### code chunk number 45: SSN.Rnw:1470-1472
###################################################
mf04c.glmssn4.BPCapeHorn <- BlockPredict(mf04c.glmssn4, "CapeHorn")
mf04c.glmssn4.BPCapeHorn


###################################################
### code chunk number 46: SSN.Rnw:1485-1488
###################################################
mf04c.missingobs <- predict(mf04c.glmssn4, "_MissingObs_")
getPreds(mf04c.missingobs, pred.type = "pred")
with(getSSNdata.frame(mf04p), Summer_mn[pid==29])


###################################################
### code chunk number 47: SSN.Rnw:1535-1537 (eval = FALSE)
###################################################
## createSSN(n, obsDesign, predDesign = noPoints, path,
##    importToR = FALSE, treeFunction = igraphKamadaKawai)


###################################################
### code chunk number 48: SimIterative
###################################################
set.seed(12)
iterative.ssn <- createSSN(n = c(30, 10),
   obsDesign = binomialDesign(c(10,10)),
   importToR = TRUE, path = "./SimIterative.ssn",
   treeFunction = iterativeTreeLayout)
plot(iterative.ssn, lwdLineCol = "addfunccol", lwdLineEx = 8,
   lineCol = "blue", cex = 2, xlab = "x-coordinate",
   ylab = "y-coordinate", pch = 1)


###################################################
### code chunk number 49: SimSSN1
###################################################
set.seed(101)
raw.ssn <- createSSN(n = c(10, 10, 10),
   obsDesign = binomialDesign(c(40, 40, 40)),
   predDesign = systematicDesign(c(0.2, 0.4, 0.8)), importToR = TRUE,
   path = "./raw.ssn")
plot(raw.ssn, lwdLineCol = "addfunccol", lwdLineEx = 8,
   lineCol = "blue", cex = 2, xlab = "x-coordinate",
   ylab = "y-coordinate", pch = 1)
plot(raw.ssn, PredPointsID = "preds", add = TRUE, cex = .5, pch = 19,
   col = "green")


###################################################
### code chunk number 50: SimHardcore
###################################################
set.seed(13)
hardcore.ssn <- createSSN(n = c(10, 10),
   obsDesign = hardCoreDesign(c(200, 200), c(0.2, 0.4)),
   importToR = TRUE, path = "./SimHardcore.ssn")
plot(hardcore.ssn, lwdLineCol = "addfunccol", lwdLineEx = 8,
   lineCol = "blue", cex = 2, xlab = "x-coordinate",
   ylab = "y-coordinate", pch = 1)
plot(hardcore.ssn, PredPointsID = NULL, add = TRUE, cex = .5,
   pch = 19, col = "green")


###################################################
### code chunk number 51: SSN.Rnw:1673-1674
###################################################
createDistMat(raw.ssn, "preds", o.write=TRUE, amongpred = TRUE)


###################################################
### code chunk number 52: SSN.Rnw:1687-1689
###################################################
rawDFobs <- getSSNdata.frame(raw.ssn, "Obs")
rawDFpred <- getSSNdata.frame(raw.ssn, "preds")


###################################################
### code chunk number 53: SSN.Rnw:1694-1698
###################################################
rawDFobs[,"X1"] <- rnorm(length(rawDFobs[,1]))
rawDFpred[,"X1"] <- rnorm(length(rawDFpred[,1]))
rawDFobs[,"X2"] <- rnorm(length(rawDFobs[,1]))
rawDFpred[,"X2"] <- rnorm(length(rawDFpred[,1]))


###################################################
### code chunk number 54: SSN.Rnw:1703-1707
###################################################
rawDFobs[,"F1"] <- as.factor(sample.int(4,length(rawDFobs[,1]),
   replace = TRUE))
rawDFpred[,"F1"] <- as.factor(sample.int(4,length(rawDFpred[,1]),
   replace = TRUE))


###################################################
### code chunk number 55: SSN.Rnw:1713-1721
###################################################
rawDFobs[,"RE1"] <- as.factor(sample(1:3,length(rawDFobs[,1]),
   replace = TRUE))
rawDFobs[,"RE2"] <- as.factor(sample(1:4,length(rawDFobs[,1]),
   replace = TRUE))
rawDFpred[,"RE1"] <- as.factor(sample(1:3,length(rawDFpred[,1]),
   replace = TRUE))
rawDFpred[,"RE2"] <- as.factor(sample(1:4,length(rawDFpred[,1]),
   replace = TRUE))


###################################################
### code chunk number 56: SSN.Rnw:1726-1728
###################################################
names(rawDFobs)
names(rawDFpred)


###################################################
### code chunk number 57: SSN.Rnw:1744-1752
###################################################
set.seed(102)
sim.out <- SimulateOnSSN(raw.ssn, ObsSimDF = rawDFobs,
   PredSimDF = rawDFpred, PredID = "preds",
   formula = ~ X1 + X2 + F1, coefficients = c(10,1,0,-2,0,2),
   CorModels = c("LinearSill.tailup", "Mariah.taildown",
   "Exponential.Euclid", "RE1", "RE2"), use.nugget = TRUE,
   CorParms = c(3, 10, 2, 10, 1, 5, 1, .5, .1),
   addfunccol = "addfunccol")


###################################################
### code chunk number 58: SSN.Rnw:1782-1783
###################################################
with(rawDFobs, colnames(model.matrix( ~ X1 + X2 + F1)))


###################################################
### code chunk number 59: SSN.Rnw:1807-1809
###################################################
sim.out$FixedEffects
sim.out$CorParms


###################################################
### code chunk number 60: SSN.Rnw:1813-1814
###################################################
sim.ssn <- sim.out$ssn.object


###################################################
### code chunk number 61: SimSSN2
###################################################
plot(sim.ssn, "Sim_Values",
   xlab = "x-coordinate", ylab = "y-coordinate",
   cex = 1.5)


###################################################
### code chunk number 62: SSN.Rnw:1838-1840
###################################################
simDFobs <- getSSNdata.frame(sim.ssn, "Obs")
simDFpred <- getSSNdata.frame(sim.ssn, "preds")


###################################################
### code chunk number 63: SSN.Rnw:1845-1848
###################################################
simpreds <- simDFpred[,"Sim_Values"]
simDFpred[,"Sim_Values"] <- NA
sim.ssn <- putSSNdata.frame(simDFpred, sim.ssn, "preds")


###################################################
### code chunk number 64: SSN.Rnw:1853-1857
###################################################
glmssn.out <- glmssn(Sim_Values ~ X1 + X2 + F1, sim.ssn,
   CorModels = c("LinearSill.tailup", "Mariah.taildown",
   "Exponential.Euclid", "RE1", "RE2"),
   addfunccol = "addfunccol")


###################################################
### code chunk number 65: SSN.Rnw:1861-1862
###################################################
summary(glmssn.out)


###################################################
### code chunk number 66: SimTvP
###################################################
glmssn.pred <- predict(glmssn.out,"preds")
predDF <- getSSNdata.frame(glmssn.pred, "preds")
plot(simpreds, predDF[,"Sim_Values"], xlab = "True",
   ylab = "Predicted", pch = 19)


