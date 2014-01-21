additive.function <- function(ssn, VarName, afvName)
{

	if(!(VarName %in% colnames(ssn@data)))
	{
		stop(paste("Column ", VarName, " not found in ssn object", sep=""))
	}

	uniqueNets <- unique(ssn@data[,"netID"])
	nNets <- length(uniqueNets)
	ridData <- ssn@data

	ssnPath <- ssn@path
	driver<-dbDriver("SQLite")
	connect.name <- file.path(ssnPath,"binaryID.db")

	connect <- dbConnect(SQLite(), connect.name)

	afvStore <- NULL
	for(ni in 1:nNets) {
		binTable <- dbReadTable(connect, paste("net",uniqueNets[ni], sep = ""))
		binTableOrd <- binTable[order(binTable[,"rid"]),]
		afvTable <- ridData[ridData[,"rid"] %in% binTable[,"rid"], c("rid", VarName)]
		afvTableOrd <- afvTable[order(afvTable[,"rid"]),]

		if(length(binTable[,1]) == 1) {
			pival <- c(binTable[1,"rid"],1)
		} else {
			ssbTab <- strsplit(binTableOrd[,"binaryID"],"")
			cLength <- unlist(lapply(ssbTab,length))
			maxLength <- max(cLength)
			pival <- cbind(binTableOrd[,"rid"], rep(NA, times = length(binTableOrd[,1])))
			pival[cLength == 1, 2] <- 1

			for (k in 2:maxLength) {
				binTableOrd[cLength == k,"rid"]
				binAtLength <- binTableOrd[cLength == k,]
				uniqueSplits <- unique(substr(binAtLength[,"binaryID"], 1, k - 1))
				j <- 1
				for(j in 1:length(uniqueSplits)) {
					ind <- substr(binAtLength[,"binaryID"], 1, k - 1) == uniqueSplits[j]
					if(sum(ind) == 1) pival[pival[,1] == binAtLength[ind,"rid"], 2] <-
						pival[uniqueSplits[j] == binTableOrd[,"binaryID"], 2]
					if(sum(ind) == 2) {
						piv <- afvTableOrd[afvTableOrd[,1] == binAtLength[ind,"rid"][1], 2]/
							(afvTableOrd[afvTableOrd[,1] == binAtLength[ind,"rid"][1], 2] +
							afvTableOrd[afvTableOrd[,1] == binAtLength[ind,"rid"][2], 2])
						pival[pival[,1] == binAtLength[ind,"rid"][1], 2] <-
							pival[uniqueSplits[j] == binTableOrd[,"binaryID"], 2] * piv
						pival[pival[,1] == binAtLength[ind,"rid"][2], 2] <- 
							pival[uniqueSplits[j] == binTableOrd[,"binaryID"], 2] * (1 - piv)
					}
				}		
			}
		}
		afvStore <- rbind(afvStore, pival)
	}	

	sqliteCloseConnection(connect)
	sqliteCloseDriver(driver)

	colnames(afvStore) <- c("rid", afvName)
	afvStore <- as.data.frame(afvStore)
	ridData <- merge(ridData,afvStore, by = "rid")

	ssn@data <- ridData[order(ridData$rid),]
	pData <- ssn@obspoints@SSNPoints[[1]]@point.data
  rnames <- rownames(pData)
	pData <- merge(pData, ridData[,c("rid",afvName)], by.x = "rid")
  pData <- pData[order(pData$pid),]
  rownames(pData) <- rnames
	ssn@obspoints@SSNPoints[[1]]@point.data <- pData[order(pData$pid),]

	pIDlist <- ssn@predpoints@ID
	if(length(pIDlist) > 0) {
		for(i in 1:length(pIDlist)) {
			pData <- ssn@predpoints@SSNPoints[[i]]@point.data
      rnames <- rownames(pData)
			pData <- merge(pData, ridData[,c("rid",afvName)], by.x = "rid")
      pData <- pData[order(pData$pid),]
      rownames(pData) <- rnames
			ssn@predpoints@SSNPoints[[i]]@point.data <- pData
		}
	}

	ssn

}

