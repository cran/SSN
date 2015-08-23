additive.function.rohan <- function(ssn, VarName, afvName)
{
	influenceColumn<- VarName
	functionalColumn <- afvName
	if(!(influenceColumn %in% colnames(ssn@data)))
	{
		stop(paste("Column ", influenceColumn, " not found in ssn object", sep=""))
	}
	networkIDs <- unique(ssn@data[,"netID"])
	binaryIDs = c()

	for(netid in networkIDs)
	{	
		binaryIDs = rbind(read.table(file = file(paste(ssn@path, "/netID",netid,".dat", sep="")), header = T, sep = ",", colClasses = c("numeric","character")), binaryIDs)
	}
	binaryIDs = binaryIDs[match(ssn@data[,"rid"], binaryIDs$rid, ),]

	result = .Call("additiveFunctionValues", as.integer(ssn@data[,"rid"]), binaryIDs$binaryID, ssn@data[,influenceColumn], ssn@data[,"netID"])
	
	if(is.null(result)) return(NULL)
	
	copiedssn <- ssn
	copiedssn@data <- cbind(ssn@data, result)
	colnames(copiedssn@data) <- c(colnames(ssn@data), functionalColumn)
	
	for(i in 1:length(copiedssn@obspoints@SSNPoints))
	{
		rids = copiedssn@obspoints@SSNPoints[[i]]@point.data[,"rid"]
		functionalValues = result[match(rids, copiedssn@data[,"rid"])]
		copiedssn@obspoints@SSNPoints[[i]]@point.data <- cbind(copiedssn@obspoints@SSNPoints[[i]]@point.data, functionalValues)
		colnames(copiedssn@obspoints@SSNPoints[[i]]@point.data) <- c(colnames(ssn@obspoints@SSNPoints[[i]]@point.data), functionalColumn)
	}
	if(length(copiedssn@predpoints@SSNPoints) > 0) {
	for(i in 1:length(copiedssn@predpoints@SSNPoints))
	{
		rids = copiedssn@predpoints@SSNPoints[[i]]@point.data[,"rid"]
		functionalValues = result[match(rids, copiedssn@data[,"rid"])]
		copiedssn@predpoints@SSNPoints[[i]]@point.data <- cbind(copiedssn@predpoints@SSNPoints[[i]]@point.data, functionalValues)
		colnames(copiedssn@predpoints@SSNPoints[[i]]@point.data) <- c(colnames(ssn@predpoints@SSNPoints[[i]]@point.data), functionalColumn)
	}}
	return(copiedssn)
}
