getSSNdata.frame <-
function(x, Name = "Obs")
{
  if(Name == "Obs") {
    if(class(x)[[1]] == "SpatialStreamNetwork")
      return(x@obspoints@SSNPoints[[1]]@point.data)
    if(class(x)[[1]] == "influenceSSN")
	  return(x$ssn.object@obspoints@SSNPoints[[1]]@point.data)
    if(class(x)[[1]] == "glmssn")
	  return(x$ssn.object@obspoints@SSNPoints[[1]]@point.data)
    if(class(x)[[1]] == "glmssn.predict")
	  return(x$ssn.object@obspoints@SSNPoints[[1]]@point.data)
    return("Class of argument is not a recognized")
  }
  if(Name != "Obs") {
	if(class(x)[[1]] == "SpatialStreamNetwork"){
	    np <- length(x@predpoints)
      if(length(np) == 0) stop("No prediction data sets in SSN object")
			if(!(Name %in% x@predpoints@ID)) stop(
				paste("Name does not match any prediction data sets, try one of these: ", 
					x@predpoints@ID))
	    return(x@predpoints@SSNPoints[
				x@predpoints@ID == Name][[1]]@point.data) 
    }
	if(class(x)[[1]] == "influenceSSN"){
		np <- length(x$ssn.object@predpoints)
		if(length(np) == 0) stop("No prediction data sets in SSN object")
		if(!(Name %in% x$ssn.object@predpoints@ID)) stop(
			paste("Name does not match any prediction data sets, try one of these: ", 
				x@predpoints@ID))
		return(x$ssn.object@predpoints@SSNPoints[
						x$ssn.object@predpoints@ID == Name][[1]]@point.data) 
	}
	if(class(x)[[1]] == "glmssn"){
		np <- length(x$ssn.object@predpoints)
		if(length(np) == 0) stop("No prediction data sets in SSN object")
		if(!(Name %in% x$ssn.object@predpoints@ID)) stop(
			paste("Name does not match any prediction data sets, try one of these: ", 
				x@predpoints@ID))
		return(x$ssn.object@predpoints@SSNPoints[
						x$ssn.object@predpoints@ID == Name][[1]]@point.data) 
	}
	if(class(x)[[1]] == "glmssn.predict"){
		np <- length(x$ssn.object@predpoints)
		if(length(np) == 0) stop("No prediction data sets in SSN object")
		if(!(Name %in% x$ssn.object@predpoints@ID)) stop(
			paste("Name does not match any prediction data sets, try one of these: ", 
				x@predpoints@ID))
		return(x$ssn.object@predpoints@SSNPoints[
			x$ssn.object@predpoints@ID == Name][[1]]@point.data) 
	}
	return("Class of argument is not a recognized")
  }
}
