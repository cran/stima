mean_f <-
function(x)
{
	## computes the mean of vector x using the Fortran interface
	return(.Fortran("rs_mean",m=as.double(0),
	       as.double(x),as.integer(length(x)),PACKAGE="stima")$m)
}

