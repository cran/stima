stdev.f <-
function(x)
{
	## computes the standard deviation of x using the Fortran interface
	return(.Fortran("rs_stdev",s=as.double(0),
	                as.double(x),as.double(mean_f(x)),
	                as.integer(length(x)),PACKAGE="stima")$s)
}

