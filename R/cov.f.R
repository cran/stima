cov.f <-
function(x,y)
{
	## computes the covariance between vectors x and y
	## using the Fortran interface
	n<-length(x)
	c<-0
	if (n!=length(y))
		print("Length Error")
	else
		c<-.Fortran("rs_cov",c=as.double(0),
		            as.double(x),as.double(y),as.double(mean_f(x)),as.double(mean_f(y)),
		            as.integer(n),PACKAGE="stima")$c
	return(c)
}
