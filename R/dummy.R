dummy <-
function(x){
if(!is.factor(x))# old:  class(x)!="factor")
	{return(x)
	}else{
	x=as.integer(x)
	new=matrix(0,length(x),max(x)-1)
	for(i in 1:(max(x)-1)){new[x==i,i]=1}
	return(new)}
}
