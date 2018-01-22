#shorthand: map 1->1, 2->2, 3->3, 4->1, ..., 149->2, 150->3
I = function(i)
	(i-1) %% 3 + 1

#MOCK data; NOTE: could be in inst/testdata as well
getDataTest = function(n)
{
	data = Data$new()
	x = seq(0,9.5,0.1)
	L = length(x) #96 1/4h
	s1 = cos(x)
	s2 = sin(x)
	s3 = c( s1[1:(L%/%2)] , s2[(L%/%2+1):L] )
	#sum((s1-s2)^2) == 96
	#sum((s1-s3)^2) == 58
	#sum((s2-s3)^2) == 38
	s = list(s1, s2, s3)
	series = list()
	for (i in seq_len(n))
	{
		serie = s[[I(i)]] + rnorm(L,sd=0.01)
		# 10 series with NAs for index 2
		if (I(i) == 2 && i >= 60 && i<= 90)
			serie[sample(seq_len(L),1)] = NA
		time = as.POSIXct((i-1)*60*60*24+15*60*(1:96), origin="2007-01-01", tz="GMT")
		exo = runif(4)
		exo_hat = runif(4)
		data$append(time=time, value=serie, level_hat=cumsum(serie),
			exo=exo, exo_hat=exo_hat)
	}
	data
}
