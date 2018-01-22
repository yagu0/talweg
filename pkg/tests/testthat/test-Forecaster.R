context("Check that forecasters behave as expected")

ts_data = system.file("testdata","ts_test.csv",package="talweg")
exo_data = system.file("testdata","exo_test.csv",package="talweg")
data_p <<- getData(ts_data, exo_data, date_format="%Y-%m-%d %H:%M", limit=Inf)
#Forecasts from monday to sunday (series 1 to 7)
indices <<- seq(as.Date("2007-04-02"),as.Date("2007-04-08"),"days")
pred_order = c(7,1:6) #will facilitate tests

test_that("Average method behave as expected",
{
	pred00_z = computeForecast(data_p, indices, "Average", "LastValue", 1, Inf, 24, ncores=1)
	pred00_p = computeForecast(data_p, indices, "Average", "Persistence", 1, Inf, 24)
	for (i in 1:7)
	{
		#zero jump: should predict true values minus 1
		expect_equal( pred00_z$getForecast(i), rep(pred_order[i],24) )
		#persistence jump == 1: should predict true values
		expect_equal( pred00_p$getForecast(i), rep(i,24) )
	}

	#NOTE: 24h-block become
	#1 1 1 1 1 1 1 1 1 1 1 2 2 2 2 2 2 2 2 2 2 2 2 2 (14h-->0h then 1h-->13h)
	#No jump between days, thus zero and persistence are equivalent (and correct)
	pred13_z = computeForecast(data_p, indices, "Average", "LastValue", 14, Inf, 24)
	pred13_p = computeForecast(data_p, indices, "Average", "Persistence", 14, Inf, 24)
	for (i in 1:7)
	{
		expect_equal( pred13_z$getForecast(i), rep(i,24) )
		expect_equal( pred13_p$getForecast(i), rep(i,24) )
	}

	#A few extra checks
	expect_equal( pred00_p$getIndexInData(2), dateIndexToInteger("2007-04-03",data_p) )
	expect_equal( pred00_z$getIndexInData(2), dateIndexToInteger("2007-04-03",data_p) )
	expect_equal( pred13_p$getIndexInData(5), dateIndexToInteger("2007-04-06",data_p) )
	expect_equal( pred13_z$getIndexInData(5), dateIndexToInteger("2007-04-06",data_p) )
})

test_that("Persistence method behave as expected",
{
	#Situation A: +Zero; (generally) correct if jump, wrong otherwise
	pred00_sd = computeForecast(data_p, indices, "Persistence", "LastValue", 1, Inf, 24,
		ncores=1, same_day=TRUE)
	pred00_dd = computeForecast(data_p, indices, "Persistence", "LastValue", 1, Inf, 24,
		ncores=1, same_day=FALSE)
	for (i in 1:7)
	{
		expect_equal(pred00_sd$getForecast(i), rep(pred_order[i],24))
		expect_equal(pred00_dd$getForecast(i), rep(pred_order[i],24))
	}

	pred13_sd = computeForecast(data_p, indices, "Persistence", "LastValue", 14, Inf, 24,
		ncores=1, same_day=TRUE)
	pred13_dd = computeForecast(data_p, indices, "Persistence", "LastValue", 14, Inf, 24,
		ncores=1, same_day=FALSE)
	for (i in 1:7)
	{
		expect_equal(pred13_sd$getForecast(i), rep(i,24) )
		expect_equal(pred13_dd$getForecast(i), rep(i,24) )
	}

	#Situation B: +Persistence, (generally) correct
	pred00_sd = computeForecast(data_p, indices, "Persistence", "Persistence", 1, Inf, 24,
		ncores=1, same_day=TRUE)
	pred00_dd = computeForecast(data_p, indices, "Persistence", "Persistence", 1, Inf, 24,
		ncores=1, same_day=FALSE)
	for (i in 3:7)
	{
		expect_equal(pred00_sd$getForecast(i), rep(i,24))
		expect_equal(pred00_dd$getForecast(i), rep(i,24))
	}
	#boundaries are special cases: OK if same day, quite wrong otherwise
	expect_equal(pred00_sd$getForecast(1), rep(1,24) )
	expect_equal(pred00_dd$getForecast(1), rep(8,24) )
	expect_equal(pred00_sd$getForecast(2), rep(2,24) )
	expect_equal(pred00_dd$getForecast(2), rep(-5,24) )

	pred13_sd = computeForecast(data_p, indices, "Persistence", "Persistence", 14, Inf, 24,
		ncores=1, same_day=TRUE)
	pred13_dd = computeForecast(data_p, indices, "Persistence", "Persistence", 14, Inf, 24,
		ncores=1, same_day=FALSE)
	for (i in 1:7)
	{
		expect_equal(pred13_sd$getForecast(i), rep(i,24) )
		expect_equal(pred13_dd$getForecast(i), rep(i,24) )
	}

	#A few extra checks
	expect_equal( pred00_sd$getIndexInData(3), dateIndexToInteger("2007-04-04",data_p) )
	expect_equal( pred00_dd$getIndexInData(6), dateIndexToInteger("2007-04-07",data_p) )
	expect_equal( pred13_sd$getIndexInData(3), dateIndexToInteger("2007-04-04",data_p) )
	expect_equal( pred13_dd$getIndexInData(6), dateIndexToInteger("2007-04-07",data_p) )
})

test_that("Neighbors method behave as expected",
{
	#Situation A: +Zero; correct if jump, wrong otherwise
	pred00 = computeForecast(data_p, indices, "Neighbors", "LastValue", 1, Inf, 24,
		simtype="mix", local=FALSE, window=c(1,1))
	for (i in 1:7)
		expect_equal(pred00$getForecast(i), rep(pred_order[i],24))

	pred13 = computeForecast(data_p, indices, "Persistence", "LastValue", 14, Inf, 24,
		simtype="mix", local=FALSE, window=c(1,1))
	for (i in 1:7)
		expect_equal(pred13$getForecast(i), rep(i,24) )

	#Situation B: +Neighbors == too difficult to eval in a unit test
#	pred00 = computeForecast(data_p, indices, "Neighbors", "Neighbors", 1, Inf, 24,
#		simtype="endo", local=FALSE)
#	jumps = ...
#	for (i in 1:7)
#		expect_equal(pred00$getForecast(i), rep(pred_order[i]+jumps[i],24))
#	pred13 = computeForecast(data_p, indices, "Neighbors", "Neighbors", 14, Inf, 24,
#		simtype="endo", local=FALSE)
#	for (i in 1:7)
#		expect_equal(pred13$getForecast(i), c( rep(i,11), rep(i%%7+1,13) ) )

	#A few extra checks
	expect_equal( pred00$getIndexInData(1), dateIndexToInteger("2007-04-02",data_p) )
	expect_equal( pred00$getIndexInData(4), dateIndexToInteger("2007-04-05",data_p) )
	expect_equal( pred13$getIndexInData(1), dateIndexToInteger("2007-04-02",data_p) )
	expect_equal( pred13$getIndexInData(4), dateIndexToInteger("2007-04-05",data_p) )
})
