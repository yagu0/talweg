context("Date <--> integer conversions")

ts_data = system.file("testdata","ts_test.csv",package="talweg")
exo_data = system.file("testdata","exo_test.csv",package="talweg")
data0 <<- getData(ts_data, exo_data, date_format="%Y-%m-%d %H:%M", limit=Inf)
data7 <<- getData(ts_data, exo_data, date_format="%Y-%m-%d %H:%M", limit=Inf)

test_that("dateIndexToInteger works as expected",
{
	expect_identical( dateIndexToInteger("2007-01-01",data0),   1 )
	expect_identical( dateIndexToInteger("2007-01-02",data0),   2 )
	expect_identical( dateIndexToInteger("2007-02-01",data0),  32 )
	expect_identical( dateIndexToInteger("2007-03-01",data0),  60 )
	expect_identical( dateIndexToInteger("2007-05-31",data0), 151 )

	expect_identical( dateIndexToInteger("2007-01-01",data7),   1 )
	expect_identical( dateIndexToInteger("2007-01-02",data7),   2 )
	expect_identical( dateIndexToInteger("2007-02-01",data7),  32 )
	expect_identical( dateIndexToInteger("2007-03-01",data7),  60 )
	expect_identical( dateIndexToInteger("2007-05-31",data7), 151 )
})

test_that("integerIndexToDate works as expected",
{
	expect_identical( integerIndexToDate(  1,data0), as.Date("2007-01-01") )
	expect_identical( integerIndexToDate(  2,data0), as.Date("2007-01-02") )
	expect_identical( integerIndexToDate( 32,data0), as.Date("2007-02-01") )
	expect_identical( integerIndexToDate( 60,data0), as.Date("2007-03-01") )
	expect_identical( integerIndexToDate(151,data0), as.Date("2007-05-31") )

	expect_identical( integerIndexToDate(  1,data7), as.Date("2007-01-01") )
	expect_identical( integerIndexToDate(  2,data7), as.Date("2007-01-02") )
	expect_identical( integerIndexToDate( 32,data7), as.Date("2007-02-01") )
	expect_identical( integerIndexToDate( 60,data7), as.Date("2007-03-01") )
	expect_identical( integerIndexToDate(151,data7), as.Date("2007-05-31") )
})

test_that("dateIndexToInteger(integerIndexToDate) == Id",
{
	expect_identical( dateIndexToInteger(integerIndexToDate(  1,data0),data0),   1 )
	expect_identical( dateIndexToInteger(integerIndexToDate(  1,data7),data7),   1 )
	expect_identical( dateIndexToInteger(integerIndexToDate(  2,data0),data0),   2 )
	expect_identical( dateIndexToInteger(integerIndexToDate(  2,data7),data7),   2 )
	expect_identical( dateIndexToInteger(integerIndexToDate( 32,data0),data0),  32 )
	expect_identical( dateIndexToInteger(integerIndexToDate( 32,data7),data7),  32 )
	expect_identical( dateIndexToInteger(integerIndexToDate( 60,data0),data0),  60 )
	expect_identical( dateIndexToInteger(integerIndexToDate( 60,data7),data7),  60 )
	expect_identical( dateIndexToInteger(integerIndexToDate(151,data0),data0), 151 )
	expect_identical( dateIndexToInteger(integerIndexToDate(151,data7),data7), 151 )
})

test_that("integerIndexToDate(dateIndexToInteger) == Id",
{
	expect_identical(integerIndexToDate(dateIndexToInteger("2007-01-01",data0),data0),
		as.Date("2007-01-01") )
	expect_identical(integerIndexToDate(dateIndexToInteger("2007-01-01",data7),data7),
		as.Date("2007-01-01") )
	expect_identical(integerIndexToDate(dateIndexToInteger("2007-01-02",data0),data0),
		as.Date("2007-01-02") )
	expect_identical(integerIndexToDate(dateIndexToInteger("2007-01-02",data7),data7),
		as.Date("2007-01-02") )
	expect_identical(integerIndexToDate(dateIndexToInteger("2007-02-01",data0),data0),
		as.Date("2007-02-01") )
	expect_identical(integerIndexToDate(dateIndexToInteger("2007-02-01",data0),data0),
		as.Date("2007-02-01") )
	expect_identical(integerIndexToDate(dateIndexToInteger("2007-03-01",data0),data0),
		as.Date("2007-03-01") )
	expect_identical(integerIndexToDate(dateIndexToInteger("2007-03-01",data0),data0),
		as.Date("2007-03-01") )
	expect_identical(integerIndexToDate(dateIndexToInteger("2007-05-31",data0),data0),
		as.Date("2007-05-31") )
	expect_identical(integerIndexToDate(dateIndexToInteger("2007-05-31",data0),data0),
		as.Date("2007-05-31") )
})
