context("Get similar days")

test_that("getSimilarDaysIndices works as expected",
{
	data = getDataTest(150)

	# Index 142 is a tuesday (142 = 2 mod 7)
	N142_1 = getSimilarDaysIndices(142, data, limit=7, same_season=FALSE, days_in=NULL)
	expect_equal(N142_1, c(141,137,136,135,134,130,129))
	# Index 139 = saturday
	N139_1 = getSimilarDaysIndices(139, data, limit=7, same_season=FALSE, days_in=NULL)
	expect_equal(N139_1, c(132,125,118,111,104,97,90))

	# With 'days_in' constraint
	N142_2 = getSimilarDaysIndices(142, data, limit=7, same_season=FALSE, days_in=2*(1:75))
	expect_equal(N142_2, c(136,134,130,128,122,120,116))
	N139_2 = getSimilarDaysIndices(139, data, limit=7, same_season=FALSE, days_in=2*(1:75))
	expect_equal(N139_2, c(132,118,104,90,76,62,48))
})

test_that("getConstrainedNeighbs works as expected",
{
#	data = getDataTest(150)
#	N142_1 = .getConstrainedNeighbs(142, data, fdays, min_neighbs=7, max_neighbs=7)
#	#...maybe we need an easier test data
})

