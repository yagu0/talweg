context("computeFilaments")

test_that("output is as expected on simulated series",
{
	data = getDataTest(150)

	# index 144 : serie type 3, yersteday type 2
	pred = computeForecast(data, 144, "Neighbors", "Zero", predict_from=1,
		horizon=length(data$getSerie(1)), simtype="endo", local=FALSE, window=1, opera=TRUE)
	f = computeFilaments(data, pred, 1, limit=60, plot=FALSE)

	# Expected output: 50-3-10 series of type 2+1 = 3,
	# then 23 series of type 3+1 %% 3 = 1 (3 = closest next)
	expect_identical(length(f$neighb_indices), as.integer(60))
	expect_identical(length(f$colors), as.integer(60))
	expect_equal(f$index, 144)
	expect_true(all(I(f$neighb_indices) != 2))
	for (i in 1:37)
	{
		expect_equal(I(f$neighb_indices[i]), 3)
		expect_match(f$colors[i], f$colors[1])
	}
	for (i in 38:60)
	{
		expect_equal(I(f$neighb_indices[i]), 1)
		expect_match(f$colors[i], f$colors[38])
	}
	expect_match(f$colors[1], "#1*")
	expect_match(f$colors[38], "#E*")

	# index 143 : serie type 2
	pred = computeForecast(data, 143, "Neighbors", "Zero", predict_from=1,
		horizon=length(data$getSerie(1)), simtype="endo", local=FALSE, window=1, opera=TRUE)
	f = computeFilaments(data, pred, 1, limit=50, plot=FALSE)

	# Expected output: 50-10-3 series of type 1+1=2,
	# then 13 series of type 3+1 %% 3 = 1 (closest next)
	# NOTE: -10 because only past tomorrows with no-NAs yerstedays
	#        => exclude type 2 in [60,90[
	expect_identical(length(f$neighb_indices), as.integer(50))
	expect_identical(length(f$colors), as.integer(50))
	expect_equal(f$index, 143)
	expect_true(all(I(f$neighb_indices) <= 2))
	for (i in 1:37)
	{
		expect_equal(I(f$neighb_indices[i]), 2)
		expect_match(f$colors[i], f$colors[1])
	}
	for (i in 38:50)
	{
		expect_equal(I(f$neighb_indices[i]), 1)
		expect_match(f$colors[i], f$colors[38])
	}
	expect_match(f$colors[1], "#1*")
	expect_match(f$colors[38], "#E*")
})
