context('Makeuniqv a logical vector')
library(hts2)

test_that("Makeuniqv #0 works for node list(2)", {
	n <- list(2)
	expect_equal(do.call(sum,n) + 1,3)	# includes Total
	u <- makeuniqv(n)
	expect_equal(length(u),3)
	expect_true(is.logical(u))
	expect_true(all(u),T)
})

skip('makeuniqv: skipping passed tests')
test_that("Makeuniqv #1 works for node list(3)", {
	n <- list(3)
	expect_equal(do.call(sum,n) + 1,4)
	u <- makeuniqv(n)
	expect_equal(length(u),4)
	expect_true(is.logical(u))
	expect_true(all(u),T)
})

test_that("Makeuniqv #2 works for node list(2,c(3,1))", {
	n <- list(2,c(3,1))
	expect_equal(do.call(sum,n) + 1,7)
	u <- makeuniqv(n)
	expect_equal(length(u),7)
	expect_true(is.logical(u))
	expect_false(all(u),T)

	expect_equal(all(u[1:6]),T)
	expect_equal(u[7],F)
})

test_that("Makeuniqv #3 works for node list(4,c(2,1,1,1),c(1,2,1,2,1))", {
	n <- list(4,c(2,1,1,1),c(1,2,1,2,1))
	expect_equal(do.call(sum,n) + 1,17)
	u <- makeuniqv(n)
	expect_equal(length(u),17)
	expect_true(is.logical(u))
	expect_false(all(u),T)

	expect_equal(all(u[1:7]),T)
	expect_equal(all(u[8:10]),F)
	expect_equal(all(u[11]),F)
	expect_equal(all(u[12:13]),T)
	expect_equal(all(u[14]),F)
	expect_equal(all(u[15:16]),T)
	expect_equal(all(u[17]),F)
})

test_that("Makeuniqv #4 works for node list(4,c(1,1,1,2),c(2,1,1,1,1))", {
	n <- list(4,c(1,1,1,2),c(2,1,1,1,1))
	expect_equal(do.call(sum,n) + 1,16)
	u <- makeuniqv(n)
	#print(c("u: ", paste(u)))
	expect_equal(length(u),16)
	expect_true(is.logical(u))
	expect_false(all(u),T)

	expect_equal(all(u[1:5]),T)
	expect_equal(all(u[6:8]),F)
	expect_equal(all(u[9:12]),T)
	expect_equal(all(u[13:16]),F)
})

test_that("Makeuniqv #5 works for node list(4,c(1,1,1,2),c(1,1,1,2,1))", {
	n <- list(4,c(1,1,1,2),c(1,1,1,2,1))
	expect_equal(do.call(sum,n) + 1,16)
	u <- makeuniqv(n)
	#print(c("u: ", paste(u)))
	expect_equal(length(u),16)
	expect_true(is.logical(u))
	expect_false(all(u),T)

	expect_equal(all(u[1:5]),T)
	expect_equal(all(u[6:8]),F)
	expect_equal(all(u[9:10]),T)
	expect_equal(all(u[11:13]),F)
	expect_equal(all(u[14:15]),T)
	expect_equal(all(u[16]),F)
})

test_that("Makeuniqv #6 works for node list(6,c(2,1,1,1,1,1),c(1,1,1,1,1,2,1))", {
	n <- list(6,c(2,1,1,1,1,1),c(1,1,1,1,1,2,1))
	expect_equal(do.call(sum,n) + 1,22)
	u <- makeuniqv(n)
	#print(c("u: ", paste(u)))
	expect_equal(length(u),22)
	expect_true(is.logical(u))
	expect_false(all(u),T)

	expect_equal(all(u[1:9]),T)
	expect_equal(all(u[10:19]),F)
	expect_equal(all(u[20:21]),T)
	expect_equal(all(u[22]),F)
})
