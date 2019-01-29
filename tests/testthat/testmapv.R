context('Makemapv a numeric vector')
library(hts2)

# tests
# m
test_that("Makemapv #0 works for node list(2)", {
	n <- list(2)
	u <- makeuniqv(n)
	m <- makemapv(n,u)
	expect_true(is.numeric(m))
	expect_equal(length(u),3)
	expect_equal(m,rep(0,3))
})

skip('makemapv: skipping passed tests')
# m
test_that("Makemapv #1 works for node list(3)", {
	n <- list(3)
	u <- makeuniqv(n)
	m <- makemapv(n,u)
	expect_true(is.numeric(m))
	expect_equal(length(m),4)
	expect_equal(m,rep(0,4))
})

# m
test_that("Makemapv #2 works for node list(2,c(3,1))", {
	n <- list(2,c(3,1))
	u <- makeuniqv(n)
	m <- makemapv(n,u)
	expect_true(is.numeric(m))
	expect_equal(length(m),7)

	expect_equal(m[1:6],rep(0,6))
	expect_equal(m[7],3)
})

# m
test_that("Makemapv #3 works for node list(4,c(2,1,1,1),c(1,2,1,2,1))", {
	n <- list(4,c(2,1,1,1),c(1,2,1,2,1))
	u <- makeuniqv(n)
	m <- makemapv(n,u)
	expect_equal(length(m),17)
	expect_true(is.numeric(m))

	expect_equal(m[1:7],rep(0,7))
	expect_equal(m[8],3)
	expect_equal(m[9],4)
	expect_equal(m[10],5)
	expect_equal(m[11],6)
	expect_equal(m[12:13],rep(0,2))
	expect_equal(m[14],8)
	expect_equal(m[15:16],rep(0,2))
	expect_equal(m[17],10)
})

# m
test_that("Makemapv #4 works for node list(4,c(1,1,1,2),c(2,1,1,1,1))", {
	n <- list(4,c(1,1,1,2),c(2,1,1,1,1))
	u <- makeuniqv(n)
	m <- makemapv(n,u)
	expect_equal(length(m),16)
	expect_true(is.numeric(m))

	expect_equal(m[1:5],rep(0,5))
	expect_equal(m[6],2)
	expect_equal(m[7],3)
	expect_equal(m[8],4)
	expect_equal(m[9:12],rep(0,4))
	expect_equal(m[13],7)
	expect_equal(m[14],8)
	expect_equal(m[15],9)
	expect_equal(m[16],10)
})

# m
test_that("Makemapv #5 works for node list(4,c(1,1,1,2),c(1,1,1,2,1))", {
	n <- list(4,c(1,1,1,2),c(1,1,1,2,1))
	u <- makeuniqv(n)
	m <- makemapv(n,u)
	expect_equal(length(m),16)
	expect_true(is.numeric(m))

	expect_equal(m[1:5],rep(0,5))
	expect_equal(m[6],2)
	expect_equal(m[7],3)
	expect_equal(m[8],4)
	expect_equal(m[9:10],rep(0,2))
	expect_equal(m[11],6)
	expect_equal(m[12],7)
	expect_equal(m[13],8)
	expect_equal(m[14:15],rep(0,2))
	expect_equal(m[16],10)
})

# m
test_that("Makemapv #6 works for node list(6,c(2,1,1,1,1,1),c(1,1,1,1,1,2,1))", {
	n <- list(6,c(2,1,1,1,1,1),c(1,1,1,1,1,2,1))
	u <- makeuniqv(n)
	m <- makemapv(n,u)
	expect_equal(length(m),22)
	expect_true(is.numeric(m))

	expect_equal(m[1:9],rep(0,9))
	expect_equal(m[10],3)
	expect_equal(m[11],4)
	expect_equal(m[12],5)
	expect_equal(m[13],6)
	expect_equal(m[14],7)
	expect_equal(m[15],8)
	expect_equal(m[16],9)
	expect_equal(m[17],10)
	expect_equal(m[18],11)
	expect_equal(m[19],12)
	expect_equal(m[20:21],rep(0,2))
	expect_equal(m[22],14)
})
