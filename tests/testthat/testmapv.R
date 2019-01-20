context('Makemapv a numeric vector')
library(hts2)

n1 <- list(3)
n2 <- list(2,c(3,1))
n3 <- list(4,c(2,1,1,1),c(1,2,1,2,1))

# count of nodes for each case, including Total
expect_equal(do.call(sum,n1) + 1,4)
expect_equal(do.call(sum,n2) + 1,7)
expect_equal(do.call(sum,n3) + 1,17)

u1 <- makeuniqv(n1)
u2 <- makeuniqv(n2)
u3 <- makeuniqv(n3)

m1 <- makemapv(n1,u1)
m2 <- makemapv(n2,u2)
m3 <- makemapv(n3,u3)

# tests
# m1
expect_true(is.numeric(m1))
expect_equal(length(m1),4)
expect_equal(m1,rep(0,4))

# m2
expect_true(is.numeric(m2))
expect_equal(length(m2),7)

expect_equal(m2[1:6],rep(0,6))
expect_equal(m2[7],3)

# m3
expect_equal(length(m3),17)
expect_true(is.numeric(m3))

expect_equal(m3[1:7],rep(0,7))
expect_equal(m3[8],3)
expect_equal(m3[9],4)
expect_equal(m3[10],5)
expect_equal(m3[11],6)
expect_equal(m3[12:13],rep(0,2))
expect_equal(m3[14],8)
expect_equal(m3[15:16],rep(0,2))
expect_equal(m3[17],10)

