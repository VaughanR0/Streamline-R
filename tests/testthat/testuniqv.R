context('Makeuniqv a logical vector')
library(hts2)

n1 <- list(3)
n2 <- list(2,c(3,1))
n3 <- list(4,c(2,1,1,1),c(1,2,1,2,1))

# count of nodes for each case, including Total
expect_equal(do.call(sum,n1) + 1,4)
expect_equal(do.call(sum,n2) + 1,7)
expect_equal(do.call(sum,n3) + 1,17)

u1 <- makeuniqv(n1)
expect_equal(length(u1),4)
expect_true(is.logical(u1))
expect_true(all(u1),T)

u2 <- makeuniqv(n2)
expect_equal(length(u2),7)
expect_true(is.logical(u2))
expect_false(all(u2),T)

expect_equal(all(u2[1:6]),T)
expect_equal(u2[7],F)

u3 <- makeuniqv(n3)
expect_equal(length(u3),17)
expect_true(is.logical(u3))
expect_false(all(u3),T)

expect_equal(all(u3[1:7]),T)
expect_equal(all(u3[8:10]),F)
expect_equal(all(u3[11]),F)
expect_equal(all(u3[12:13]),T)
expect_equal(all(u3[14]),F)
expect_equal(all(u3[15:16]),T)
expect_equal(all(u3[17]),F)
