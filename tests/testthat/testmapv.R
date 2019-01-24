context('Makemapv a numeric vector')
library(hts2)

n1 <- list(3)
n2 <- list(2,c(3,1))
n3 <- list(4,c(2,1,1,1),c(1,2,1,2,1))
n4 <- list(4,c(1,1,1,2),c(2,1,1,1,1))
n5 <- list(4,c(1,1,1,2),c(1,1,1,2,1))
n6 <- list(6,c(2,1,1,1,1,1),c(1,1,1,1,1,2,1))

# count of nodes for each case, including Total
expect_equal(do.call(sum,n1) + 1,4)
expect_equal(do.call(sum,n2) + 1,7)
expect_equal(do.call(sum,n3) + 1,17)
expect_equal(do.call(sum,n4) + 1,16)
expect_equal(do.call(sum,n5) + 1,16)
expect_equal(do.call(sum,n6) + 1,22)

u1 <- makeuniqv(n1)
u2 <- makeuniqv(n2)
u3 <- makeuniqv(n3)
u4 <- makeuniqv(n4)
u5 <- makeuniqv(n5)
u6 <- makeuniqv(n6)

m1 <- makemapv(n1,u1)
m2 <- makemapv(n2,u2)
m3 <- makemapv(n3,u3)
m4 <- makemapv(n4,u4)
m5 <- makemapv(n5,u5)
m6 <- makemapv(n6,u6)

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

# m4
expect_equal(length(m4),16)
expect_true(is.numeric(m4))

expect_equal(m4[1:5],rep(0,5))
expect_equal(m4[6],2)
expect_equal(m4[7],3)
expect_equal(m4[8],4)
expect_equal(m4[9:12],rep(0,4))
expect_equal(m4[13],7)
expect_equal(m4[14],8)
expect_equal(m4[15],9)
expect_equal(m4[16],10)

# m5
expect_equal(length(m5),16)
expect_true(is.numeric(m5))

expect_equal(m5[1:5],rep(0,5))
expect_equal(m5[6],2)
expect_equal(m5[7],3)
expect_equal(m5[8],4)
expect_equal(m5[9:10],rep(0,2))
expect_equal(m5[11],6)
expect_equal(m5[12],7)
expect_equal(m5[13],8)
expect_equal(m5[14:15],rep(0,2))
expect_equal(m5[16],10)

# m6
expect_equal(length(m6),22)
expect_true(is.numeric(m6))

expect_equal(m6[1:9],rep(0,9))
expect_equal(m6[10],3)
expect_equal(m6[11],4)
expect_equal(m6[12],5)
expect_equal(m6[13],6)
expect_equal(m6[14],7)
expect_equal(m6[15],8)
expect_equal(m6[16],9)
expect_equal(m6[17],10)
expect_equal(m6[18],11)
expect_equal(m6[19],12)
expect_equal(m6[20:21],rep(0,2))
expect_equal(m6[22],14)
