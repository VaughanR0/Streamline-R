context('Rebuild a numeric matrix')
library(hts2)

n1 <- list(3)
l1 <- do.call(sum,n1) + 1
# These would be a matrix of time-series
M1 <- matrix(rep(seq(1:l1),4),nrow=4,ncol=l1)
print("matrix M1")
print(M1)
# a list for comparison
M1.l <- lapply(seq_len(ncol(M1)), function(i) M1[,i])
#View(M1.l, title="list M1")

u1 <- makeuniqv(n1)
m1 <- makemapv(n1,u1)
# Reduce size of matrix
M1.red <- M1[,u1];
print("Reduced matrix M1")
print(M1.red)
# In this case only
expect_true(identical(M1,M1.red))

# Forecasts return a list
M1.fc <- lapply(seq_len(ncol(M1.red)), function(i) M1.red[,i])
# Apply rebuild function
M1.res <- lapply(seq(to=ncol(M1)),FUN=rebuild,y=M1.fc,u=u1,m=m1)
expect_true(identical(M1.l,M1.res))
#View(M1.res, title="Rebuilt list M1")

n2 <- list(2,c(3,1))
l2 <- do.call(sum,n2) + 1
M2 <- matrix(rep(seq(1:l2),4),nrow=4,ncol=l2)
# create the identical columns for this case
M2[,7] <- M2[,3]
print("matrix M2")
print(M2)
# a list for comparison
M2.l <- lapply(seq_len(ncol(M2)), function(i) M2[,i])
#View(M2.l, title="list M2")
u2 <- makeuniqv(n2)
m2 <- makemapv(n2,u2)
M2.red <- M2[,u2];
# Forecasts return a list
M2.fc <- lapply(seq_len(ncol(M2.red)), function(i) M2.red[,i])
# Apply rebuild function
M2.res <- lapply(seq(to=ncol(M2)),FUN=rebuild,y=M2.fc,u=u2,m=m2)
expect_true(identical(M2.l,M2.res))
#View(M2.res, title="Rebuilt list M2")

n3 <- list(4,c(2,1,1,1),c(1,2,1,2,1))
l3 <- do.call(sum,n3) + 1
M3 <- matrix(rep(seq(1:l3),4),nrow=4,ncol=l3)
# create the identical columns for this case
M3[,8:11] <- M3[,3:6]
M3[,14] <- M3[,8]
M3[,17] <- M3[,10]
print("matrix M3")
print(M3)
# a list for comparison
M3.l <- lapply(seq_len(ncol(M3)), function(i) M3[,i])
#View(M3.l, title="list M3")
u3 <- makeuniqv(n3)
m3 <- makemapv(n3,u3)
M3.red <- M3[,u3];
# Forecasts return a list
M3.fc <- lapply(seq_len(ncol(M3.red)), function(i) M3.red[,i])
# Apply rebuild function
M3.res <- lapply(seq(to=ncol(M3)),FUN=rebuild,y=M3.fc,u=u3,m=m3)
expect_true(identical(M3.l,M3.res))
#View(M3.res, title="Rebuilt list M3")

n4 <- list(4,c(1,1,1,2),c(2,1,1,1,1))
l4 <- do.call(sum,n4) + 1
M4 <- matrix(rep(seq(1:l4),4),nrow=4,ncol=l4)
# create the identical columns for this case
M4[,6:8] <- M4[,2:4]
M4[,13:16] <- M4[,7:10]
print("matrix M4")
print(M4)
# a list for comparison
M4.l <- lapply(seq_len(ncol(M4)), function(i) M4[,i])
#View(M4.l, title="list M4")
u4 <- makeuniqv(n4)
m4 <- makemapv(n4,u4)
M4.red <- M4[,u4];
# Forecasts return a list
M4.fc <- lapply(seq_len(ncol(M4.red)), function(i) M4.red[,i])
# Apply rebuild function
M4.res <- lapply(seq(to=ncol(M4)),FUN=rebuild,y=M4.fc,u=u4,m=m4)
expect_true(identical(M4.l,M4.res))
#View(M4.res, title="Rebuilt list M4")

n5 <- list(4,c(1,1,1,2),c(1,1,1,2,1))
l5 <- do.call(sum,n5) + 1
M5 <- matrix(rep(seq(1:l5),4),nrow=4,ncol=l5)
# create the identical columns for this case
M5[,6:8] <- M5[,2:4]
M5[,11:13] <- M5[,6:8]
M5[,16] <- M5[,10]
print("matrix M5")
print(M5)
# a list for comparison
M5.l <- lapply(seq_len(ncol(M5)), function(i) M5[,i])
#View(M5.l, title="list M5")
u5 <- makeuniqv(n5)
m5 <- makemapv(n5,u5)
M5.red <- M5[,u5];
# Forecasts return a list
M5.fc <- lapply(seq_len(ncol(M5.red)), function(i) M5.red[,i])
# Apply rebuild function
M5.res <- lapply(seq(to=ncol(M5)),FUN=rebuild,y=M5.fc,u=u5,m=m5)
expect_true(identical(M5.l,M5.res))
#View(M5.res, title="Rebuilt list M5")

n6 <- list(6,c(2,1,1,1,1,1),c(1,1,1,1,1,2,1))
l6 <- do.call(sum,n6) + 1
M6 <- matrix(rep(seq(1:l6),4),nrow=4,ncol=l6)
# create the identical columns for this case, care with overlap
M6[,10:16] <- M6[,3:9]
M6[,17:19] <- M6[,10:12]
M6[,22] <- M6[,14]
print("matrix M6")
print(M6)
# a list for comparison
M6.l <- lapply(seq_len(ncol(M6)), function(i) M6[,i])
View(M6.l, title="list M6")
u6 <- makeuniqv(n6)
m6 <- makemapv(n6,u6)
M6.red <- M6[,u6];
# Forecasts return a list
M6.fc <- lapply(seq_len(ncol(M6.red)), function(i) M6.red[,i])
# Apply rebuild function
M6.res <- lapply(seq(to=ncol(M6)),FUN=rebuild,y=M6.fc,u=u6,m=m6)
expect_true(identical(M6.l,M6.res))
View(M6.res, title="Rebuilt list M6")
