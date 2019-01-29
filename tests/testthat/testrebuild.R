context('Rebuild a numeric matrix')
library(hts2)

# m
test_that("Rebuild #0 works for node list(2)", {
	n <- list(2)
	l <- do.call(sum,n) + 1
	# These would be a matrix of time-series
	M <- matrix(rep(seq(1:l),4),nrow=4,ncol=l)
	print("matrix m")
	print(M)
	# a list for comparison
	M.l <- lapply(seq_len(ncol(M)), function(i) M[,i])
	#View(M.l, title="list m")
	u <- makeuniqv(n)
	m <- makemapv(n,u)
	# Reduce size of matrix
	M.red <- M[,u];
	print("Reduced matrix m")
	print(M.red)
	# In this case only
	expect_true(identical(M,M.red))
	# Forecasts return a list
	M.fc <- lapply(seq_len(ncol(M.red)), function(i) M.red[,i])
	# Apply rebuild function
	M.res <- lapply(seq(to=ncol(M)),FUN=rebuild,y=M.fc,u=u,m=m)
	expect_true(identical(M.l,M.res))
	#View(M.res, title="Rebuilt list m")
})

skip('rebuild: skipping passed tests')
# m1
test_that("Rebuild #1 works for node list(3)", {
	n <- list(3)
	l <- do.call(sum,n) + 1
	# These would be a matrix of time-series
	M <- matrix(rep(seq(1:l),4),nrow=4,ncol=l)
	#print("matrix m")
	#print(M)
	# a list for comparison
	M.l <- lapply(seq_len(ncol(M)), function(i) M[,i])
	#View(M.l, title="list m")
	u <- makeuniqv(n)
	m <- makemapv(n,u)
	# Reduce size of matrix
	M.red <- M[,u];
	#print("Reduced matrix m")
	#print(m.red)
	# In this case only
	expect_true(identical(M,M.red))
	# Forecasts return a list
	M.fc <- lapply(seq_len(ncol(M.red)), function(i) M.red[,i])
	# Apply rebuild function
	M.res <- lapply(seq(to=ncol(M)),FUN=rebuild,y=M.fc,u=u,m=m)
	expect_true(identical(M.l,M.res))
	#View(M.res, title="Rebuilt list m")
})

# m2
test_that("Rebuild #2 works for node list(2,c(3,1))", {
	n <- list(2,c(3,1))
	l <- do.call(sum,n) + 1
	M <- matrix(rep(seq(1:l),4),nrow=4,ncol=l)
	# create the identical columns for this case
	M[,7] <- M[,3]
	#print("matrix m")
	#print(M)
	# a list for comparison
	M.l <- lapply(seq_len(ncol(M)), function(i) M[,i])
	#View(M.l, title="list m")
	u <- makeuniqv(n)
	m <- makemapv(n,u)
	M.red <- M[,u];
	# Forecasts return a list
	M.fc <- lapply(seq_len(ncol(M.red)), function(i) M.red[,i])
	# Apply rebuild function
	M.res <- lapply(seq(to=ncol(M)),FUN=rebuild,y=M.fc,u=u,m=m)
	expect_true(identical(M.l,M.res))
	#View(M.res, title="Rebuilt list m")
})

# m3
test_that("Rebuild #3 works for node list(4,c(2,1,1,1),c(1,2,1,2,1))", {
	n <- list(4,c(2,1,1,1),c(1,2,1,2,1))
	l <- do.call(sum,n) + 1
	M <- matrix(rep(seq(1:l),4),nrow=4,ncol=l)
	# create the identical columns for this case
	M[,8:11] <- M[,3:6]
	M[,14] <- M[,8]
	M[,17] <- M[,10]
	#print("matrix m")
	#print(M)
	# a list for comparison
	M.l <- lapply(seq_len(ncol(M)), function(i) M[,i])
	#View(M.l, title="list m")
	u <- makeuniqv(n)
	m <- makemapv(n,u)
	M.red <- M[,u];
	# Forecasts return a list
	M.fc <- lapply(seq_len(ncol(M.red)), function(i) M.red[,i])
	# Apply rebuild function
	M.res <- lapply(seq(to=ncol(M)),FUN=rebuild,y=M.fc,u=u,m=m)
	expect_true(identical(M.l,M.res))
	#View(M.res, title="Rebuilt list m")
})

# m4
test_that("Rebuild #4 works for node list(4,c(1,1,1,2),c(2,1,1,1,1))", {
	n <- list(4,c(1,1,1,2),c(2,1,1,1,1))
	l <- do.call(sum,n) + 1
	M <- matrix(rep(seq(1:l),4),nrow=4,ncol=l)
	# create the identical columns for this case
	M[,6:8] <- M[,2:4]
	M[,13:16] <- M[,7:10]
	#print("matrix m")
	#print(M)
	# a list for comparison
	M.l <- lapply(seq_len(ncol(M)), function(i) M[,i])
	#View(M.l, title="list m")
	u <- makeuniqv(n)
	m <- makemapv(n,u)
	M.red <- M[,u];
	# Forecasts return a list
	M.fc <- lapply(seq_len(ncol(M.red)), function(i) M.red[,i])
	# Apply rebuild function
	M.res <- lapply(seq(to=ncol(M)),FUN=rebuild,y=M.fc,u=u,m=m)
	expect_true(identical(M.l,M.res))
	#View(M.res, title="Rebuilt list m")
})

# m5
test_that("Rebuild #5 works for node list(4,c(1,1,1,2),c(1,1,1,2,1))", {
	n <- list(4,c(1,1,1,2),c(1,1,1,2,1))
	l <- do.call(sum,n) + 1
	M <- matrix(rep(seq(1:l),4),nrow=4,ncol=l)
	# create the identical columns for this case
	M[,6:8] <- M[,2:4]
	M[,11:13] <- M[,6:8]
	M[,16] <- M[,10]
	#print("matrix m")
	#print(M)
	# a list for comparison
	M.l <- lapply(seq_len(ncol(M)), function(i) M[,i])
	#View(M.l, title="list M5")
	u <- makeuniqv(n)
	m <- makemapv(n,u)
	M.red <- M[,u];
	# Forecasts return a list
	M.fc <- lapply(seq_len(ncol(M.red)), function(i) M.red[,i])
	# Apply rebuild function
	M.res <- lapply(seq(to=ncol(M)),FUN=rebuild,y=M.fc,u=u,m=m)
	expect_true(identical(M.l,M.res))
	#View(M.res, title="Rebuilt list m")
})

# m6
test_that("Rebuild #6 works for node list(6,c(2,1,1,1,1,1),c(1,1,1,1,1,2,1))", {
	n <- list(6,c(2,1,1,1,1,1),c(1,1,1,1,1,2,1))
	l <- do.call(sum,n) + 1
	M <- matrix(rep(seq(1:l),4),nrow=4,ncol=l)
	# create the identical columns for this case, care with overlap
	M[,10:16] <- M[,3:9]
	M[,17:19] <- M[,10:12]
	M[,22] <- M[,14]
	#print("matrix M"6)
	#print(M)
	# a list for comparison
	M.l <- lapply(seq_len(ncol(M)), function(i) M[,i])
	#View(M.l, title="list M6")
	u <- makeuniqv(n)
	m <- makemapv(n,u)
	M.red <- M[,u];
	# Forecasts return a list
	M.fc <- lapply(seq_len(ncol(M.red)), function(i) M.red[,i])
	# Apply rebuild function
	M.res <- lapply(seq(to=ncol(M)),FUN=rebuild,y=M.fc,u=u,m=m)
	expect_true(identical(M.l,M.res))
	#View(M.res, title="Rebuilt list m")
})
