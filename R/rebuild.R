#' Rebuild columns of the full results matrix after running forecasts on unique columns
#' @param i is the column index into the full matrix of time-series
#' @param y is the reduced matrix of forecasts
#' @param u is a boolean vector indicating which columns in the full matrix are unique and need to be forecast
#' @param m is a numerical vector which maps the removed columns to their equivalents in the full matrix
#' 
#' @return the next column of forecasts
#' 
#' @author Vaughan Roberts
#' @seealso forecast.gts part of the hts2 package
#' 
#' @example
#' seq(to=ncol(ally)) is the full set of indices into the vector
#' of aggregated time-series (before forecast) as returned by aggts() function
#' fally is the reduced matrix of hts forecasts which was originally created by
#' fally <- ally[,uniqv]
#' \dontrun{
#' ally1 <- lapply(seq(to=ncol(ally)),FUN=rebuild,y=fally,u=uniqv,m=mapv)
#' }

rebuild <- function(i,y,u,m) {
	mdebug <- TRUE
	# y is now a list with each forecast as an element in the list
	if (!u[i]) {
		uc<-m[i]
		# get the lowest column with the same data
		while (m[uc] > 0) uc <- m[uc]
		# check that lowest column does not have missing columns before it
		# otherwise adjust column number to that of the reduced matrix (list)
		cnt <- sum(u[1:uc])		# number of Trues before uc
		# nb cnt always >= 1 as u[1] must be True
		uc <- if (cnt < uc) cnt else uc
		# copy earlier column
		if (mdebug) print(paste("rebuild: col ",i," copying reduced col ",uc))		
		out <- if (uc>0) y[[uc]]
	} else {
		curr <- sum(u[1:i])		# number of Trues
		if (mdebug) print(paste("rebuild: col ",i," using reduced col ",curr))		
		out <- y[[curr]]
	}
	return(out)
}

#' Create supporting vectors for hts object so forecasts are only run on unique columns
#' @param y is a matrix consisting of columns of time-series
#' @return a list consisting of two named elements $uniq and $map
#' @author Vaughan Roberts
#' @seealso forecast.gts part of the hts2 package
#' 
#' @example
#' red <- redparams(htseg3$nodes)
#' \dontrun{
#' fally <- ally[,red$uniq]
#' ... do forecast loop in forecast.gts
#' ally1 <- lapply(seq(to=ncol(ally)),FUN=rebuild,y=fally,u=red$uniq,m=red$map)
#' }
redparams <- function(nodes) {
	if (!is.list(nodes)) stop("Argument nodes must be a list.", call. = FALSE)
	uniqv <- makeuniqv(nodes)
	mapv <- makemapv(nodes,uniqv)
	
	out <- setNames(list(uniqv, mapv), c("uniq", "map"))
	return(out)	
}

makeuniqv <- function(nodes) {
	if (!is.list(nodes)) stop("Argument nodes must be a list.", call. = FALSE)
	mdebug <- FALSE
	if (length(nodes[[1]]) == 1 && nodes[[1]][1] == 1) stop("Incorrect nodes list, the first element should not be 1", call. = FALSE)	
	# number of columns to create for vector (+1 for Total column)
	total.nodes <- do.call(sum, nodes) + 1
	# empty vectors with total.nodes columns
	uniqv <- vector(mode='logical',length=total.nodes)
	# first element should never be 1
	cnt <- nodes[[1]]+1
	uniqv[1:cnt] <- TRUE
	if (length(nodes) < 2) return(uniqv)

	for (i in 2:length(nodes)) {
		sindx <- do.call(sum, nodes[1:i-1]) + 1
		maxj <- length(nodes[[i]])
		if (mdebug) print(paste("makeuniqv: list element ", i, " start index ", sindx, " length of list element ", maxj))
		for (j in 1:maxj) {
			cnt <- nodes[[i]][j]
			bindx <- sindx + 1
			lindx <- sindx+cnt
			if (mdebug) print(paste("makeuniqv: element ", j, " value ", cnt, " bindx ", bindx, " lindx ", lindx))			
			if (cnt > 1) {
				uniqv[bindx:lindx] <- TRUE
			} else {
				uniqv[bindx] <- FALSE
			}
			sindx <- sindx+cnt
		}
	}
	return(uniqv)
}

makemapv <- function(nodes,uniqv) {
	if (!is.list(nodes)) stop("Argument nodes must be a list.", call. = FALSE)
	mdebug <- FALSE
	if (length(nodes[[1]]) == 1 && nodes[[1]][1] == 1) stop("Incorrect nodes list, the first element should not be 1", call. = FALSE)
	# number of columns to create for vector (+1 for Total column)
	total.nodes <- do.call(sum, nodes) + 1
	# empty vectors with total.nodes columns
	mapv <- vector(mode='integer',length=total.nodes)
	
	# first element should never be 1 as hts does not like it
	mapv[1] <- 0
	tmpv <- vector()
	maxi <- length(nodes)
	if (maxi < 2) {
		mapv[1:total.nodes] <- 0
		return(mapv)
	}
	for (i in 2:maxi) {
		ln <- i - 2
		sindx <- if (i > 2) do.call(sum, nodes[1:ln]) + 1 else 1
		maxj <- length(nodes[[i]])
		if (mdebug) print(paste("makemapv: list element ", i, " start index ", sindx, " length of list element ", maxj))
		for (j in 1:maxj) {
			curr <- sindx + j			
			cnt <- nodes[[i]][j]
			if (mdebug) print(paste("makemapv: element ", j, " value ", cnt, " curr index ", curr))			
			if (cnt == 1) {
				tmpv <- c(tmpv,curr)
			} 
		}
	}
	mapv[!uniqv] <- tmpv
	return(mapv)
}
