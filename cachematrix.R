makeCacheMatrix <- function(x = matrix()) {
	inv<- NULL
	set <- function (y){ ##defining "set" part of our matrix
		x<<- y	
		inv<<-NULL		##inverted matrix is set to be NULL every time we make new cached matrix
	}
	get <-function () x
	setinv <-function (invMat) inv<<-invMat ##cacheing our inverted matrix
	getinv <- function () inv
	list(set=set,get=get,setinv=setinv,getinv=getinv) ##output of this function is this list of orders making it easy to access our "cache" 	
}


cacheSolve <- function(x, ...) {
	inv<-x$getinv
	if(!is.null(inv)){ ##if it isn't NULL inverted matrix had already been cached
	message ("getting cached data")
	return (inv)		##stopping the execution of the function and returning cached value
	}
	data<-x$get()
	inv<-solve(data) ##finding the inverted matrix
	x$setinv(inv)	##storing the inverted matrix to cache
	inv		##printing the inverted matrix when function is finished
}


