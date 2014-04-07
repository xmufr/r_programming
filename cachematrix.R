## creat a special ''vector'' consits of a list
## 1. check whether input is valid
## 2. set the value of the matrix
## 3. get the value of the matrix
## 4. set the value of the inverse of the matrix
## 5. get the value of the inverse of the matrix

makeCacheMatrix <- function(x) 
	{
		if(!is.matrix(x))
			{
				stop("Input must be a matrix!")
			}
        m <- NULL
        set <- function(y) 
			{
                x <<- y
                m <<- NULL
			}
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
	}


## Return a matrix that is the inverse of 'x'
## 1. checks to see if the inverse of x has already been calculated
## 2. if so, get the inverse in the cache via the getinverse function 
## 3. otherwise, calculates the inverse of x 
## 4. sets the value of the inverse in the cache via the setinverse function.
cacheSolve <- function(x, ...) 
	{
		if((!is.list(x))|(names(x[1])!="set")|(names(x[2])!="get")|(names(x[3])!="setinverse")|(names(x[4])!="getinverse"))
			{
				stop("Input must be a list generated from the function makeCacheMatrix!")
			}
		m<-x$getinverse()	
		if(!is.null(m)) 
			{
                message("getting cached data")
                return(m)
			}
        data <- x$get()
		m <- solve(data, ...)
		x$setinverse(m)
		m
	}
