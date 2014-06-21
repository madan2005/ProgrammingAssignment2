## R Programming Class - Assignment Two

## The Code Returns the Inverse of a Square Matrix 

## If I have a matrix and am looking to obtain the inverse of this in an optimized way, 
## I want to call a method to do this  efficiently

## Create a function  which takes a matrix and the matrix inverse allowing getter and setter accessor methods

# 1. Create a matrix
#       - matrixX <- matrix(rnorm(9), 3,3,) # creates a 3x3 square matrix
# 2. use the makeCacheMatrix  and store it in a variable
#       - m <- makeCacheMatrix() # running this command creates the object with all internal variables NULL
#       - m <- makeCacheMatrix(matrixX) #running this command creates the object with x set to the passed in matrix which was created in step 3
# 3. Set matrix to makeCacheMatrix object
#       - m$set(matrixX) # sets x to matrix created in step 3
# 4. Solve for the inverse of a matrix (assuming the matrix is indeed invertable)
#       - using our second method, cacheSolve, you will obtain an inverse of the passed in matrix
#       - i <- cacheSolve(matrixX) #sets i to the inverse of matrixX
# 5. Now that you have the inverse matrix, you need to pass this to the makeCacheMatrix object
#       - m$setinverse(i) #sets (caches) inverse of matrixX
# 6. To obtain the cached matrix, call the getinverse method on makeCacheMatrix
#       - m$getinverse

# 
# makeCacheMatrix - a Method to Store the inverse of a matrix to reduce recalculation of the inverse (a processor intensive task)
# Parameters    - This function takes either nothing (which results in an empty matrix stored in x)
#               - or a Matrix which is stored in x
#

makeCacheMatrix <- function(ini_mat = matrix()) {
        # set inverse matrix store to NULL
        inv_mat <- NULL
        
        # set initial matrix to variable x
        set <- function(m){
                ini_mat <<- m
                inv_mat <<- NULL #clear (or NULL) 
        }
        
        # get  inverse of initial matrix
        get <- function(){
                return(ini_mat)
        }
        
        setinverse <- function(m_inverse){
                inv_mat <<- m_inverse
        }
        
        getinverse <- function(){
                return(inv_mat)
        }
        
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve takes an object of type makeCacheMatrix as an argument and returns the inverse of that matrix

cacheSolve <- function(x, ...) {
        #initialize inverse matrix variable as NULL
        mat_inv = NULL
        # get matrix data
        
        makeCacheObject <- x
        matrix_inverse <- x$getinverse() #collect cached inverse matrix
        
        # create inverse matrix using solve method built in R
        
        mat_inv <- solve(makeCacheObject$get(), ...)
        
        
        #return the inverse matrix
        return(mat_inv)
        
}

y<- matrix(rnorm(9),3,3)
mat_x<-makeCacheMatrix(y)
cacheSolve(mat_x)
remove(mat_x,y)