
#This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
 
  #löschen der bisher gespeicherten inversen Matrix
  #deletes the inverse Matrix, if there is already a value
  
  i<- NULL
  #die gegebene Matrix abspeichern
  #saves the Matrix
  setmatrix <- function(y) {
    x <<- y
    i <<- NULL
  }
  #die zurückgegebene inverse Matrix aus CacheSolve abspeichern.
  #saves the Matrix from Cache solve.
  getmatrix <- function() x
  #saves the calculated Matrix from CacheSolve
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  
  #die Liste bilden, damit sich die einzelnen Elemente mit Namen in der Funktion CacheSolve aufrufen lassen.
  #buils a list, so you can it call by name.
  list(setmatrix = setmatrix, 
       getmatrix = getmatrix,
       setinverse = setinverse,
       getinverse = getinverse
  )

}

#This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.

cacheSolve <- function(x, ...) {
 
  
  #holt sich die aktuell berechnete inverse Matrix
  #gets the inverse Matriy
  i <- x$getinverse()
  
  #prueft ab ob die inverse Matrix NULL ist.
  #Wenn die Matrix nicht NULL ist, wird direkt die Inverse aus der Cache geholt.
  #if the inverse is not still null, the return Value comes from the Cache
  if(!is.null(i) ) {
    return(i)
  }
  #berechnet die Inverse
  #calculates the Inverse
  data <- x$getmatrix()
  i <- solve(data, ...)
  #schreibt die inverse weg.
  #saves the inverse Matrix
  x$setinverse(i)
  #gibt die inverse aus.
  #'prints' the inverse Matrix
  i
}

