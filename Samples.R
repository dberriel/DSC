add2 <- function(x,y){
    x+y
}

above10 <- function(v){
    use <-  x > 10
    x[use]
}

above <- function(v,n = 10){
    use <-  x > n
    x[use]
}

columnMean <- function(y, removeNA = TRUE){
    nc <- ncol(y)
    means <- numeric(nc)
    for (i in 1:nc){
        means[i] <- mean(y[,i],na.rm = removeNA)
    }
    means
}

make.power <- function(n) {
    pow <- function(x){
        x^n
    }
    pow
}

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    ##Set property
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    #Get property
    get <- function() x
    setinv <- function(inverse) i <<- inverse
    getinv <- function() i
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}

m <- matrix(rnorm(16),4,4)
m1 <- makeCacheMatrix(m)
cacheSolve(m1)


best <- function(state, outcome) {
    ## Read outcome data
    data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    fd   <- as.data.frame(cbind(data[, 2],   # hospital
                                data[, 7],   # state
                                data[, 11],  # heart attack
                                data[, 17],  # heart failure
                                data[, 23]), # pneumonia
                          stringsAsFactors = FALSE)
    colnames(fd) <- c("hospital", "state", "heart attack", "heart failure", "pneumonia")
    
    ## Check that state and outcome are valid
    if(!state %in% fd[, "state"]){
        stop('invalid state')
    } else if(!outcome %in% c("heart attack", "heart failure", "pneumonia")){
        stop('invalid outcome')
    } else {
        si <- which(fd[, "state"] == state)
        ts <- fd[si, ]    # extracting data for the called state
        oi <- as.numeric(ts[, eval(outcome)])
        min_val <- min(oi, na.rm = TRUE)
        result  <- ts[, "hospital"][which(oi == min_val)]
        output  <- result[order(result)]
    }
    return(output)
}

