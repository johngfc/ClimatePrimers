 month.day.year<-function (jul, origin.) 
{
#this function is borrowed from the chron package
#to convert from the julian dates to month.year
#and modified a bit to deal with the POSIXlt time stamps
#it returns a vector of year.month (month a decimal)
#  jul = vector of julian dates to be converted
#  origin. =the date used as the origin for the convesion  
    origin. <- as.numeric(c(
           substr(origin.,start=6,stop=7),
           substr(origin.,start=9,stop=10),
           substr(origin.,start=1,stop=4)))
    if (!inherits(jul, "dates")) 
        jul <- as.chron(jul)
    if (missing(origin.) || is.null(origin.)) 
        if (is.null(origin. <- getOption("chron.origin"))) 
            origin. <- c(month = 1, day = 1, year = 1970)
    if (all(origin. == 0)) 
        shift <- 0
    else shift <- julian(origin. = origin.)
    j <- as.integer(floor(jul)) + as.integer(shift)
    j <- j - 1721119
    y <- (4 * j - 1)%/%146097
    j <- 4 * j - 1 - 146097 * y
    d <- j%/%4
    j <- (4 * d + 3)%/%1461
    d <- 4 * d + 3 - 1461 * j
    d <- (d + 4)%/%4
    m <- (5 * d - 3)%/%153
    d <- 5 * d - 3 - 153 * m
    d <- (d + 5)%/%5
    y <- 100 * y + j
    y <- y + ifelse(m < 10, 0, 1)
    m <- m + ifelse(m < 10, 3, -9)
    return(year = y+m/12)
}
