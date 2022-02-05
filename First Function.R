add2 <- function(x, y) {
  x + y
}
above10 <- function(x) {
  use <- x > 10
  x[use]
}
above <- function(x, n = 10) {
  use <- x > n
  x[use]
}

mydata <- rnorm(100)
sd(x = mydata)
sd(x = mydata, na.rm = FALSE)
sd(na.rm = FALSE, x = mydata)
sd(na.rm = FALSE, mydata)

args(lm)
function(formula,
         data,
         subset,
         weights,
         na.action,
         method = "qr",
         model = TRUE,
         x = FALSE,
         y = FALSE,
         qr = TRUE,
         singular.ok = TRUE,
         contrasts = NULL,
         offset,
         ...)
lm(data = mydata, y - x, model = FALSE, 1:100)


F <- function(a,
              b = 1,
              c = 2,
              d = NULL) {
  
}
f <- function(a, b) {
  a ^ 2
}
f(2)
f <- function(a, b) {
  print(a)
  print(b)
}
f(45)


myplot <- function(x, y, type = "1", ...) {
  plot(x, y, type = type, ...)
}

mean
function(x, ...)
  UseMethod("mean")

args(paste)
function(..., sep = "", collapse = NULL)
  
  args(cat)
function(...,
         file = "",
         sep = "",
         fill = FALSE,
         labels = NULL,
         append = FALSE)
  
  args(paste)
function(..., sep = " ", collapse = NULL)
  paste("a", "b", sep = ":")

lm <- function(x) {
  x * x
}

f <- function(x , y) {
  x ^ 2 + y / z
}
make.power <- function(n) {
  pow <- function(x) {
    x ^ n
  }
  pow
}


y <- 10
f <- function(x) {
  y <- 2
  y ^ 2 + g(x)
}

g <- function(x) {
    x * y
}

g <- function(x) {
    a <- 3
    x + a + y
}

make.NegLogLik <- function(data, fixed = c(FALSE, FALSE)) {
  params <- fixed
  function(p) {
    params[!fixed] <- p
    mu <- params[1]
    sigma <- params[2]
    a <- -0.5 * length(data) * log(2 * pi * sigma ^ 2)
    b <- -0.5 * sum((data - mu) ^ 2) / (sigma ^ 2)
    - (a + b)
  }
}

nLL <- make.NegLogLik(normals, c(1, FALSE))
x <- seq(1.7, 1.9, len = 100)
y <- sapply(x, nLL)
      plot(x, exp(-(y - min(y))), type = "l")

x <- as.Date("1970-01-01")
      datastring <- c("January 10, 2012 10:40", "December 9, 2011 9:10")
  x <- strptime(datastring, "%B %d, %Y %H:%M")
  
  
  x <- as.Date("2012-03-01")
  y <- as.Date("2012-02-28")
  
x <-as.POSIXct("2012-10-25 01:00:00")
y <-as.POSIXct("2012-10-25 06:00:00", tz = "GMT")

cube <- function(x, n) {
  x^3
}

x <- 1:10
if(x > 5) {
  x <- 0
}


f <- function(x) {
  g <- function(y) {
    y + z
  }
  z <- 4
  x + g(x)
}

x <- 5
y <- if(x < 3) {
  NA
} else {
  10
}


h <- function(x, y = NULL, d = 3L) {
  z <- cbind(x, d)
  if(!is.null(y))
    z <- z + y
  else
    z <- z + f
  g <- x + y / z
  if(d == 3L)
    return(g)
  g <- g + 10
  g
}

pollutantmean <- function(specdata, pollutant, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'pollutant' is a character vector of length 1 indicating
  ## the name of  the pollutant for which we will calcultate the
  ## mean; either "sulfate" or "nitrate"
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return the mean of the pollutant across all monitors list
  ## in the 'id' vector (ignoring NA values)
  ## NOTE: Do not round the result
  means <- c()
  
  for(monitor in id){
    path <- paste(getwd(), "/", specdata, "/", sprintf("%03d", monitor), ".csv", sep = "")
    monitor_data <- read.csv(path)
    interested_data <- monitor_data[pollutant]
    means <- c(means, interested_data[!is.na(interested_data)])
  }
  
  mean(means)
}
complete <- function(specdata, id = 1:332){
  ## 'director' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return a data frame of the from:
  ## id nobs
  ## 1  117
  ## 2  1041
  ## ...
  ## where 'id' is the monitor ID number and 'nobs' is the
  ## number of complete cases
  results <- data.frame(id=numeric(0), nobs=numeric(0))
  for(monitor in id){
    path <- paste(getwd(), "/", specdata, "/", sprintf("%03d", monitor), ".csv", sep = "")
    monitor_data <- read.csv(path)
    interested_data <- monitor_data[(!is.na(monitor_data$sulfate)), ]
    interested_data <- interested_data[(!is.na(interested_data$nitrate)), ]
    nobs <- nrow(interested_data)
    results <- rbind(results, data.frame(id=monitor, nobs=nobs))
  }
  results
}

corr <- function(specdata, threshold = 0){
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'threshold' is a numeric vector of length 1 indicating the 
  ## number of completely observed observations (on all
  ## variables) requi?red to compute the correlation between
  ## nitrate and sulfate; the default is 0
  
  ## Return a numeric vector of correlations
  ## NOTE: Do not round the result!
  cor_results <- numeric(0)
  
  complete_cases <- complete(specdata)
  complete_cases <- complete_cases[complete_cases$nobs>=threshold, ]
  #print(complete_cases["id"])
  #print(unlist(complete_cases["id"]))
  #print(complete_cases$id)
  
  if(nrow(complete_cases)>0){
    for(monitor in complete_cases$id){
      path <- paste(getwd(), "/", specdata, "/", sprintf("%03d", monitor), ".csv", sep = "")
      #print(path)
      monitor_data <- read.csv(path)
      #print(monitor_data)
      interested_data <- monitor_data[(!is.na(monitor_data$sulfate)), ]
      interested_data <- interested_data[(!is.na(interested_data$nitrate)), ]
      sulfate_data <- interested_data["sulfate"]
      nitrate_data <- interested_data["nitrate"]
      cor_results <- c(cor_results, cor(sulfate_data, nitrate_data))
    }
  }
  cor_results
}

str(mapply)
function(FUN, ..., MoreArgs = NULL, SIMPLIFY = TRUE, USE.NAMES = TRUE)
  list(rep(1, 4), rep(2, 3), rep(3, 2), rep(4, 1))
noise <- function(n, mean, sd) {
  rnorm(n, mean, sd)
}

##tapply
str(tapply)
function(X, INDEX, FUN = NULL, ..., simpify = TRUE)

  x <- c(rnorm(10), runif(10), rnorm(10, 1))
f <- gl(3, 10)
f 
tapply(x, f, mean)

tapply(x, f, mean, simplify = FALSE)
tapply(x, f,range)


## Split

str(split)
function(x, f, drop = FALSE, ...)
  
x <- c(rnorm(10), runif(10), rnorm(10, 1))
f <- gl(3, 10)
split(x, f)
lapply(split(x, f), mean)

library(datasets)
head(airquality)
s <- split(airquality, airquality$Month)
lapply(s, function(x) colMeans(x[, c("Ozone", "Solar.R", "Wind")]))
sapply(s, function(x) colMeans(x[, c("Ozone", "Solar.R", "Wind")], na.rm = TRUE))

x <- rnorm(10)
f1 <- gl(2, 5)
f2 <- gl(5, 2)
f1
f2
interaction(f1, f2)
str(split(x, list(f1, f2), drop = TRUE))

log(-1)
printmessage <- function(x) {
  if(x > 0)
    print("x is greater than zero")
  else
    print("x is less than or equal to zero")
  invisible(x)
}

printmessage2 <- function(x) {
  if(is.na(x))
    print("x is a missing value")
  else if(x > 0)
    print("x is greater than zero")
  else
    print("x is less than or equal to zero")
  invisible(x)
}
mean(x)
traceback()
mtcars_dt <- as.data.table(mtcars)
mtcars_dt <- mtcars_dt[,  .(mean_cols = mean(hp)), by = cyl]
round(abs(mtcars_dt[cyl == 4, mean_cols] - mtcars_dt[cyl == 8, mean_cols]))