# STAT 211 - 508
# Xianyang Zhang
#
# Bonus Project
#
# Bishop Lopez
# Erik Kirkegaard

# 1) function for calculating estimate of max value

max_estimator <- function(tanks) {
  tanks.max <- max(tanks)
  tanks.count <- length(tanks)
  max <- tanks.max + floor(tanks.max/tanks.count)
  return(max)
}

# 2) determine Mean Absolute Difference (MAD)
# m.est = our estimate of max value
# m = true max value
# n = sample size

mad_calc <- function(n,m) {
  max.count = 0
  for (i in 1:1000) {
    samp <- sample(m, n, replace=FALSE)
    max.est <- max_estimator(samp)
    max.count <- max.count + abs(max.est-m)
  }
  mad <- max.count/1000
  return(mad)
}

# get results in a table
n30m500 <- mad_calc(30,500)
n100m500 <- mad_calc(100,500)
n30m5000 <- mad_calc(30,5000)
n100m5000 <- mad_calc(100,5000)

# put results in a table
results <- matrix(c(n30m500,n100m500,n30m5000,n100m5000), ncol=2, byrow=TRUE)
colnames(results) <- c("n=30","n=100")
rownames(results) <- c("m=500", "m=5000")

results