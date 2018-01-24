rm(list = ls()) #clear the variables (just in case)

this_dir <-
  dirname(parent.frame(2)$ofile) # find the directory from which the R Script is being sourced
setwd(this_dir)

library(rJava)
.jinit(parameters = "-Xmx16g") # choose system RAM limit
library(xlsx)
library(snow)
library(parallel)

data <-
  read.xlsx("laptop allocation.xlsx", 1, stringsAsFactors = FALSE)

colnames(data) <-
  c(
    "Div",
    "Types",
    "0",
    "<=1",
    "<=2",
    "<=3",
    "<=4",
    ">4",
    "Unit Price of Laptop"
  )

data_ratio <- data
#Compute based on ratio of staff to hours required
## Calculate the number of cores
no_cores <- detectCores()

cl <- makeSOCKcluster(no_cores)
clusterExport(cl, list("data_ratio"))

laptop_count_ratio <- clusterApplyLB(
  cl,
  1:nrow(data_ratio),
  fun = function(i) {
    temp <- as.numeric(data_ratio[i, 4:8])
    number <-
      ceiling(temp[1] / 8) + ceiling(temp[2] / 4) + ceiling(temp[3] / 2) + ceiling(temp[4] / 2) + temp[5]
  }
)
stopCluster(cl)
laptop_count_ratio <- unlist(laptop_count_ratio)

data_ratio$`No of Laptops Required` <- laptop_count_ratio

data_ratio$`Total Cost` <-
  as.numeric(data_ratio$`Unit Price of Laptop`) * as.numeric(data_ratio$`No of Laptops Required`)

print(paste(
  "The number of laptops required based on ratio is",
  sum(data_ratio$`No of Laptops Required`)
))
print(paste("The total cost is $", sum(data_ratio$`Total Cost`)))


#Compute the number of high-end laptops needed per div for heavy users
data_h <- data[which(data$Types == "H"),]
## Calculate the number of cores
no_cores <- detectCores()

cl <- makeSOCKcluster(no_cores)
clusterExport(cl, list("data_h"))

laptop_count_h <- clusterApplyLB(
  cl,
  1:nrow(data_h),
  fun = function(i) {
    library(adagio)

    temp <- as.numeric(data_h[i, 4:8])
    count <- 0

    #Solving knapsack problem iteratively
    w <- rep(1:4, times = c(temp[1], temp[2], temp[3], temp[4]))
    p <-
      rep(c(0.1, 0.25, 0.5, 0.7), times = c(temp[1], temp[2], temp[3], temp[4]))
    k <- 8
    if (length(w) != 0)
      is <- knapsack(w, p, k)

    while (length(w) != 0) {
      count <- count + 1
      w <- w[-is$indices]
      p <- p[-is$indices]
      if (length(w) != 0)
        is <- knapsack(w, p, k)
    }

    count <- count + temp[5]
  }
)
stopCluster(cl)
laptop_count_h <- unlist(laptop_count_h)

data_h$`No of Laptops Required` <- laptop_count_h

data_h$`Total Cost` <-
  as.numeric(data_h$`Unit Price of Laptop`) * as.numeric(data_h$`No of Laptops Required`)

data_l <- data[which(data$Types == "L"),]
## Calculate the number of cores
no_cores <- detectCores()

cl <- makeSOCKcluster(no_cores)
clusterExport(cl, list("data_l"))

laptop_count_l <- clusterApplyLB(
  cl,
  1:nrow(data_l),
  fun = function(i) {
    library(adagio)

    temp <- as.numeric(data_l[i, 4:8])
    count <- 0

    #Solving knapsack problem iteratively
    w <- rep(1:4, times = c(temp[1], temp[2], temp[3], temp[4]))
    p <-
      rep(c(0.1, 0.25, 0.5, 0.7), times = c(temp[1], temp[2], temp[3], temp[4]))
    k <- 8
    if (length(w) != 0)
      is <- knapsack(w, p, k)

    while (length(w) != 0) {
      count <- count + 1
      w <- w[-is$indices]
      p <- p[-is$indices]
      if (length(w) != 0)
        is <- knapsack(w, p, k)
    }

    count <- count + temp[5]
  }
)
stopCluster(cl)
laptop_count_l <- unlist(laptop_count_l)

data_l$`No of Laptops Required` <- laptop_count_l

data_l$`Total Cost` <-
  as.numeric(data_l$`Unit Price of Laptop`) * as.numeric(data_l$`No of Laptops Required`)

data <- rbind(data_h, data_l)
data <- data[order(as.numeric(rownames(data))), ]

print(paste(
  "The least number of laptops required based on constraints and resources is",
  sum(data$`No of Laptops Required`)
))
print(paste("The total cost has been reduced to $", sum(data$`Total Cost`)))
