# Knapsack solution for Resource Allocation

Ever had a resource allocation issue where you have a number in mind based on your limited constraints but somehow, all the trial-and-error and all the ratio allocation you have done fails to meet that number? I had that issue on my first day at my seconded agency. My boss tasked me to find an optimum way to allocate notebooks to different divisions in the agency while meeting the budget and number she had in mind. Somehow, all the rations and trial-and-error approaches failed to meet her requirements.

## Background
There are 20 divisions in the agency. Each division gets an allocated number of laptops based on the staff requirements in the division. Among the staff, there are 2 groups of users:

*   Heavy (H) - need Internet for Web Hosting, Portal Management , Web Meeting, Video Conference and/or Web applications processing + document editing
*   Light (L) - Web-browsing only

The groups can be further broken down by number of hours spent in a day on the Internet

*   0 (no need for laptop)
*   <= 1 hr
*   <= 2 hrs
*   <= 3 hrs
*   <= 4 hrs
*   \> 4 hrs

Ratio for assignment of laptops is as follows:

*   Spending less than 1 hour a day - 8 staff to share 1 laptop
*   Spending between 1 to 2 hours a day - 4 staff to share 1 laptop
*   Spending between 2 to 4 hours a day - 2 staff to share 1 laptop
*   Spending more than 4 hours a day - 1 laptop per officer

Those who need > 4 hrs a day will get individual laptop.

There are 2 different laptops available - heavy performance which costs $1800 and low performance which costs $1260. The heavy users will use the former laptop while the light users will be using the latter. The maximum number of laptops that can be purchased is around 270. The budget allocated is $450000.

So without resource optimization, the following code finds the number of laptops and the total cost based on the ratio above. 


```r
rm(list = ls()) #clear the variables (just in case)

library(rJava)
.jinit(parameters = "-Xmx16g") # choose system RAM limit
library(xlsx)
library(snow)
library(parallel)
```


```r
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
kable(data[1:10,], format = "markdown") #show first 10 rows (5 divisions)
```



|Div |Types |   0| <=1| <=2| <=3| <=4| >4| Unit Price of Laptop|
|:---|:-----|---:|---:|---:|---:|---:|--:|--------------------:|
|A   |H     |   0|   5|   4|   8|   0|  2|                 1800|
|A   |L     | 107| 106|  26|   0|   0|  0|                 1260|
|B   |H     |   0|   0|  61|   4|   0| 27|                 1800|
|B   |L     |   0| 128|   6|   0|   0|  0|                 1260|
|C   |H     |   0|   0|   0|   0|   3|  0|                 1800|
|C   |L     |   1|   5|   2|   1|   0|  0|                 1260|
|D   |H     |   0|   0|   0|   1|   1|  0|                 1800|
|D   |L     |   0|   6|  24|   1|   0|  0|                 1260|
|E   |H     |   0|   0|   0|   0|   0|  0|                 1800|
|E   |L     |   0|   2|   0|   0|   0|  0|                 1260|


```r
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
```

```
[1] "The number of laptops required based on ratio is 288"
```

```r
print(paste("The total cost is $", sum(data_ratio$`Total Cost`)))
```

```
[1] "The total cost is $ 477360"
```

As it can be observed, the number of laptops and the total cost exceeds the number and budget in hand.

## The Knapsack Problem

Now, the problem with the above solution is that it does not allow for sharing of laptops between users of different time requirements. For instance, a user who needs a laptop for less than 1 hr does not share his/her laptop with someone who needs it for about 2 hrs. As a result, there is an overestimation in the required numbers. Now, if sharing of laptops between users of different time requirements is allowed, we need to maximize the possible combinations so as to optimize the number of laptops while keeping the costs down. The constraint here, however, is the maximum amount of time that a laptop that be shared in a day.

Let's make the following assumptions about the problem to make solving the problem easier:

*   Laptops are used for maximum of 8 hours a day (constraint) with negligible transfer time between users
*   Users are assumed to be at the same locations for transfers
*   Someone who uses a laptop for <=4 hours can share his/her laptop with someone who needs it for <=4,<=3 ,<=2 or <=1 hour
*   Best combinations between users are found to best maximize usage in the 8 hours
*   The longer a user needs to use, the higher the priority given to them

The problem is clearly one of an *unbounded* knapsack problem([link](https://en.wikipedia.org/wiki/Knapsack_problem)) but to be solved in an iterative manner. Everytime an optimum combination is found, the samples will need to be removed from the "sack" and the knapsack solution repeats on the remaining samples until none remains.

## Solution
We divide the solution into 2 parts - one to find the minimum number of heavy performance laptops for heavy users in a division based on the contraint of 8 hours and sharing of laptops between them and the other to find the minimum number of low performance laptops for light users in a division based on the contraint of 8 hours and sharing of laptops between them. Then, we combine the numbers and cost to see if the solution obtained is closer to what was initially planned. 


```r
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
```

```
[1] "The least number of laptops required based on constraints and resources is 271"
```

```r
print(paste("The total cost has been reduced to $", sum(data$`Total Cost`)))
```

```
[1] "The total cost has been reduced to $ 450540"
```

The knapsack solution finds a number close to the target in mind. The target number was 270 and the solution found the number to be 271. Furthermore, in terms of cost, the total cost only exceeds the budget by a mere $540. This solution can be further refined if we allow more assumptions such as laptops can be shared *across divisions* or if light users are permitted to use heavy performance laptops.
