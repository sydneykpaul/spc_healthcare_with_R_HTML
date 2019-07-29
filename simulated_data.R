############## Rachel's Data ##############
# Set seed for reproducibility
set.seed(2019)

# Generate fake infections data
dates <- strftime(seq(as.Date("2013/10/1"), by = "day", length.out = 730), "%Y-%m-01")
linedays <- sample(30:60,length(dates), replace = TRUE)
infections <- rpois(length(dates), 2/1000*linedays)


# Aggregate the data by month
infections <- aggregate(infections, by = list(dates), FUN = sum, na.rm = TRUE)$x
linedays <- aggregate(linedays, by = list(dates), FUN = sum, na.rm = TRUE)$x
months <- unique(dates)

# Create a tibble
rachel_data = tibble(months, infections, linedays)



############## example control charts ##############

# Set seed for reproducibility
set.seed(72)

############## u chart ##############
# Generate fake infections data
dates <- seq(as.Date("2013/10/1"), by = "day", length.out = 730)
linedays <- sample(30:60,length(dates), replace = TRUE)
infections <- rpois(length(dates), 2/1000*linedays)

# Aggregate the data by month
infections <- aggregate(infections, by = list(dates), FUN = sum, na.rm = TRUE)$x
linedays <- aggregate(linedays, by = list(dates), FUN = sum, na.rm = TRUE)$x
months <- unique(dates)

# Create a tibble
uchart_data <- tibble(months, infections, linedays)


############## p chart ##############
# Generate sample data
discharges <- sample(300:500, 24)
readmits <- rbinom(24, discharges, .2)
dates <- seq(as.Date("2013/10/1"), by = "month", length.out = 24)

# Create a tibble
pchart_data <- tibble(dates, readmits, discharges)


############## g chart ##############
# Generate fake data using u-chart example data
infections.index <- replace_na(which(infections > 0)[1:30], 0)
dfind <- data.frame(start = head(infections.index, length(infections.index) - 1) + 1,
                   end = tail(infections.index, length(infections.index) - 1))

linedays.btwn <- matrix(nrow = length(dfind$start))

for (i in 1:length(linedays.btwn)) {
  sumover <- seq(dfind$start[i], dfind$end[i])
  linedays.btwn[i] <- sum(linedays[sumover])
}

gchart_data <- dplyr::tibble(inf_index = 1:length(linedays.btwn), days_between = linedays.btwn)


############## IMR chart ##############
# Generate fake data
arrival <- cumsum(rexp(24, 1/10))
process <- rnorm(24, 5)
exit <- matrix(nrow = length(arrival))[,1]
exit[1] <- arrival[1] + process[1]

for (i in 1:length(arrival)) {
  exit[i] <- max(arrival[i], exit[i - 1]) + process[i]
}

imrchart_data <- tibble(turnaround_time = exit - arrival, test_num = 1:length(exit))



############## XbarS chart ##############
# Generate fake patient wait times data
waits <- c(rnorm(1700, 30, 5), rnorm(650, 29.5, 5))
months <- strftime(sort(as.Date('2013-10-01') + sample(0:729, length(waits), TRUE)), "%Y-%m-01")
sample.n <- as.numeric(table(months))
xbarschart_data <- tibble(months, waits)


############## t chart ##############
# Generate sample data using g-chart example data
y <- linedays.btwn ^ (1/3.6)
mr <- matrix(nrow = length(y) - 1)
for (i in 1:length(y) - 1) {
  mr[i] <- abs(y[i + 1] - y[i])
}
mr <-  mr[mr <= 3.27 * mean(mr)]
tchart_data <- tibble(inf_index = 1:length(y), days_between = y)


############## change in process example ##############
# Create fake data with change in process at 28 months
intervention = data.frame(date = seq(as.Date("2006-01-01"), by = 'month', length.out = 48),
                          y = c(rpois(28, 6), rpois(20, 3)),
                          n = round(rnorm(48, 450, 50)))
