# function to calculate standard error
se <- function(data) {
    data <- data[complete.cases(data)]
    sd(data)/sqrt(length(data))
}
