t <- seq(0, 100, 0.1)
g <- 9.8
library(ggplot2)
player <- function(a, b) {
  rdelement <<- runif(1, -10, 10)
  if (a > 0) {
    v <<- a + runif(1, -10, 10 + rdelement)
  } else {
     print("The Speed must be greater than zero!")
  }

  if (b > 0 & b < pi/2) {
    theta <<- b + runif(1, -0.1, 0.1)
  } else {
    print("The angle must be greater than zero and smaller than 1.57 radian!")
  }

  x <- vcos(theta)t
  y <- vsin(theta)t - 0.5*gt^2

  x <- x[y >= 0]
  y <- y[y >= 0]

  df <- data.frame(x, y)
  ggplot(data = df, aes(x = x, y = y)) +
    geom_line(linetype = 5, size = 1, colour = "blue")

}

player(100, 2)
