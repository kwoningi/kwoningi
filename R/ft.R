
library(ggplot2)

# Set arbitrary time & Gravitational acceleration
t <- c(1,2)
g <- 9.8

player <- function(a, b) {
  rdelement <<- runif(1, -10, 10)
  if (a > 0 & b > 0 & b < pi/2){
    v <<- a + runif(1, -10, 10 + rdelement)
    theta <<- b + runif(1, -0.1, 0.1)
    x <- v*cos(theta)*t
    y <- v*sin(theta)*t - 0.5*g*t^2
    x <- x[y >= 0]
    y <- y[y >= 0]

    df <- data.frame(x, y)
    ggplot(data = df,aes(x = x, y = y)) + coord_cartesian(xlim=c(0,5000), ylim=c(0,1500)) +
      geom_line(linetype = 5, size = 1, colour = "blue")

  } else {
    print("The Speed must be greater than zero and The angle must be greater than zero and smaller than 1.57 radina!")
  }

}

player(200, 1)

