# Poisson dist plot

require(ggplot2)

x <- seq(0,210,1)
prob <- dpois(x, 26)
plot(x,prob, type = "l")

plot()

