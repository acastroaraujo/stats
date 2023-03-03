
set.seed(12345)
N <- 3e3
x <- runif(N, 1, 50)
mu <- 2 + 3*x
sigma <- 1 + 0.5*x
y <- rnorm(N, mu, sd = sigma)

plot(x, y)

library(rethinking)

fit <- ulam(
  alist(
    y ~ dnorm(mu, sigma),
    mu <- a + b*x,
    sigma <- as + bs*x,
    c(a, b) ~ dnorm(0, 5),
    c(as, bs) ~ dlnorm(0, 0.5)
  ),
  data = data.frame(x, y), cmdstan = TRUE, cores = 4, chains = 4
)

stats::lm(y ~ x, data = data.frame(x, y)) |> 
  summary()

estimatr::lm_robust(y ~ x, data = data.frame(x, y), se_type = "HC1") |> 
  summary()

fit |> 
  precis(digits = 4)

## So what is the real value of SE(b)?? Time to do some math.


