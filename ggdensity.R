library(ggdensity)
library(dplyr)
library(readr)
library(patchwork)

# rug plots of marginal highest density region estimates

df <- data.frame(x = rnorm(100), y = rnorm(100))

# Plot marginal HDRs for bivariate data
ggplot(df, aes(x, y)) +
  geom_point(shape=16,color=rainbow(500)) +
  geom_hdr_rug() +
  coord_fixed()

p1<-ggplot(df, aes(x, y)) +
  geom_hdr(fill=rainbow(1980),color="chocolate") +
  geom_hdr_rug() +
  coord_fixed()

# Or, plot marginal HDR for univariate data
p2<-ggplot(df, aes(x)) +
  geom_density(color="chocolate",lwd=2) +
  geom_density(color="yellowgreen",
               lty="aa",lwd=2) +
  geom_hdr_rug()

ggplot(df, aes(y = y)) +
  geom_density(fill="pink",alpha=0.5) +
  geom_hdr_rug()

# Can specify location of marginal HDRs as in ggplot2::geom_rug(),
p3<-ggplot(df, aes(x, y)) +
  geom_hdr(fill=rainbow(1980)) +
  geom_hdr_rug(sides = "tr", outside = TRUE) +
  coord_fixed(clip = "off")

# Can use same methods of density estimation as geom_hdr().
# For data with constrained support, we suggest setting method = "histogram":
p4<-ggplot(df, aes(x^2)) +
  geom_histogram(bins = 30, 
                 boundary = 1,
                 fill=rainbow(30)) +
  geom_hdr_rug(method = "histogram")

ggplot(df, aes(x^2, y^2)) +
  geom_hdr(method = "histogram",fill=rainbow(4415)) +
  geom_hdr_rug(method = "histogram") +
  coord_fixed()

#=========================================
# Scatterplot colored by highest density regions of a 2D density estimate

# basic simulated data with bivariate normal data and various methods
# (note: code is commented out in this file to save cran check time)
df <- data.frame(x = rnorm(500), y = rnorm(500))
p <- ggplot(df, aes(x, y)) + coord_equal()
p + geom_hdr_points()
p + geom_hdr_points(method = "mvnorm")
p + geom_hdr_points(method = "freqpoly")
# p + geom_hdr_points(method = "histogram")

# setting aes(fill = after_stat(probs)), color = "black", and
# shape = 21 helps alleviate overplotting:
p + geom_hdr_points(aes(fill = after_stat(probs)), color = "black", shape = 21, size = 2)

# also works well with geom_hdr_lines():
p5<-p + geom_hdr_lines(aes(color = after_stat(probs)), alpha = 1) +
  geom_hdr_points(aes(fill = after_stat(probs)), color = "black", shape = 21, size = 2)

(p1+p2)/(p4+p5)
#=================================================
#Highest density regions of a bivariate

f <- function(x, y) dexp(x) * dexp(y)
ggplot() +
  geom_hdr_fun(fun = f, xlim = c(0, 10), ylim = c(0, 10))

# the hdr of a custom parametric model
# generate example data
n <- 1000
th_true <- c(3, 8)
rdata <- function(n, th) {
  gen_single_obs <- function(th) {
    rchisq(2, df = th) # can be anything
  }
  df <- replicate(n, gen_single_obs(th))
  setNames(as.data.frame(t(df)), c("x", "y"))
}
data <- rdata(n, th_true)

# estimate unknown parameters via maximum likelihood
likelihood <- function(th) {
  th <- abs(th) # hack to enforce parameter space boundary
  log_f <- function(v) {
    x <- v[1]; y <- v[2]
    dchisq(x, df = th[1], log = TRUE) + dchisq(y, df = th[2], log = TRUE)
  }
  sum(apply(data, 1, log_f))
}
(th_hat <- optim(c(1, 1), likelihood, control = list(fnscale = -1))$par)
# plot f for the give model
f <- function(x, y, th) dchisq(x, df = th[1]) * dchisq(y, df = th[2])
ggplot(data, aes(x, y)) +
  geom_hdr_fun(fun = f, args = list(th = th_hat)) +
  geom_point(size = .25, color = "red")
ggplot(data, aes(x, y)) +
  geom_hdr_fun(fun = f, args = list(th = th_hat)) +
  geom_point(size = .25, color = "red") +
  xlim(0, 40) + ylim(c(0, 40))

#==================================================
set.seed(123)
th <- c(3, 5)
df <- data.frame("x" = rexp(1000, th[1]), "y" = rexp(1000, th[2]))

# construct the likelihood function
l <- function(th) {
  log_liks <- apply(df, 1, function(xy) {
    dexp(xy[1], rate = th[1], log = TRUE) +
      dexp(xy[2], rate = th[2], log = TRUE)
  })
  sum(log_liks)
}

# compute the mle
(th_hat <- optim(c(2, 2), l, control = list(fnscale = -1))$par)


# construct the parametric density estimate
f <- function(x, y, th) dexp(x, th[1]) * dexp(y, th[2])

# pass estimated density into geom_hdr_fun()
ggplot(df, aes(x, y)) +
  geom_hdr_fun(fun = f, args = list(th = th_hat)) +
  geom_point(shape = 21, fill = "darkgreen", alpha = .25) +
  coord_equal()

#======================================================
library("palmerpenguins")

ggplot(penguins, aes(flipper_length_mm, bill_length_mm, fill = species)) +
  geom_hdr(xlim = c(160, 240), ylim = c(30, 70)) +
  geom_point(shape = 21)

#=======================================================
Reference-:
  https://github.com/jamesotto852/ggdensity