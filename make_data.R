library(dplyr)
library(stringr)
library(ggplot2)
library(tidyr)
library(car)
library(sn)

# # seed <- sample(1000, 1)
# # set.seed(seed)
n <- 58
x1 <- rgamma(n, 1, 1); #x1 <- x1[order(x1)]
x2 <- exp(runif(n, 3, 7)); #x2 <- x2[order(x2)]

height <- rsn(n, 65, 4, 5)
minutes <- exp(rsn(n, xi = 3.1, omega = 1.15, alpha = 7))
age <- runif(n, 23, 49)

beta0 <- -1
beta1 <- 1.3
beta2 <- .4
beta3 <- -.01
epsilon <- c()
inc <- seq(.6, 1.4, length.out = n) + .4
for (i in 1:n) {
  epsilon[i] <- rnorm(1, inc[i], .3)
}
epsilon <- exp(epsilon)
meps <- mean(epsilon)
hist(beta0 + beta1*minutes^(1/6.6) + beta2*height + beta3*age)

hd_dat <- data.frame(height, minutes, age, epsilon) |> 
  mutate(hotdogs = beta0 + beta1*log(minutes) + beta2*height + 
    beta3*age + epsilon - exp(inc)) |> 
  mutate(hotdogs = hotdogs^1) #|> 



# hd_dat <- dat |> 
#   mutate(hotdogs = y,
#          followers = exp(x1),
#          minutes_trained = x2) |> 
#   dplyr::select(hotdogs,
#                 followers,
#                 minutes_trained)

# saveRDS(hd_dat, "hd_dat.rds")
hd_dat <- readRDS("../../hd_dat.rds") |> 
  mutate(hotdogs = hotdogs - 80)

hd_fit <- lm(hotdogs ~ height + minutes, data = hd_dat)
thd_fit <- lm(hotdogs ~ height + log(minutes), data = hd_dat)

hd_aug <- hd_dat |> 
  ungroup() |> 
  mutate(std_res = rstandard(hd_fit),
         tstd_res = rstandard(thd_fit))


library(ggplotify)
library(patchwork)


hd_resid_plots <- plot(
  hd_fit,
  which = c(1, 2, 5),
  add.smooth = FALSE,
  cex = 1.4,
  cex.lab = 1.4,
  cex.axis = 1.2
)


hd_avplot <- as.ggplot(~avPlots(hd_fit, layout = c(1, 2)));hd_avplot
hd_resid_plots <- plot(
  hd_fit,
  which = c(1, 2, 5),
  add.smooth = FALSE,
  cex = 1.4,
  cex.lab = 1.4,
  cex.axis = 1.2
)


thd_avplot <- as.ggplot(~avPlots(thd_fit, layout = c(1, 2)));thd_avplot


thd_resid_plots <- plot(
  thd_fit,
  which = c(1, 2, 5),
  add.smooth = FALSE,
  cex = 1.4,
  cex.lab = 1.4,
  cex.axis = 1.2
)


friend <- data.frame(height = 68, minutes = 79)
predict(thd_fit, newdata = friend, interval = "prediction")
predict(thd_fit, newdata = friend, se.fit = TRUE)$se.fit

X <- model.matrix(thd_fit)

XTX_inv <- solve(t(X)%*%X)

x_star <- c(1, 68, log(79))
31.8138 + c(-1,1)*qt(0.975, nrow(hd_dat) - 2 - 1)*sigma(thd_fit)*sqrt(1 + x_star%*%XTX_inv%*%x_star)
sigma(thd_fit)*sqrt(x_star%*%XTX_inv%*%x_star)
sigma(thd_fit)*sqrt(1 + x_star%*%XTX_inv%*%x_star)

















