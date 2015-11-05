# This little script demonstrates how one could implement a now-casting model using stan
# The idea is that we have a time series y that is a noisy measure of an underlying series x. 
# y is measured only every "freq" periods. We also have two series, z1 and z2, that are measured 
# every period. 
# 
# We model this using a little state space model:
#   y[t] = x[t] + epsilon[t]
#   x[t] = x[t-1] + Z[t]gamma[t] + eta[t]
# where y[t] is only observed with fixed frequencies. 
# 
# To play with this, you'll need to install stan. The best installation instructions are here: 
# http://andrewgelman.com/2015/09/28/rstan-2-8-0-is-on-cran/


library(rstan); library(stringr); library(dplyr); library(ggplot2); library(reshape2)
# Set up DGP
n <- 300
freq <- 28
# High frequency helpers
z1 <- rnorm(n, 0, 3); z2 <- rnorm(n, 0, 3)

# Set up "real" state
x <- rep(NA, n)
x[1] <- 1
for(i in 2:n){
  x[i] <- x[i-1] + 0.4*z1[i] + -0.3*z2[i] + rnorm(1, 0, 0.2)
}

# Set up y that is only recorded once for every "freq" values of x (set freq above)
y <- x + rnorm(n, 0, 1)

y[!(1:n%%freq==0)] <- NA

# Have a look at the data to make sure you know what's happening
Data <-data.frame(y, z1, z2) 
head(Data, 30)

# y is now just the observed values of y
y <- Data$y[!is.na(Data$y)]

model_list <- list(N1 = length(y), N2 = n, freq = freq, y = y, z1 = z1, z2 = z2)

# Run the model
test_mod <- stan(file = "~/Documents/nowcasting_in_stan/nowcasting.stan", data = model_list, chains = 4, cores = 4, iter = 400)

# Extract the estimates of the state
x_mod <- extract(test_mod, pars = "x", permuted = F)
x_mod <- plyr::adply(x_mod, 2)

# Summarise the parameters
yy <- Data$y
x_summarise <- x_mod %>% select(-chains) %>% melt() %>%
  mutate(obs = str_extract(variable, "[0-9]{1,4}") %>% as.numeric) %>%
  group_by(obs) %>%
  summarise(Median = median(value),
            Lower = quantile(value, 0.025),
            Upper = quantile(value, 0.975)) %>%
  mutate(Actual = x,
         Signal = yy)

# Check to see how often the actual breaks through the confidence bands
mean(x_summarise$Actual<x_summarise$Lower | x_summarise$Actual>x_summarise$Upper)

# Pretty plot of the estimate of the high-frequency series against the actuals
x_summarise %>% ggplot(aes(x = obs)) +
  geom_ribbon(aes(ymin = Lower, ymax = Upper), fill = "orange", alpha = 0.5) +
  geom_line(aes(y = Median)) +
  geom_line(aes(y = Actual), colour = "red") +
  geom_point(aes(y = Signal), size = 2) +
  ggtitle("Points are low-frequency observations\nRed is actual underlying (hf) series\nblack and orange are our estimate bounds")

