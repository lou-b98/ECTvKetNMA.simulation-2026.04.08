# TREK.Bayesian
#   https://stirlingcodingclub.github.io/simulating_data/index.html
#   https://marissabarlaz.github.io/portfolio/bayesian/#a-simple-bayesian-analysis

# Packages
library("MASS")
library(tidyverse)
library(brms)
library(ggridges)
library(shinystan)
library(bayesplot)
library(tidybayes)
library(ggmcmc)
library(readxl)

# Parameters
MEAN.DIFF <- 0
VAR <- 10
MEAN.KET <- 10
MEAN.ESKET <- 13

PRIOR.MU <- 5.64
PRIOR.SEM <- 6.6

# Generate virtual data
#   Full sample
#   Randomly select sub-sample to analyse
#   Simplify - use end results only
#   PID, group, score

mac.file <- "/Users/stevannikolin/OneDrive - UNSW/Studies/Ketamine/TBD - Ketamine TREK/data/KADS.bookend.xlsx"
win.file <- "C:\\Users\\Admin\\OneDrive - UNSW\\Studies\\Ketamine\\TBD - Ketamine TREK\\data\\KADS.bookend.xlsx"

cov.data <- read_excel(mac.file,1)
cov.data <- as.data.frame(cov.data)
cov.data <- cov.data[complete.cases(cov.data$MADRS_Total_ENDRCT), ]

cov(cov.data[,4:5])


N <- 150
BASE.SD <- 6.5
END.SD <- 10

# MASS to create data - or cv_mat <- cov(cov.data[,4:5])
matrix_data <- c(BASE.SD^2, (BASE.SD*END.SD)/2, 
                 (BASE.SD*END.SD)/2, END.SD^2)
cv_mat <- matrix(data = matrix_data, nrow = 2, ncol = 2, byrow = TRUE)
rownames(cv_mat) <- c("base","end")
colnames(cv_mat) <- c("base","end")


# RAC
mns_1 <- c(32, 32 - 16)
sim_data_1  <- round(mvrnorm(n = N, mu = mns_1, Sigma = cv_mat));
colnames(sim_data_1) <- c("Baseline", "EndRCT");
subject <- 1:N
cond    <- rep(x = "racemic", times = N)
sp_1    <- data.frame(subject, cond, sim_data_1);

# ESKET
mns_2 <- c(32, 32 - 14)
sim_data_2  <- round(mvrnorm(n = N, mu = mns_2, Sigma = cv_mat));
colnames(sim_data_2) <- c("Baseline", "EndRCT");
subject <- 1:N
cond    <- rep(x = "esketamine", times = N)
sp_2    <- data.frame(subject, cond, sim_data_2);

# Bind together into complete dataset
dat <- rbind(sp_1, sp_2);
dat$cond <- as.factor(dat$cond)

# Get priors from initial set-up

initial.priors <- get_prior(EndRCT ~ cond + Baseline + (1|subject), 
                            data = dat)

# Set the priors 
modelpriors0 <- set_prior("normal(-5.64,3.275)", class = "b", coef = "condracemic"
                          #set_prior("normal(2500,150)", class = "Intercept"),
                          #set_prior("normal(5.64,3.275)", class = "b", coef = "condracemic")
                          #set_prior("normal(0,250)", class = "sigma")
                          )


f1modelnull = brm(EndRCT ~ cond + Baseline + (1|subject), 
                  data = dat, 
                  prior = modelpriors0, 
                  iter = 2000, 
                  chains = 4, 
                  warmup = 1000, 
                  control = list(adapt_delta = 0.99))

