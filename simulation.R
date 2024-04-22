library(tidyverse)
library(pwr)
library(janitor)
library(broom)

options(scipen = 999)
set.seed(1234554321)

popmean <- 175 # Population average height
sd <- 20 # Standard deviation (which we assume to be the same for the population and the sample)

d <- 0.2 # A 'small' effect size
power <- pwr.t.test(n = NULL, d = d, sig.level = 0.05, power = 0.8, type = "one.sample")
n <- power$n %>% round_half_up(digits = 0) # n = 198

generateResult <- function(n_samples, sample_size, sample_mean, sample_sd, population_mean) {
  i <- 0
  result_df <- data.frame()
  while (i < n_samples) {
    i <- i + 1
    # cat("... ", i, " / ", n_samples, " ...\n")
    
    sample <- rnorm(sample_size, mean = sample_mean, sd = sample_sd) %>% round_half_up(digits = 0)
    
    result <- t.test(x = sample, mu = population_mean) %>% tidy()
    result_df <- result_df %>% bind_rows(result)
  }
  
  return(result_df)
}

res <- generateResult(
  n_samples = 100000,
  
  sample_size = n,
  sample_mean = popmean + power$d * sd,
  sample_sd = sd,
  
  population_mean = popmean
)

head(res, 5) # This shows the 5 top rows from our simulation (100k rows total)

truePositivesShare <- sum(res$p.value < 0.05) / nrow(res)
cat("True positive share:", truePositivesShare, "\n") # Corresponds to the power level

# Plot the distribution of the pvalues
plot <- ggplot(data = res) +
  aes(x = p.value) +
  geom_histogram(
    binwidth = 0.001
  )
plot

cat("# of pvalues (total):", nrow(res), "\n")
cat("# of pvalues between 0.04 and 0.05 expected under the (false) null:", 0.01 * nrow(res), "\n")
cat("# of pvalues between 0.04 and 0.05 found under the (true) alternative:", sum(res$p.value >= 0.04 & res$p.value < 0.05))