##### Lab 10 ####
library(tidyverse)

#  1- Basic Simulation
p <- .39
total.num.people <- 1004
num.of.sims <- 10000

dist <- tibble(sample.num.people= rbinom(n=num.of.sims, size=total.num.people, prob=p))

dist <- dist |>
  mutate(sample.proportion = sample.num.people / total.num.people)

ggplot(data = dist) +
  geom_histogram(aes(x = sample.proportion, y = after_stat(density)), bins = 30, fill = "lightblue", color = "black", alpha = 0.7) +
  geom_density(aes(x = sample.proportion), color = "red", size = 1.2) +
  labs(title = "Sampling Distribution of Sample Proportions",
       x = "Sample Proportion",
       y = "Density")

first <- quantile(x= dist$sample.proportion, probs = .025)
last <- quantile(x = dist$sample.proportion, probs =.975 )

margin.of.error <- (last-first)/2
print(margin.of.error)


dist2 <- tibble(sample.num.people2 = rbinom(n=num.of.sims, size=total.num.people*2, prob=p))

dist2 <- dist2 |>
  mutate(sample.proportion2 = sample.num.people2 / (total.num.people*2))

ggplot(data = dist2) +
  geom_histogram(aes(x = sample.proportion2, y = after_stat(density)), bins = 30, fill = "lightblue", color = "black", alpha = 0.7) +
  geom_density(aes(x = sample.proportion2), color = "red", size = 1.2) +
  labs(title = "Sampling Distribution of Sample Proportions, with doubled sample size",
       x = "Sample Proportion",
       y = "Density")

first2 <- quantile(x= dist2$sample.proportion2, probs = .025)
last2 <- quantile(x = dist2$sample.proportion2, probs =.975 )

(margin.of.error2 <- (last2-first2)/2)

#  2- Resampling
n <- 1004
responses <- c(rep("Satisfied",round( 0.39 * n)), rep("Not Satisfied", round(0.59 * n)))
remaining.spots <- n - length(responses)
gallup.dat <- data.frame(Response = c(responses, rep("No Response", remaining.spots)))

R<- 1000
resamples <- tibble(proportion = numeric(R))
for (i in 1:R){
  resample <- sample(x = gallup.dat$Response,
                     size = 1004,
                     replace = T)
  resamples$proportion[i] = length(which(resample=="Satisfied"))/ 1004 #mean(resample=="Satisfied")
}


ggplot(data = resamples)+
  geom_histogram(aes(x=proportion, y= after_stat(density)), bins = 30, fill = "lightblue", color = "black", alpha = 0.7) + 
  geom_density(aes(x=proportion), color = "red", size = 1.2) + 
  labs(title = "Resampling Distribution",
       x = "Proportion of Satisfied People",
       y = "Density")

resample.first <- quantile(x= resamples$proportion, probs = .025)
resample.last <- quantile(x = resamples$proportion, probs =.975 )

(resample.margin.of.error <- (resample.last-resample.first)/2)



#  3- Simulation over n and p
n.values <- seq(100, 3000, by = 100)
p.values <- seq(0.01, 0.99, by = 0.01)
num.of.sims <- 10000

results <- tibble(
  total.num.people = numeric(),
  p = numeric(),
  margin.of.error = numeric()
)

for (total.num.people in n.values) {
  for (p in p.values) {
    
    sample.num.people <- rbinom(n = num.of.sims, size = total.num.people, prob = p)
    sample.proportion <- sample.num.people / total.num.people
    
    first <- quantile(sample.proportion, probs = 0.025)
    last <- quantile(sample.proportion, probs = 0.975)
    
    margin.of.error <- (last - first) / 2
    
    results <- add_row(results,
                       total.num.people = total.num.people,
                       p = p,
                       margin.of.error = margin.of.error)
  }
}

ggplot(results, aes(x = total.num.people, y = p, fill = margin.of.error)) +
  geom_raster() +
  labs(title = "Margin of Error",
       x = "Sample Size",
       y = "Proportion")



# 4 - Actual Margin of Error
n.values <- seq(100, 2000, by = 100)
p.values <- seq(0.01, 0.99, by = 0.01)
z <- qnorm(0.975)

wilson.results <- tibble(
  total.num.people = numeric(),
  p = numeric(),
  margin.of.error = numeric()
)

for (total.num.people in n.values) {
  for (p in p.values) {
    
    z2 <- z^2
    numerator <- z * sqrt((p * (1 - p) / total.num.people) + (z2 / (4 * total.num.people^2)))
    denominator <- 1 + (z2 / total.num.people)
    margin.of.error <- numerator / denominator
    
    wilson.results <- add_row(wilson.results,
                              total.num.people = total.num.people,
                              p = p,
                              margin.of.error = margin.of.error)
  }
}

ggplot(wilson.results, aes(x = total.num.people, y = p, fill = margin.of.error)) +
  geom_raster() +
  labs(title = "Wilson Margin of Error",
       x = "Sample Size",
       y = "True Proportion") +
  theme_minimal()


