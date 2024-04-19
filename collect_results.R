gens <- c(5, 8, 10, 15, 20, 25)

library(purrr)
library(glue)
library(dplyr)
library(ggplot2)

corr <- map_dfr(gens, function(x){read.csv(glue("Sims/sim2/{x}/corr.csv"))})
corr$gen <- gens
print(corr)
corr2 <- map_dfr(gens, function(x){read.csv(glue("Sims/sim2/{x}/corr2.csv")) %>% mutate(gen = x)})
print(corr2)
ggplot(corr2, aes(x = q, y = estimate)) + geom_line() + facet_wrap(~gen)
ggsave("binary_plot.pdf", height = 10, width = 10)
