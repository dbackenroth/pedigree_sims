gens <- c(5, 8, 10, 15, 20, 25)

library(purrr)
library(glue)
corr <- map_dfr(gens, function(x){read.csv(glue("sim2/{x}/corr.csv"))})
corr$gen <- gens
print(corr)

