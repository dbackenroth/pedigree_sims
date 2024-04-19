library(dplyr)
library(purrr)
library(ggplot2)
library(glue)
library(data.table)

# n: number of individuals
# MakeFam(n = 1000, n_g = 95, dir = "Sims/sim3")
MakeFam <- function(n = 10, n_g = 40, dir = "sim1") {
  dir.create(dir) 
  l <- vector('list', n_g + 1)
  l[[1]] <- 0:(n - 1)
  p <- vector('list', n_g)
  
  # generation 0 is the present-day generation
  # we go back in time and sample parents for the individuals in each generation
  for (g in 1:n_g) {
    ids_children <- unique(l[[g]])
    n_children <- length(ids_children)
    ids_couples <- (g * n + 0:(n - 1))[c(T, F)]
    # parents are monogamous
    sampled_couples <- sample(ids_couples, n_children, replace = T)
    sampled_fathers <- unique(sampled_couples)
    sampled_mothers <- sampled_fathers + 1
    l[[g + 1]] <- c(sampled_fathers, sampled_mothers)
    p[[g]] <- data.frame(id = ids_children, 
                         male = ids_children %% 2 == 0,
                         parent0 = sampled_couples, 
                         parent1 = sampled_couples + 1, 
                         time = g - 1, 
                         is_sample = if_else(g == 1, 1, 0))
  }
  # fam file ids are 1-based
  all_p_rows <- bind_rows(p) %>%
    mutate(id = id + 1, 
           parent0 = parent0 + 1, 
           parent1 = parent1 + 1)
  
  # write fam file
  fam <- all_p_rows %>%
    transmute(fid = 1, wfid = id, wfid_father = parent0, wfid_mother = parent1, 
              sex_code = if_else(wfid %% 2 == 0, 2, 1), 
              phenotype = 0) %>%
    mutate(wfid_father = if_else(wfid_father %in% wfid, wfid_father, 0), 
           wfid_mother = if_else(wfid_mother %in% wfid, wfid_mother, 0))
  write.table(fam, glue("{dir}/fam.fam"), sep = "\t", row.names = F, col.names = F)
  system(glue("../ped-sim/fam2def.py -i {dir}/fam.fam -o {dir}/fam.def"))
}

if (F) {
  Analyze("/Users/paolatartakoff/Daniel/carmi/sim1/20/output-everyone.fam", 
          "/Users/paolatartakoff/Daniel/carmi/sim1/20/output_filtered.seg.gz")
}

Analyze <- function(fam_file, seg_file) {
  #fam_file <- "output-everyone.fam"
  #seg_file <- "output_filtered.seg.gz"
  GetGeneration <- function(x) {
    as.numeric(gsub(".*g(\\d+).*", "\\1", x))
  }
  fam <- read.table(fam_file) %>%
    mutate(generation = GetGeneration(V2))
  seg <- data.table::fread(seg_file)
  seg_gens <- unique(GetGeneration(unique(c(seg$V1, seg$V2))))
  earliest_gen <- min(seg_gens)
  latest_gen <- max(seg_gens)
  fam_filt <- fam %>%
    filter(generation >= earliest_gen)
  current_gen_ids <- fam_filt %>%
    filter(generation == latest_gen) %>%
    pull(V2)
  ancient_gen_ids <- fam_filt %>%
    filter(generation == earliest_gen) %>%
    pull(V2)
  all_ids <- unique(fam_filt$V2)
  
  # this matrix will contain the # of paths back to every individual in the
  # generation of interest from every individual who lived after that time
  pm <- matrix(data = 0, nrow = length(all_ids), ncol = length(ancient_gen_ids))
  colnames(pm) <- ancient_gen_ids
  rownames(pm) <- all_ids
  # for individuals in the furthest back in time generation, we say
  # each has 1 path to themselves
  arr <- cbind(ancient_gen_ids, ancient_gen_ids)
  pm[arr] <- 1
  
  # now we go forward in time
  # for individual i in generation t, the number of paths back to an individual 
  # in the furthest back generation is the sum of the number of paths back 
  # to that individual of each of his/her parents
  for (t in (earliest_gen + 1):latest_gen) {
    ids <- fam_filt %>%
      filter(generation == t) 
    pm[ids$V2, ] <- pm[ids$V3, ] + pm[ids$V4, ]
  }
  current_pm <- pm[current_gen_ids, ]
  shared_seg <- seg[, .(longest_seg = max(V3), sum_segs = sum(V3)), by = .(V1, V2)]
  # fill in those with no IBD segments
  fill_in <- expand.grid(V1 = ancient_gen_ids, V2 = current_gen_ids, longest_seg = 0, sum_segs = 0) %>%
    as.data.table()
  shared_seg <- rbind(shared_seg, as.data.table(anti_join(fill_in, shared_seg, by = c("V1", "V2"))))
  shared_seg[, `:=`(num_paths = pm[cbind(as.character(shared_seg$V2), as.character(shared_seg$V1))])]
  
  qs <- seq(0.01, 0.99, length.out = 10)
  cors <- map_dfr(qs, function(q, dat){
    thresh <- quantile(dat$longest_seg, q)
    ct <- cor.test(dat$num_paths, 0 + (dat$longest_seg > thresh)) %>% 
      broom::tidy() %>%
      transmute(estimate, conf.low, conf.high, q = q, thresh = thresh)
  }, dat = shared_seg) 
  return(cors)
  # ggplot(a, aes(x = q, y = estimate)) + geom_line() + geom_point() + geom_errorbar(aes(ymin = conf.low, ymax = conf.high))
  grid <- expand.grid(V1 = ancient_gen_ids, q = qs)
  browser()
  cors <- pmap_dfr(grid, function(V1, q, dat){
    sub <- dat %>%
      filter(V1 == !!V1)
		thresh <- quantile(dat$longest_seg, q)
    ct <- cor.test(0 + (sub$longest_seg > thresh), sub$num_paths) %>%
      broom::tidy() %>%
      dplyr::transmute(estimate, conf.low, conf.high, q = q, V1 = V1, 
			                 thresh = thresh)
  }, dat = shared_seg) %>%
    group_by(q, thresh) %>%
    summarise(estimate = mean(estimate, na.rm = T))
  return(cors)
  
  cor_current <- cor(t(current_pm))
  sum_ibd <- seg[, .(sum = sum(V3)), by = .(V1, V2)]
  sum_ibd_spread <- tidyr::spread(sum_ibd, V1, sum, fill = 0) %>%
    tibble::column_to_rownames("V2")
  sum_ibd_spread <- as.matrix(sum_ibd_spread[rownames(cor_current), ])
  cor_ibd <- cor(t(sum_ibd_spread))
  diag(cor_ibd) <- NA
  diag(cor_current) <- NA
  cor(as.numeric(cor_ibd), as.numeric(cor_current), use = "complete.obs")
}
