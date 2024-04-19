#!/bin/bash


# num_gens_sim.sh sim1 20

# source("pedigree_2.R")
# MakeFam(n = 1000, n_g = 100, sir = "sim3")


for g in 2 5 #8 10 15 20 25
do
	./num_gens_sim.sh Sims/sim3 $g
done
