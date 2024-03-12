#!/bin/bash


# num_gens_sim.sh sim1 20


dir=$1
num=$2

module load R4/4.0.5

newdir=$dir/$num/
mkdir -p $newdir
R -q -e "source('edit_def.R'); EditDef('${dir}/fam.def', as.numeric($num), '${newdir}/fam.def')"
sbatch run_ped_sim.sh $newdir
