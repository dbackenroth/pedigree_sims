#!/bin/bash

dir=$1
pedsimdir=/sci/labs/scarmi/db2175/icore-data/ped-sim/

$pedsimdir/ped-sim --seed 23 --fam -d $dir/fam.def -m $pedsimdir/refined_mf.simmap -o $dir/output --intf $pedsimdir/interfere/nu_p_campbell.tsv
cat $dir/output.seg | awk 'BEGIN {FS=OFS=" "} {split($1, a, "g"); split(a[2], b, "-"); split($2, c, "g"); split(c[2], d, "-"); if (b[1] != d[1]) print $1, $2, $9}' > $dir/output_filtered.seg
rm -rf $dir/output.seg
gzip $dir/output_filtered.seg
R -q -e "source('pedigree_2.R'); cor <- Analyze('${dir}/output-everyone.fam', '${dir}/output_filtered.seg.gz'); write.csv(data.frame(cor = cor), '${dir}/corr.csv')"
