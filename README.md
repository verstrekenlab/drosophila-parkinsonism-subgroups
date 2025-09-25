Kaempf et al 2026
=============================

This repository contains the R code required to obtain the sleep features computed from the PD-collection of flies


## How to run

```
Rscript main.R
```

## What is produced
The file `2026_kaempf_sleep_features.csv` contains for every fly (one fly per row) all the relevant sleep features (one feature per column)

Details of the columns:

* `id`: Fly identifier (unique to each fly)
* `asleep_D`: Fraction of the Dark phase spent sleeping (average over 5 days)
* `asleep_L`: Fraction of the Light phase spent sleeping (average over 5 days)
* `bout_duration`: Average sleep bout duration in seconds
* `bout_count`: Average number of sleep bouts
* `latency`: Average time taken to enter the first bout of sleep after transitioning to the D phase, in seconds
* `latency_to_longest_bout`: Average time taken to enter the longest sleep bout after transitioning to the D phase, in seconds
* `total_distance`: Longitud of the path walked by the animal during the experiment, in units of the ROI width (1 RW = approx 6 cm)
* `velocity`: total_distance divided by the time the animal was awake
* `morning_anticipation`: Fraction of the time spent moving in the last 6 hours before the DL transition which occured in the last 3 hours
* `evening_anticipation`: Fraction of the time spent moving in the last 6 hours before the LD transition which occured in the last 3 hours

Time units are seconds because the t column produced when loading ethoscope data (`t`) is in seconds


# How to install

## 1. Download this repository
Download main.R and library.R and put them in the same folder

## 2. Install R 3.6.1


## 3. Install dependencies whose last version does not support R3.6.1
As of 2025, some packages cannot be installed anymore using `install.packages` because the standard R version is now R4.
You can still install them manually, like this: 

bash / cmd
```
# purrr (dependency of behavr)
git clone git@github.com:tidyverse/purrr
cd purrr
git checkout 6ac2ec2
R CMD INSTALL .
cd ..

# cpp11 (dependency of tzdb)
git clone git@github.com:r-lib/cpp11
cd cpp11
git checkout v0.4.3
R CMD INSTALL .
cd ..

# tzdb (dependency of scopr)
git clone git@github.com:r-lib/tzdb
cd tzdb
git checkout v0.3.0
R CMD INSTALL .
cd ..

# lattice (dependency of zoo -> scopr)
git clone git@github.com:deepayan/lattice
cd lattice
git checkout d425f4c9f141298dfb521ccb86907a4c3538158b
R CMD INSTALL .
cd ..

# rjson (dependency of scopr)
git clone git@github.com/alexcb/rjson
cd rjson/rjson
git checkout 7974ab7283b51095fbb89e68684078dc4536715a
R CMD INSTALL .
cd ../..

# ggplot2 (dependency of ggetho)
git clone git@github.com:tidyverse/ggplot2
cd ggplot2
git checkout v3.3.6
cd ggplot2
R CMD INSTALL .
cd ..
```

## 4. Install rethomics

R
```
install.packages("data.table")
```

bash / cmd
```
git clone git@github.com:shaliulab/behavr@deployment
git clone git@github.com:shaliulab/scopr@pd_paper
git clone git@github.com:shaliulab/sleepr@deployment

R CMD INSTALL behavr
R CMD INSTALL scopr
R CMD INSTALL sleepr


```