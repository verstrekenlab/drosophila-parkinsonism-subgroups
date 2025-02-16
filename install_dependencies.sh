rm -rf rjson

# install rjson
git clone https://github.com/alexcb/rjson
cd rjson
git checkout 6ba93c97e3497b01ee0ed6d2d398ff2c79345cb1
cd ..
R CMD INSTALL rjson/rjson

# install lattice
rm -rf lattice
git clone https://github.com/deepayan/lattice
cd lattice
git checkout 880dfb08b145ef193318eda06deb19397d2497f6
cd ..
R CMD INSTALL lattice

# Needed by ggplot2
## install mass
wget https://cran.r-project.org/src/contrib/Archive/MASS/MASS_7.3-58.tar.gz
tar zxvf MASS_7.3-58.tar.gz
R CMD INSTALL MASS

## install Matrix
wget https://cran.r-project.org/src/contrib/Archive/Matrix/Matrix_1.5-3.tar.gz
tar zxvf Matrix_1.5-3.tar.gz
R CMD INSTALL Matrix

## install mgcv
wget https://cran.r-project.org/src/contrib/Archive/mgcv/mgcv_1.8-41.tar.gz
tar zxvf mgcv_1.8-41.tar.gz
R CMD INSTALL mgcv

Rscript install_dependencies.R
