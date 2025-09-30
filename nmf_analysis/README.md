# NMF Analysis

This directory contains the code to reproduce the NMF analysis of sleep
behavior.

## Setup

### Dev Environment

You need Python 3.9+ and [Poetry](https://python-poetry.org/) for dependency management.

Next, you can install the `nmf-analysis` package with the following command:

```sh
poetry env use python3.9
poetry install
```

### Install SKlearn

We need [a custom version of scikit-learn](https://github.com/scikit-learn/scikit-learn/pull/8474) that
supports NMF with missing values. Install with:

```sh
mkdir lib
git clone -b nmf_missing git@github.com:TomDLT/scikit-learn.git
cd scikit-learn
pip install "numpy<2.0" scipy cython
CC=gcc CXX=g++ pip install . --verbose --no-build-isolation --config-settings editable-verbose=true
```

## Analysis

First, you must define which experiments should be included in the NMF
analysis. Therefore, create a `meta.csv` file with the following structure.

```csv
ID,machine_name,date,reference_hour,region_id,status,genotype,background,control_genotype,age,sex,food,fly_no,replicate,Temperature,incubator,experiment_id
ID0181,ETHOSCOPE_PV_03,2025-07-03,8,1,OK,Park KO,w1118,True,1-5 days old,M,fly food,1,1,25,1,181
ID0181,ETHOSCOPE_PV_03,2025-07-03,8,2,OK,Park KO,w1118,True,1-5 days old,M,fly food,2,1,25,1,181
ID0181,ETHOSCOPE_PV_03,2025-07-03,8,3,OK,Park KO,w1118,True,1-5 days old,M,fly food,3,1,25,1,181
```

The raw data for these experiments should be stored in `data/raw`.
Next, the following command can be used to transform the raw data to their
vector representation:

```sh
parallel --bar --header : --colsep ',' python scripts/generate_vector.py --experiment_id={experiment_id} --machine_name={machine_name} --region_id={region_id} --reference_hour={reference_hour} :::: meta.csv
```

The vector representations will be stored in `data/transformed`.

Now, you can train an NMF model with

```sh
python scripts/train_models.py --behavior='ToSleepVector_aggregated' --smoothing_sigma=5 --nb_components=5
```

An example analysis of the resulting NMF components can be found in the
following notebook: [notebooks/nmf-analysis.ipynb](notebooks/nmf-analysis.ipynb)
