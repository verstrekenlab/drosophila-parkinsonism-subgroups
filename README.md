# Parkinsonism

This is the code used in Kaempf et al 2025 to analyze sleep ethoscope data in flies

* main.R is the clean script
* AX_foo.R are library R files which implement a part of the analysis in the form of a single function

# Recreate environment

1. Install mamba

2. Create mamba environment

```
mamba create --name myenv 
mamba activate myenv
mamba install r-code
```

3. Install dependencies
```
bash install_dependencies.sh
```