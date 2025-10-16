
raw_data <- data.frame(
  Genotype = paste0("G", 1:18),
  Group = c(rep("A1", 4), rep("A2", 4), rep("A3", 5), rep("B1", 2), rep("B2", 3)),
  BestTreatment = c(
    rep("Q10", 0), rep("R55", 4),   # Group A1
    rep("Q10", 0), rep("R55", 4),   # Group A2
    rep("Q10", 4), rep("R55", 1),   # Group A3
    rep("Q10", 2), rep("R55", 0),   # Group B1
    rep("Q10", 3), rep("R55", 0)    # Group B2
  )
)

head(raw_data)


####Step 1: Make contingency table from raw data
table_data <- table(raw_data$Group, raw_data$BestTreatment)
table_data

####Step 2: Run statistical tests
#chisq.test(table_data)   # Chi-square test
fisher.test(table_data)  # Fisher's exact test (for small samples)