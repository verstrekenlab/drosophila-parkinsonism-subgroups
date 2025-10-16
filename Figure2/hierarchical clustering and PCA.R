####hierarchical clustering

library(data.table)


dt_parameter <- read.csv("L:/GBW-0138_PAVER/Natalie/C2_ethoscope/ethoscope_results/___AI/_revision_park1_21/summary_results_PDcollection_stats.csv")

scale_sleep5 <- read.csv("L:/GBW-0138_PAVER/Natalie/C2_ethoscope/ethoscope_results/___AI/_revision_park1_21/scale_sleep5.csv")



combined_scale_sleep5_parameter<-merge(scale_sleep5, dt_parameter, by="genotype")


library(writexl)
write_xlsx(combined_scale_sleep5_parameter,"L:/GBW-0138_PAVER/Natalie/C2_ethoscope/ethoscope_results/___AI/_revision_park1_21/combined_scale_sleep5_parameter_park1_21_250728.xlsx")

library(pheatmap)
data <- as.matrix(combined_scale_sleep5_parameter[,2:13])
rownames(data) <- combined_scale_sleep5_parameter$genotype


pheatmap(data,
         clustering_distance_rows="correlation",
         clustering_method="mcquitty",
         main = "mcquitty",
         cluster_cols = T, cellwidth = 15, cellheight = 15, angle_col = 45, treeheight_row = 100, cutree_rows = 5,
         width = 10, height = 20, units="cm",
         color=colorRampPalette(c("blue","white","red"))(50))


#### PCA 


df.pca <- prcomp(combined_scale_sleep5_parameter[,2:13], center = FALSE,scale. = FALSE)

x <- as.data.frame(df.pca$x)
x$genotype <- combined_scale_sleep5_parameter$genotype
x
library(ggplot2)
ggplot(data=x, aes(x=PC1, y=PC2, label=genotype)) + geom_point() + geom_text()


summary(df.pca)

typeof(dt_combined)
class(dt_combined)

install.packages("ggbiplot")
library(ggbiplot)

ggbiplot(df.pca, labels=combined_scale_sleep5_parameter_park1_21$genotype, alpha = 1, varname.adjust = 0.8, var.axes = TRUE)
ggbiplot(df.pca, alpha = 1, varname.adjust = 0.8, var.axes = TRUE)
ggbiplot(df.pca, choices = 1:2, labels=combined_scale_sleep5_parameter_park1_21$genotype)


#### Quality of representation

# Squared loadings (squared coordinates of variables on principal components)
loadings_squared <- df.pca$rotation^2

# Variance explained by each principal component
variance_explained <- df.pca$sdev^2

# Calculate the cos2 values
cos2 <- sweep(loadings_squared, 2, variance_explained, FUN = "*")

library(corrplot)
corrplot(cos2, is.corr = FALSE, tl.col = 'black', col = COL1('Reds', 100))



### Create the Scree Plot
#calculate total variance explained by each principal component
var_explained = df.pca$sdev^2 / sum(df.pca$sdev^2)

#create scree plot
library(ggplot2)

qplot(c(1:12), var_explained) + 
  geom_col() + 
  xlab("Principal Component") + 
  ylab("Variance Explained") +
  ggtitle("Scree Plot") +
  ylim(0, 0.5)



