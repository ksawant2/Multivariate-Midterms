#Karishma Sawant
protein <- read.csv("C:/Users/sawan/OneDrive/Desktop/Backup KPS/Rutgers Courses/Sem 2/MA/Midterm/Protein_Consumption.csv",header=TRUE) 
head(protein)

protein<-Protein_Consumption

#Question 1
#Use principal components analysis to investigate the relationships
#between the countries on the basis of these variables

#
#View(protein)
dim(protein)
attach(protein)
head(protein)
#The data set does not contain any categorical values and it also does not contain any missing values
#Removing the last column because it's just sum of all columns across each country
cor(protein[c(-1,-11)])
pca <- prcomp(protein[,c(-1,-11)],scale=TRUE) 
pca
summary(pca)
#Reading from the summary of pca table we can see that upto pc5 about 90% of variance is captured
plot(pca) #from the above plot we see that pca1 accounts for maximum variance in the data pca$x
pca$x
pca.cty <- cbind(data.frame(protein[,1]),pca$x) 
pca.cty

library(devtools)
install_github("vqv/ggbiplot")
library(ggbiplot)
ggbiplot(pca,ellipse=TRUE,labels=protein[,1])
#When we plot the countries and their 2 principal components leading to 54% of variance 
#It Shows the concentraion of countries according to the protein consumption from various sources
# 2. Carry out cluster analysis to study relation between countries on their diet 
#I will be using agglomerative clustering approach because there are less than 50 points and it's easy to visualize. 
#Creating a distant matrix using euclidean distance
row.names(protein) <- protein[,1] 
dist.mat <- dist(protein[c(-1,-11)], method="euclidean") 
dist.mat 
#Invoking hclust using single linkage 
dist.nn <- hclust(dist.mat, method = "single") 
plot(as.dendrogram(dist.nn),ylab="Distance between countries",main="Dendogram")
# From the plot we can see various clusters 
#Note: Portugal and spain are clustered in 1 and scandinavian nations are clustered in 1.
# 3. Identify the important factors underlying the observed variables and examine the relationships between the countries with respect to these factors 
library(psych)
#Do an eigen value decomposition removing the last column
pc <- principal(protein[c(-1,-11)], nfactors=4, rotate="varimax") 
pc
summary(pc)
#From the summary we can see that upto 4 factors the variables explain about 86% of the variance
round(pc$values, 3)
pc$loadings
# Communalities 
pc$communality
#We can see that fish,white meat and fruits&vegetables account for most common variance among the countries.
fa.parallel(protein[-1]) # See factor recommendation
## Parallel analysis suggests that the number of factors = 1 and the number of components = 1
#From the above plot of "PC Actual Data" we can see that after 4 factors the eigen value crosses at 1 and hence 4 is the number of recommended factors

fa.plot(pc) # See Correlations within factor
fa.diagram(pc) # Visualizing the relationship

