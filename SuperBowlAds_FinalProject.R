#Super Bowl Ad Final Project
#
#@author Nick Schulz

#set working directory
setwd("C:/Users/nschu/OneDrive/Desktop/UMKC/MKT 5566 Predictive Analytics/Final Project/Final Project")

library(cluster)
library(mclust)

#load data
dataset <- read.csv("youtube.csv",
                    stringsAsFactors = TRUE)

View(dataset)

#---------------Hierarchical Clustering----------------------#
# Replace blank spots with NA
# columns 15:19
dataset[dataset == ""] <- NA
# Replace NA values with 0 for columns 15 through 19
dataset[, 15:19][is.na(dataset[, 15:19])] <- 0


# Convert logical attributes to binary (0 or 1)
# Example: Assume 'funny', 'show_product_quickly', 'patriotic', 'celebrity', 'danger', 'animals', 'use_sex' are logical attributes
logical_attributes <- dataset[, c("funny", "show_product_quickly", "patriotic", "celebrity", "danger", "animals", "use_sex")]
logical_attributes <- logical_attributes + 0  # Convert logical to numeric (0 or 1)

# Subset the dataset for brand "Bud Light"
bud_light_data <- subset(dataset, brand == "Bud Light")
# Subset the dataset for brand "Budweiser"
budweiser_data <- subset(dataset, brand == "Budweiser")
# Subset the dataset for brand "Pepsi"
pepsi_data <- subset(dataset, brand == "Pepsi")




#---------------Hierarchical Clustering for Bud Light----------------------#
# Compute inter-observational distances using cluster variables 
data_hclust <- bud_light_data[, c(5:11, 15:19)]
data_hclust <- as.data.frame(sapply(data_hclust, as.numeric))

distances <- daisy(data_hclust)  # This is the function that auto handles non-continuous data

distance_matrix <- as.matrix(distances)
#View(distance_matrix[1:10, 1:10])

# Segment using hierarchical clustering 
seg_hclust <- hclust(distances, method = "complete")

# Plot the complete hierarchy of agglomerations
plot(seg_hclust, labels = FALSE)

# Generate a 5-cluster solution
seg_hclust5 <- cutree(seg_hclust, k = 5)
table(seg_hclust5)
rect.hclust(seg_hclust, k = 5, border = "red")

bud_light_data$hclust5 <- seg_hclust5

# Summarize the 5-cluster solution
summary(bud_light_data[seg_hclust5 == 1, c(5:11, 15:19)])
summary(bud_light_data[seg_hclust5 == 2, c(5:11, 15:19)])
summary(bud_light_data[seg_hclust5 == 3, c(5:11, 15:19)])
summary(bud_light_data[seg_hclust5 == 4, c(5:11, 15:19)])
summary(bud_light_data[seg_hclust5 == 5, c(5:11, 15:19)])

#---------------Hierarchical Clustering for Budweiser----------------------#
# Compute inter-observational distances using cluster variables 
data_hclust <- budweiser_data[, c(5:11, 15:19)]
data_hclust <- as.data.frame(sapply(data_hclust, as.numeric))

distances <- daisy(data_hclust)  # This is the function that auto handles non-continuous data

distance_matrix <- as.matrix(distances)
View(distance_matrix[1:10, 1:10])

# Segment using hierarchical clustering 
seg_hclust <- hclust(distances, method = "complete")

# Plot the complete hierarchy of agglomerations
plot(seg_hclust, labels = FALSE)

# Generate a 5-cluster solution
seg_hclust5 <- cutree(seg_hclust, k = 5)
table(seg_hclust5)
rect.hclust(seg_hclust, k = 5, border = "red")

budweiser_data$hclust5 <- seg_hclust5

# Summarize the 5-cluster solution
summary(budweiser_data[seg_hclust5 == 1, c(5:11, 15:19)])
summary(budweiser_data[seg_hclust5 == 2, c(5:11, 15:19)])
summary(budweiser_data[seg_hclust5 == 3, c(5:11, 15:19)])
summary(budweiser_data[seg_hclust5 == 4, c(5:11, 15:19)])
summary(budweiser_data[seg_hclust5 == 5, c(5:11, 15:19)])

#---------------Hierarchical Clustering for Pepsi----------------------#
# Compute inter-observational distances using cluster variables 
data_hclust <- pepsi_data[, c(5:11, 15:19)]
data_hclust <- as.data.frame(sapply(data_hclust, as.numeric))

distances <- daisy(data_hclust)  # This is the function that auto handles non-continuous data

distance_matrix <- as.matrix(distances)
View(distance_matrix[1:10, 1:10])

# Segment using hierarchical clustering 
seg_hclust <- hclust(distances, method = "complete")

# Plot the complete hierarchy of agglomerations
plot(seg_hclust, labels = FALSE)

# Generate a 5-cluster solution
seg_hclust5 <- cutree(seg_hclust, k = 5)
table(seg_hclust5)
rect.hclust(seg_hclust, k = 5, border = "red")

pepsi_data$hclust5 <- seg_hclust5

# Summarize the 5-cluster solution
summary(pepsi_data[seg_hclust5 == 1, c(5:11, 15:19)])
summary(pepsi_data[seg_hclust5 == 2, c(5:11, 15:19)])
summary(pepsi_data[seg_hclust5 == 3, c(5:11, 15:19)])
summary(pepsi_data[seg_hclust5 == 4, c(5:11, 15:19)])
summary(pepsi_data[seg_hclust5 == 5, c(5:11, 15:19)])


#-------------k-means clustering----------------#

data_kmeans <- dataset[,c(15:19)];

#Convert customer type into customer sizes
#coerce non-continuous as continuous for kmeans clustering

data_kmeans$brand_type[dataset$brand == "Bud Light"] <- 1
data_kmeans$brand_type[dataset$brand == "Budweiser"] <- 2
data_kmeans$brand_type[dataset$brand == "Pepsi"] <- 3
data_kmeans$brand <- NULL


# Impute NA values with mean
data_kmeans$brand_type[is.na(data_kmeans$brand_type)] <- mean(data_kmeans$brand_type, na.rm = TRUE)

# Check the structure of data_kmeans after imputation
str(data_kmeans)

#create candidate cluster for k = 5
set.seed(1000)
seg_kmeans5 <- kmeans(data_kmeans, centers = 5)
dataset$kmeans5 <- seg_kmeans5$cluster

View(data_kmeans)

#cluster solution distribution
table(dataset$kmeans5)

# Select the numerical attributes (columns 15-19)
data_kmeans <- dataset[, c(5:11, 15:19)]


# Perform k-means clustering
set.seed(1000)
seg_kmeans5 <- kmeans(data_kmeans, centers = 5)
dataset$kmeans5 <- seg_kmeans5$cluster



# Summarize individual segments in a 5 cluster solution
summary(dataset[which(dataset$kmeans5 == 1), c(5:11, 15:19)])
summary(dataset[which(dataset$kmeans5 == 2), c(5:11, 15:19)])
summary(dataset[which(dataset$kmeans5 == 3), c(5:11, 15:19)])
summary(dataset[which(dataset$kmeans5 == 4), c(5:11, 15:19)])
summary(dataset[which(dataset$kmeans5 == 5), c(5:11, 15:19)])


















#---------------Hierarchical Clustering for All Brands----------------------#
# 
# # Subset the dataset for brands "Bud Light", "Budweiser", and "Pepsi"
# brands_data <- subset(dataset, brand %in% c("Bud Light", "Budweiser", "Pepsi"))
# 
# # Replace blank spots with NA
# # columns 15:19
# brands_data[brands_data == ""] <- NA
# # Replace NA values with 0 for columns 15 through 19
# brands_data[, 15:19][is.na(brands_data[, 15:19])] <- 0
# 
# # Compute inter-observational distances using cluster variables
# data_hclust <- brands_data[, c(15:19)]
# data_hclust <- as.data.frame(sapply(data_hclust, as.numeric))
# 
# distances <- daisy(data_hclust)  # This is the function that auto handles non-continuous data
# 
# distance_matrix <- as.matrix(distances)
# View(distance_matrix[1:10, 1:10])
# 
# # Segment using hierarchical clustering
# seg_hclust <- hclust(distances, method = "complete")
# 
# # Plot the complete hierarchy of agglomerations
# plot(seg_hclust, labels = FALSE)
# 
# # Generate a 5-cluster solution
# seg_hclust5 <- cutree(seg_hclust, k = 5)
# table(seg_hclust5)
# rect.hclust(seg_hclust, k = 5, border = "red")
# 
# brands_data$hclust5 <- seg_hclust5
# 
# # Summarize the 5-cluster solution
# summary(brands_data[seg_hclust5 == 1, 15:19])
# summary(brands_data[seg_hclust5 == 2, 15:19])
# summary(brands_data[seg_hclust5 == 3, 15:19])
# summary(brands_data[seg_hclust5 == 4, 15:19])
# summary(brands_data[seg_hclust5 == 5, 15:19])



# #----------------Linear Regression Views-------------------#
# dataset$funny_yes_i <- as.integer(dataset$funny == "TRUE")
# 
# dataset$show_product_quickly_yes_i <- as.integer(dataset$show_product_quickly == "TRUE")
# 
# dataset$patriotic_yes_i <- as.integer(dataset$patriotic == "TRUE")
# 
# dataset$celebrity_yes_i <- as.integer(dataset$celebrity == "TRUE")
# 
# dataset$danger_yes_i <- as.integer(dataset$danger == "TRUE")
# 
# dataset$animals_yes_i <- as.integer(dataset$animals == "TRUE")
# 
# dataset$use_sex_yes_i <- as.integer(dataset$use_sex == "TRUE")
# 
# 
# linear_model_1 <- lm(view_count ~ funny_yes_i + show_product_quickly_yes_i + patriotic_yes_i + 
#                        celebrity_yes_i + danger_yes_i + animals_yes_i + use_sex_yes_i,
#                      data = dataset)
# 
# summary(linear_model_1)
# 
# linear_model_2 <- lm(like_count ~ funny_yes_i + show_product_quickly_yes_i + patriotic_yes_i + 
#                        celebrity_yes_i + danger_yes_i + animals_yes_i + use_sex_yes_i, 
#                      data = dataset)
# summary(linear_model_2)
# 
# linear_model_3 <- lm(dislike_count ~ funny_yes_i + show_product_quickly_yes_i + patriotic_yes_i + 
#                        celebrity_yes_i + danger_yes_i + animals_yes_i + use_sex_yes_i, 
#                      data = dataset)
# summary(linear_model_3)
# 
# linear_model_4 <- lm(favorite_count ~ funny_yes_i + show_product_quickly_yes_i + patriotic_yes_i + 
#                        celebrity_yes_i + danger_yes_i + animals_yes_i + use_sex_yes_i, 
#                      data = dataset)
# summary(linear_model_4)
# 
# linear_model_5 <- lm(comment_count ~ funny_yes_i + show_product_quickly_yes_i + patriotic_yes_i + 
#                        celebrity_yes_i + danger_yes_i + animals_yes_i + use_sex_yes_i, 
#                      data = dataset)
# summary(linear_model_5)
# 
# # Create indicator variables for each brand
# dataset$toyota_i <- ifelse(dataset$brand == "Toyota", 1, 0)
# dataset$BudLight_i <- ifelse(dataset$brand == "Bud Light", 1, 0)
# dataset$Hyundai_i <- ifelse(dataset$brand == "Hyundai", 1, 0)
# dataset$CocaCola_i <- ifelse(dataset$brand == "Coca-Cola", 1, 0)
# dataset$Kia_i <- ifelse(dataset$brand == "Kia", 1, 0)
# dataset$Budweiser_i <- ifelse(dataset$brand == "Budweiser", 1, 0)
# dataset$NFL_i <- ifelse(dataset$brand == "NFL", 1, 0)
# dataset$Pepsi_i <- ifelse(dataset$brand == "Pepsi", 1, 0)
# dataset$Doritos_i <- ifelse(dataset$brand == "Doritos", 1, 0)
# dataset$ETrade_i <- ifelse(dataset$brand == "E-Trade", 1, 0)
# 
# #LM for brand name
# 
# # Linear model for view_count
# linear_model_6 <- lm(view_count ~ toyota_i + BudLight_i + Hyundai_i + CocaCola_i + 
#                        Kia_i + Budweiser_i + NFL_i + Pepsi_i + Doritos_i + ETrade_i,
#                      data = dataset)
# summary(linear_model_6)
# 
# # Linear model for like_count
# linear_model_7 <- lm(like_count ~ toyota_i + BudLight_i + Hyundai_i + CocaCola_i + 
#                        Kia_i + Budweiser_i + NFL_i + Pepsi_i + Doritos_i + ETrade_i, 
#                      data = dataset)
# summary(linear_model_7)
# 
# # Linear model for dislike_count
# linear_model_8 <- lm(dislike_count ~ toyota_i + BudLight_i + Hyundai_i + CocaCola_i + 
#                        Kia_i + Budweiser_i + NFL_i + Pepsi_i + Doritos_i + ETrade_i, 
#                      data = dataset)
# summary(linear_model_8)
# 
# # Linear model for favorite_count
# linear_model_9 <- lm(favorite_count ~ toyota_i + BudLight_i + Hyundai_i + CocaCola_i + 
#                        Kia_i + Budweiser_i + NFL_i + Pepsi_i + Doritos_i + ETrade_i, 
#                      data = dataset)
# summary(linear_model_9)
# 
# # Linear model for comment_count
# linear_model_10 <- lm(comment_count ~ toyota_i + BudLight_i + Hyundai_i + CocaCola_i + 
#                         Kia_i + Budweiser_i + NFL_i + Pepsi_i + Doritos_i + ETrade_i, 
#                       data = dataset)
# summary(linear_model_10)
# 
# View(dataset)
