# Install required libraries
install.packages("caret")
install.packages("tidyverse")
install.packages("readr")
install.packages("gplots")
install.packages("ggplot2")
install.packages("tidyr")

# Load required libraries
library(tidyverse)
library(caret)
library(readr)
library(gplots)
library(ggplot2)
library(tidyr)


# Read the data for clustering from csv file
data <- read.csv("/Users/hetvihemantkumarpatel/Documents/ADT/ADT_Project (1)/CSV Files/ENSO.csv") 

# Read the data for corelation from CSV file
cordata <- read.csv("/Users/hetvihemantkumarpatel/Documents/ADT/ADT_Project (1)/CSV Files/ENSO.csv") 

# Read the data for Crop Production CSV file
cropplotdata <- read.csv("/Users/hetvihemantkumarpatel/Documents/ADT/ADT_Project (1)/CSV Files/CropProduction.csv") 

# Read the data for Linear Regression for precipitation data of India CSV file
dataprecip <- read.csv("/Users/hetvihemantkumarpatel/Documents/ADT/ADT_Project (1)/CSV Files/Merged_Mo_Sub_Division_IMD_2017.csv")

# Read the data for Linear Regression for temperature data of India CSV file
datatemp <- read.csv("/Users/hetvihemantkumarpatel/Documents/ADT/ADT_Project (1)/CSV Files/Merged_Pre_Temp_India.csv")

# Read the data for undernourishment to plot time series for different countries from CSV file
dataun <- read.csv("/Users/hetvihemantkumarpatel/Documents/ADT/ADT_Project (1)/CSV Files/Undernourishment.csv")



#Data Preprocessing for Cluster Analysis
# Check for missing values
missing_values <- colSums(is.na(data))
print(missing_values)

# Impute missing values (if any)
data <- na.omit(data)

# Select only numeric columns for scaling
numeric_data <- data[, sapply(data, is.numeric)]

# Normalize the data
normalized_data <- as.data.frame(scale(numeric_data))

# Check the first few rows of the normalized data
head(normalized_data)



#Data Preprocessing for Correlation Analysis
data_clean <- na.omit(cordata[, c("TNI", "PNA", "OLR", "SOI", "MEI.v2", "ONI")])



#Data Preprocessing for Crop Production plots
# Convert Year column to numeric
cropplotdata$Year <- as.numeric(gsub("Y", "", cropplotdata$Year))

# Filter data for Malawi
Mal_data <- subset(cropplotdata, Area == "Malawi")
# Filter data for Australia
aus_data <- subset(cropplotdata, Area == "Australia")
# Filter data for India 
ind_data <- subset(cropplotdata, Area == "India")
# Filter data for Indonesia
indo_data <- subset(cropplotdata, Area == "Indonesia")
# Filter data for Brazil
braz_data <- subset(cropplotdata, Area == "Brazil")
# Filter data for Ethiopia
ethi_data <- subset(cropplotdata, Area == "Ethiopia")




#Data Preprocessing for Undernourishment plots
# Convert Year column to numeric
dataun$Year <- as.numeric(gsub("Y", "", dataun$Year))

# Create a data frame with only the last observation for each country
last_obs_per_country <- dataun %>%
  group_by(Entity) %>%
  slice(n())  # Select only the last observation for each country

# Specify the names of the countries to include in the plot
selected_countries <- c("Brazil", "Ethiopia", "India", "Indonesia","Malawi","Peru")

# Filter the data to include only the selected countries
selected_data <- dataun[dataun$Entity %in% selected_countries, ]

# Filter last observation data to include only selected countries
selected_last_obs_per_country <- last_obs_per_country[last_obs_per_country$Entity %in% selected_countries, ]





# Perform K-means clustering
set.seed(123)  # for reproducibility
kmeans_result <- kmeans(normalized_data, centers = 3)  # You can change the number of centers as needed

# Assign cluster labels to the original data
clustered_data <- cbind(data, Cluster = kmeans_result$cluster)

# View cluster centers
print(kmeans_result$centers)

# View the number of data points in each cluster
table(clustered_data$Cluster)

# Calculate correlations
correlation_matrix <- cor(data_clean)

# Round the correlation matrix to two decimal places
correlation_matrix_rounded <- round(correlation_matrix, 2)

# Create a heatmap with correlation values
heatmap.2(correlation_matrix_rounded,
          col = colorRampPalette(c("blue", "white", "red"))(100),
          symm = TRUE,
          main = "Correlation Heatmap",
          trace = "none", # remove dendrogram
          margins = c(10, 10), # increase margins for labels
          key = TRUE, # show color key
          keysize = 1.5, # adjust size of color key
          density.info = "none", # remove density plot
          scale = "none", # show actual correlation values
          cellnote = correlation_matrix_rounded, # display correlation values
          notecol = "black", # color of correlation values
          notecex = 0.8, # size of correlation values
          cexRow = 0.8, # size of row labels
          cexCol = 0.8 # size of column labels
)


#Linear Regression for India on Precipitation data
# Check the structure of the data
str(dataprecip)

# Perform linear regression
modelprecip <- lm(Precipitation ~ GTA, data = dataprecip)

# Summarize the linear regression model
summary(modelprecip)


#Linear Regression for India on Temperature data
# Check the structure of the data
str(datatemp)

# Perform linear regression
modeltemp <- lm(Temp ~ GTA, data = datatemp)

# Summarize the linear regression model
summary(modeltemp)



# Create a scatter plot of two variables, colored by cluster
ggplot(clustered_data, aes(x = Nino3SSTAnomalies, y = Nino4SSTAnomalies, color = factor(Cluster),shape = factor(ENSO))) +
  geom_point(size=3) +
  labs(x = "Nino3 SST Anomalies", y = "Nino4 SST Anomalies", color = "Cluster") +
  theme_minimal()


# Create a scatter plot of three variables, colored by cluster and ENSO
ggplot(clustered_data, aes(x = GTA, y = ONI, color = factor(Cluster), shape = factor(ENSO))) +
  geom_point(size = 3) +  # Adjust the size of the points as needed
  labs(x = "GTA", y = "ONI", color = "Cluster", shape = "ENSO") +
  scale_color_manual(values = c("red", "blue", "green", "purple", "orange", "yellow", "cyan", "magenta")) +  # Define colors for different levels of ENSO
  scale_shape_manual(values = c(1, 2, 3, 4, 5, 6, 7, 8 )) +  # Define shapes for different levels of ENSO
  theme_minimal()


# Plot time series plots for different countries
# Plot time series for Malawi by Item
ggplot(Mal_data, aes(x = Year, y = Value, color = El_Nino_Year, group = Item)) +
  geom_line() +
  labs(title = "Time Series Plot for Malawi by Item",
       x = "Year", y = "Value",
       color = "El Nino Year") +
  facet_wrap(~Item, scales = "free_y") +
  theme_minimal()

# Plot time series for Australia by Item
ggplot(aus_data, aes(x = Year, y = Value, color = El_Nino_Year, group = Item)) +
  geom_line() +
  labs(title = "Time Series Plot for Australia by Item",
       x = "Year", y = "Value",
       color = "El Nino Year") +
  facet_wrap(~Item, scales = "free_y") +
  theme_minimal()

# Plot time series for India by Item
ggplot(ind_data, aes(x = Year, y = Value, color = El_Nino_Year, group = Item)) +
  geom_line() +
  labs(title = "Time Series Plot for India by Item",
       x = "Year", y = "Value",
       color = "El Nino Year") +
  facet_wrap(~Item, scales = "free_y") +
  theme_minimal()

# Plot time series for Indonesia by Item
ggplot(indo_data, aes(x = Year, y = Value, color = El_Nino_Year, group = Item)) +
  geom_line() +
  labs(title = "Time Series Plot for Indonesia by Item",
       x = "Year", y = "Value",
       color = "El Nino Year") +
  facet_wrap(~Item, scales = "free_y") +
  theme_minimal()

# Plot time series for Brazil by Item
ggplot(braz_data, aes(x = Year, y = Value, color = El_Nino_Year, group = Item)) +
  geom_line() +
  labs(title = "Time Series Plot for Brazil by Item",
       x = "Year", y = "Value",
       color = "El Nino Year") +
  facet_wrap(~Item, scales = "free_y") +
  theme_minimal()

# Plot time series for Ethiopia by Item
ggplot(ethi_data, aes(x = Year, y = Value, color = El_Nino_Year, group = Item)) +
  geom_line() +
  labs(title = "Time Series Plot for Ethiopia by Item",
       x = "Year", y = "Value",
       color = "El Nino Year") +
  facet_wrap(~Item, scales = "free_y") +
  theme_minimal()


# Plot undernourishment time series for selected countries by Item

ggplot(selected_data, aes(x = Year, y = Prevelance, group = Entity)) +
  geom_line(aes(color = El_Nino_Year), size = 1.0) +
  geom_text(dataun = selected_last_obs_per_country, aes(label = " "),
            hjust = -0.1, nudge_y = 0.5, size = 3) +
  labs(title = "Undernourishment Plot",
       x = "Year", y = "Prevelance of Undernourishment") +
  scale_color_manual(values = c("FALSE" = "black", "TRUE" = "red"), 
                     labels = c("Normal Year", "El Nino Year")) +
  facet_wrap(~ Entity, scales = "free_y", ncol = 3) +
  theme_minimal()

