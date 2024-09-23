#Group 28
#
#HEW KANG YOU, TP073666
#LEE EE-ERN, TP067238
#CHIN PEI FUNG, TP068172
#JACKSON HOW, TP073686
#TEE HAO BIN, TP074435

################################################################################
#Import Data
install.packages("rstudioapi")
library(rstudioapi)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) #Set Working Directory
data <- read.csv("5. credit score classification data.csv", header = TRUE)
data

#Exclude Duplicated Rows
data <- unique(data)

#Create subset
new_data <- data[,c("Changed_Credit_Limit", "Credit_History_Age",
                    "Credit_Utilization_Ratio", "Num_of_Delayed_Payment",
                    "Delay_from_due_date", "Credit_Score")]

#View in Table Form
View(new_data)
cat("Number of Rows: ", nrow(new_data), "\nNumber of Columns: ", ncol(new_data));


################################################################################
#Data Cleaning
#Changed_Credit_Limit
#Check for NA
No_of_Null_CCL <- colSums(is.na(new_data[1]))
No_of_Null_CCL

#Check for Empty Value
No_of_Empty_CCL <- sum(new_data$Changed_Credit_Limit == "")
No_of_Empty_CCL

#Remove underscores
new_data$Changed_Credit_Limit <- gsub("_", "",new_data$Changed_Credit_Limit)

#Replace empty value to it's before value
for (i in 1:nrow(new_data)) { 
  if (new_data$Changed_Credit_Limit[i] == "") {
    new_data$Changed_Credit_Limit[i] <- new_data$Changed_Credit_Limit[i-1]
  }
}

#Convert data type
new_data$Changed_Credit_Limit <- as.numeric(new_data$Changed_Credit_Limit)
str(new_data)

#Make all value positive
new_data$Changed_Credit_Limit <- abs(new_data$Changed_Credit_Limit)

#Credit_History_Age
#Check for NA
No_of_Null_CHA <- colSums(is.na(new_data[2]))
No_of_Null_CHA

#Check for Empty
No_of_Empty_CHA <- sum(new_data$Credit_History_Age == "")
No_of_Empty_CHA

# Function to convert "Years and Months" format to total months
convert_to_months <- function(age_str) {
  parts <- strsplit(age_str, " Years and | Months")[[1]]
  years <- as.numeric(parts[1])
  months <- as.numeric(parts[2])
  total_months <- years * 12 + months
  return(total_months)
}

# Convert "Credit_History_Age" to total months
new_data$Credit_History_Age <- sapply(new_data$Credit_History_Age, convert_to_months)

# Iterate through the column to replace NA values
for (i in 2:nrow(new_data)) {
  if (is.na(new_data$Credit_History_Age[i])) {
    new_data$Credit_History_Age[i] <- new_data$Credit_History_Age[i-1] + 1
  }
}

#Check again for NA
No_of_Null_CHA <- colSums(is.na(new_data[2]))
No_of_Null_CHA

#Credit_Utilization_Ratio
#Check for NA
No_of_Null_CUR <- colSums(is.na(new_data[3]))
No_of_Null_CUR

No_of_Empty_CUR <- sum(new_data$Credit_Utilization_Ratio == "")
No_of_Empty_CUR

#Num_of_Delayed_Payment
#Check for NA
No_of_Null_NoDP <- colSums(is.na(new_data[4]))
No_of_Null_NoDP

#Check for number of empty column
No_of_Empty_NoDP <- sum(new_data$Num_of_Delayed_Payment == "")
No_of_Empty_NoDP

#Remove "_"
new_data$Num_of_Delayed_Payment <- gsub("_", "", new_data$Num_of_Delayed_Payment)

#Replace empty value to it's before value
for (i in 1:nrow(new_data)) {
  if (new_data$Num_of_Delayed_Payment[i] == "") {
    new_data$Num_of_Delayed_Payment[i] <- new_data$Num_of_Delayed_Payment[i-1]
  }
}

#Check if empty data exist after update
No_of_Empty_NoDP <- sum(new_data$Num_of_Delayed_Payment == "")
No_of_Empty_NoDP

#Convert Datatype
new_data$Num_of_Delayed_Payment <- as.numeric(new_data$Num_of_Delayed_Payment)
str(new_data)

#Replace value > 50 to it's before value
for (i in 1:nrow(new_data)) {
  if (new_data$Num_of_Delayed_Payment[i] > 50) {
    new_data$Num_of_Delayed_Payment[i] <- new_data$Num_of_Delayed_Payment[i-1]
  }
}

#Delay_from_due_date
#Check for NA
No_of_Null_Dfdd <- colSums(is.na(new_data[5]))
No_of_Null_Dfdd

#Check for number of empty column
No_of_Empty_Dfdd <- sum(new_data$Delay_from_due_date == "")
No_of_Empty_Dfdd

# Convert to numeric IF data type is different
new_data$Delay_from_due_date <- as.numeric(new_data$Delay_from_due_date)
str(new_data)


################################################################################
#Pre-processing
#View Table
View(new_data)

#Data Structure
str(new_data)

#Summary
summary(new_data)


################################################################################
#3.0 Analysis
################################################################################
# 3.1 HEW KANG YOU(TP073666)
# D (Density Ridges)
install.packages("ggridges")
library(ggridges)
library(dplyr)
library(ggplot2)

ggplot(new_data, aes(x = Num_of_Delayed_Payment,
                     y = Credit_Score, fill = Credit_Score)) +
  geom_density_ridges() +
  labs(title = "Density Plot of Num_of_Delayed_Payment by Credit Score",
       x = "Number of Delayed Payments",
       y = "Credit Score",
       fill = "Credit Score")

# D + E (Violin)
filtered_data_D_E <- new_data %>%
  filter(Num_of_Delayed_Payment <= 10)

ggplot(filtered_data_D_E, aes(x = Credit_Score,
                              y = Num_of_Delayed_Payment +
                                Delay_from_due_date,
                              fill = Credit_Score)) +
  geom_violin() +
  labs(title = "Violin Plot of D + Delay_from_due_date by Credit Score",
       x = "Credit Score",
       y = "D + Delay from Due Date",
       fill = "Credit Score")


# D + E + A (Heatmap)
filtered_data_D_E_A <- new_data %>%
  filter(Num_of_Delayed_Payment <= 10 & Delay_from_due_date <= 15)

ggplot(filtered_data_D_E_A, aes(x = Credit_Score, y =
                                  Num_of_Delayed_Payment + Delay_from_due_date
                                + Changed_Credit_Limit)) +
  geom_bin2d(bins = 30) +
  scale_fill_gradient(low = "white", high = "blue") +
  theme_minimal() +
  labs(title = "Heatmap of D + E + Changed_Credit_Limit by Credit Score",
       x = "Credit Score",
       y = "Sum of D, E, and Changed_Credit_Limit",
       fill = "Count")

# D + E + A + B (Scatter)
filtered_data_D_E_A_B <- new_data %>%
  filter(Num_of_Delayed_Payment <= 10 & Delay_from_due_date <= 15
         & Changed_Credit_Limit <= 22)

ggplot(filtered_data_D_E_A_B, aes(x = Num_of_Delayed_Payment
                                  + Delay_from_due_date + Changed_Credit_Limit,
                                  y = Credit_History_Age, color = Credit_Score,
                                  size = Credit_History_Age)) +
  geom_point(alpha = 0.6) +
  theme_minimal() +
  labs(title = "Scatter Plot of D + E + A vs Credit_History_Age by Credit Score",
       x = "D + E + A",
       y = "Credit History Age",
       color = "Credit Score",
       size = "Credit History Age") +
  scale_color_manual(values = c("Good" = "green",
                                "Standard" = "yellow", "Poor" = "red")) +
  theme(legend.title = element_blank())

# D + E + A + B + C (3D Scatter)
install.packages("plotly")
library(plotly)

filtered_data_D_E_A_B_C <- new_data %>%
  filter(Num_of_Delayed_Payment <= 10 &
           Delay_from_due_date <= 15 &
           Changed_Credit_Limit <= 22 &
           Credit_History_Age > 180)

plot_ly(filtered_data_D_E_A_B_C, 
        x = ~Num_of_Delayed_Payment + Delay_from_due_date + Changed_Credit_Limit, 
        y = ~Credit_Utilization_Ratio, 
        z = ~Credit_History_Age, 
        color = ~Credit_Score, 
        colors = c("Good" = "green", "Standard" = "yellow", "Poor" = "red"),
        size = ~Credit_Utilization_Ratio,
        type = 'scatter3d',
        mode = 'markers') %>%
  layout(title = "3D Scatter Plot of D + E + A + B + C by Credit Score",
         scene = list(
           xaxis = list(title = 'D + E + A'),
           yaxis = list(title = 'Credit Utilization Ratio'),
           zaxis = list(title = 'B')
         ))

#F (Bar Graph)
#Import
new_data2 <- data[,c("Credit_Mix", "Credit_Score")]
str(new_data2)
View(new_data2)

#Data Cleaning
#Check for NA
No_of_Null <- colSums(is.na(new_data2[1]))
No_of_Null

#Check for number of empty column
No_of_Empty <- sum(new_data2$Credit_Mix == "")
No_of_Empty

No_of_Underscore <- sum(new_data2$Credit_Mix == "_")
No_of_Underscore

#Remove "_"
new_data2$Credit_Mix <- gsub("_", "", new_data2$Credit_Mix)

#Replace empty value to it's before value
new_data2$Credit_Mix[1] <- new_data2$Credit_Mix[2]
for (i in 2:nrow(new_data2)) {
  if (new_data2$Credit_Mix[i] == "") {
    new_data2$Credit_Mix[i] <- new_data2$Credit_Mix[i-1]
  }
}

credit_mix_data <- new_data2 %>%
  group_by(Credit_Mix, Credit_Score) %>%
  summarise(count = n(), .groups = 'drop')

# Ensure Credit_Mix is a factor with the correct order
credit_mix_data$Credit_Mix <- factor(credit_mix_data$Credit_Mix,
                                     levels = c("Bad", "Standard", "Good"))

ggplot(credit_mix_data, aes(x = Credit_Mix,
                            y = count, fill = Credit_Score)) +
  geom_bar(stat = "identity") +
  labs(title = "Credit Mix vs Credit Score",
       x = "Credit Mix",
       y = "Count",
       fill = "Credit Score") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# D + E + A + B + C + F (SCatter)
# Merge the datasets
merge_data <- merge(new_data, new_data2, by = "row.names",
                    suffixes = c(".new_data", ".new_data2"))

# Remove the duplicated row.names column
merge_data <- merge_data[,-1]

# Remove duplicated Credit_Score columns
merge_data <- merge_data[, -which(names(merge_data) == "Credit_Score.new_data")]

# Rename the remaining Credit_Score column
names(merge_data)[which(names(merge_data) == "Credit_Score.new_data2")] <- "Credit_Score"

filtered_data_D_E_A_B_C_F <- merge_data %>%
  filter(Num_of_Delayed_Payment <= 10 & Delay_from_due_date <= 15
         & Changed_Credit_Limit <= 22 & Credit_History_Age > 180)

ggplot(filtered_data_D_E_A_B_C_F, aes(x = Num_of_Delayed_Payment +
                                        Delay_from_due_date + Changed_Credit_Limit +
                                        Credit_History_Age + Credit_Utilization_Ratio, 
                                      y = Credit_Mix, 
                                      color = Credit_Score, 
                                      size = Credit_Mix)) +
  geom_point(alpha = 0.6) +
  theme_minimal() +
  labs(title = "Scatter Plot of Factors D + E + A + B + C + F by Credit Score",
       x = "Sum of D + E + A + B + C",
       y = "Credit Mix",
       color = "Credit Score",
       size = "Credit Mix") + 
  scale_color_manual(values = c("Good" = "green", "Standard" = "yellow", "Poor" = "red"))
theme(legend.title = element_blank())

################################################################################
#3.2 LEE EE_ERN (TP067238)
#Import Library
install.packages("tidyr")
install.packages("stringr")
install.packages("scales")
install.packages("ggthemes")
install.packages("forcats")
install.packages("GGally")
library(tidyr)
library(stringr)
library(scales)
library(ggthemes)
library(plotly)
library(forcats)
library(GGally)
library(ggridges)
library(dplyr)
library(ggplot2)

# Annual Income
# Create Subset
Annual_InC <- data[,c("Annual_Income", "Credit_Score")]

#Data Cleaning
Annual_InC$Annual_Income <- gsub("_", "",Annual_InC$Annual_Income)
Annual_InC$Annual_Income <- as.numeric(Annual_InC$Annual_Income)
summary(Annual_InC$Annual_Income)

# Visualization
# Reshape the data to long format
long_data <- new_data %>%
  pivot_longer(cols = c(Changed_Credit_Limit, Credit_History_Age, 
                        Credit_Utilization_Ratio, Num_of_Delayed_Payment, 
                        Delay_from_due_date),
               names_to = "Variable",
               values_to = "Value")

# Create the combined plot
ggplot(long_data, aes(x = Value, y = seq_along(Value), color = Credit_Score)) +
  geom_point() +
  facet_wrap(~ Variable, scales = "free_x", ncol = 1) +
  labs(title = "Various Credit Variables vs. Index",
       x = "Value",
       y = "Index") +
  theme_minimal()

# A
# Calculate summary statistics
summary_stats <- new_data %>%
  group_by(Credit_Score) %>%
  summarise(
    mean = mean(Changed_Credit_Limit, na.rm = TRUE),
    Q1 = quantile(Changed_Credit_Limit, 0.25, na.rm = TRUE),
    Q3 = quantile(Changed_Credit_Limit, 0.75, na.rm = TRUE)
  )

# Filter summary_stats for Credit_Score == "Good"
summary_good_stats <- summary_stats %>% filter(Credit_Score == "Good")

# Density plot with vertical lines for mean, Q1, and Q3 for Credit_Score == "Good"
ggplot(new_data, aes(x = Changed_Credit_Limit, fill = Credit_Score)) +
  geom_density(alpha = 0.2) +
  geom_vline(data = summary_good_stats, aes(xintercept = mean), linetype = "dotdash", size = 1) +
  geom_vline(data = summary_good_stats, aes(xintercept = Q1), linetype = "dotdash", size = 1) +
  geom_vline(data = summary_good_stats, aes(xintercept = Q3), linetype = "dotdash", size = 1) +
  labs(title = "Density Plot of Changed Credit Limit by Credit Score",
       x = "Changed Credit Limit",
       y = "Density") +
  theme_minimal() +
  annotate("text", x = summary_good_stats$mean, y = 0.02, label = paste("Mean:", round(summary_good_stats$mean, 2)), angle = 90, vjust = -0.5, color = "blue") +
  annotate("text", x = summary_good_stats$Q1, y = 0.02, label = paste("Q1:", round(summary_good_stats$Q1, 2)), angle = 90, vjust = -0.5, color = "red") +
  annotate("text", x = summary_good_stats$Q3, y = 0.02, label = paste("Q3:", round(summary_good_stats$Q3, 2)), angle = 90, vjust = -0.5, color = "darkgreen")

# A + B
options(repr.plot.width=8, repr.plot.height=6)

# Creating the plot
ggplot(new_data, aes(x = Credit_Score, y = Changed_Credit_Limit, color = Credit_History_Age)) +
  geom_jitter(width = 0.3, alpha = 0.7) +
  theme_minimal() +
  labs(title = "Effect of Changed Credit Limit and Credit History Age on Credit Score",
       x = "Credit Score",
       y = "Changed Credit Limit",
       color = "Credit History Age") +
  scale_color_gradient(low = "#1f77b4", high = "#d62728", name = "Credit History Age") +
  theme(axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10))

# A + B + C
ggplot(new_data, aes(x = Credit_Utilization_Ratio, y = Credit_History_Age, 
                     color = Credit_Score, size = Changed_Credit_Limit)) +
  geom_point(alpha = 0.3) +
  theme_minimal() +
  labs(title = "Effect of Credit Utilization Ratio, Credit_History_Age, 
       and Change_Credit_Limit on Credit Score", 
       x = "Credit Utilization Ratio", 
       y = "Credit History Age", 
       size = "Changed_Credit_Limit") +
  scale_color_manual(values = c("Good" = "green", "Standard" = "yellow", "Poor" = "red")) +
  theme(legend.title = element_blank())

# C
ggplot(new_data, aes(x = Credit_Score, y = Credit_Utilization_Ratio, fill = factor(Credit_Score))) +
  geom_violin(trim = FALSE, alpha = 0.6) +  # Adding violin plot with transparency
  geom_boxplot(width = 0.1, outlier.shape = NA, color = "black", fill = "white") +  # Adding box plot with black outline and white fill
  scale_fill_manual(values = c("#FF5733", "#FFC300", "#DAF7A6", "#C70039")) +  # Customizing fill colors
  labs(title = "Credit Utilization Ratio vs. Credit Score",
       x = "Credit Score",
       y = "Credit Utilization Ratio") +
  theme_minimal() +
  theme(legend.position = "none")  # Hiding legend for fill colors'

# A + B + C + D
x1 <- filter(new_data, Credit_History_Age > 180)%>%
  filter(Changed_Credit_Limit < 18)

ggplot(x1, aes(x = Num_of_Delayed_Payment, fill = Credit_Score)) +
  geom_histogram(binwidth = 1, position = "dodge", alpha = 0.7, color = "white") +
  scale_fill_manual(values = c("Good" = "pink", "Standard" = "lightblue",
                               "Poor" = "black")) +
  labs(title = "Distribution of Number of Delayed Payments by Credit Score",
       x = "Number of Delayed Payments",
       y = "Count",
       fill = "Credit Score") +
  theme_minimal() +
  theme(legend.position = "top",
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 8),
        axis.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 8),
        axis.title = element_text(size = 10),
        plot.title = element_text(size = 12, hjust = 0.5),
        panel.grid.major = element_line(color = "lightgray", size = 0.1),
        panel.grid.minor = element_blank())

# A + B + C + D + E
x2 <- filter(new_data, Credit_History_Age > 180) %>%
  filter(Changed_Credit_Limit < 18 & Num_of_Delayed_Payment < 13)

# Create the heatmap plot
ggplot(x2, aes(x = Credit_Score, y = Delay_from_due_date)) +
  stat_bin2d(geom = "tile", aes(fill = ..count..), bins = 30) +
  scale_fill_gradient(low = "#F0F0F0", high = "#3F48CC", na.value = NA, 
                      name = "Frequency") +  # Adjust color scale
  labs(title = "Heatmap of Delay from Due Date by Credit Score",
       x = "Credit Score",
       y = "Delay from Due Date") +
  theme_minimal() +  # Use minimal theme
  theme(axis.text = element_text(size = 10),  # Adjust axis text size
        axis.title = element_text(size = 12),  # Adjust axis title size
        plot.title = element_text(size = 14, hjust = 0.5),  # Adjust plot title size and center it
        legend.title = element_text(size = 10),  # Adjust legend title size
        legend.text = element_text(size = 8))  # Adjust legend text size

# F
Annual_InC$Log_Annual_Income <- log10(Annual_InC$Annual_Income)

ggplot(Annual_InC, aes(x = Log_Annual_Income, y = Credit_Score, fill = Credit_Score)) +
  geom_density_ridges(alpha = 0.8, scale = 1) +
  scale_fill_brewer(palette = "Spectral") + # Use a distinctive color palette
  labs(
    title = "Ridgeline Plot of Log-Transformed Annual Income by Credit Score",
    x = "Log(Annual Income)",
    y = "Credit Score",
  ) +
  theme_minimal(base_size = 14) + # Increase base size for better readability
  theme(
    legend.position = "none", # Remove legend if Credit_Score is on y-axis
    panel.grid.major = element_blank(), # Remove major grid lines
    panel.grid.minor = element_blank(), # Remove minor grid lines
    plot.title = element_text(hjust = 0.5, face = "bold"), # Center and bold title
    plot.subtitle = element_text(hjust = 0.5, margin = margin(b = 10)), # Center subtitle
    axis.text = element_text(size = 12) # Increase axis text size
  )

################################################################################
#3.3 CHIN PEI FUNG (TP068172)
install.packages("hexbin")
library(dplyr)
library(ggplot2)
library(plotly)
library(ggplot2)
library(hexbin)
library(ggridges)

#B
#Credit_History_Age comparisons
CHA <- new_data %>%
  select(Credit_History_Age, Credit_Score) %>%
  group_by(Credit_Score) %>%
  summarise(
    `>good_CHA` = sum(Credit_History_Age > 190),
    `<poor_CHA` = sum(Credit_History_Age < 190)
  ) %>%
  ungroup()

print(CHA)

#Visualize the results by density plot
ggplot(new_data, aes(x = Credit_History_Age, fill = Credit_Score)) +
  geom_density(alpha = 0.5) +
  theme_minimal() +
  labs(title = "Density Plot of Credit History Age by Credit Score",
       x = "Credit History Age", 
       y = "Density") +
  scale_fill_manual(values = c("Good" = "#171717", "Standard" = "#FFFAFA", "Poor" = "#FFC1C1")) +
  theme(legend.title = element_blank())

#B + C
# Credit Utilization Ratio comparisons
CUR <- new_data %>%
  filter(Credit_History_Age > 190) %>%
  select(Credit_Utilization_Ratio, Credit_Score) %>%
  group_by(Credit_Score) %>%
  summarise(
    `>good_CUR` = sum(Credit_Utilization_Ratio > 30),
    `<poor_CUR` = sum(Credit_Utilization_Ratio <= 30)
  ) %>%
  ungroup()

print(CUR)

ggplot(new_data, aes(x = Credit_History_Age, y = Credit_Utilization_Ratio,color = Credit_Score)) +
  geom_point(size = 1, alpha = 0.5) +
  labs(
    title = 'Credit History Age vs Credit Utilization Ratio by Credit Score',
    x = 'Credit History Age (years)',
    y = 'Credit Utilization Ratio'
  ) +
  scale_color_manual(values = c('Good' = '#9FB6CD', 'Standard' = '#EEE0E5', 'Poor' = '#EEAEEE')) +
  facet_wrap(~ Credit_Score) +  # Facet by CreditScore
  theme_minimal()+
  scale_x_continuous(breaks =seq(0,400,50))

#B+C+D
# Number of Delayed Payment comparisons
NoDP <- new_data %>%
  filter(Credit_History_Age > 190) %>%
  filter(Credit_Utilization_Ratio > 30) %>%
  select(Num_of_Delayed_Payment, Credit_Score) %>%
  group_by(Credit_Score) %>%
  summarise(
    `>good_NoDP` = sum(Num_of_Delayed_Payment > 10),
    `<poor_NoDP` = sum(Num_of_Delayed_Payment <= 10)
  ) %>%
  ungroup()

print(NoDP)

new_data1<- filter(new_data,Credit_History_Age>180)%>%
  filter(Credit_Utilization_Ratio<43)

filtered_data <- new_data1 %>%
  filter(Num_of_Delayed_Payment < 30)

ggplot(filtered_data, aes(x = Credit_History_Age, y = Num_of_Delayed_Payment, 
                          color = Credit_Score)) +
  geom_point(alpha = 0.8) +
  theme_minimal() +
  labs(title ='Credit History Age and Credit Utilization Ratio by Credit Score',
       x = "Credit History Age", 
       y = "Num of Delayed Payment") +
  scale_color_manual(values = c("Good" = "#00008B", "Standard" = "#ADD8E6", 
                                "Poor" = "#FF83FA")) +
  theme(legend.title = element_blank())+
  scale_y_continuous(breaks =seq(-5, 30, 5))

# Hexbin plot
ggplot(filtered_data, aes(x = Num_of_Delayed_Payment, y = Credit_Score)) +
  geom_hex(bins = 25) +
  scale_fill_gradient(low = "#FFB6C1", high = "purple") +
  labs(
    title = 'Number of Delayed Payments vs Credit Score',
    x = 'Number of Delayed Payments',
    y = 'Credit Score'
  ) +
  theme_minimal()

#B+C+D+E
# Delay from due date comparisons
DfDd <- new_data %>%
  filter(Credit_History_Age > 190) %>%
  filter(Credit_Utilization_Ratio > 30) %>%
  filter(Num_of_Delayed_Payment <= 10) %>%
  select(Delay_from_due_date, Credit_Score) %>%
  group_by(Credit_Score) %>%
  summarise(
    `>good_DfDd` = sum(Delay_from_due_date > 20),
    `<poor_DfDd` = sum(Delay_from_due_date <= 20)
  ) %>%
  ungroup()

print(DfDd)

new_data2<- filter(new_data,Credit_History_Age>180)%>%
  filter(Credit_Utilization_Ratio<43)%>% filter(Num_of_Delayed_Payment<13)

# Violin plot
ggplot(new_data2, aes(x = Credit_Score, y = Delay_from_due_date, fill = Credit_Score)) +
  geom_violin(trim = FALSE) +
  labs(
    title = "Effect of Delay from Due Date on Credit Score",
    x = "Credit Score",
    y = "Delay from Due Date"
  ) +
  scale_fill_manual(values = c("Good" = "#CD96CD", "Standard" = "#9FB6CD", "Poor" = "#EEA2AD")) +
  theme_minimal()+scale_y_continuous(breaks =seq(-5, 50, 5))

#B+C+D+E+A
# Changed Credit Limit comparisons
CCL <- new_data %>%
  filter(Credit_History_Age > 190) %>%
  filter(Credit_Utilization_Ratio > 30) %>%
  filter(Num_of_Delayed_Payment <= 10) %>%
  filter(Delay_from_due_date <= 20) %>%
  select(Changed_Credit_Limit, Credit_Score) %>%
  group_by(Credit_Score) %>%
  summarise(
    `>good_CCL` = sum(Changed_Credit_Limit > 12),
    `<poor_CCL` = sum(Changed_Credit_Limit <= 12)
  ) %>%
  ungroup()
print(CCL)

new_data3<- filter(new_data,Credit_History_Age>180)%>%
  filter(Credit_Utilization_Ratio<43)%>% filter(Num_of_Delayed_Payment<13)%>%
  filter(Delay_from_due_date<17.5)

# Convert Credit Score to a factor with ordered levels
new_data3$Credit_Score <- factor(new_data3$Credit_Score, levels = c("Poor", "Standard", "Good"), ordered = TRUE)

ggplot(new_data3, aes(x = Credit_History_Age, y = Changed_Credit_Limit, fill = Credit_Score)) +
  geom_bin2d(aes(alpha = ..count..), bins = 20) +
  scale_fill_manual(values = c("#8B1C62", "#4A708B", "#6B8E23")) +
  labs(
    title = "Credit History Age and Changed Credit Limit by Credit Score",
    x = "Credit History Age",
    y = "Changed Credit Limit",
    fill = "Credit Score"
  ) +
  theme_minimal() +
  theme(legend.position = "right")+facet_wrap(~ Credit_Score)

#F
# Create subset
new_extra_data <- data[, c("Outstanding_Debt", "Credit_Score", "Credit_History_Age")]
View(new_extra_data)
cat("Number of Rows: ", nrow(new_data), "\nNumber of Columns: ", ncol(new_data))

# Check for NA
No_of_Null <- sum(is.na(new_extra_data$Outstanding_Debt))
cat("Number of NA: ", No_of_Null, "\n")

# Check for number of empty columns
No_of_Empty <- sum(new_extra_data$Outstanding_Debt == "")
cat("Number of Empty: ", No_of_Empty, "\n")

# Remove "_" (this should be before numeric conversion)
new_extra_data$Outstanding_Debt <- gsub("_", "", new_extra_data$Outstanding_Debt)

# Convert to numeric (IF data type is different)
new_extra_data$Outstanding_Debt <- as.numeric(new_extra_data$Outstanding_Debt)

# Replace empty values to the previous value
for (i in 1:nrow(new_extra_data)) { 
  if (is.na(new_extra_data$Outstanding_Debt[i])) {
    if (i == 1) {
      new_extra_data$Outstanding_Debt[i] <- 0  # Handle the first row if it is NA
    } else {
      new_extra_data$Outstanding_Debt[i] <- new_extra_data$Outstanding_Debt[i - 1]
    }
  }
}

# Check if empty data exist after update
No_of_Empty <- sum(is.na(new_extra_data$Outstanding_Debt))
cat("Number of Empty after update: ", No_of_Empty, "\n")

# Convert Datatype
str(new_extra_data)
new_extra_data$Outstanding_Debt <- as.numeric(new_extra_data$Outstanding_Debt)
str(new_extra_data)

# Convert "Credit_History_Age" to total months
new_extra_data$Credit_History_Age <- sapply(new_extra_data$Credit_History_Age, convert_to_months)

# Iterate through the column to replace NA values
for (i in 2:nrow(new_extra_data)) {
  if (is.na(new_extra_data$Credit_History_Age[i])) {
    new_extra_data$Credit_History_Age[i] <- new_extra_data$Credit_History_Age[i-1] + 1
  }
}

#Open the data to view
View(new_extra_data)

# Outstanding_Debt comparisons with Credit_Score
Outstanding_Debt <- new_extra_data %>%
  select(Outstanding_Debt, Credit_Score) %>%
  group_by(Credit_Score) %>%
  summarise(
    `>good_OD` = sum(Outstanding_Debt > 2500),
    `<poor_OD` = sum(Outstanding_Debt < 2500)
  ) %>%
  ungroup()

print(Outstanding_Debt)

# Create a new dataframe with integer Credit Score for better heatmap representation
new_extra_data$Credit_Score_Int <- as.integer(factor(new_extra_data$Credit_Score, levels = c("Good", "Standard", "Poor")))

# Create the heatmap
ggplot(new_extra_data, aes(x = Outstanding_Debt, y = Credit_Score_Int)) +
  geom_bin2d(bins = 10) +  # Adjust bins as necessary
  scale_fill_gradient(low = "pink", high = "black") +
  scale_y_continuous(breaks = 1:3, labels = c("Good", "Standard", "Poor")) +
  labs(title = "Outstanding Debt by Credit Score",
       x = "Outstanding Debt",
       y = "Credit Score",
       fill = "Count") +
  theme_minimal()

# Merge the datasets
new_extra_data <- merge(new_extra_data, new_data, by = "row.names",
                    suffixes = c(".new_data", ".new_data2"))

# Remove the duplicated row.names column
new_extra_data <- new_extra_data[,-1]

# Remove duplicated Credit_Score columns
new_extra_data <- new_extra_data[, -which(names(new_extra_data) == "Credit_Score.new_data")]

# Rename the remaining Credit_Score column
names(new_extra_data)[which(names(new_extra_data) == "Credit_Score.new_data2")] <- "Credit_Score"

# Remove duplicated Credit_History_Age columns
new_extra_data <- new_extra_data[, -which(names(new_extra_data) == "Credit_History_Age.new_data")]

# Rename the remaining Credit_History_Age column
names(new_extra_data)[which(names(new_extra_data) == "Credit_History_Age.new_data2")] <- "Credit_History_Age"

#B+C+D+E+A+F
Outstanding_Debt <- new_extra_data %>%
  filter(Credit_History_Age > 190) %>%
  filter(Credit_Utilization_Ratio > 30) %>%
  filter(Num_of_Delayed_Payment <= 10) %>%
  filter(Delay_from_due_date <= 20) %>%
  filter(Changed_Credit_Limit > 12) %>%
  select(Outstanding_Debt, Credit_Score) %>%
  group_by(Credit_Score) %>%
  summarize(
    `>good_OD` = sum(Outstanding_Debt > 2500),
    `<poor_OD` = sum(Outstanding_Debt < 2500)
  ) %>%
  ungroup()

print(Outstanding_Debt)

ggplot(new_extra_data, aes(x = Credit_History_Age, y = Outstanding_Debt, color = Credit_Score)) +
  geom_point(size = 1, alpha = 1) +
  labs(
    title = 'Outstanding Debt by Credit History Age',
    x = 'Credit History Age (years)',
    y = 'Outstanding Debt',
  ) +
  scale_color_manual(values = c('Good' = 'green', 'Standard' = 'yellow', 'Poor' = 'red')) +
  theme_minimal()+
  scale_x_continuous(breaks =seq(0,400,50))

################################################################################
#3.4 JACKSON HOW (TP073686)
#C
credit_utilization_ranges <- new_data %>%
  mutate(CreditUtilizationRange = cut(Credit_Utilization_Ratio, 
                                      breaks = seq(0, 50, by = 10))) %>%
  count(CreditUtilizationRange, Credit_Score) %>%
  group_by(CreditUtilizationRange) %>%
  mutate(Proportion = n / sum(n)) %>%
  ungroup()

# Improved ggplot code
ggplot(credit_utilization_ranges, aes(x = CreditUtilizationRange, 
                                      y = Proportion, fill = Credit_Score)) +
  geom_bar(stat = "identity", position = "fill") +
  theme_minimal() +
  labs(
    title = "Distribution of Credit Score Category by 
    Credit Utilization Ratio Range", 
    x = "Credit Utilization Ratio Range", 
    y = "Proportion"
  ) +
  scale_fill_manual(values = c("Good" = "green", "Standard" = "yellow",
                               "Poor" = "red")) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),  
    axis.title.x = element_text(size = 12, face = "bold"),  
    axis.title.y = element_text(size = 12, face = "bold"),  
  )

ggplot(new_data, aes(x = Credit_Score, y = Credit_Utilization_Ratio, 
                     color = Credit_Score)) +
  geom_boxplot() +
  theme_minimal() +
  labs(
    title = "Credit Score vs Credit Utilization Ratio",
    x = "Credit Score",
    y = "Credit Utilization Ratio"
  ) +
  scale_y_continuous(breaks = seq(0, 50, by = 5)) +
  scale_color_manual(values = c("Good" = "green", "Standard" = "blue",
                                "Poor" = "red")) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"), 
    axis.title.x = element_text(size = 12, face = "bold"),  # Style x-axis title
    axis.title.y = element_text(size = 12, face = "bold"),  # Style y-axis title
    legend.position = "none"  # Remove the legend
  )

#C+D
ggplot(new_data, aes(x = Credit_Utilization_Ratio, y = Num_of_Delayed_Payment,
                     color = Credit_Score)) +
  geom_point(alpha = 0.6) +
  theme_minimal() +
  labs(title = "Effect of Credit Utilization Ratio and Number of 
       Delayed Payments on Credit Score", 
       x = "Credit Utilization Ratio", 
       y = "Number of Delayed Payments") +
  scale_color_manual(values = c("Good" = "green", "Standard" = "yellow", 
                                "Poor" = "red")) +
  theme(legend.title = element_blank())

RemoveNODPabove30 <- new_data %>% filter(Num_of_Delayed_Payment <= 30)
ggplot(RemoveNODPabove30, aes(x = Credit_Utilization_Ratio,
                              y = Num_of_Delayed_Payment, 
                              color = Credit_Score)) +
  geom_point(alpha = 0.6) +
  theme_minimal() +
  labs(title = "Effect of Credit Utilization Ratio and 
       Number of Delayed Payments on Credit Score", 
       x = "Credit Utilization Ratio", 
       y = "Number of Delayed Payments") +
  scale_color_manual(values = c("Good" = "green", "Standard" = "yellow",
                                "Poor" = "red")) +
  theme(legend.title = element_blank())

#C+D+E
removeNODP<- new_data %>% filter(Num_of_Delayed_Payment<12)


ggplot(removeNODP, aes(x = Delay_from_due_date, fill = Credit_Score)) +
  geom_density(alpha = 0.5) +
  theme_minimal() +
  labs(
    title = "Density Plot of Delay from Due Date by Credit Score", 
    x = "Delay from Due Date", 
    y = "Density"
  ) +
  scale_fill_manual(
    values = c("Good" = "green", "Standard" = "yellow", "Poor" = "red"),
    name = "Credit Score"
  ) +
  scale_x_continuous(
    breaks = seq(0, 70, by = 5)
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"), 
    axis.title.x = element_text(size = 12, face = "bold"),  # Style x-axis title
    axis.title.y = element_text(size = 12, face = "bold"),  # Style y-axis title
    legend.position = "right",  # Position the legend on the right
    legend.title = element_text(size = 12, face = "bold"),  # Style legend title
    legend.text = element_text(size = 10)  # Style legend text
  )

#C+D+E+A
removeNODPandDFDD <- removeNODP %>% filter(Delay_from_due_date < 17.5) 
ggplot(removeNODPandDFDD, aes(x = Credit_Utilization_Ratio, 
                              y = Changed_Credit_Limit)) +
  geom_bin2d(bins = 30) +  
  scale_fill_gradient(low = "lightblue", high = "darkblue") +  
  theme_minimal() +  # Use a minimal theme
  labs(
    title = "Density Heatmap of Credit Utilization Ratio 
    and Changed Credit Limit", 
    x = "Credit Utilization Ratio", 
    y = "Changed Credit Limit",
    fill = "Count"
  ) +
  scale_y_continuous(breaks = seq(0, 23, by = 1)) +  # Define y-axis breaks
  facet_wrap(~ Credit_Score) +  # Use facet wrap for Credit Score
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),  
    axis.title.x = element_text(size = 12, face = "bold"),  # Style x-axis title
    axis.title.y = element_text(size = 12, face = "bold"),  # Style y-axis title
    legend.title = element_text(size = 12, face = "bold"),  # Style legend title
    legend.text = element_text(size = 10),  # Style legend text
    strip.text = element_text(size = 12, face = "bold")  # Style facet labels
  )

#C+D+E+A+B
removeNODPandDFDDandCCL <- removeNODPandDFDD %>%
  filter(Changed_Credit_Limit < 12.5)

ggplot(removeNODPandDFDDandCCL, aes(x = Credit_Score, y = Credit_History_Age)) +
  geom_hex(bins = 30) +  # Increase number of bins for finer granularity
  scale_fill_gradient(low = "lightyellow", high = "darkblue", name = "Count") + 
  theme_minimal() +
  labs(
    title = "Hexagonal Binning of Credit History Age by Credit Score",
    x = "Credit Score",
    y = "Credit History Age"
  ) +
  scale_y_continuous(breaks = seq(0, 400, by = 10)) +  # Adjust y-axis breaks
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),  
    axis.title.x = element_text(size = 12, face = "bold"),  # Style x-axis title
    axis.title.y = element_text(size = 12, face = "bold"),  # Style y-axis title
    legend.position = "right"  # Position the legend on the right
  )

#F
#Import
jacksondata <- data[,c("Payment_of_Min_Amount", "Credit_Score")]
str(jacksondata)
View(jacksondata)

#Data Cleaning
#Check for NA
No_of_Null <- colSums(is.na(jacksondata[1]))
No_of_Null

#Check for number of empty column
No_of_Empty <- sum(jacksondata$Payment_of_Min_Amount == "")
No_of_Empty

ggplot(jacksondata, aes(x = Credit_Score, fill = Payment_of_Min_Amount)) +
  geom_bar(position = "dodge", color = "black") +
  labs(title = "Payment of Minimum Amount by Credit Score", x = "Credit Score",
       y = "Count", fill = "Payment of Min Amount") +
  theme_minimal()+
  theme(legend.title = element_blank())+
  scale_y_continuous(breaks = seq(0, 30000, by = 5000))

#C+D+E+A+B+F
# Merge the datasets
merge_jacksondata <- merge(new_data, jacksondata, by = "row.names",
                    suffixes = c(".new_data", ".new_data2"))

# Remove the duplicated row.names column
merge_jacksondata <- merge_jacksondata[,-1]

# Remove duplicated Credit_Score columns
merge_jacksondata <- merge_jacksondata[, -which(names(merge_jacksondata) == "Credit_Score.new_data")]

# Rename the remaining Credit_Score column
names(merge_jacksondata)[which(names(merge_jacksondata) == "Credit_Score.new_data2")] <- "Credit_Score"

#Call Library
install.packages("ggmosaic")
library(ggmosaic)

removeNODPandDFDDandCCLandCHA <- merge_jacksondata %>%
  filter(Changed_Credit_Limit < 12.5) %>%
  filter(Delay_from_due_date < 17.5)  %>%
  filter(Num_of_Delayed_Payment<12) %>%
  filter(Credit_History_Age >180)

ggplot(data = removeNODPandDFDDandCCLandCHA) +
  geom_mosaic(aes(x = product(Credit_Score), fill = Payment_of_Min_Amount), divider = mosaic("h")) +
  theme_minimal() +  # Use a minimal theme
  labs(
    title = "Mosaic Plot of Payment of Minimum Amount by Credit Score",
    x = "Credit Score",
    y = "Proportion",
    fill = "Payment of Minimum Amount"
  ) +
  scale_fill_manual(name = "Payment of Minimum Amount", values = 
                      c("Yes" = "blue", "No" = "green", "NM" = "red")) +
  theme(legend.title = element_blank())

################################################################################
#3.5 TEE HAO BIN (TP074435)
#E
ggplot(new_data, aes(x = Delay_from_due_date, fill = Credit_Score)) + geom_bar(position = "dodge") + 
  scale_fill_manual(values = c("Good" = "green", "Standard" = "orange", "Poor" = "red")) +
  labs(title = "Distribution of Delay from Due Date (Dfdd) by Credit Score", x = "Delay from Due Date", y = "Count") +
  theme_minimal() + scale_x_continuous(breaks = seq(0, 100, 1))

#E + A
# Filter Dfdd
filtered_Dfdd_data <- new_data %>% filter(Delay_from_due_date < 15)

#### Violin Plot
ggplot(filtered_Dfdd_data, aes(x = Credit_Score, y = Changed_Credit_Limit, fill = Credit_Score)) + geom_violin() + 
  scale_fill_manual(values = c("Good" = "green", "Standard" = "orange", "Poor" = "red")) +
  labs(title = "Distribution of Changed Credit Limit (CCL) by Credit Score filtered by Dfdd", x = "Credit Score", y = "Changed Credit Limit") +
  theme_minimal() + scale_y_continuous(breaks = seq(0, 40, 1))

#E + A + B
# Filter Dfdd and CCL
filtered_Dfdd_CCL_data <- new_data %>% filter(Delay_from_due_date < 15) %>% filter(Changed_Credit_Limit < 11)

### Density Plot
ggplot(filtered_Dfdd_CCL_data, aes(x = Credit_History_Age, fill = Credit_Score)) + geom_density(alpha = 0.6) + 
  scale_fill_manual(values = c("Good" = "green", "Standard" = "orange", "Poor" = "red")) +
  labs(title = "Density Plot of Credit History Age (CHA) by Credit Score filtered by Dfdd and CCL", x = "Credit History Age", y = "Density") +
  theme_minimal() + scale_x_continuous(breaks = seq(0, 400, 20))

#E + A + B + C
# Filter Dfdd, CCL and CHA
filtered_Dfdd_CCL_CHA_data = new_data %>% filter(Delay_from_due_date < 15) %>% filter(Changed_Credit_Limit < 11) %>% filter(Credit_History_Age > 200)

#### Box Plot
ggplot(filtered_Dfdd_CCL_CHA_data, aes(x = Credit_Score, y = Credit_Utilization_Ratio, fill = Credit_Score)) + geom_boxplot() + 
  scale_fill_manual(values = c("Good" = "green", "Standard" = "orange", "Poor" = "red")) +
  labs(title = "Box Plot of Credit Utilization Ratio (CUR) by Credit Score filtered by Dfdd, CCL and CHA", x = "Credit Score", y = "Credit Utilization Ratio") +
  theme_minimal() + scale_y_continuous(breaks = seq(0, 50, 1))

#E + A + B + C + D
# Filter Dfdd, CCL, CHA and CUR (NOT REQUIRED to filter CUR)
filtered_Dfdd_CCL_CHA_CUR_data = new_data %>% filter(Delay_from_due_date < 15) %>% filter(Changed_Credit_Limit < 11) %>% filter(Credit_History_Age > 200)

# Summarize the data for heat map
heatmap_data <- filtered_Dfdd_CCL_CHA_CUR_data %>% group_by(Num_of_Delayed_Payment, Credit_Score) %>% filter(Num_of_Delayed_Payment < 30) %>% summarise(count = n())

# Create the heat map
ggplot(heatmap_data, aes(x = Num_of_Delayed_Payment, y = Credit_Score, fill = count)) + geom_tile() + scale_fill_gradient(low = "yellow", high = "red") +
  labs(title = "Heat Map of Number of Delayed Payment (NoDP) by Credit Score filtered by Dfdd, CCL, CHA and CUR", x = "Number of Delayed Payment", y = "Credit Score", fill = "Count") +
  theme_minimal() + scale_x_continuous(breaks = seq(0, 50, 1))

#E + A + B + C + D + F
#Create Subset
external_data <- data[,c("Age","Credit_Score")]
View(external_data)

#Data Cleaniing
# To check if there is any NULL value in "Age" column
age_na <- sum(is.na(external_data$Age))
age_na

#Check for number of empty column
age_Empty <- sum(external_data$Age == "")
age_Empty

#Remove "_"
external_data$Age <- gsub("_", "", external_data$Age)

# Convert to numeric IF data type is different
external_data$Age <- as.numeric(external_data$Age)

#Replace empty value to it's before value
for (i in 1:nrow(external_data)) { 
  if (external_data$Age[i] > 80 | external_data$Age[i] < 0) {
    external_data$Age[i] <- external_data$Age[i-1]
  }
}
# Check summary statistic
summary(external_data)

### Merge external_data with new_data
# Merge the datasets on their corresponding rows, specifying suffixes
merge_external_data <- merge(new_data, external_data, by = "row.names", suffixes = c(".new_data", ".external_data"))

# Remove the duplicated row.names column
merge_external_data <- merge_external_data[,-1]

# Remove one of the duplicated Credit_Score columns
merge_external_data <- merge_external_data[, -which(names(merge_external_data) == "Credit_Score.new_data")]

# Rename the remaining Credit_Score column to remove the suffix
names(merge_external_data)[which(names(merge_external_data) == "Credit_Score.external_data")] <- "Credit_Score"

# View the final merged dataset
View(merge_external_data)

# Filter Dfdd, CCL, CHA, CUR and NoDP
filtered_Dfdd_CCL_CHA_CUR_NoDP_data = merge_external_data %>% filter(Delay_from_due_date < 15)%>%
  filter(Changed_Credit_Limit < 11) %>%
  filter(Credit_History_Age > 200) %>%
  filter(Num_of_Delayed_Payment < 13)

#Histogram
ggplot(filtered_Dfdd_CCL_CHA_CUR_NoDP_data, aes(x = Age, fill = Credit_Score)) + geom_histogram(binwidth = 1, position = "dodge") + 
  scale_fill_manual(values = c("Good" = "green", "Average" = "orange", "Poor" = "red")) +
  labs(title = "Histogram of Delay from Due Date by Credit Score filtered by Dfdd, CCL, CHA, CUR and NoDP", x = "Age", y = "Frequency") +theme_minimal()

################################################################################
#Create new subset with 
#Merge merge_data & jacksondata as finaldata
finaldata <- merge(merge_data, jacksondata, by = "row.names",
                           suffixes = c(".new_data", ".new_data2"))
finaldata <- finaldata[,-1]
finaldata <- finaldata[, -which(names(finaldata) == "Credit_Score.new_data")]
names(finaldata)[which(names(finaldata) == "Credit_Score.new_data2")] <- "Credit_Score"

#Merge finaldata & new_extra_data
finaldata <- merge(finaldata, new_extra_data, by = "row.names",
                   suffixes = c(".new_data", ".new_data2"))
finaldata <- finaldata[,-1]
finaldata <- finaldata[, -which(names(finaldata) == "Credit_Score.new_data")]
names(finaldata)[which(names(finaldata) == "Credit_Score.new_data2")] <- "Credit_Score"
finaldata <- finaldata[, -which(names(finaldata) == "Credit_History_Age.new_data")]
names(finaldata)[which(names(finaldata) == "Credit_History_Age.new_data2")] <- "Credit_History_Age"

View(finaldata)

################################################################################
#Call library
install.packages("corrplot")
library(corrplot)

credit_score_mapping <- c("Good" = 3, "Standard" = 2, "Poor" = 1)
payment_of_min_amount_mapping <- c("Yes" = 4, "No" = 5, "NM" = 6)
Credit_Mix_mapping <- c("Good" = 7, "Standard" = 8, "Bad" = 9)

# Convert Credit_Score to numeric
CreditScorenumericdata <- finaldata %>%
  mutate(Credit_Score_Numeric = as.numeric(credit_score_mapping[Credit_Score])) %>%
  mutate(Payment_of_Min_Amount_Numeric = as.numeric(payment_of_min_amount_mapping[Payment_of_Min_Amount])) %>%
  mutate(Credit_Mix_Numeric = as.numeric(Credit_Mix_mapping[Credit_Mix]))

# Prepare the data
numeric_data <- CreditScorenumericdata %>%
  select(Outstanding_Debt, Payment_of_Min_Amount_Numeric,
         Credit_Mix_Numeric, Credit_Score_Numeric)

# Compute the correlation matrix
cor_matrix <- cor(numeric_data, use = "complete.obs")

# Create the correlation plot
corrplot(cor_matrix, method = "color", type = "upper", 
         tl.col = "black", tl.srt = 45, 
         addCoef.col = "black", number.cex = 0.7,
         col = colorRampPalette(c("red", "white", "blue"))(200))

#Original Hypothesis data
summarydata <- finaldata %>%
  group_by(Credit_Score) %>%
  filter(Delay_from_due_date<15) %>%
  filter(Changed_Credit_Limit<11)%>%
  filter(Credit_History_Age>200)%>%
  filter(Num_of_Delayed_Payment<13)%>%
  filter(Credit_Score=="Good")
nrow(summarydata)

#Summary data with Outstanding_Debt
summarydata <- finaldata %>%
  group_by(Credit_Score) %>%
  filter(Delay_from_due_date<15) %>%
  filter(Credit_History_Age>200)%>%
  filter(Outstanding_Debt<1600)%>%
  filter(Num_of_Delayed_Payment<13)%>%
  filter(Credit_Score=="Good")                                                     
nrow(summarydata)
################################################################################
