# Texas Department of Insurance. Insurance complaints: All data,
#       2024. URL https://catalog.data.gov/dataset/insurance-complaints-all-data. 
#       Accessed: October 21, 2024.

library(dplyr)
library(lubridate)
library(ggplot2)  
library(broom)  
library(psychometric)  
library(nnet) 
library(corrplot)
library(purrr)

full_dataset <- read.csv("InsuranceComplaints.csv", header = TRUE)
str(full_dataset)
head(full_dataset)

# Information on the full dataset
full_info <- str(full_dataset)

# Select variable columns
columns <- c(
  "Complaint.filed.against",
  "Complaint.number",              
  "Respondent.type",
  "Confirmed.complaint",
  "Received.date",                 
  "Closed.date",                
  "Coverage.type"
)
dataset <- full_dataset[columns]




#########################################**Data Conversions**###################################################

# Convert date columns to Date type
dataset$Received.date <- as.Date(dataset$Received.date, format = "%m/%d/%Y")
dataset$Closed.date <- as.Date(dataset$Closed.date, format = "%m/%d/%Y")

# Sort the data by 'Respondent.type' and 'Received.date'
dataset <- dataset %>%
  arrange(Respondent.type, Received.date)

# Calculate complaint duration in days
dataset <- dataset %>%
  mutate(Complaint.Duration = as.numeric(difftime(Closed.date, Received.date, units = "days")))

# Filter out rows with Complaint.Duration of zero
dataset <- dataset %>%
  filter(Complaint.Duration > 0)

# Remove complaints with 'Complaint.filed.against' that appears only once
dataset <- dataset %>%
  group_by(Complaint.filed.against) %>%
  filter(n() > 1) %>%
  ungroup()

# Summary statistics of the complaint duration
summary_stats <- summary(dataset$Complaint.Duration)
summary_stats

# New column for Duration.Class (categorical)
dataset <- dataset %>%
  mutate(Duration.Class = case_when(
    Complaint.Duration < 30 ~ "Short",
    Complaint.Duration >= 30 & Complaint.Duration <= 70 ~ "Moderate",
    Complaint.Duration > 70 & Complaint.Duration <= 119 ~ "Long",
    Complaint.Duration > 119 ~ "Extended"
  ))

# Calculating the 'Time.Between.Complaints' and 'Number.of.Previous.Complaints'
dataset <- dataset %>%
  group_by(Complaint.filed.against) %>%
  mutate(Time.Between.Complaints = as.numeric(difftime(Received.date, lag(Received.date), units = "days")),
         Number.of.Previous.Complaints = row_number() - 1) %>%
  ungroup()

# Set all null values in Time.Between.Complaints to zero
dataset <- dataset %>%
  mutate(Time.Between.Complaints = ifelse(is.na(Time.Between.Complaints), 0, Time.Between.Complaints))

# Filter out rows with Number.of.Previous.Complaints of zero
dataset <- dataset %>%
  filter(Number.of.Previous.Complaints > 0)

InsComp <- dataset %>%
  dplyr::select(Respondent.type, Confirmed.complaint, Coverage.type, 
                Complaint.Duration, Duration.Class, 
                Time.Between.Complaints, Number.of.Previous.Complaints, Complaint.filed.against)

# Dataset after conversions
dataset_info <- str(InsComp)




#########################################**Category Filtering**#################################################

# Extract unique responses for each categorical column
unique_values <- InsComp %>%
  dplyr::select(Respondent.type, Confirmed.complaint, Coverage.type, 
         Duration.Class) %>%
  map(~ unique(.x))
unique_values

# Count the occurrences of each unique response in the Complaint.filed.against column
against_counts <- InsComp %>%
  count(Complaint.filed.against, name = "Frequency")
against_counts

# Count the occurrences of each unique response in the Respondent.type column
respondent_type_counts <- InsComp %>%
  count(Respondent.type, name = "Frequency")
respondent_type_counts

# Count the occurrences of each unique response in the Confirmed.complaint column
confirmed_complaint_counts <- InsComp %>%
  count(Confirmed.complaint, name = "Frequency")
confirmed_complaint_counts

# Remove rows with "null" values ('')
InsComp_filtered <- InsComp %>%
  filter(!(Confirmed.complaint %in% c("")))

# Count the occurrences of each unique response in the Coverage.type column
coverage_type_counts <- InsComp %>%
  count(Coverage.type, name = "Frequency")
coverage_type_counts

# Remove rows with values 'Fire, Allied Lines & CMP', 'Homeowners', Liability', and 'Miscellaneous'
InsComp_filtered <- InsComp %>%
  filter(!(Coverage.type %in% c("Fire, Allied Lines & CMP", "Homeowners", "Liability", "Miscellaneous")))

# Filtered unique responses for each categorical column
filtered_unique <- InsComp_filtered %>%
  dplyr::select(Respondent.type, Confirmed.complaint, Coverage.type, 
                Duration.Class) %>%
  map(~ unique(.x))
filtered_unique




#########################################**Categorical Variable Factorization**#################################

InsComp_filtered$Duration.Class <- as.numeric(factor(InsComp_filtered$Duration.Class, 
                                            levels = c("Short", "Moderate", "Long", "Extended")))

InsComp_final <- InsComp_filtered

# Factorizing 'Respondent.type' 
InsComp_final <- InsComp_final %>%
  mutate(Resp.Individual = ifelse(Respondent.type == "Individual", 1, 0),
         Resp.Organization = ifelse(Respondent.type == "Organization", 1, 0))

# Factorizing 'Confirmed.complaint' 
InsComp_final <- InsComp_final %>%
  mutate(Conf.Yes = ifelse(Confirmed.complaint == "Yes", 1, 0),
         Conf.No = ifelse(Confirmed.complaint == "No", 1, 0))

# Factorizing 'Coverage.type'
InsComp_final <- InsComp_final %>%
  mutate(Cov.AccidentHealth = ifelse(Coverage.type == "Accident and Health", 1, 0),
         Cov.Automobile = ifelse(Coverage.type == "Automobile", 1, 0),
         Cov.LifeAnnuity = ifelse(Coverage.type == "Life & Annuity", 1, 0))

# Set "Individual" as the complaint type reference
InsComp_final <- InsComp_final %>%
  dplyr::select(-Resp.Individual)

# Set "Yes" as the complaint type reference
InsComp_final <- InsComp_final %>%
  dplyr::select(-Conf.Yes)

# Set "Life & Annuity" as the complaint type reference
InsComp_final <- InsComp_final %>%
  dplyr::select(-Cov.LifeAnnuity)

str(InsComp_final)

# Take a random sample of 4% of entries from InsComp
set.seed(123)
InsComp_final <- InsComp_final %>% sample_frac(0.04)

# Information on the final dataset 
final_info <- str(InsComp_final)
head(InsComp_final)

write.csv(InsComp_final, "InsComp_final.csv", row.names = FALSE)




#########################################**Regression Model**###################################################
library(dplyr)
library(lubridate)
library(ggplot2)  
library(broom)  
library(psychometric)  
library(nnet) 
library(corrplot)
library(purrr)
library(xtable)
library(car)
library(psych)
library(lmtest)
library(lessR)
library(MASS)

InsComp_final <- read.csv("InsComp_final.csv", header = TRUE)
head(InsComp_final)
str(InsComp_final)

CD <- InsComp_final$Complaint.Duration
CD <- InsComp_final$Complaint.Duration
TbC <- InsComp_final$Time.Between.Complaints
TbC <- ifelse(TbC <= 0, TbC + .001, TbC) # Adding .001 in case of zero values
NPC <- InsComp_final$Number.of.Previous.Complaints
RO <- InsComp_final$Resp.Organization
CfN <- InsComp_final$Conf.No
CvAH <- InsComp_final$Cov.AccidentHealth
CvAu <- InsComp_final$Cov.Automobile

fit <- lm(CD ~ TbC + NPC + RO + CfN + CvAH + CvAu, 
          data = InsComp_final)
summary(fit)

layout(matrix(c(1,2,3,4),2,2))
plot(fit)

# Box-Cox Transformation (BoxCox_Results.png)
boxcox_results <- boxcox(fit, lambda = seq(-2, 2, by = 0.1))
lambda_values <- boxcox_results$x
log_likelihoods <- boxcox_results$y
optimal_lambda <- lambda_values[which.max(log_likelihoods)]
cat("Optimal lambda value:", optimal_lambda, "\n") # \lamda = 0.1414141


# log-transformation of Complaint.Duration
# ln_CD <- CD^optimal_lambda 
ln_CD <- log(CD)

# log-transformed Time.between.Complaints
ln_TbC <- log(TbC)  

fit_log <- lm(ln_CD ~ ln_TbC + NPC + RO + CfN + CvAH + CvAu, 
              data = InsComp_final)
summary(fit_log)

layout(matrix(c(1,2,3,4),2,2))
plot(fit_log)

# Test the form of the Model (Linear Form) (Model_Linearity.png)
fitted <- fitted(fit_log)
residuals <- residuals(fit_log)
# plot(fitted, residuals,
#      main = "Residuals vs. Fitted",
#      xlab = "Fitted values",
#      ylab = "Residuals")
# abline(h = 0, col = "red")
# 
# # Test of Normality Assumption
# # (Model_Resid_Hist.png)
# hist(residuals, main = "Histogram of Residuals",
#      xlab = "Residuals", col = "orange")
# 
# # (Model_QQ.png)
# qqnorm(residuals)
# qqline(residuals, col = "red", lwd = 2)
# 
# # Test of Constant Variance Assumption (Model_Scale_Location.png)
# plot(fitted, sqrt(abs(residuals)),
#      main = "Scale-Location",
#      xlab = "Fitted values",
#      ylab = "Square Root of |Residuals|")
# abline(h = 0, col = "red")
# 
# # Test of Outliers (Model_Resid_Boxplot.png)
# boxplot(residuals, main = "Boxplot of Residuals", col = "lightgreen", horizontal = TRUE)
# SummaryStats(residuals)

# Function to remove outliers using IQR
remove_outliers <- function(data, column) {
  Q1 <- quantile(data[[column]], 0.25, na.rm = TRUE)
  Q3 <- quantile(data[[column]], 0.75, na.rm = TRUE)
  IQR_value <- Q3 - Q1
  
  lower_bound <- Q1 - 1.5 * IQR_value
  upper_bound <- Q3 + 1.5 * IQR_value
  
  # Calculate the number of rows before filtering
  before_count <- nrow(data)
  
  # Filter the data
  filtered_data <- data %>%
    filter(data[[column]] >= lower_bound & data[[column]] <= upper_bound)
  
  # Calculate the number of rows after filtering
  after_count <- nrow(filtered_data)
  
  # Print the number of rows removed
  cat("Outliers removed for", column, ":", before_count - after_count, "\n")
  
  return(filtered_data)
}

# Remove outliers for quantitative variables
InsComp_final <- remove_outliers(InsComp_final, "Complaint.Duration") # 761 outliers removed
InsComp_final <- remove_outliers(InsComp_final, "Time.Between.Complaints") # 11226 outliers removed
InsComp_final <- remove_outliers(InsComp_final, "Number.of.Previous.Complaints") # 755 outliers removed
 
# Test of Multicollinearity (CorrPlot.png)
numeric_data <- InsComp_final %>%
  dplyr::select_if(is.numeric) %>%
  dplyr::select(-Complaint.Duration, -Duration.Class)
corr_matrix <- cor(numeric_data)
cor(numeric_data)
corrplot(corr_matrix) # Disregard Cov.Automobile because of high correlation with Cov.AccidentHealth

# Test of Influential Observations 
lev <- hatvalues(fit_log)
cooks_distance <- cooks.distance(fit_log)

# # (Model_Influence_Plot.png)
# influencePlot(fit_log)
# 
# # (Model_Resid_Lev.png)
# plot(lev, residuals,
#      main = "Residuals vs. Leverage",
#      xlab = "Leverage",
#      ylab = "Residuals")
# abline(h = 0, col = "red")

cook_cutoff <- 4 / length(residuals)
# abline(v = cook_cutoff, col = "blue", lty = 2)


influential_points <- which(cooks_distance > cook_cutoff | lev > (2 * mean(lev)))

# Calculate the number of influential points
num_influential_points <- length(influential_points)
cat("Number of influential points:", num_influential_points, "\n") # 914 Influential Points

# Remove influential points
InsComp_cleaned <- InsComp_final[-influential_points, ]
str(InsComp_cleaned) # 4679 observations remaining

CD <- InsComp_cleaned$Complaint.Duration
ln_CD <- log(CD)
TbC <- InsComp_cleaned$Time.Between.Complaints
TbC <- ifelse(TbC <= 0, TbC + .001, TbC) # Adding .001 in case of zero values
ln_TbC <- log(TbC)
NPC <- InsComp_cleaned$Number.of.Previous.Complaints
RO <- InsComp_cleaned$Resp.Organization
CfN <- InsComp_cleaned$Conf.No
CvAH <- InsComp_cleaned$Cov.AccidentHealth

# Model with no categorical variables
fit_num_only <- lm(ln_CD ~ ln_TbC + NPC, 
                      data = InsComp_cleaned)
summary(fit_num_only)

# Full Model
fit_log_cleaned <- lm(ln_CD ~ ln_TbC + NPC + RO + CfN + CvAH, 
                      data = InsComp_cleaned)
summary(fit_log_cleaned)

confint(fit_log_cleaned)

numeric_data <- InsComp_cleaned %>%
  dplyr::select_if(is.numeric) %>%
  dplyr::select(-Complaint.Duration, -Duration.Class)

# Compute the correlation matrix
custom_abbreviations <- c("C.D." = "Complaint.Duration", 
                          "T.B.C." = "Time.Between.Complaints", 
                          "N.P.C." = "Number.of.Previous.Complaints", 
                          "Resp. Org." = "Resp.Organization", 
                          "Conf. No" = "Conf.No", 
                          "Cov. A.H." = "Cov.AccidentHealth")

# Replace colnames and rownames with custom abbreviations
colnames(cor_matrix) <- names(custom_abbreviations)
rownames(cor_matrix) <- names(custom_abbreviations)

# Plot with custom abbreviations
corrplot(cor_matrix, method = "circle", 
         tl.col = "black", 
         tl.srt = 45)







fitted = fitted(fit_log_cleaned)
resid = residuals(fit_log_cleaned)
std_resid = rstandard(fit_log_cleaned)
leverage = hatvalues(fit_log_cleaned)
cooks_distance = cooks.distance(fit_log_cleaned)

library(ggplot2)
library(patchwork)

# Residuals vs Fitted (Model_ResidvFit.pdf)
plot1 <- ggplot(mapping = aes(x = fitted, y = resid)) +
  geom_point(color = "maroon") +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  labs(x = "Fitted Values", y = "Residuals")
plot1

# Normal Q-Q (Model_QQ.pdf)
plot2 <- ggplot(mapping = aes(sample = std_resid)) +
  stat_qq(color = "chartreuse4") +
  stat_qq_line(color = "red") +
  labs(x = "Theoretical Quantiles", y = "Standardized Residuals")
plot2

# Scale-Location (Model_ScaleLocation.pdf)
plot3 <- ggplot(mapping = aes(x = fitted, y = sqrt(abs(std_resid)))) +
  geom_point(color = "darkorchid") +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  labs(x = "Fitted Values", y = "Sqrt(|Standardized Residuals|)")
plot3

# Residuals vs Leverage (Model_ResidvLev.pdf)
plot4 <- ggplot(mapping = aes(x = leverage, y = std_resid)) +
  geom_point(color = "cadetblue") +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  labs(x = "Leverage", y = "Standardized Residuals")
plot4

# Combine plots
combined_plot <- (plot1 | plot2) / (plot3 | plot4)
combined_plot # Full_DiagPlots.pdf




#########################################**Small & Large Organizations Regression Models**######################

# Calculate the frequency of complaints filed against each entity
against_counts <- InsComp_cleaned %>%
  count(Complaint.filed.against, name = "Frequency")

# Merge the frequency back into the main dataset
InsComp_cleaned <- InsComp_cleaned %>%
  left_join(against_counts, by = "Complaint.filed.against")

# Categorize companies into Small, Medium, and Large
InsComp_cleaned <- InsComp_cleaned %>%
  mutate(
    Category = case_when(
      Frequency <= 10 ~ "Small",
      Frequency > 10 & Frequency <= 50 ~ "Medium",
      Frequency > 50 ~ "Large"
    )
  )

# Split the dataset into three groups
InsComp_small <- InsComp_cleaned %>% filter(Category == "Small")
InsComp_medium <- InsComp_cleaned %>% filter(Category == "Medium")
InsComp_large <- InsComp_cleaned %>% filter(Category == "Large")

# Verify the split
cat("Number of small organizations:", nrow(InsComp_small), "\n")
cat("Number of medium organizations:", nrow(InsComp_medium), "\n")
cat("Number of large organizations:", nrow(InsComp_large), "\n")

# # Save the datasets to CSV files
# write.csv(InsComp_small, "InsComp_small.csv", row.names = FALSE)
# write.csv(InsComp_medium, "InsComp_medium.csv", row.names = FALSE)
# write.csv(InsComp_large, "InsComp_large.csv", row.names = FALSE)

# Summary of counts in each category
summary_counts <- InsComp_cleaned %>%
  group_by(Category) %>%
  summarise(Total_Entities = n_distinct(Complaint.filed.against),
            Total_Complaints = n())

print(summary_counts)

##############Small Companies###################

InsComp_small <- remove_outliers(InsComp_small, "Complaint.Duration")
InsComp_small <- remove_outliers(InsComp_small, "Time.Between.Complaints")
InsComp_small <- remove_outliers(InsComp_small, "Number.of.Previous.Complaints")

CD <- InsComp_small$Complaint.Duration
ln_CD <- log(CD)
TbC <- InsComp_small$Time.Between.Complaints
TbC <- ifelse(TbC <= 0, TbC + .001, TbC) # Adding .001 in case of zero values
ln_TbC <- log(TbC)
NPC <- InsComp_small$Number.of.Previous.Complaints
RO <- InsComp_small$Resp.Organization
CfN <- InsComp_small$Conf.No
CvAH <- InsComp_small$Cov.AccidentHealth

fit_small <- lm(ln_CD ~ ln_TbC + NPC + RO + CfN + CvAH, 
                data = InsComp_small)
summary(fit_small)
layout(matrix(c(1, 2, 3, 4), 2, 2)) # Small_DiagPlot.png
plot(fit_small)

##############Medium Companies###################

InsComp_medium <- remove_outliers(InsComp_medium, "Complaint.Duration")
InsComp_medium <- remove_outliers(InsComp_medium, "Time.Between.Complaints")
InsComp_medium <- remove_outliers(InsComp_medium, "Number.of.Previous.Complaints")

CD <- InsComp_medium$Complaint.Duration
ln_CD <- log(CD)
TbC <- InsComp_medium$Time.Between.Complaints
TbC <- ifelse(TbC <= 0, TbC + .001, TbC) # Adding .001 in case of zero values
ln_TbC <- log(TbC)
NPC <- InsComp_medium$Number.of.Previous.Complaints
RO <- InsComp_medium$Resp.Organization
CfN <- InsComp_medium$Conf.No
CvAH <- InsComp_medium$Cov.AccidentHealth

fit_medium <- lm(ln_CD ~ ln_TbC + NPC + RO + CfN + CvAH, 
                data = InsComp_medium)
summary(fit_medium)
layout(matrix(c(1, 2, 3, 4), 2, 2)) # Medium_DiagPlot.png
plot(fit_medium)

##############Large Companies###################

InsComp_large <- remove_outliers(InsComp_large, "Complaint.Duration")
InsComp_large <- remove_outliers(InsComp_large, "Time.Between.Complaints")
InsComp_large <- remove_outliers(InsComp_large, "Number.of.Previous.Complaints")

fit_large <- lm(ln_CD ~ ln_TbC + NPC + RO + CfN + CvAH + CvAu, 
                data = InsComp_large)
summary(fit_large)
layout(matrix(c(1, 2, 3, 4), 2, 2)) # Large_DiagPlot.png
plot(fit_large)


#########################################**Graphs & Summary Statistics**########################################

# Histogram of Complaint Duration (CompDur_Histogram.pdf)
ggplot(InsComp_final, aes(x = Complaint.Duration)) +
  geom_histogram(bins = 30, color = "black", fill = "orange") +
  labs(x = "Complaint Duration (days)", 
       y = "Frequency") 

# Boxplot of Complaint Duration by Duration Class (CompDur_Boxplot.pdf)
ggplot(InsComp_final, aes(x = factor(Duration.Class, 
                                     labels = c("Short \n(<30 Days)", 
                                                                "Moderate \n(30-70 Days)", 
                                                                "Long \n(71-119 Days)", 
                                                                "Extended \n(>119 Days)")), 
                          y = Complaint.Duration)) +
  geom_boxplot(fill = "lightblue", color = "black") +
  labs(x = "Duration Class", 
       y = "Complaint Duration (days)")

# Bar Plot of Coverage Type Frequencies (CovType_BarPlot.png)
ggplot(InsComp_final, aes(x = Coverage.type)) +
  geom_bar(fill = "green", color = "black") +
  labs(title = "Frequency of Each Coverage Type", 
       x = "Coverage Type", 
       y = "Frequency") +
  theme_minimal()

# Scatter Plot of Complaint Duration vs. Time Between Complaints (CompDur_TimeBComp_Scatter.png)
ggplot(InsComp_final, aes(x = Time.Between.Complaints, y = Complaint.Duration)) +
  geom_point(color = "blue", alpha = 0.5) +
  labs(title = "Complaint Duration vs Time Between Complaints", 
       x = "Time Between Complaints (days)", 
       y = "Complaint Duration (days)") +
  theme_minimal()

# Residual Plot for Log-Transformed Regression Model (LogCompDur_ResPlot.png)
ggplot(fit_log, aes(.fitted, .resid)) +
  geom_point(alpha = 0.5, color = "maroon") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "Residuals vs Fitted for Log-Transformed Model", 
       x = "Fitted Values", 
       y = "Residuals") +
  theme_minimal()

# Density Plot of Complaint Duration by Respondent Type (CompDur_ResType_Density.png)
ggplot(InsComp_final, aes(x = Complaint.Duration, fill = Respondent.type)) +
  geom_density(alpha = 0.6) +
  labs(title = "Density of Complaint Duration by Respondent Type", 
       x = "Complaint Duration (days)", 
       y = "Density") +
  theme_minimal()

# Heatmap of Complaint Counts by Respondent and Coverage Type (Cov_Res_Heatmap.png)
InsComp_final %>%
  count(Respondent.type, Coverage.type) %>%
  ggplot(aes(x = Respondent.type, y = Coverage.type, fill = n)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  labs(title = "Heatmap of Complaint Counts by Respondent and Coverage Type",
       x = "Respondent Type", 
       y = "Coverage Type", 
       fill = "Count") +
  theme_minimal()

# Diagnostic plots (Diag_Plots.png)
layout(matrix(c(1,2,3,4),2,2))
plot(fit_log)

# Summary Statistics for Quantitative Variables
summary(InsComp_final$Complaint.Duration)
sd(InsComp_final$Complaint.Duration)
summary(InsComp_final$Time.Between.Complaints)
sd(InsComp_final$Time.Between.Complaints)
summary(InsComp_final$Number.of.Previous.Complaints)
sd(InsComp_final$Number.of.Previous.Complaints)