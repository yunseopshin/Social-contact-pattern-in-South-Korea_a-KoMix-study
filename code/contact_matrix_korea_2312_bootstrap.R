################################################################################
# Bootstrap analysis for R0 estimation
# Data: KoMix (2023.11)
################################################################################

# Set working directory and load data
setwd('C:/project/Social-contact-pattern-in-South-Korea-a-KoMix-study/data/Survey on Changes in Contact Patterns After COVID-19 (23.11)')
data = read.csv('Data(2311).csv', fileEncoding = "utf-8")

# Load required libraries
library(stringr)
library(dplyr)
library(boot)

# Column index for contact age groups (50 contacts maximum)
contact_num_index = 8:57  # Q8A_1_1 to Q8A_50_1

################################################################################
# Data preparation
################################################################################

# Replace NA with 0 for COVID-19 vaccination data
data$Q3A[is.na(data$Q3A)] = 0

# Extract contact data columns:
# SQ2(1), SQ5AA(2), Q8(7), Q8A_1_1~Q8A_50_1(8-57), Q4(5), Q7(6), Q2(3), Q3A(4)
contact_data = data[, c(1, 2, 7, contact_num_index, 5, 6, 3, 4)]
contact_data[is.na(contact_data)] = 0

# Load Korea population structure
setwd('C:/project/Social-contact-pattern-in-South-Korea-a-KoMix-study/data')
skage = read.csv('skage.csv', header = T)
skage = skage[1:101, 5]
skage = as.numeric(sapply(skage, function(x) gsub(',', '', x)))

################################################################################
# R0 calculation function
################################################################################

R0_function = function(contact_data) {
    # Initialize contact matrices
    phi = matrix(0, nrow = 10, ncol = 10)
    phi_week = matrix(0, nrow = 10, ncol = 10)
    phi_weekend = matrix(0, nrow = 10, ncol = 10)
    
    # Age group boundaries
    age = c(0, 3, 7, 13, 16, 19, 30, 40, 50, 60)
    
    # Filter weekday and weekend data
    contact_week = contact_data %>% filter(SQ5AA == 5)
    contact_weekend = contact_data %>% filter(SQ5AA == 2)
    
    # Construct contact matrices by age group
    for(i in 1:10) {
        if(i != 10) {
            temp = contact_data %>% filter(SQ2 >= age[i] & SQ2 < age[i+1])
            temp_week = contact_week %>% filter(SQ2 >= age[i] & SQ2 < age[i+1])
            temp_weekend = contact_weekend %>% filter(SQ2 >= age[i] & SQ2 < age[i+1])
        } else {
            temp = contact_data %>% filter(SQ2 >= age[10])
            temp_week = contact_week %>% filter(SQ2 >= age[10])
            temp_weekend = contact_weekend %>% filter(SQ2 >= age[10])
        }
        
        for(j in 1:10) {
            if(j != 10) {
                phi[i, j] = sum(sapply(1:nrow(temp), function(x) length(which(temp[x, 4:53] == j)))) / nrow(temp)
                phi_week[i, j] = sum(sapply(1:nrow(temp_week), function(x) length(which(temp_week[x, 4:53] == j)))) / nrow(temp_week)
                phi_weekend[i, j] = sum(sapply(1:nrow(temp_weekend), function(x) length(which(temp_weekend[x, 4:53] == j)))) / nrow(temp_weekend)
            } else {
                phi[i, j] = sum(sapply(1:nrow(temp), function(x) length(which(temp[x, 4:53] >= j)))) / nrow(temp)
                phi_week[i, j] = sum(sapply(1:nrow(temp_week), function(x) length(which(temp_week[x, 4:53] >= j)))) / nrow(temp_week)
                phi_weekend[i, j] = sum(sapply(1:nrow(temp_weekend), function(x) length(which(temp_weekend[x, 4:53] >= j)))) / nrow(temp_weekend)
            }
        }
    }
    
    # Weighted average of weekday and weekend contacts
    phi_res = (5/7) * phi_week + (2/7) * phi_weekend
    
    # Calculate population distribution by age group
    n = rep(0, 10)
    for(i in 1:9) {
        n[i] = sum(skage[(age[i]+1):(age[i+1])])
    }
    n[10] = sum(skage[61:101])
    
    # Symmetric contact matrix (Sebastian method)
    phi_res_sb = matrix(0, nrow = 10, ncol = 10)
    for(i in 1:10) {
        for(j in 1:10) {
            phi_res_sb[i, j] = (phi_res[i, j] * n[i] + phi_res[j, i] * n[j]) / (2 * n[i])
        }
    }
    
    # Next Generation Matrix (NGM)
    NGM = matrix(0, nrow = length(n), ncol = length(n))
    for(i in 1:length(n)) {
        for(j in 1:length(n)) {
            NGM[i, j] = phi_res_sb[i, j] * n[i] / n[j]
        }
    }
    
    # Return R0 (largest eigenvalue of NGM)
    return(eigen(NGM)$values[1])
}

# Test R0 calculation
cat("Original R0 value:", R0_function(contact_data), "\n\n")

################################################################################
# Bootstrap analysis for R0
################################################################################

# Global variables for progress bar
pb_counter <- 0
pb_total <- 0

# Bootstrap statistic function
R0_statistic <- function(data, indices) {
    # Generate bootstrap sample
    boot_data <- data[indices, , drop = FALSE]
    
    # Calculate R0 for bootstrap sample
    R0_value <- R0_function(boot_data)
    
    # Update progress bar
    pb_counter <<- pb_counter + 1
    setTxtProgressBar(pb, pb_counter)
    
    return(R0_value)
}

# Bootstrap execution
set.seed(2025)  # For reproducibility
B <- 1000  # Number of bootstrap iterations

# Initialize progress bar
cat("Bootstrap in progress...\n")
pb_counter <- 0
pb_total <- B
pb <- txtProgressBar(min = 0, max = B, style = 3, char = "=")

# Execute bootstrap
boot_result <- boot(data = contact_data, 
                    statistic = R0_statistic, 
                    R = B, 
                    stype = "i")

# Close progress bar
close(pb)

################################################################################
# Results
################################################################################

cat("\n\n=== Bootstrap Results ===\n")
print(boot_result)

# Calculate 95% confidence interval (percentile method)
R0_CI <- quantile(boot_result$t, probs = c(0.025, 0.975), na.rm = TRUE)
cat("\nR0 Original Value:", boot_result$t0, "\n")
cat("95% Confidence Interval (Percentile): [", R0_CI[1], ", ", R0_CI[2], "]\n")

# Calculate confidence interval using boot.ci
boot_ci_result <- boot.ci(boot_result, type = c("perc"))
print(boot_ci_result)

################################################################################
# Optional: Save results
################################################################################

# result_summary <- data.frame(
#     R0_original = boot_result$t0,
#     R0_mean = mean(boot_result$t, na.rm = TRUE),
#     R0_se = sd(boot_result$t, na.rm = TRUE),
#     CI_lower_percentile = R0_CI[1],
#     CI_upper_percentile = R0_CI[2]
# )
# 
# print(result_summary)
# write.csv(result_summary, "R0_bootstrap_result.csv", row.names = FALSE)
