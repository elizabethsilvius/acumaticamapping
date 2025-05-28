library(dplyr)
library(readr)
library(stringr)
library(lubridate)
library(tidyr)
library(survival)

date_to_numeric <- function(date_vector) {
  # Convert to Date if not already
  date_vector <- as.Date(date_vector)
  
  # Rank the dates: earlier dates get lower numbers
  ranked <- rank(date_vector, ties.method = "first")
  
  return(ranked)
}

assign_deciles <- function(x) {
  # Remove NA and ensure it's numeric
  if (!is.numeric(x)) stop("Input must be a numeric vector")
  
  # Compute deciles using ntile from dplyr
  deciles <- dplyr::ntile(x, 10)
  
  return(deciles)
}

proximity_weights <- function(n, a) {
  # Ensure input is numeric
  if (!is.numeric(n) || !is.numeric(a)) stop("Both n and a must be numeric.")
  
  # Compute distances
  dist <- abs(a - n)
  
  # Invert distances: smaller distance = higher weight
  inv_dist <- 1 / (dist + 1e-10)  # Add small epsilon to avoid division by zero
  
  # Normalize to sum to 1
  weights <- inv_dist / sum(inv_dist)
  
  return(weights)
}

# Load data
acumatica <- read_csv("redding/Acumatica_data.csv")
redding <- read_csv("redding/redding_items_withcosts.csv")
progress <- read_csv("redding/Redding Project progress as of March 2025.csv")
payroll <- read_csv("redding/payroll.csv")


# Ensure date format is proper
acumatica <- acumatica %>%
  mutate(Date = mdy(Date))

payroll <- payroll %>%
  mutate(Date = mdy(Date))


# Normalize cost codes
acumatica <- acumatica %>%
  mutate(Cost_Code_Num = str_replace_all(`Cost Code`, "-", ""))


# make dates into numeric value
acumatica <- acumatica %>%
  mutate(numeric_dates = date_to_numeric(Date))

payroll <- payroll %>%
  mutate(numeric_dates = date_to_numeric(Date))

# assign date deciles
acumatica <- acumatica %>%
  mutate(date_decile = assign_deciles(numeric_dates))

payroll <- payroll %>%
  mutate(date_decile = assign_deciles(numeric_dates))


# select items and progress columns and create percent installed
progress <- progress %>%
  select(Job_Item_No, Planned_Quantity, Total_Installed) %>%
  mutate(perc_installed = Total_Installed/Planned_Quantity)

# merge progress with redding
redding <- merge(redding, progress, by.x = "Job_Item_ID", by.y = "Job_Item_No", 
                 all.x = TRUE)

# fill in perc installed NAs
redding <- redding %>% 
  mutate(perc_installed = replace_na(perc_installed, 0))

redding <- redding %>% 
  filter(perc_installed > 0)

# Normalize redding Cost_Code for matching
redding <- redding %>%
  mutate(Cost_Code_Num = str_replace_all(Cost_Code, "\\D", "")) %>%
  mutate(SOV_Category_Number = as.numeric(SOV_Category_Number))

# assign SOV_Category_Number deciles
redding <- redding %>%
  mutate(sov_decile = assign_deciles(SOV_Category_Number))

# filter for redding items with a labor budget
redding <- redding %>% 
  filter(Labor_Cost > 0)

# Prepare output list
output_rows <- list()
unmatched <- c()


for (i in 1:nrow(payroll)){
  # match the date
  row <- payroll[i, ]
  print(i)
  if(row$Pay_plus != "#N/A"){
    date <- row$Date
    date_decile <- row$date_decile
    amount <- as.numeric(row$Pay_plus)
    n <- 5
    # Prepare output list
    temp <- data_frame()
    while (nrow(temp) < 1){
      prior <- date - days(n)
      after <- date + days(n)
      # subset acumatica rows 
      temp <- acumatica %>%
        filter(Date >= prior & Date <= after)
      n <- n + 1
    }
    ccs <- unique(temp$Cost_Code_Num)
    temp <- temp %>%
      group_by(Cost_Code_Num) %>%
      summarise(Total_Amount = sum(Amount)) 
    temp$Amount_prop <- (temp$Total_Amount/sum(temp$Total_Amount)) * amount
    result <- list()
    for (cc in ccs){
      ind <- which(temp$Cost_Code_Num == cc)
      amount_to_assign <- round(temp$Amount_prop[ind],2)
      if(cc == "000000"){
        cc <- "010000"
      }
      if(cc == "221313"){
        cc <- "321300"
      }
      if(cc == "033000"){
        cc <- "321313"
      }
      if (startsWith(cc, "98") || startsWith(cc, "99")) {
        # Filter Redding by SOV Category >= 15
        eligible <- redding %>% filter(SOV_Category_Number >= 15)
        
        # Weight based on date: earlier dates to lower SOV numbers
        elg_decile <- eligible$sov_decile
        weights <- proximity_weights(date_decile, elg_decile)
        
        distribution <- amount_to_assign * weights
        
        if(amount_to_assign - sum(distribution) > .1){
          print(i)
        }
        
        result <- rbind(result, cbind(eligible %>% select(Job_Item_ID), 
                                      Amount_Allocated = round(distribution,2)))
      } else {
        # Match best Cost_Code
        matches <- redding %>%
          filter(str_sub(Cost_Code_Num, 1, 2) == str_sub(cc, 1, 2)) %>%
          mutate(match_score_3 = ifelse(str_sub(Cost_Code_Num, 3, 3) == str_sub(cc, 3, 3), 1, 0),
                 match_score_4 = ifelse(str_sub(Cost_Code_Num, 4, 4) == str_sub(cc, 4, 4), 1, 0),
                 match_score_5 = ifelse(str_sub(Cost_Code_Num, 5, 5) == str_sub(cc, 5, 5), 1, 0),
                 match_score_6 = ifelse(str_sub(Cost_Code_Num, 6, 6) == str_sub(cc, 6, 6), 1, 0))
        if(nrow(matches) > 0){
          # if there is at least one cc that matches with 6 digits, filter to those ccs
          if(sum(matches$match_score_6) > 0){
            matches <- matches %>%
              filter(match_score_6 == 1)
          }else{
            # else, if there is at least one cc that matches 5 digits, filter to those ccs
            if(sum(matches$match_score_5) > 0){
              matches <- matches %>%
                filter(match_score_5 == 1)
            }else{
              # else, if there is at least one cc that matches 4 digits, filter to those ccs
              if(sum(matches$match_score_4) > 0){
                matches <- matches %>%
                  filter(match_score_4 == 1)
              }else{
                # else, if there is at least one cc that matches 3 digits, filter to those ccs
                if(sum(matches$match_score_3) > 0){
                  matches <- matches %>%
                    filter(match_score_3 == 1)
                }
              }
            }
          }
        }
        if(nrow(matches) > 0){
          weights <-  (matches$Labor_Cost *matches$perc_installed) / (sum(matches$Labor_Cost * matches$perc_installed))
          distribution <- amount_to_assign * weights
          if(amount_to_assign - sum(distribution) > .1){
            print(i)
          }
          result <- rbind(result, cbind(matches %>% select(Job_Item_ID), 
                                        Amount_Allocated = round(distribution,2)))
        } else {
          unmatched <- c(unmatched, cc)
          result <- rbind(result, data.frame(Job_Item_ID = NA, Amount_Allocated = 0))
        }
      }
      
      # Append job allocations
      output_rows[[i]] <- cbind(payroll[i, ], result)
    }
  }
  
}

# Combine all
final_result <- bind_rows(output_rows)


# Write to CSV
write_csv(final_result, "redding/payroll_allocated_results.csv")

total_allocated <- final_result %>%
  group_by(Job_Item_ID) %>%
  summarise(Total_Amount_Allocated = sum(Amount_Allocated)) 

# Write to CSV
write_csv(total_allocated, "redding/payroll-to_redding_total_allocated.csv")

unmatched <- unique(unmatched)
unmatched

sum(total_allocated$Total_Amount_Allocated)
sum(as.numeric(payroll$Pay_plus), na.rm = TRUE)
