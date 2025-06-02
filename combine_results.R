library(dplyr)
library(readr)
library(lubridate)

acumatica <- read_csv("redding/acumatica_allocated_results.csv")
payroll <- read_csv("redding/payroll_allocated_results.csv")
redding <- read_csv("redding/redding_items_withcosts.csv")

an <- nrow(acumatica)
pn <- nrow(payroll)

payroll <- payroll %>%
  mutate(Date = ymd(Date))
acumatica <- acumatica %>%
  mutate(Date = ymd(Date))

redding <- redding %>%
  select(Job_Item_ID, SOV_Category_Number, SOV_Category_Description)

Description <- c(acumatica$Description, paste("Payroll_",payroll$Date))
Date <- c(acumatica$Date, payroll$Date)
jobno <- rep(23011, an+pn)
Vendor <- c(acumatica$`Customer/Vendor`, rep("Kent Cunstruction", pn))
Amount <- c(acumatica$Amount_Allocated, payroll$Amount_Allocated)
appliedto <- c(acumatica$Job_Item_ID, payroll$Job_Item_ID)
costcode <- c(acumatica$`Cost Code`, payroll$`Cost Code`)
Class <- c(acumatica$`Debit Account Group`, rep("LABOR", pn))
Type <- c(acumatica$`Orig. Doc. Type`, rep("Payroll Expense", pn))

final_output <- data.frame(cbind(Description, Date, jobno, Vendor, Amount, 
                      appliedto, costcode, Class, Type))

oldnames = c("jobno", "appliedto", "costcode")
newnames = c("Job No","Applied To", "Cost Code")

final_output <- final_output %>% rename_at(vars(oldnames), ~ newnames)

final_output <- merge(final_output, redding, by.x = "Applied To", 
                      by.y = "Job_Item_ID", all.x =TRUE)

final_output$Status<- rep("PAID", an+pn)
final_output$Backup <- rep("Imputed Expense", an+pn)
final_output$'Cost from Shipping/Hauling' <- rep(NA, an+pn)
final_output$'Change Status' <- rep(NA, an+pn)
final_output$'Change No' <- rep(NA, an+pn)

ccid <- seq.int(nrow(final_output))
final_output <- cbind(ccid, final_output)
names(final_output)[names(final_output) == 'ccid'] <-  "Cost Item ID"


write.csv(final_output, "redding/final_output_acumatica_and_payroll.csv", row.names = FALSE)
      