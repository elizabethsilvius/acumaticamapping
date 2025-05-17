# acumaticamapping
Work to map acumatica amounts to Redding Jobs. 

## Where to Start
You will need three files inside a folder called redding:
1. redding_items_withcosts.csv - comes from the redding workbook. Copy and paste the costs from the cost sheet into the items sheet and save it as a .csv
2. Acumatica_data.csv - the data sheet from the acumatica workbook
3. Redding Project progress as of March 2025.csv - a file that has all the items that have been installed, by March 2025

## What to Do	
Currently, there is only one file. It is commented pretty well and fairly self explanatory. It takes each line from the acumatica
data sheet and assigns the amount to various reccing Job IDs, by matching cost codes and type of job.
