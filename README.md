# Query-Rate-Analysis
A novel depiction of clinical trial query rate analysis using R

This code contains a looping structure that reads SAS datasets from a given directory, summarizes the eCRF/form count for each dataset
by study site and eCRF and returns a list of dataframes for these forms. Furthermore, it reads in query data from a separate standard 
report and joins the two together, calculates the rate, interquartile range, and produces a clean ggplot boxplot visualization with stunning
insights.
