library(tidyverse)
library(haven)
library(stringr)
library(plotly)
library(ggthemes)
library(purrr)
library(rJava)
library(xlsxjars)
library(xlsx)

datasets_XXXX <- list.files("location redacted", all.files = TRUE, pattern = "*.sas7bdat")

data_path <- file.path("location redacted")

CRFdata <- read.csv("location redacted")

for (i in datasets_XXXX) {
  if (i %in% CRFdata$datasets_XXXX) {
    loc <- str_locate(i, "\\.")[1]
    x <- substr(i, 1, loc-1)
    y <- read_sas(paste0(data_path, "/", i))
  if (nrow(y) >0) {
    z <- y %>% filter(SUBJID != "") %>% select(one_of(c("INVID", "SUBJID"))) %>% count(INVID)
    #adding new column with formname from dataset
    z["Form"] <- NA
    z["Form"] <- y[1, "DATAPAGENAME"]
    colnames(z)[2] <- "pgct"
    assign(paste0(x,"ct"), z)
    dflist <- lapply(ls(pattern = "ct"), function(z) get(z))
  } 
}}


#pagect <- bind_rows(unlist(dflist))

pagecount <- reduce(dflist, bind_rows)


## creating query summaries

queryXXXX<- read_csv("location redacted")

query1 <- select(queryXXXX, one_of
                 (c("SiteName", 
                    "SubjectName", 
                    "Folder",
                    "Form",
                    "Field",
                    "Log#",
                    "QryOpenDateLocalized",
                    "QueryText",
                    "MarkingGroupName",
                    "QryResponseDateLocalized",
                    "Name",
                    "SiteGroupName")))

query1 <- mutate(query1, SiteName = ifelse(
  str_sub(SiteName, 5, 5) == " ",
  paste("0",str_sub(SiteName, 1, 4),sep = ""),
  str_sub(SiteName, 1, 5)
)
)

#converting dates to date format from factor

query1 <- mutate(query1, 
                 QOpenDt = as.Date(query1$QryOpenDateLocalized, "%d-%b-%y"), 
                 QAnsDt = as.Date(query1$QryResponseDateLocalized, "%d-%b-%y"))
query1 <- select(query1, -QryOpenDateLocalized)
query1 <- select(query1, -QryResponseDateLocalized)

#creating summary of queries by site and form

queryct <- count(query1, SiteName, Form)

#joining data between the two metrics by form and invid

plotdata <- left_join(pagecount, queryct, by = c("INVID"="SiteName", "Form")) %>%
            mutate(qperp = n/pgct)

q1 <- plotdata %>% select(-n, -pgct) %>% filter(!is.na(qperp))

##plotting data:

p <- ggplot(q1, aes(x=reorder(q1$Form, q1$qperp, FUN = median), y=q1$qperp, fill = "limegreen")) + geom_boxplot(outlier.colour = "blue", coef=0.75) +theme_economist()
p <- p + labs(y = "Normalized Query Rate (by Form Count)", x = "Form Name", title = "GS-US-296-XXXX - Query Rate", subtitle = "by Form")
p <- p + ylim(0, 15) 
p <- p + scale_fill_manual(values = c("limegreen"))
p <- p + theme (legend.position = "none", axis.text.y = element_text(size = 8, hjust = 1, vjust = 0.2)) +coord_flip()
p

#Creating outlier columns
q_lIQR <- q1 %>% group_by(Form) %>% summarize(lIQR = quantile(qperp, probs=0.25))
q_hIQR <- q1 %>% group_by(Form) %>% summarize(hIQR = quantile(qperp, probs=0.75))
q_IQR <- (q1 %>% group_by(Form) %>% summarize(IQR = IQR(qperp))) %>% mutate(., IQR = 0.75*IQR)
q_IQR$IQR <- q_IQR$IQR  *0.75


IQRs <- left_join(q_lIQR, q_hIQR, by = "Form") %>%
        mutate(lower = lIQR - 0.75*(hIQR-lIQR)) %>%
        mutate(higher = hIQR + 0.75*(hIQR-lIQR)) %>%
        select(one_of(c("Form", "lower", "higher")))

q2 <- q1 %>% left_join(IQRs, by = "Form")
q2[, 6] <- NA 
colnames(q2)[6] <- "outlabel"

for (i in 1:nrow(q2)) {
  if (q2$qperp[i] < q2$lower[i] | q2$qperp[i] > q2$higher[i]) {
    q2$outlabel[i] <- q2$INVID[i]
  } else {q2$outlabel[i] <- ""}
}

#creating final outlier list:

outlier_list <- filter(q2, outlabel != "") %>% select(-lower, -higher, -outlabel) %>%
                rename(QueryRate = qperp) 

filepath <- "location redacted"
write.xlsx(as.data.frame(outlier_list), filepath, sheetName = "XXX-XXXX_Outliers", col.names = TRUE, row.names = FALSE, showNA = FALSE)

