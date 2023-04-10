library(tidyverse)

library(readxl)
GCV_Titration <- read_excel("Downloads/Titration GCV based on PCR 12.19.21.xlsx", 
                            sheet = "Kruskal")

GCV_Titration$kwallisgroup <- as.factor(GCV_Titration$kwallisgroup)
GCV_Titration$kwallisgroup = factor(GCV_Titration$kwallisgroup, labels = c("Saline", "GCV 0.1 mg/kg", "GCV 1 mg/kg", "GCV 10 mg/kg"))

class(GCV_Titration$kwallisgroup)

kruskal.test(kwallis ~ kwallisgroup, data = GCV_Titration)

library(FSA)

dunnTest(kwallis ~ kwallisgroup, data=GCV_Titration, method="bonferroni")
