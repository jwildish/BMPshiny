library(readxl)
library(dplyr)
library(tidyverse)
WQ <- read_xlsx("C:/Users/Jordan/Documents/bWQ BMP FlatFile BMP Indiv Anal_Rev 10-2014.xlsx")

glimpse(WQ)

WQ$concat <- paste(WQ$SITEID, WQ$BMPID, WQ$SAMPLEDATE, WQ$`WQX Parameter`, WQ$SAMPLETIME, WQ$`Storm .`)

table(WQ$`Use in BMP Category Analysis`)
table(WQ$`Use in BMP WQ Analysis`)

inflow <- subset(WQ, `Monitoring Station Type` == "Inflow")
outflow <- subset(WQ, `Monitoring Station Type` == "Outflow")


Combined <- merge(inflow, outflow, by = c("concat", "Analysis_Category",
                                          "Group", "WQX Parameter", "Sample Fraction", "WQ Units", "STATE", "COUNTRY"), all.y = F)

Combined <- Combined %>% select(concat, Analysis_Category,
                    Group, `WQX Parameter`, `Sample Fraction`, `WQ Units`, `STATE`, `COUNTRY`, `WQ Analysis Value.x`, `WQ Analysis Value.y`)

Combined <- subset(Combined, `Use in BMP Category Analysis.x` == "Yes" | `Use in BMP Category Analysis.x` == "yes")

Combined <- subset(Combined, `WQ Analysis Value.x` > 0.000000001)
Combined <- subset(Combined, `WQ Analysis Value.y` > 0.000000001)


Combined$PercentReduction <- ((Combined$`WQ Analysis Value.x` - Combined$`WQ Analysis Value.y`)/Combined$`WQ Analysis Value.y`)


hist(Combined$PercentReduction)
