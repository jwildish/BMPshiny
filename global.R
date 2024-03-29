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

flowfull <- read_xlsx("C:/Users/Jordan/Documents/cFlowBMPFlatFile.xlsx")

flowfull$concat <- paste(flowfull$`Test Site ID`, flowfull$BMPID, flowfull$STARTDATE, flowfull$ENDDATE, flowfull$`Storm .`)

flowfull <- subset(flowfull, `Scrn 2 In Out Diff` == "P")

flowfull2 <- merge(flowfull, WQtable, by = "BMPID")


flowinflow <- subset(flowfull, `Monitoring Station Type` == "Inflow")
flowoutflow <- subset(flowfull, `Monitoring Station Type` == "Outflow")



names(flow)
flow <- merge(flowinflow, flowoutflow, by = c("concat", "BMPID", "Storm .", "Test Site ID", "SITENAME"), all.y = F)

flow$percentTOTVOLEFF <- (flow$TOTVOLEFF.x - flow$TOTVOLEFF.y)/flow$TOTVOLEFF.x

flow$percentPEAKVOLEFF <- (flow$PEAKEFFFLOW.x - flow$PEAKEFFFLOW.y)/flow$PEAKEFFFLOW.x

WQtable <- WQ %>% group_by(BMPID, Analysis_Category) %>% summarise(tot = sum(`WQ Analysis Value`))

flow2 <- merge(flow, WQtable, by = "BMPID")

flow2 <- flow2 %>% select(concat, BMPID, `Storm .`, `Test Site ID`, SITENAME, Analysis_Category, 
                                              percentTOTVOLEFF, percentPEAKVOLEFF, TOTVOLEFF.x, TOTVOLEFF.y, PEAKEFFFLOW.x, PEAKEFFFLOW.y)

