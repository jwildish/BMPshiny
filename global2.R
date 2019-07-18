library(readxl)
library(dplyr)
library(tidyverse)
WQ <- read_xlsx("C:/Users/Jordan/Documents/bWQ BMP FlatFile BMP Indiv Anal_Rev 10-2014.xlsx")

glimpse(WQ)

WQ$concat <- paste(WQ$SITEID, WQ$BMPID, WQ$SAMPLEDATE, WQ$`WQX Parameter`, WQ$SAMPLETIME, WQ$`Storm .`)

WQ <- subset(WQ, `Monitoring Station Type` == "Inflow" | `Monitoring Station Type` == "Outflow")

table(WQ$`Use in BMP Category Analysis`)
table(WQ$`Use in BMP WQ Analysis`)

inflow <- subset(WQ, `Monitoring Station Type` == "Inflow")
outflow <- subset(WQ, `Monitoring Station Type` == "Outflow")


Combined <- merge(inflow, outflow, by = c("concat", "Analysis_Category",
                                          "Group", "WQX Parameter", "Sample Fraction", "WQ Units", "STATE", "COUNTRY"), all.y = F)

Combined <- subset(Combined, `Use in BMP Category Analysis.x` == "Yes" | `Use in BMP Category Analysis.x` == "yes")

Combined <- Combined %>% select(concat, Analysis_Category,
                    Group, `WQX Parameter`, `Sample Fraction`, `WQ Units`, `STATE`, `COUNTRY`, `WQ Analysis Value.x`, `WQ Analysis Value.y`)

names(Combined)
Combined <- subset(Combined, `WQ Analysis Value.x` > 0.000000001)
Combined <- subset(Combined, `WQ Analysis Value.y` > 0.000000001)


Combined$PercentReduction <- ((Combined$`WQ Analysis Value.x` - Combined$`WQ Analysis Value.y`)/Combined$`WQ Analysis Value.y`)

Combined <- Combined %>% add_count(Analysis_Category, `WQX Parameter`)

Combined <- Combined %>% 
  group_by(Analysis_Category, `WQX Parameter`, n)

Combined <- subset(Combined, n > 30)

glimpse(Combined)


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

flow2tot <- flow2 %>% group_by(Analysis_Category) %>% dplyr::summarise(inflow = median(TOTVOLEFF.x),outflow = median(TOTVOLEFF.y), prcttotvol = median(percentTOTVOLEFF, na.rm = T))


flow2tot$percent <- ((flow2tot$inflow - flow2tot$outflow)/flow2tot$inflow)
flow2tot$prcttotvol <- NULL

write.csv(flow2tot, "totalflowbmp.csv")
flow3 <- flow2 
flow3 <- subset(flow3, PEAKEFFFLOW.x > 0 & PEAKEFFFLOW.y > 0)

flow3peak <- flow3 %>% group_by(Analysis_Category) %>% dplyr::summarise(inflow = median(PEAKEFFFLOW.x),outflow = median(PEAKEFFFLOW.y), prcttotvol = median(percentPEAKVOLEFF, na.rm = T))

flow3peak$percent <- ((flow3peak$inflow - flow3peak$outflow)/flow3peak$inflow)

flow3peak$prcttotvol <- NULL
write.csv(flow3peak, "peakflowbmp.csv")

getwd()
