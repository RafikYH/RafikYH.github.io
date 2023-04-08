---
title: ' Projets tutorés 2 | Devoir 1'
author: "YAHIA Mohammed Rafik"
date: "2023-03-24"
output:
  prettydoc::html_pretty:
    theme: cayman
    number_sections: true
    toc: true
    toc_depth: 3
    df_print: paged
    css: rafik.css
    highlight : github
  pdf_document: default
---

<style>
.table-hover > tbody > tr:hover { 
  background-color: #e1f1fd;
}
</style>

```{css, echo=FALSE}
pre {
  max-height: 200px;
  overflow-y: auto;
}

pre[class] {
  max-height: 80px;
}
```

```{r setup, include=FALSE, echo=FALSE}
require(tidyverse)
require(tableone)
require(skimr)
require(kableExtra)
require(tibble)
require(stringr)
require(prettydoc)
require(rmdformats)

#### In patient protocol
###Screening for patients with baseline informations
###DM Table with variables ARM AGE SEX
dm <- read.csv("ascii-data-files nida-ctn-0001-20230318/dm.csv", header = T, dec = ".")
tabDM <- dm %>% select(USUBJID, ARM, AGE, SEX)


###VS Table with variables SBP DBP BMI PULSE RESP TEMP
##VSTESTCD is the vital test needed, multiple measurements are taken for 1 patient, we got duplicated rows.
vs <- read.csv("ascii-data-files nida-ctn-0001-20230318/vs.csv", header = T, dec = ".")
tabTEMP <- vs %>% filter(EPOCH=="SCREENING", VSTESTCD=="TEMP") %>% select(USUBJID, VSORRES)
tabTEMP <- distinct(tabTEMP)
colnames(tabTEMP) <- c("USUBJID", "TEMP") #Fahrenheit

tabRESP <- vs %>% filter(EPOCH=="SCREENING", VSTESTCD=="RESP") %>% select(USUBJID, VSORRES)
tabRESP <- distinct(tabRESP)
colnames(tabRESP) <- c("USUBJID", "RESP") #Breaths per minute

tabPULSEsitting <- vs %>% filter(EPOCH=="SCREENING", VSPOS=="SITTING", VSTESTCD=="PULSE") %>% select(USUBJID, VSORRES)
tabPULSEsitting <- distinct(tabPULSEsitting)
colnames(tabPULSEsitting) <- c("USUBJID", "PULSEsitting") #Beats per minute

tabPULSEstanding <- vs %>% filter(EPOCH=="SCREENING", VSPOS=="STANDING", VSTESTCD=="PULSE") %>% select(USUBJID, VSORRES)
tabPULSEstanding <- distinct(tabPULSEstanding)
colnames(tabPULSEstanding) <- c("USUBJID", "PULSEstanding") #Beats per minute

tabSBPsitting <- vs %>% filter(EPOCH=="SCREENING", VSPOS=="SITTING", VSTESTCD=="SBP") %>% select(USUBJID, VSORRES)
tabSBPsitting <- distinct(tabSBPsitting)
colnames(tabSBPsitting) <- c("USUBJID", "SBPsitting") #MMHG

tabSBPstanding <- vs %>% filter(EPOCH=="SCREENING", VSPOS=="STANDING", VSTESTCD=="SBP") %>% select(USUBJID, VSORRES)
tabSBPstanding <- distinct(tabSBPstanding)
colnames(tabSBPstanding) <- c("USUBJID", "SBPstanding") #MMHG

tabDBPsitting <- vs %>% filter(EPOCH=="SCREENING", VSPOS=="SITTING", VSTESTCD=="DBP") %>% select(USUBJID, VSORRES)
tabDBPsitting <- distinct(tabDBPsitting)
colnames(tabDBPsitting) <- c("USUBJID", "DBPsitting") #MMHG

tabDBPstanding <- vs %>% filter(EPOCH=="SCREENING", VSPOS=="STANDING", VSTESTCD=="DBP") %>% select(USUBJID, VSORRES)
tabDBPstanding <- distinct(tabDBPstanding)
colnames(tabDBPstanding) <- c("USUBJID", "DBPstanding") #MMHG

tabHEIGHT <- vs %>% filter(EPOCH=="SCREENING", VSTESTCD=="HEIGHT") %>% select(USUBJID, VSORRES)
tabHEIGHT <- distinct(tabHEIGHT)
colnames(tabHEIGHT) <- c("USUBJID", "HEIGHT") #Inches

tabWEIGHT <- vs %>% filter(EPOCH=="SCREENING", VSTESTCD=="WEIGHT") %>% select(USUBJID, VSORRES)
tabWEIGHT <- distinct(tabWEIGHT)
colnames(tabWEIGHT) <- c("USUBJID", "WEIGHT") #Pounds

#Creating BMI from Weight and Height tables.
calculate_bmi <- function(dataframe, height_col, weight_col, bmi_col) {
  height_meters <- dataframe[[height_col]] * 0.0254 # convert height to meters
  weight_kg <- dataframe[[weight_col]] * 0.453592 # convert weight to kilograms
  bmi <- weight_kg / (height_meters^2) # calculate BMI
  dataframe[[bmi_col]] <- bmi 
  return(dataframe)
}

tabBMI <- left_join(tabHEIGHT, tabWEIGHT, by="USUBJID")
tabBMI <- calculate_bmi(tabBMI, "HEIGHT", "WEIGHT", "BMI") %>% select("USUBJID", "BMI")

list_df1 = list(tabBMI, tabDBPsitting, tabSBPsitting, tabDBPstanding, tabSBPstanding, tabPULSEsitting, tabPULSEstanding, tabRESP, tabTEMP)
tabVS <- list_df1 %>% reduce(full_join, by='USUBJID')

###SC Table with variables MARITAL EDUCATION EMPLOYEMENT
sc <- read.csv("ascii-data-files nida-ctn-0001-20230318/sc.csv", header = T, dec = ".")

tabMARITAL <- sc %>% filter(SCTESTCD=="MARITAL") %>% select(USUBJID, SCORRES)
tabMARITAL <- distinct(tabMARITAL)
colnames(tabMARITAL) <- c("USUBJID", "MARITAL") #Marital status

tabEDUC <- sc %>% filter(SCTESTCD=="EDUCYRS") %>% select(USUBJID, SCORRES)
tabEDUC <- distinct(tabEDUC)
colnames(tabEDUC) <- c("USUBJID", "EDUCYRS") #Education in years

tabEMPLOY30 <- sc %>% filter(SCTESTCD=="EMPLOY30") %>% select(USUBJID, SCORRES)
tabEMPLOY30 <- distinct(tabEMPLOY30)
colnames(tabEMPLOY30) <- c("USUBJID", "EMPLOY30") #Employement 30 past days

tabEMPLOY3Y <- sc %>% filter(SCTESTCD=="EMPLOY3Y") %>% select(USUBJID, SCORRES)
tabEMPLOY3Y <- distinct(tabEMPLOY3Y)
colnames(tabEMPLOY3Y) <- c("USUBJID", "EMPLOY3Y") #Employement 3 past years

tabEMPLOY <- left_join(tabEMPLOY30, tabEMPLOY3Y, by="USUBJID") #Left join for both type of employement data

list_df2 = list(tabEDUC, tabEMPLOY, tabMARITAL)
tabSC <- list_df2 %>% reduce(full_join, by='USUBJID')


###LB Table with variables METHADONE MORPHINE on baseline
lb <- read.csv("ascii-data-files nida-ctn-0001-20230318/lb.csv", header = T, dec = ".")

#LBMETHOD on Methadone Central lab, because it's the most important one according to documentation.
tabMETHADONE <- lb %>% filter(EPOCH=="SCREENING", LBTESTCD=="METHADON", LBMETHOD=="CENTRAL LAB") %>% select(USUBJID, LBORRES)
tabMETHADONE <- distinct(tabMETHADONE)
colnames(tabMETHADONE) <- c("USUBJID", "METHADONE") #POS or NEG

tabMORPHINE <- lb %>% filter(EPOCH=="SCREENING", LBTESTCD=="MORPHINE") %>% select(USUBJID, LBORRES)
tabMORPHINE <- distinct(tabMORPHINE)
colnames(tabMORPHINE) <- c("USUBJID", "MORPHINE") #POS or NEG

list_df3 = list(tabMETHADONE, tabMORPHINE)
tabLB <- list_df3 %>% reduce(full_join, by='USUBJID')


###QS Table with variables VAS on baseline
qs <- read.csv("ascii-data-files nida-ctn-0001-20230318/qs.csv", header = T, dec = ".")

tabQS <- qs %>% filter(EPOCH=="SCREENING", QSTESTCD=="VAS001") %>% select(USUBJID, QSORRES)
tabQS <- distinct(tabQS)
colnames(tabQS) <- c("USUBJID", "VAS001") #Visual Analogic scale for opiates cravings


#Joining all 5 tables into 01 full table.
list_df4 = list(tabDM, tabSC, tabVS, tabQS, tabLB)
tabALL_01 <- list_df4 %>% reduce(full_join, by='USUBJID')


#### Out patient protocol
### Screening for patients with baseline informations
### Redoing the same thing we did above for the out patient protocol.
dm2 <- read.csv("ascii-data-files nida-ctn-0002-20230318/dm.csv", header = T, dec = ".")
tabDM2 <- dm2 %>% select(USUBJID, ARM, AGE, SEX)

vs2 <- read.csv("ascii-data-files nida-ctn-0002-20230318/vs.csv", header = T, dec = ".")

tabTEMP2 <- vs2 %>% filter(EPOCH=="SCREENING", VSTESTCD=="TEMP") %>% select(USUBJID, VSORRES)
tabTEMP2 <- distinct(tabTEMP2)
colnames(tabTEMP2) <- c("USUBJID", "TEMP") #Fahrenheit
tabTEMP2$TEMP[tabTEMP2$TEMP == 999.9] <- 99.9 #Replace 999.9 value in TEMP.

tabRESP2 <- vs2 %>% filter(EPOCH=="SCREENING", VSTESTCD=="RESP") %>% select(USUBJID, VSORRES)
tabRESP2 <- distinct(tabRESP2)
colnames(tabRESP2) <- c("USUBJID", "RESP") #Breaths per minute

tabPULSEsitting2 <- vs2 %>% filter(EPOCH=="SCREENING", VSPOS=="SITTING", VSTESTCD=="PULSE") %>% select(USUBJID, VSORRES)
tabPULSEsitting2 <- distinct(tabPULSEsitting2)
colnames(tabPULSEsitting2) <- c("USUBJID", "PULSEsitting") #Beats per minute

tabPULSEstanding2 <- vs2 %>% filter(EPOCH=="SCREENING", VSPOS=="STANDING", VSTESTCD=="PULSE") %>% select(USUBJID, VSORRES)
tabPULSEstanding2 <- distinct(tabPULSEstanding2)
colnames(tabPULSEstanding2) <- c("USUBJID", "PULSEstanding") #Beats per minute

tabSBPsitting2 <- vs2 %>% filter(EPOCH=="SCREENING", VSPOS=="SITTING", VSTESTCD=="SBP") %>% select(USUBJID, VSORRES)
tabSBPsitting2 <- distinct(tabSBPsitting2)
colnames(tabSBPsitting2) <- c("USUBJID", "SBPsitting") #MMHG

tabSBPstanding2 <- vs2 %>% filter(EPOCH=="SCREENING", VSPOS=="STANDING", VSTESTCD=="SBP") %>% select(USUBJID, VSORRES)
tabSBPstanding2 <- distinct(tabSBPstanding2)
colnames(tabSBPstanding2) <- c("USUBJID", "SBPstanding") #MMHG

tabDBPsitting2 <- vs2 %>% filter(EPOCH=="SCREENING", VSPOS=="SITTING", VSTESTCD=="DBP") %>% select(USUBJID, VSORRES)
tabDBPsitting2 <- distinct(tabDBPsitting2)
colnames(tabDBPsitting2) <- c("USUBJID", "DBPsitting") #MMHG

tabDBPstanding2 <- vs2 %>% filter(EPOCH=="SCREENING", VSPOS=="STANDING", VSTESTCD=="DBP") %>% select(USUBJID, VSORRES)
tabDBPstanding2 <- distinct(tabDBPstanding2)
colnames(tabDBPstanding2) <- c("USUBJID", "DBPstanding") #MMHG

tabHEIGHT <- vs2 %>% filter(EPOCH=="SCREENING", VSTESTCD=="HEIGHT") %>% select(USUBJID, VSORRES)
tabHEIGHT <- distinct(tabHEIGHT)
colnames(tabHEIGHT) <- c("USUBJID", "HEIGHT") #Inches, weird us metrics...

tabWEIGHT <- vs2 %>% filter(EPOCH=="SCREENING", VSTESTCD=="WEIGHT") %>% select(USUBJID, VSORRES)
tabWEIGHT <- distinct(tabWEIGHT)
colnames(tabWEIGHT) <- c("USUBJID", "WEIGHT") #Pounds, weird us metrics...

tabHEIGHT2 <- vs2 %>% filter(EPOCH=="SCREENING", VSTESTCD=="HEIGHT") %>% select(USUBJID, VSORRES)
tabHEIGHT2 <- distinct(tabHEIGHT2)
colnames(tabHEIGHT2) <- c("USUBJID", "HEIGHT") #Inches, weird us metrics...
tabHEIGHT2$HEIGHT[tabHEIGHT2$HEIGHT == 715] <- 71 #Replace 715 value in HEIGHT.

tabWEIGHT2 <- vs2 %>% filter(EPOCH=="SCREENING", VSTESTCD=="WEIGHT") %>% select(USUBJID, VSORRES)
tabWEIGHT2 <- distinct(tabWEIGHT2)
colnames(tabWEIGHT2) <- c("USUBJID", "WEIGHT") #Pounds, weird us metrics...

#Creating BMI from Weight and Height tables.
calculate_bmi <- function(dataframe, height_col, weight_col, bmi_col) {
  height_meters <- dataframe[[height_col]] * 0.0254 # convert height to meters
  weight_kg <- dataframe[[weight_col]] * 0.453592   # convert weight to kilograms
  bmi <- weight_kg / (height_meters^2)   # calculate BMI
  dataframe[[bmi_col]] <- bmi
  return(dataframe)
}

tabBMI2 <- left_join(tabHEIGHT2, tabWEIGHT2, by="USUBJID")

tabBMI2 <- calculate_bmi(tabBMI2, "HEIGHT", "WEIGHT", "BMI") %>% select("USUBJID", "BMI")

list_df5 = list(tabBMI2, tabDBPstanding2, tabSBPstanding2, tabDBPsitting2, tabSBPsitting2, tabPULSEstanding2, tabPULSEsitting2, tabRESP2, tabTEMP2)
tabVS2 <- list_df5 %>% reduce(full_join, by='USUBJID')

sc2 <- read.csv("ascii-data-files nida-ctn-0002-20230318/sc.csv", header = T, dec = ".")

tabMARITAL2 <- sc2 %>% filter(SCTESTCD=="MARITAL") %>% select(USUBJID, SCORRES)
tabMARITAL2 <- distinct(tabMARITAL2)
colnames(tabMARITAL2) <- c("USUBJID", "MARITAL") #MARITAL STATUS

tabEDUC2 <- sc2 %>% filter(SCTESTCD=="EDUCYRS") %>% select(USUBJID, SCORRES)
tabEDUC2 <- distinct(tabEDUC2)
colnames(tabEDUC2) <- c("USUBJID", "EDUCYRS") #Education in years

tabEMPLOY302 <- sc2 %>% filter(SCTESTCD=="EMPLOY30") %>% select(USUBJID, SCORRES)
tabEMPLOY302 <- distinct(tabEMPLOY302)
colnames(tabEMPLOY302) <- c("USUBJID", "EMPLOY30") #Employement 30 past days

tabEMPLOY3Y2 <- sc2 %>% filter(SCTESTCD=="EMPLOY3Y") %>% select(USUBJID, SCORRES)
tabEMPLOY3Y2 <- distinct(tabEMPLOY3Y2)
colnames(tabEMPLOY3Y2) <- c("USUBJID", "EMPLOY3Y") #Employement 3 past years

tabEMPLOY2 <- left_join(tabEMPLOY302, tabEMPLOY3Y2, by="USUBJID") #Left join for both type of employement data

list_df6 = list(tabEDUC2, tabEMPLOY2, tabMARITAL2)
tabSC2 <- list_df6 %>% reduce(full_join, by='USUBJID')

lb2 <- read.csv("ascii-data-files nida-ctn-0002-20230318/lb.csv", header = T, dec = ".")

tabMETHADONE2 <- lb2 %>% filter(EPOCH=="SCREENING", LBTESTCD=="METHADON", LBMETHOD=="CENTRAL LAB") %>% select(USUBJID, LBORRES)
tabMETHADONE2 <- distinct(tabMETHADONE2)
colnames(tabMETHADONE2) <- c("USUBJID", "METHADONE") #POS or NEG

tabMORPHINE2 <- lb2 %>% filter(EPOCH=="SCREENING", LBTESTCD=="MORPHINE") %>% select(USUBJID, LBORRES)
tabMORPHINE2 <- distinct(tabMORPHINE2)
colnames(tabMORPHINE2) <- c("USUBJID", "MORPHINE") #POS or NEG

list_df7 = list(tabMETHADONE2, tabMORPHINE2)
tabLB2 <- list_df7 %>% reduce(full_join, by='USUBJID')

qs2 <- read.csv("ascii-data-files nida-ctn-0002-20230318/qs.csv", header = T, dec = ".")

tabQS2 <- qs2 %>% filter(EPOCH=="SCREENING", QSTESTCD=="VAS001") %>% select(USUBJID, QSORRES)
tabQS2 <- distinct(tabQS2)
colnames(tabQS2) <- c("USUBJID", "VAS001") #Visual Analogic scale for opiates cravings


#Joining all 5 tables into 01 full table.
list_df8 = list(tabDM2, tabSC2, tabVS2, tabQS2, tabLB2)
tabALL_02 <- list_df8 %>% reduce(full_join, by='USUBJID')

#Joining both full tables of In patient and Out patient protocol, to obtain a OneProcAway (OPA) Table.
TableALL <- rbind(tabALL_02, tabALL_01)
str(TableALL)

TableALL$VAS001 <- as.numeric(TableALL$VAS001) #Changing into a numeric value


## Eliminating duplicated rows and multiple values for one patient.
#Issue was in VS tables as they had different sitting and standing values for one patient (PULSE,SBP,DBP...) we calculated their mean.
#Issue was also in LB tables as they had different tests for METHADONE (some patients got tested twice so they can include them in the study), we took the 2nd test that determined their participation.
#Issue was in QS table for one patient only.
#Data type correction
TableAgg <- TableALL %>%
  group_by(USUBJID) %>%
  mutate(DBPstanding = mean(DBPstanding),
         DBPsitting = mean(DBPsitting),
         SBPstanding = mean(SBPstanding),
         SBPsitting = mean(SBPsitting),
         PULSEstanding = mean(PULSEstanding),
         PULSEsitting = mean(PULSEsitting),
         RESP = mean(RESP),
         TEMP = mean(TEMP),
         VAS001 = mean(VAS001))

TableAgg <- distinct(TableAgg) %>% 
  filter(!(ARM == "")) %>% 
  filter(!(ARM == "SCREEN FAILURE"))

TableAgg$ARM <- as.factor(TableAgg$ARM)
TableAgg$SEX <- as.factor(TableAgg$SEX)
TableAgg$EDUCYRS <- as.numeric(TableAgg$EDUCYRS)
TableAgg$EMPLOY30 <- as.factor(TableAgg$EMPLOY30)
TableAgg$EMPLOY3Y <- as.factor(TableAgg$EMPLOY3Y)
TableAgg$MARITAL <- as.factor(TableAgg$MARITAL)
TableAgg$METHADONE <- as.factor(TableAgg$METHADONE)
TableAgg$MORPHINE <- as.factor(TableAgg$MORPHINE)
TableAgg <- as.data.frame(TableAgg) 

str(TableAgg)

#Creating a table with ARM Strata
TableONE <- CreateTableOne(vars = c("ARM","AGE","SEX","EDUCYRS","EMPLOY30","EMPLOY3Y","MARITAL","BMI","DBPstanding","SBPstanding","DBPsitting","SBPsitting","PULSEstanding","PULSEsitting","RESP","TEMP","VAS001","METHADONE","MORPHINE"), 
                           data = TableAgg, 
                           strata = "ARM")
print(TableONE, test = FALSE)

#Creating a SKIMR table with ARM strata
TableSKIMR <- TableAgg %>% group_by(ARM) %>% skim() %>% print()

#Both tables are modified in the Rmarkdown files because of their properties and use in the HTML context.

##################################################################
#Patients that did not provide the urine on D13 or D14 test were removed.
perprotocol1 <- lb %>% filter(VISIT %in% c("STUDY DAY 13","STUDY DAY 14")) %>% 
  filter((LBSTAT != "NOT DONE")) %>%  
  group_by(USUBJID)
length(unique(perprotocol1$USUBJID))

perprotocol2 <- lb2 %>% filter(VISIT %in% c("STUDY DAY 13","STUDY DAY 14")) %>% 
  filter((LBSTAT != "NOT DONE")) 
length(unique(perprotocol2$USUBJID))

#Merged with dm dataframe, that contains the ARM information
TablePROT1 <- left_join(perprotocol1, dm, by='USUBJID') %>% 
  select("USUBJID" , "EPOCH.x" ,   "LBSEQ"  ,  "LBTESTCD", "LBTEST"  , "LBCAT"   , "LBORRES" ,
        "LBSTRESC", "LBSTRESN", "LBSTRESU", "LBNRIND", "LBSTAT" ,  "LBREASND", "LBSPEC" ,  "LBMETHOD" , "VISIT.x"  ,  "VISITNUM.x" ,  "LBDY", "ARM")  
length(unique(TablePROT1$USUBJID)) #70 Number of patients on D13 or D14 on Protocol 1

TablePROT2 <- left_join(perprotocol2, dm2, by='USUBJID') %>% 
  select("USUBJID" , "EPOCH.x" ,   "LBSEQ"  ,  "LBTESTCD", "LBTEST"  , "LBCAT"   , "LBORRES" ,
         "LBSTRESC", "LBSTRESN", "LBSTRESU", "LBNRIND", "LBSTAT" ,  "LBREASND", "LBSPEC" ,  "LBMETHOD" , "VISIT.x"  ,  "VISITNUM.x" ,  "LBDY", "ARM" )
length(unique(TablePROT2$USUBJID)) #107 Number of patients on D13 or D14 on Protocol 2

#Protocole 1 : D1 113 patients / D14 70 patients
TablePROT1BupNx <- TablePROT1 %>% filter(ARM=="BUPRENORPHINE/NALOXONE")
length(unique(TablePROT1BupNx$USUBJID)) #62 Patients on Bup/Nx

TablePROT1Clon <- TablePROT1 %>% filter(ARM=="CLONIDINE")
length(unique(TablePROT1Clon$USUBJID)) #8 Patients on Clon

#Protocole 2. : D1 230 patients / D14 108 patients
TablePROT2BupNx <- TablePROT2 %>% filter(ARM=="BUPRENORPHINE/NALOXONE")
length(unique(TablePROT2BupNx$USUBJID)) #88 Patients on Bup/Nx

TablePROT2Clon <- TablePROT2 %>% filter(ARM=="CLONIDINE")
length(unique(TablePROT2Clon$USUBJID)) #20 Patients on Clon

####################################################################
```
\tableofcontents

# Introduction
This is an R Markdown document. Below is an explanation and a summary of my script that should hopefully be a guide. Attached to this generated HTML file there is:  

- Flow chart powerpoint 
- Workflow diagram 
- R script  

\
\

# Objective 01 : Completing the patients flow chart

## Protocoles & Excel files

In this study there are two protocols (IN/OUT) each one with its respective excel files is found in a seperate folder. These seperate folders contain each 26 excel files with all of the patients data. The information we need to extract in order to complete the flow chart is found in 05 main excel files for each protocole.  

The 05 excel files (with the needed variables) are :

- dm (USUBJID, ARM, AGE, SEX)
- vs (USUBJID, HEIGHT, WEIGHT, DBP, SBP, PULSE, RESP, TEMP)
- sc (USUBJID, MARITAL, EDUCYRS, EMPLOY30, EMPLOY3Y)
- lb (USUBJID, METHADONE, MORPHINE)
- qs (USUBJID, VAS001)

NOTE : All of these informations are taken from the Protocole paper, please refer to it first before reading this document.

We start by importing the excel files, then we select the variables of interest and we apply a filter on each dataframe we create according to our needs. In this case we are looking for Baseline informations only, on the second part we will add Day 13 and Day 14 informations.

\
Importing the necessary libraries

```{r message=FALSE, warning=FALSE}
require(tidyverse)
require(stringr)
require(tibble)
require(tableone)
require(skimr)
```
\
\

## First half (Baseline / Screening)

Down below is a preview of the excel files import, and filter + selection of the important variables. 
```{r, echo=TRUE}
# In patient protocol (folder 01)
dm <- read.csv("ascii-data-files nida-ctn-0001-20230318/dm.csv", header = T, dec = ".")
tabDM <- dm %>% select(USUBJID, ARM, AGE, SEX)

vs <- read.csv("ascii-data-files nida-ctn-0001-20230318/vs.csv", header = T, dec = ".")
tabTEMP <- vs %>% filter(EPOCH=="SCREENING", VSTESTCD=="TEMP") %>% select(USUBJID, VSORRES)
tabTEMP <- distinct(tabTEMP)
colnames(tabTEMP) <- c("USUBJID", "TEMP") #Fahrenheit
```

Once all the files are imported we move to the next step which is joining by Patient ID (USUBJID) the different dataframes into one.
```{r, echo=TRUE}
# Joining all 5 tables into 01 full table.
list_df4 = list(tabDM, tabSC, tabVS, tabQS, tabLB)
tabALL_01 <- list_df4 %>% reduce(full_join, by='USUBJID')
```

Once we're done we repeat the process for the second folder with the OUT patient protocol, obtaining different dataframes, then merging them into one.
```{r, echo=TRUE}
# Out patient protocol (folder 02)
dm2 <- read.csv("ascii-data-files nida-ctn-0002-20230318/dm.csv", header = T, dec = ".")
tabDM2 <- dm2 %>% select(USUBJID, ARM, AGE, SEX)

vs2 <- read.csv("ascii-data-files nida-ctn-0002-20230318/vs.csv", header = T, dec = ".")
tabRESP2 <- vs2 %>% filter(EPOCH=="SCREENING", VSTESTCD=="RESP") %>% select(USUBJID, VSORRES)
tabRESP2 <- distinct(tabRESP2)
colnames(tabRESP2) <- c("USUBJID", "RESP") #Breaths per minute
```

```{r, echo=TRUE}
# Joining all 5 tables into 01 full table.
list_df8 = list(tabDM2, tabSC2, tabVS2, tabQS2, tabLB2)
tabALL_02 <- list_df8 %>% reduce(full_join, by='USUBJID')
```

Now that we have all the columns and rows needed for IN patients (tabALL_01) and OUT patients (tabALL_02) we merge them into one complete table (TableALL).
```{r, echo=TRUE}
#Joining both full tables of In patient and Out patient protocol, to obtain a OneProcAway (OPA) Table.
TableALL <- rbind(tabALL_02, tabALL_01)





TableALL$VAS001 <- as.numeric(TableALL$VAS001)

#Changing into a numeric value, this step is crucial otherwise it won't be possible to calculate the mean of this column down below.

#I like to leave changing format of columns after applying the different filters and reducing the number of rows to a minimum.
```

The merged table (TableALL) has many observations per patient. We need to reformate it as 1 row per patient, to get meaningful statistics out of it, a straighforward way to do this is to calculate the mean of the multiple measurements.
```{r, echo=TRUE}
# summarizing multiple values into one per patient.
TableAgg <- TableALL %>%
  group_by(USUBJID) %>%
  mutate(DBPstanding = mean(DBPstanding),
         DBPsitting = mean(DBPsitting),
         SBPstanding = mean(SBPstanding),
         SBPsitting = mean(SBPsitting),
         PULSEstanding = mean(PULSEstanding),
         PULSEsitting = mean(PULSEsitting),
         RESP = mean(RESP),
         TEMP = mean(TEMP),
         VAS001 = mean(VAS001)) %>% 
  distinct() 
```

Once we summarized the dataframe, we can print the values of ARM column to complete the 1st half of the Flow chart powerpoint.
```{r, echo=TRUE}
kable(table(TableAgg$ARM, useNA = "ifany"), 
      col.names = c("ARM","Count")) %>% 
  row_spec(row = 0, color = "white", background = "#255980")
```

```{r, echo=FALSE}
TableAgg <- TableAgg %>%  
  filter(!(ARM == "")) %>% 
  filter(!(ARM == "SCREEN FAILURE"))

TableAgg$ARM <- as.factor(TableAgg$ARM)
TableAgg$SEX <- as.factor(TableAgg$SEX)
TableAgg$EDUCYRS <- as.numeric(TableAgg$EDUCYRS)
TableAgg$EMPLOY30 <- as.factor(TableAgg$EMPLOY30)
TableAgg$EMPLOY3Y <- as.factor(TableAgg$EMPLOY3Y)
TableAgg$MARITAL <- as.factor(TableAgg$MARITAL)
TableAgg$METHADONE <- as.factor(TableAgg$METHADONE)
TableAgg$MORPHINE <- as.factor(TableAgg$MORPHINE)
TableAgg <- as.data.frame(TableAgg)
```

\
\

## Second half (Day 13/Day 14)

A quick recap of what was done above : 

- Screening : 415 (We obtained the number of all the candidates of this study) 
- Randomized : 343 (After Screening we got the actual number of participants as some candidates do no meet the criteria of selection) 
- Not included : 72 (Screening - Randomized)
- BUP/NX : 233 (Applied a filter by ARM to our dataframe TableAgg)
- Clonidine : 110 (Applied a filter by ARM to our dataframe TableAgg)

We proceed now to finish the second half and extract the information of the patients that finished the protocole (reached day 13 or 14 with providing urine tests). By merging lb table that contains the urine tests and date they were provided with dm table that contains the ARM to which patient belongs.
```{r, echo=TRUE}
#Patients that did not provide the urine on D13 or D14 test were removed.
perprotocol1 <- lb %>% filter(VISIT %in% c("STUDY DAY 13","STUDY DAY 14")) %>% 
  filter((LBSTAT != "NOT DONE"))

#Merged with dm dataframe, that contains the ARM information
TablePROT1 <- left_join(perprotocol1, dm, by='USUBJID') %>% 
  select("USUBJID","EPOCH.x","LBSEQ","LBTESTCD","LBTEST","LBCAT","LBORRES","LBSTRESC","LBSTRESN","LBSTRESU","LBNRIND","LBSTAT","LBREASND","LBSPEC","LBMETHOD","VISIT.x","VISITNUM.x","LBDY","ARM")
```

We have a total of 70 IN patients (BUP + CLON) that did reached the end of the study.
```{r, echo=TRUE}
#Protocole 1 :
TablePROT1BupNx <- TablePROT1 %>% filter(ARM=="BUPRENORPHINE/NALOXONE")
length(unique(TablePROT1BupNx$USUBJID)) #62 Patients on Bup/Nx
```
```{r, echo=TRUE}
TablePROT1Clon <- TablePROT1 %>% filter(ARM=="CLONIDINE")
length(unique(TablePROT1Clon$USUBJID)) #8 Patients on Clon
```
\

We redo the same with the OUT protocole patients.
```{r, echo=TRUE}
#Patients that did not provide the urine on D13 or D14 test were removed.
perprotocol2 <- lb2 %>% filter(VISIT %in% c("STUDY DAY 13","STUDY DAY 14")) %>% 
  filter((LBSTAT != "NOT DONE")) 

#Merged with dm dataframe, that contains the ARM information
TablePROT2 <- left_join(perprotocol2, dm2, by='USUBJID') %>% 
  select("USUBJID","EPOCH.x","LBSEQ","LBTESTCD","LBTEST","LBCAT","LBORRES","LBSTRESC","LBSTRESN","LBSTRESU","LBNRIND","LBSTAT","LBREASND","LBSPEC","LBMETHOD","VISIT.x","VISITNUM.x","LBDY","ARM")
```

We have a total of 108 OUT patients (BUP + CLON) that did reached the end of the study.
```{r, echo=TRUE}
#Protocole 2 : 
TablePROT2BupNx <- TablePROT2 %>% filter(ARM=="BUPRENORPHINE/NALOXONE")
length(unique(TablePROT2BupNx$USUBJID)) #88 Patients on Bup/Nx
```
```{r, echo=TRUE}
TablePROT2Clon <- TablePROT2 %>% filter(ARM=="CLONIDINE")
length(unique(TablePROT2Clon$USUBJID)) #20 Patients on Clon
```

\
\

# Objective 02 : Statistical description per ARM
Now that out table is cleaned and formatted properly, generating statistical summary is as easy as writing one line of code (for ugly tables that is).
\
\

## Table One Library

```{r, echo=TRUE}
# Creating a table with ARM Strata
TableONE <- CreateTableOne(vars=c("ARM","AGE","SEX","EDUCYRS","EMPLOY30","EMPLOY3Y","MARITAL","BMI","DBPstanding","SBPstanding","DBPsitting","SBPsitting","PULSEstanding","PULSEsitting","RESP","TEMP","VAS001","METHADONE","MORPHINE"), data = TableAgg, strata = "ARM")
```

```{r, echo=FALSE, results = 'hide'}  
TableONE <- print(TableONE, test = FALSE) 
```

```{r, echo=TRUE}
TableONE %>% kable() %>%  
  kable_styling(bootstrap_options = c("striped", "hover"),
                full_width = F, 
                font_size = 10) %>% 
  row_spec(row = 0, color = "white", background = "#255980") %>%
  pack_rows("Employement in the past 30 days", 5, 14) %>%
  pack_rows("Employement in the past 3 years", 15, 23) %>% 
  pack_rows("Civil status", 24, 30) %>% 
  pack_rows("Vital sign measurements", 31, 40) %>% 
  pack_rows("Drug tests", 41, 42) # Added hover styling and modified its color with CSS code at the start of the Rmarkdown document. # There is a hidden code of print function that got saved into an object than fed into kable function. Because it's impossible to use the TableONE object as an argument in kable.
```
\
\

## Skimr Library

```{r echo=T, results='hide'}
# Separating factor variables so the HTML table has less columns to display per table.
TableSKIMRfac <- TableAgg %>% 
  group_by(ARM) %>% 
  select(ARM,SEX,EMPLOY30,EMPLOY3Y,MARITAL,METHADONE,MORPHINE) %>% 
  skim() %>% 
  dplyr::select(-skim_type)
```

```{r, echo=TRUE}
TableSKIMRfac %>% kable() %>% 
  kable_styling(bootstrap_options = c("striped", "hover"),
                full_width = F, 
                font_size = 10) %>%
  row_spec(row = 0, color = "white", background = "#255980")
```

```{r echo=T, results='hide'}
# Separating numeric variables so the HTML table has less columns to display per table.
TableSKIMRnum <- TableAgg %>% 
  group_by(ARM) %>% 
  select(ARM,AGE,EDUCYRS,BMI,DBPsitting,SBPsitting,PULSEsitting,RESP,TEMP,VAS001) %>% 
  skim() %>% 
  dplyr::select(-skim_type)
```

```{r, echo=TRUE}
TableSKIMRnum %>% kable() %>% 
  kable_styling(bootstrap_options = c("striped", "hover"),
                full_width = F, 
                font_size = 6) %>%
  row_spec(row = 0, color = "white", background = "#255980")
```

\

# Annex

## Flow chart powerpoint

```{r,  include=TRUE, fig.align="center", fig.cap=c("Flow chart powerpoint"), echo=FALSE}
knitr::include_graphics("TemplateFlowChart.png")
```

\
\

## Workflow diagram

```{r, include=TRUE, fig.align="center", fig.cap=c("Workflow diagram"), echo=FALSE}
knitr::include_graphics("Diagram_YAHIA_FINALE.png")
```

\
\

## R script

```{r, eval=F, echo=T}
require(tidyverse)
require(tableone)
require(skimr)
require(kableExtra)
require(tibble)
require(stringr)
require(prettydoc)
require(rmdformats)

#### In patient protocol
###Screening for patients with baseline informations
###DM Table with variables ARM AGE SEX
dm <- read.csv("ascii-data-files nida-ctn-0001-20230318/dm.csv", header = T, dec = ".")
tabDM <- dm %>% select(USUBJID, ARM, AGE, SEX)


###VS Table with variables SBP DBP BMI PULSE RESP TEMP
##VSTESTCD is the vital test needed, multiple measurements are taken for 1 patient, we got duplicated rows.
vs <- read.csv("ascii-data-files nida-ctn-0001-20230318/vs.csv", header = T, dec = ".")
tabTEMP <- vs %>% filter(EPOCH=="SCREENING", VSTESTCD=="TEMP") %>% select(USUBJID, VSORRES)
tabTEMP <- distinct(tabTEMP)
colnames(tabTEMP) <- c("USUBJID", "TEMP") #Fahrenheit

tabRESP <- vs %>% filter(EPOCH=="SCREENING", VSTESTCD=="RESP") %>% select(USUBJID, VSORRES)
tabRESP <- distinct(tabRESP)
colnames(tabRESP) <- c("USUBJID", "RESP") #Breaths per minute

tabPULSEsitting <- vs %>% filter(EPOCH=="SCREENING", VSPOS=="SITTING", VSTESTCD=="PULSE") %>% select(USUBJID, VSORRES)
tabPULSEsitting <- distinct(tabPULSEsitting)
colnames(tabPULSEsitting) <- c("USUBJID", "PULSEsitting") #Beats per minute

tabPULSEstanding <- vs %>% filter(EPOCH=="SCREENING", VSPOS=="STANDING", VSTESTCD=="PULSE") %>% select(USUBJID, VSORRES)
tabPULSEstanding <- distinct(tabPULSEstanding)
colnames(tabPULSEstanding) <- c("USUBJID", "PULSEstanding") #Beats per minute

tabSBPsitting <- vs %>% filter(EPOCH=="SCREENING", VSPOS=="SITTING", VSTESTCD=="SBP") %>% select(USUBJID, VSORRES)
tabSBPsitting <- distinct(tabSBPsitting)
colnames(tabSBPsitting) <- c("USUBJID", "SBPsitting") #MMHG

tabSBPstanding <- vs %>% filter(EPOCH=="SCREENING", VSPOS=="STANDING", VSTESTCD=="SBP") %>% select(USUBJID, VSORRES)
tabSBPstanding <- distinct(tabSBPstanding)
colnames(tabSBPstanding) <- c("USUBJID", "SBPstanding") #MMHG

tabDBPsitting <- vs %>% filter(EPOCH=="SCREENING", VSPOS=="SITTING", VSTESTCD=="DBP") %>% select(USUBJID, VSORRES)
tabDBPsitting <- distinct(tabDBPsitting)
colnames(tabDBPsitting) <- c("USUBJID", "DBPsitting") #MMHG

tabDBPstanding <- vs %>% filter(EPOCH=="SCREENING", VSPOS=="STANDING", VSTESTCD=="DBP") %>% select(USUBJID, VSORRES)
tabDBPstanding <- distinct(tabDBPstanding)
colnames(tabDBPstanding) <- c("USUBJID", "DBPstanding") #MMHG

tabHEIGHT <- vs %>% filter(EPOCH=="SCREENING", VSTESTCD=="HEIGHT") %>% select(USUBJID, VSORRES)
tabHEIGHT <- distinct(tabHEIGHT)
colnames(tabHEIGHT) <- c("USUBJID", "HEIGHT") #Inches

tabWEIGHT <- vs %>% filter(EPOCH=="SCREENING", VSTESTCD=="WEIGHT") %>% select(USUBJID, VSORRES)
tabWEIGHT <- distinct(tabWEIGHT)
colnames(tabWEIGHT) <- c("USUBJID", "WEIGHT") #Pounds

#Creating BMI from Weight and Height tables.
calculate_bmi <- function(dataframe, height_col, weight_col, bmi_col) {
  height_meters <- dataframe[[height_col]] * 0.0254 # convert height to meters
  weight_kg <- dataframe[[weight_col]] * 0.453592 # convert weight to kilograms
  bmi <- weight_kg / (height_meters^2) # calculate BMI
  dataframe[[bmi_col]] <- bmi 
  return(dataframe)
}

tabBMI <- left_join(tabHEIGHT, tabWEIGHT, by="USUBJID")
tabBMI <- calculate_bmi(tabBMI, "HEIGHT", "WEIGHT", "BMI") %>% select("USUBJID", "BMI")

list_df1 = list(tabBMI, tabDBPsitting, tabSBPsitting, tabDBPstanding, tabSBPstanding, tabPULSEsitting, tabPULSEstanding, tabRESP, tabTEMP)
tabVS <- list_df1 %>% reduce(full_join, by='USUBJID')

###SC Table with variables MARITAL EDUCATION EMPLOYEMENT
sc <- read.csv("ascii-data-files nida-ctn-0001-20230318/sc.csv", header = T, dec = ".")

tabMARITAL <- sc %>% filter(SCTESTCD=="MARITAL") %>% select(USUBJID, SCORRES)
tabMARITAL <- distinct(tabMARITAL)
colnames(tabMARITAL) <- c("USUBJID", "MARITAL") #Marital status

tabEDUC <- sc %>% filter(SCTESTCD=="EDUCYRS") %>% select(USUBJID, SCORRES)
tabEDUC <- distinct(tabEDUC)
colnames(tabEDUC) <- c("USUBJID", "EDUCYRS") #Education in years

tabEMPLOY30 <- sc %>% filter(SCTESTCD=="EMPLOY30") %>% select(USUBJID, SCORRES)
tabEMPLOY30 <- distinct(tabEMPLOY30)
colnames(tabEMPLOY30) <- c("USUBJID", "EMPLOY30") #Employement 30 past days

tabEMPLOY3Y <- sc %>% filter(SCTESTCD=="EMPLOY3Y") %>% select(USUBJID, SCORRES)
tabEMPLOY3Y <- distinct(tabEMPLOY3Y)
colnames(tabEMPLOY3Y) <- c("USUBJID", "EMPLOY3Y") #Employement 3 past years

tabEMPLOY <- left_join(tabEMPLOY30, tabEMPLOY3Y, by="USUBJID") #Left join for both type of employement data

list_df2 = list(tabEDUC, tabEMPLOY, tabMARITAL)
tabSC <- list_df2 %>% reduce(full_join, by='USUBJID')


###LB Table with variables METHADONE MORPHINE on baseline
lb <- read.csv("ascii-data-files nida-ctn-0001-20230318/lb.csv", header = T, dec = ".")

#LBMETHOD on Methadone Central lab, because it's the most important one according to documentation.
tabMETHADONE <- lb %>% filter(EPOCH=="SCREENING", LBTESTCD=="METHADON", LBMETHOD=="CENTRAL LAB") %>% select(USUBJID, LBORRES)
tabMETHADONE <- distinct(tabMETHADONE)
colnames(tabMETHADONE) <- c("USUBJID", "METHADONE") #POS or NEG

tabMORPHINE <- lb %>% filter(EPOCH=="SCREENING", LBTESTCD=="MORPHINE") %>% select(USUBJID, LBORRES)
tabMORPHINE <- distinct(tabMORPHINE)
colnames(tabMORPHINE) <- c("USUBJID", "MORPHINE") #POS or NEG

list_df3 = list(tabMETHADONE, tabMORPHINE)
tabLB <- list_df3 %>% reduce(full_join, by='USUBJID')


###QS Table with variables VAS on baseline
qs <- read.csv("ascii-data-files nida-ctn-0001-20230318/qs.csv", header = T, dec = ".")

tabQS <- qs %>% filter(EPOCH=="SCREENING", QSTESTCD=="VAS001") %>% select(USUBJID, QSORRES)
tabQS <- distinct(tabQS)
colnames(tabQS) <- c("USUBJID", "VAS001") #Visual Analogic scale for opiates cravings


#Joining all 5 tables into 01 full table.
list_df4 = list(tabDM, tabSC, tabVS, tabQS, tabLB)
tabALL_01 <- list_df4 %>% reduce(full_join, by='USUBJID')


#### Out patient protocol
### Screening for patients with baseline informations
### Redoing the same thing we did above for the out patient protocol.
dm2 <- read.csv("ascii-data-files nida-ctn-0002-20230318/dm.csv", header = T, dec = ".")
tabDM2 <- dm2 %>% select(USUBJID, ARM, AGE, SEX)

vs2 <- read.csv("ascii-data-files nida-ctn-0002-20230318/vs.csv", header = T, dec = ".")

tabTEMP2 <- vs2 %>% filter(EPOCH=="SCREENING", VSTESTCD=="TEMP") %>% select(USUBJID, VSORRES)
tabTEMP2 <- distinct(tabTEMP2)
colnames(tabTEMP2) <- c("USUBJID", "TEMP") #Fahrenheit
tabTEMP2$TEMP[tabTEMP2$TEMP == 999.9] <- 99.9 #Replace 999.9 value in TEMP.

tabRESP2 <- vs2 %>% filter(EPOCH=="SCREENING", VSTESTCD=="RESP") %>% select(USUBJID, VSORRES)
tabRESP2 <- distinct(tabRESP2)
colnames(tabRESP2) <- c("USUBJID", "RESP") #Breaths per minute

tabPULSEsitting2 <- vs2 %>% filter(EPOCH=="SCREENING", VSPOS=="SITTING", VSTESTCD=="PULSE") %>% select(USUBJID, VSORRES)
tabPULSEsitting2 <- distinct(tabPULSEsitting2)
colnames(tabPULSEsitting2) <- c("USUBJID", "PULSEsitting") #Beats per minute

tabPULSEstanding2 <- vs2 %>% filter(EPOCH=="SCREENING", VSPOS=="STANDING", VSTESTCD=="PULSE") %>% select(USUBJID, VSORRES)
tabPULSEstanding2 <- distinct(tabPULSEstanding2)
colnames(tabPULSEstanding2) <- c("USUBJID", "PULSEstanding") #Beats per minute

tabSBPsitting2 <- vs2 %>% filter(EPOCH=="SCREENING", VSPOS=="SITTING", VSTESTCD=="SBP") %>% select(USUBJID, VSORRES)
tabSBPsitting2 <- distinct(tabSBPsitting2)
colnames(tabSBPsitting2) <- c("USUBJID", "SBPsitting") #MMHG

tabSBPstanding2 <- vs2 %>% filter(EPOCH=="SCREENING", VSPOS=="STANDING", VSTESTCD=="SBP") %>% select(USUBJID, VSORRES)
tabSBPstanding2 <- distinct(tabSBPstanding2)
colnames(tabSBPstanding2) <- c("USUBJID", "SBPstanding") #MMHG

tabDBPsitting2 <- vs2 %>% filter(EPOCH=="SCREENING", VSPOS=="SITTING", VSTESTCD=="DBP") %>% select(USUBJID, VSORRES)
tabDBPsitting2 <- distinct(tabDBPsitting2)
colnames(tabDBPsitting2) <- c("USUBJID", "DBPsitting") #MMHG

tabDBPstanding2 <- vs2 %>% filter(EPOCH=="SCREENING", VSPOS=="STANDING", VSTESTCD=="DBP") %>% select(USUBJID, VSORRES)
tabDBPstanding2 <- distinct(tabDBPstanding2)
colnames(tabDBPstanding2) <- c("USUBJID", "DBPstanding") #MMHG

tabHEIGHT <- vs2 %>% filter(EPOCH=="SCREENING", VSTESTCD=="HEIGHT") %>% select(USUBJID, VSORRES)
tabHEIGHT <- distinct(tabHEIGHT)
colnames(tabHEIGHT) <- c("USUBJID", "HEIGHT") #Inches, weird us metrics...

tabWEIGHT <- vs2 %>% filter(EPOCH=="SCREENING", VSTESTCD=="WEIGHT") %>% select(USUBJID, VSORRES)
tabWEIGHT <- distinct(tabWEIGHT)
colnames(tabWEIGHT) <- c("USUBJID", "WEIGHT") #Pounds, weird us metrics...

tabHEIGHT2 <- vs2 %>% filter(EPOCH=="SCREENING", VSTESTCD=="HEIGHT") %>% select(USUBJID, VSORRES)
tabHEIGHT2 <- distinct(tabHEIGHT2)
colnames(tabHEIGHT2) <- c("USUBJID", "HEIGHT") #Inches, weird us metrics...
tabHEIGHT2$HEIGHT[tabHEIGHT2$HEIGHT == 715] <- 71 #Replace 715 value in HEIGHT.

tabWEIGHT2 <- vs2 %>% filter(EPOCH=="SCREENING", VSTESTCD=="WEIGHT") %>% select(USUBJID, VSORRES)
tabWEIGHT2 <- distinct(tabWEIGHT2)
colnames(tabWEIGHT2) <- c("USUBJID", "WEIGHT") #Pounds, weird us metrics...

#Creating BMI from Weight and Height tables.
calculate_bmi <- function(dataframe, height_col, weight_col, bmi_col) {
  height_meters <- dataframe[[height_col]] * 0.0254 # convert height to meters
  weight_kg <- dataframe[[weight_col]] * 0.453592   # convert weight to kilograms
  bmi <- weight_kg / (height_meters^2)   # calculate BMI
  dataframe[[bmi_col]] <- bmi
  return(dataframe)
}

tabBMI2 <- left_join(tabHEIGHT2, tabWEIGHT2, by="USUBJID")

tabBMI2 <- calculate_bmi(tabBMI2, "HEIGHT", "WEIGHT", "BMI") %>% select("USUBJID", "BMI")

list_df5 = list(tabBMI2, tabDBPstanding2, tabSBPstanding2, tabDBPsitting2, tabSBPsitting2, tabPULSEstanding2, tabPULSEsitting2, tabRESP2, tabTEMP2)
tabVS2 <- list_df5 %>% reduce(full_join, by='USUBJID')

sc2 <- read.csv("ascii-data-files nida-ctn-0002-20230318/sc.csv", header = T, dec = ".")

tabMARITAL2 <- sc2 %>% filter(SCTESTCD=="MARITAL") %>% select(USUBJID, SCORRES)
tabMARITAL2 <- distinct(tabMARITAL2)
colnames(tabMARITAL2) <- c("USUBJID", "MARITAL") #MARITAL STATUS

tabEDUC2 <- sc2 %>% filter(SCTESTCD=="EDUCYRS") %>% select(USUBJID, SCORRES)
tabEDUC2 <- distinct(tabEDUC2)
colnames(tabEDUC2) <- c("USUBJID", "EDUCYRS") #Education in years

tabEMPLOY302 <- sc2 %>% filter(SCTESTCD=="EMPLOY30") %>% select(USUBJID, SCORRES)
tabEMPLOY302 <- distinct(tabEMPLOY302)
colnames(tabEMPLOY302) <- c("USUBJID", "EMPLOY30") #Employement 30 past days

tabEMPLOY3Y2 <- sc2 %>% filter(SCTESTCD=="EMPLOY3Y") %>% select(USUBJID, SCORRES)
tabEMPLOY3Y2 <- distinct(tabEMPLOY3Y2)
colnames(tabEMPLOY3Y2) <- c("USUBJID", "EMPLOY3Y") #Employement 3 past years

tabEMPLOY2 <- left_join(tabEMPLOY302, tabEMPLOY3Y2, by="USUBJID") #Left join for both type of employement data

list_df6 = list(tabEDUC2, tabEMPLOY2, tabMARITAL2)
tabSC2 <- list_df6 %>% reduce(full_join, by='USUBJID')

lb2 <- read.csv("ascii-data-files nida-ctn-0002-20230318/lb.csv", header = T, dec = ".")

tabMETHADONE2 <- lb2 %>% filter(EPOCH=="SCREENING", LBTESTCD=="METHADON", LBMETHOD=="CENTRAL LAB") %>% select(USUBJID, LBORRES)
tabMETHADONE2 <- distinct(tabMETHADONE2)
colnames(tabMETHADONE2) <- c("USUBJID", "METHADONE") #POS or NEG

tabMORPHINE2 <- lb2 %>% filter(EPOCH=="SCREENING", LBTESTCD=="MORPHINE") %>% select(USUBJID, LBORRES)
tabMORPHINE2 <- distinct(tabMORPHINE2)
colnames(tabMORPHINE2) <- c("USUBJID", "MORPHINE") #POS or NEG

list_df7 = list(tabMETHADONE2, tabMORPHINE2)
tabLB2 <- list_df7 %>% reduce(full_join, by='USUBJID')

qs2 <- read.csv("ascii-data-files nida-ctn-0002-20230318/qs.csv", header = T, dec = ".")

tabQS2 <- qs2 %>% filter(EPOCH=="SCREENING", QSTESTCD=="VAS001") %>% select(USUBJID, QSORRES)
tabQS2 <- distinct(tabQS2)
colnames(tabQS2) <- c("USUBJID", "VAS001") #Visual Analogic scale for opiates cravings


#Joining all 5 tables into 01 full table.
list_df8 = list(tabDM2, tabSC2, tabVS2, tabQS2, tabLB2)
tabALL_02 <- list_df8 %>% reduce(full_join, by='USUBJID')

#Joining both full tables of In patient and Out patient protocol, to obtain a OneProcAway (OPA) Table.
TableALL <- rbind(tabALL_02, tabALL_01)
str(TableALL)

TableALL$VAS001 <- as.numeric(TableALL$VAS001) #Changing into a numeric value, this step is crucial otherwise it won't be possible to calculate the mean of this column down below.

## Eliminating duplicated rows and multiple values for one patient.
#Issue was in VS tables as they had different sitting and standing values for one patient (PULSE,SBP,DBP...) we calculated their mean.
#Issue was also in LB tables as they had different tests for METHADONE (some patients got tested twice so they can include them in the study), we took the 2nd test that determined their participation.
#Issue was in QS table for one patient only.
#Data type correction
TableAgg <- TableALL %>%
  group_by(USUBJID) %>%
  mutate(DBPstanding = mean(DBPstanding),
         DBPsitting = mean(DBPsitting),
         SBPstanding = mean(SBPstanding),
         SBPsitting = mean(SBPsitting),
         PULSEstanding = mean(PULSEstanding),
         PULSEsitting = mean(PULSEsitting),
         RESP = mean(RESP),
         TEMP = mean(TEMP),
         VAS001 = mean(VAS001))

TableAgg <- distinct(TableAgg) %>% 
  filter(!(ARM == "")) %>% 
  filter(!(ARM == "SCREEN FAILURE"))

TableAgg$ARM <- as.factor(TableAgg$ARM)
TableAgg$SEX <- as.factor(TableAgg$SEX)
TableAgg$EDUCYRS <- as.numeric(TableAgg$EDUCYRS)
TableAgg$EMPLOY30 <- as.factor(TableAgg$EMPLOY30)
TableAgg$EMPLOY3Y <- as.factor(TableAgg$EMPLOY3Y)
TableAgg$MARITAL <- as.factor(TableAgg$MARITAL)
TableAgg$METHADONE <- as.factor(TableAgg$METHADONE)
TableAgg$MORPHINE <- as.factor(TableAgg$MORPHINE)
TableAgg <- as.data.frame(TableAgg) 

str(TableAgg)

#Creating a table with ARM Strata
TableONE <- CreateTableOne(vars = c("ARM","AGE","SEX","EDUCYRS","EMPLOY30","EMPLOY3Y","MARITAL","BMI","DBPstanding","SBPstanding","DBPsitting","SBPsitting","PULSEstanding","PULSEsitting","RESP","TEMP","VAS001","METHADONE","MORPHINE"), 
                           data = TableAgg, 
                           strata = "ARM")
print(TableONE, test = FALSE)

#Creating a SKIMR table with ARM strata
TableSKIMR <- TableAgg %>% group_by(ARM) %>% skim() %>%  print(include_summary = FALSE)

#Both tables are modified in the Rmarkdown files because of their properties and use in the HTML context.

##################################################################
#Patients that did not provide the urine on D13 or D14 test were removed.
perprotocol1 <- lb %>% filter(VISIT %in% c("STUDY DAY 13","STUDY DAY 14")) %>% 
  filter((LBSTAT != "NOT DONE")) %>%  
  group_by(USUBJID)
length(unique(perprotocol1$USUBJID))

perprotocol2 <- lb2 %>% filter(VISIT %in% c("STUDY DAY 13","STUDY DAY 14")) %>% 
  filter((LBSTAT != "NOT DONE")) 
length(unique(perprotocol2$USUBJID))

#Merged with dm dataframe, that contains the ARM information
TablePROT1 <- left_join(perprotocol1, dm, by='USUBJID') %>% 
  select("USUBJID" , "EPOCH.x" ,   "LBSEQ"  ,  "LBTESTCD", "LBTEST"  , "LBCAT"   , "LBORRES" ,
        "LBSTRESC", "LBSTRESN", "LBSTRESU", "LBNRIND", "LBSTAT" ,  "LBREASND", "LBSPEC" ,  "LBMETHOD" , "VISIT.x"  ,  "VISITNUM.x" ,  "LBDY", "ARM")  
length(unique(TablePROT1$USUBJID)) #70 Number of patients on D13 or D14 on Protocol 1

TablePROT2 <- left_join(perprotocol2, dm2, by='USUBJID') %>% 
  select("USUBJID" , "EPOCH.x" ,   "LBSEQ"  ,  "LBTESTCD", "LBTEST"  , "LBCAT"   , "LBORRES" ,
         "LBSTRESC", "LBSTRESN", "LBSTRESU", "LBNRIND", "LBSTAT" ,  "LBREASND", "LBSPEC" ,  "LBMETHOD" , "VISIT.x"  ,  "VISITNUM.x" ,  "LBDY", "ARM" ) 
length(unique(TablePROT2$USUBJID)) #107 Number of patients on D13 or D14 on Protocol 2

#Protocole 1 : D1 113 patients / D14 70 patients
TablePROT1BupNx <- TablePROT1 %>% filter(ARM=="BUPRENORPHINE/NALOXONE")
length(unique(TablePROT1BupNx$USUBJID)) #62 Patients on Bup/Nx

TablePROT1Clon <- TablePROT1 %>% filter(ARM=="CLONIDINE")
length(unique(TablePROT1Clon$USUBJID)) #8 Patients on Clon

#Protocole 2. : D1 230 patients / D14 108 patients
TablePROT2BupNx <- TablePROT2 %>% filter(ARM=="BUPRENORPHINE/NALOXONE")
length(unique(TablePROT2BupNx$USUBJID)) #88 Patients on Bup/Nx

TablePROT2Clon <- TablePROT2 %>% filter(ARM=="CLONIDINE")
length(unique(TablePROT2Clon$USUBJID)) #20 Patients on Clon

####################################################################
```

In case of any questions or suggestions to improve the code, do not hesitate to contact me at yahia.rafik.m@gmail.com

Thank you!
