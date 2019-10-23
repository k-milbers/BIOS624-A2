
data <- read.csv("HRTdata2019.csv")
attach(data)


### DATA CLEANING 

#----Colon Cancer Indicator----
#Classify cases as 1, controls as 0

cCASE <- ifelse(CASE == 'Control', 0, 1)


#----Blackhole effect----
# *Note there is likely a cleaner way to do this step. 

#STEP 1: For any year effected by the black hole (BH), if they were exposed in the year prior to BH or the year after to the BH, then code them as 1 for the missing year 

#Code for year 3:
BH_EXP5_03 <- ifelse(BH03 =='1', 
               ifelse(EXP5_02 =='1' | EXP5_04 =='1', 1,0))

#For all other years 1 to 15 (Note years 16-21 for the black hole are not included as the exposure data only goes to year 15)
BH_EXP5_01 <- ifelse(BH01 =='1', 
                     ifelse(EXP5_02 =='1', 1,0))

BH_EXP5_02 <- ifelse(BH02 =='1', 
                     ifelse(EXP5_01 =='1' | EXP5_03 =='1', 1,0))

BH_EXP5_04 <- ifelse(BH04 =='1', 
                     ifelse(EXP5_03 =='1' | EXP5_05 =='1', 1,0))

BH_EXP5_05 <- ifelse(BH05 =='1', 
                     ifelse(EXP5_04 =='1' | EXP5_06 =='1', 1,0))

BH_EXP5_06 <- ifelse(BH06 =='1', 
                     ifelse(EXP5_05 =='1' | EXP5_07 =='1', 1,0))

BH_EXP5_07 <- ifelse(BH07 =='1', 
                     ifelse(EXP5_06 =='1' | EXP5_08 =='1', 1,0))

BH_EXP5_08 <- ifelse(BH08 =='1', 
                     ifelse(EXP5_07 =='1' | EXP5_09 =='1', 1,0))

BH_EXP5_09 <- ifelse(BH09 =='1', 
                     ifelse(EXP5_08 =='1' | EXP5_10 =='1', 1,0))

BH_EXP5_10 <- ifelse(BH10 =='1', 
                     ifelse(EXP5_09 =='1' | EXP5_11 =='1', 1,0))

BH_EXP5_11 <- ifelse(BH11 =='1', 
                     ifelse(EXP5_10 =='1' | EXP5_12 =='1', 1,0))

BH_EXP5_12 <- ifelse(BH12 =='1', 
                     ifelse(EXP5_11 =='1' | EXP5_13 =='1', 1,0))

BH_EXP5_13 <- ifelse(BH13 =='1', 
                     ifelse(EXP5_12 =='1' | EXP5_14 =='1', 1,0))

BH_EXP5_14 <- ifelse(BH14 =='1', 
                     ifelse(EXP5_13 =='1' | EXP5_15 =='1', 1,0))

BH_EXP5_15 <- ifelse(BH15 =='1', 
                     ifelse(EXP5_14 =='1', 1,0))

#Step 2: Create new columns for the exposure years - if they were exposed in year x or if they were classified as exposed for year x due to the black hole

cEXP5_01 <- ifelse(EXP5_01 == '1' | BH_EXP5_01 == '1', 1, 0)
cEXP5_02 <- ifelse(EXP5_02 == '1' | BH_EXP5_02 == '1', 1, 0)
cEXP5_03 <- ifelse(EXP5_03 == '1' | BH_EXP5_03 == '1', 1, 0)
cEXP5_04 <- ifelse(EXP5_04 == '1' | BH_EXP5_04 == '1', 1, 0)
cEXP5_05 <- ifelse(EXP5_05 == '1' | BH_EXP5_05 == '1', 1, 0)
cEXP5_06 <- ifelse(EXP5_06 == '1' | BH_EXP5_06 == '1', 1, 0)
cEXP5_07 <- ifelse(EXP5_07 == '1' | BH_EXP5_07 == '1', 1, 0)
cEXP5_08 <- ifelse(EXP5_08 == '1' | BH_EXP5_08 == '1', 1, 0)
cEXP5_09 <- ifelse(EXP5_09 == '1' | BH_EXP5_09 == '1', 1, 0)
cEXP5_10 <- ifelse(EXP5_10 == '1' | BH_EXP5_10 == '1', 1, 0)
cEXP5_11 <- ifelse(EXP5_11 == '1' | BH_EXP5_11 == '1', 1, 0)
cEXP5_12 <- ifelse(EXP5_12 == '1' | BH_EXP5_12 == '1', 1, 0)
cEXP5_13 <- ifelse(EXP5_13 == '1' | BH_EXP5_13 == '1', 1, 0)
cEXP5_14 <- ifelse(EXP5_14 == '1' | BH_EXP5_14 == '1', 1, 0)
cEXP5_15 <- ifelse(EXP5_15 == '1' | BH_EXP5_15 == '1', 1, 0)


#----Reclassify exposure groups----

#Group <5 years, excluding year 1 and 2. If they are exposed in either year 3 or 4 then they are categorized exposed. 
#Cat 1 is exposed, 0 is not exposed

cEXP5_Cat_less <- ifelse(cEXP5_03=='1',1,
                        ifelse(cEXP5_04=='1',1,0))

#Group >=5 years. If they are exposed in any year 5 to 15 then they are categorized exposed. 
#Cat 1 is exposed, 0 is not exposed

cEXP5_Cat_greater <- ifelse(cEXP5_05=='1',1,
                           ifelse(cEXP5_06=='1',1,
                                  ifelse(cEXP5_07=='1',1,
                                         ifelse(cEXP5_08=='1',1,
                                                ifelse(cEXP5_09=='1',1,
                                                       ifelse(cEXP5_10=='1',1,
                                                              ifelse(cEXP5_11=='1',1,
                                                                     ifelse(cEXP5_12=='1',1,
                                                                            ifelse(cEXP5_13=='1',1,
                                                                                   ifelse(cEXP5_14=='1',1,
                                                                                          ifelse(cEXP5_15=='1',1,0)))))))))))
#Group no exposure.
#Cat 1 is not exposed (never exposed), 0 is exposed (ever exposed)
cEXP5_Cat_Non <- ifelse(cEXP5_Cat_less=='1', 0,
                       ifelse(cEXP5_Cat_less=='1',0, 1))



#----Compressing the drug data----
#Y/N to having ever taken X drug are split into 3 categories: Y1-5, Y6-10, Y11-15. In this step we compress Y6-10 and Y11-15 into one.

#*Note: I realize that this cleaning doesn't distinguish between: Non or Unknown, did we want to distinguish?

#NSAIDS
cNS_EXP1 <- NS_EXP1
cNS_EXP2 <- ifelse(NS_EXP2 == '1' | NS_EXP3 =='1', 1, 0)

#CVD drugs
cNUM56_1 <- NUM56_1
cNUM56_2 <- ifelse(NUM56_2 == "1 or more" | NUM56_3 =="1 or more", 1, 0)

#CNS
cNUM57_1 <- NUM57_1
cNUM57_2 <- ifelse(NUM57_2 == "1 or more" | NUM57_3 =="1 or more", 1, 0)

#Other hormones
cNUM61_1 <- NUM61_1
cNUM61_2 <- ifelse(NUM61_2 == "1 or more" | NUM61_3 =="1 or more", 1, 0)

#Vitamins
cNUM63_1 <- NUM63_1
cNUM63_2 <- ifelse(NUM63_2 == "1 or more" | NUM63_3 =="1 or more", 1, 0)

#Oral Contraception
cOC1<- OC1
cOC2 <- ifelse(OC2 == '1' | OC3 =='1', 1, 0)


#Sigmonoscopy
cSIG2EVR <- SIG2EVR

#Doctor Visits 
cTOTDOC <- TOTDOC


#----Cleaned dataset----

cdata <- cbind(STUD_ID, CASE_ID, cCASE, COLON, cEXP5_Cat_Non, cEXP5_Cat_less, cEXP5_Cat_greater, cNS_EXP1, cNS_EXP2, 
               cNUM56_1, cNUM56_2, cNUM57_1, cNUM57_2, cNUM61_1, cNUM61_2, cNUM63_1, cNUM63_2, cOC1, cOC2, cSIG2EVR, cTOTDOC)

cdata <- as.data.frame(cdata)
detach(data)


#----Conditional logistic regression----


