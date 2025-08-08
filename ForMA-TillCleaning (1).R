# Loading Data ####
#setwd("D:/Projects/GitHub/MortgageDA/code")
rawData <- read.csv("D:/Projects/GitHub/_Datasets/MortgageDA-hmda_data/2023_public_lar.csv")
rdBkp <- rawData
colnames(rawData)
dim(rawData)
str(rawData)

# Cleaning Data ####
require(dplyr)
## Type Conversions ####
# Categorical Columns with numerical data 
catColumns <- c('lei',
                'state_code',
                'conforming_loan_limit',
                'derived_loan_product_type',
                'derived_dwelling_category',
                'derived_ethnicity',
                'derived_race',
                'derived_sex',
                'action_taken',
                'purchaser_type',
                'preapproval',
                'loan_type',
                'loan_purpose',
                'lien_status',
                'reverse_mortgage',
                'open_end_line_of_credit',
                'business_or_commercial_purpose',
                'hoepa_status',
                'negative_amortization',
                'interest_only_payment',
                'balloon_payment',
                'other_nonamortizing_features',
                'construction_method',
                'occupancy_type',
                'manufactured_home_secured_property_type',
                'manufactured_home_land_property_interest',
                'total_units',
                'debt_to_income_ratio',
                'applicant_credit_score_type',
                'co_applicant_credit_score_type',
                'applicant_ethnicity_1',
                'applicant_ethnicity_2',
                'applicant_ethnicity_3',
                'applicant_ethnicity_4',
                'applicant_ethnicity_5',
                'co_applicant_ethnicity_1',
                'co_applicant_ethnicity_2',
                'co_applicant_ethnicity_3',
                'co_applicant_ethnicity_4',
                'co_applicant_ethnicity_5',
                'applicant_ethnicity_observed',
                'co_applicant_ethnicity_observed',
                'applicant_race_1',
                'applicant_race_2',
                'applicant_race_3',
                'applicant_race_4',
                'applicant_race_5',
                'co_applicant_race_1',
                'co_applicant_race_2',
                'co_applicant_race_3',
                'co_applicant_race_4',
                'co_applicant_race_5',
                'applicant_race_observed',
                'co_applicant_race_observed',
                'applicant_sex',
                'co_applicant_sex',
                'applicant_sex_observed',
                'co_applicant_sex_observed',
                'applicant_age',
                'co_applicant_age',
                'applicant_age_above_62',
                'co_applicant_age_above_62',
                'submission_of_application',
                'initially_payable_to_institution',
                'aus_1',
                'aus_2',
                'aus_3',
                'aus_4',
                'aus_5',
                'denial_reason_1',
                'denial_reason_2',
                'denial_reason_3',
                'denial_reason_4')
# Census Data attached at the end
censusColumns <- c('tract_population',
                   'tract_minority_population_percent',
                   'ffiec_msa_md_median_family_income',
                   'tract_to_msa_income_percentage',
                   'tract_owner_occupied_units',
                   'tract_one_to_four_family_homes',
                   'tract_median_age_of_housing_units')
#Columns with Integer Data
intColumns <- c('activity_year',
                'loan_amount',
                'loan_term',
                'prepayment_penalty_term',
                'intro_rate_period',
                'property_value',
                'multifamily_affordable_units',
                'tract_population',
                'ffiec_msa_md_median_family_income',
                'tract_owner_occupied_units',
                'tract_one_to_four_family_homes',
                'tract_median_age_of_housing_units')
#Colummn with Numerical Data (fractions)
numColumns <- c('combined_loan_to_value_ratio',
                'interest_rate','rate_spread',
                'total_loan_costs',
                'total_points_and_fees',
                'origination_charges',
                'discount_points',
                'lender_credits',
                'income',
                'tract_minority_population_percent',
                'tract_to_msa_income_percentage')

# Conversion 
## 1. Categorical Data
#cData = Converted  Dataset
cData <- rawData %>% 
  mutate_at(catColumns, as.factor)
str(cData)
## 2. Integer Data
cData <- cData %>%
  mutate_at(intColumns, as.integer)
## 3. Numeric Data
cData <- cData %>%
  mutate_at(numColumns, as.numeric)

# Not Applicable v/s Null
preNullCount <- colSums(is.na(rawData))
postNullCount <- colSums(is.na(cData))

for (i in 1:length(preNullCount)){
  if(preNullCount[i] != postNullCount[i]){
    print(paste(names(preNullCount[i]),
                "Pre:",preNullCount[i],
                "Post:",postNullCount[i]))
  }
}
rm(i)
# For Non-Categorical Data: 
# We have 'Not Applicable' as a value along with numeric and integer data. When
# converting, this 'Not Applicable' will be converted to NA (Not Applicable)
# which is similar to Null. Basically, text data (not sure whether significant 
# or not) is getting converted to Null Value

##Renaming Columns####
#Columns
newColNames <- c("ActivityYear",
                 "LEI",
                 "DerivedMsaMd",
                 "State",
                 "County",
                 "CensusTract",
                 "ConformingLoanLimit",
                 "DerivedLoanProductType",
                 "DerivedDwellingCategory",
                 "DerivedEthnicity",
                 "DerivedRace",
                 "DerivedSex",
                 "ActionTaken",
                 "PurchaserType",
                 "Preapproval",
                 "LoanType",
                 "LoanPurpose",
                 "LienStatus",
                 "ReverseMortgage",
                 "OpenEndLineOfCredit",
                 "BusinessOrCommercialPurpose",
                 "LoanAmount",
                 "CombinedLoanToValueRatio",
                 "InterestRate",
                 "RateSpread",
                 "HoepaStatus",
                 "TotalLoanCosts",
                 "TotalPoints",
                 "OriginationCharges",
                 "DiscountPoints",
                 "LenderCredits",
                 "LoanTerm",
                 "PrepaymentPenaltyTerm",
                 "IntroRatePeriod",
                 "NegativeAmortization",
                 "InterestOnlyPayment",
                 "BalloonPayment",
                 "OtherNonamortizingFeatures",
                 "PropertyValue",
                 "ConstructionMethod",
                 "OccupancyType",
                 "MHSecuredPropertyType",
                 "MHLandPropertyInterest",
                 "TotalUnits",
                 "MultifamilyAffordableUnits",
                 "Income",
                 "DTRatio",
                 "CreditScoreType",
                 "CoCreditScoreType",
                 "Ethnicity1",
                 "Ethnicity2",
                 "Ethnicity3",
                 "Ethnicity4",
                 "Ethnicity5",
                 "CoEthnicity1",
                 "CoEthnicity2",
                 "CoEthnicity3",
                 "CoEthnicity4",
                 "CoEthnicity5",
                 "EthnicityObv",
                 "CoEthnicityObv",
                 "Race1",
                 "Race2",
                 "Race3",
                 "Race4",
                 "Race5",
                 "CoRace1",
                 "CoRace2",
                 "CoRace3",
                 "CoRace4",
                 "CoRace5",
                 "RaceObv",
                 "CoRaceObv",
                 "Sex",
                 "CoSex",
                 "SexObv",
                 "CoSexObv",
                 "Age",
                 "CoAge",
                 "AgeAbove62",
                 "CoAgeAbove62",
                 "SubmissionOfApplication",
                 "InitiallyPayableToInstitution",
                 "Aus1",
                 "Aus2",
                 "Aus3",
                 "Aus4",
                 "Aus5",
                 "DenialReason1",
                 "DenialReason2",
                 "DenialReason3",
                 "DenialReason4",
                 "TractPopulation",
                 "TractMinorityPopulationPercent",
                 "FFIECMsaMdMedianFamilyIncome",
                 "TractToMsaIncomePercentage",
                 "TractOwnerOccupiedUnits",
                 "TractOneToFourFamilyHomes",
                 "TractMedianAgeOfHousingUnits")

#rData = Replaced/Renamed Dataset
rData <- cData
names(rData) <- newColNames
colnames(rData)

##Filtering Columns ####
fData <- rData %>% select(c(
  "State",
  "ActionTaken",
  "PurchaserType",
  "LoanType",
  "LoanPurpose",
  "LoanAmount",
  "OccupancyType",
  "DTRatio",
  "Ethnicity1",
  "CoEthnicity1",
  "Race1",
  "CoRace1",
  "Sex",
  "CoSex",
  "Age",
  "CoAge",
  "TractPopulation",
  "TractMinorityPopulationPercent",
  "FFIECMsaMdMedianFamilyIncome",
  "TractToMsaIncomePercentage",
  "TractOwnerOccupiedUnits",
  "TractOneToFourFamilyHomes",
  "TractMedianAgeOfHousingUnits"))

# Categorical Fields

## Ethnicity1 (Not Focusing on Ethnicity2, 3, 4 and 5)
table(fData$Ethnicity1)
sum(is.na(fData$Ethnicity1))
levels(fData$Ethnicity1)
# Adding Missing Level
levels(fData$Ethnicity1) <- c(levels(fData$Ethnicity1),
                              "MissingData")
# Replacing NA with Missing
fData$Ethnicity1[is.na(fData$Ethnicity1)] <- "MissingData"

## Race1 (Not Focusing on Race2, 3, 4 and 5)
table(fData$Race1)
sum(is.na(fData$Race1))
levels(fData$Race1)
# Adding Missing Level
levels(fData$Race1) <- c(levels(fData$Race1),"MissingData")
# Replacing NA with Missing
fData$Race1[is.na(fData$Race1)] <- "MissingData"

#Repeating Same for CoRace1 and CoEthnicity1
levels(fData$CoEthnicity1) <- c(levels(fData$CoEthnicity1),
                                "MissingData")
fData$CoEthnicity1[is.na(fData$CoEthnicity1)] <- "MissingData"
levels(fData$CoRace1) <- c(levels(fData$CoRace1),
                           "MissingData")
fData$CoRace1[is.na(fData$CoRace1)] <- "MissingData"

fDataBkp <- fData #Backup With MissingData Field 

#Renaming Fields
levels(fData$PurchaserType) <- c("NotApplicable","FNMA","GNMA","FHLMC","AGM",
                                 "Private","Commercial","AffiliateInstitute",
                                 "Other","CreditUnion","LIC")

levels(fData$LoanType) <- c("Conventional","FHA","VA","RHAandFSA")

levels(fData$LoanPurpose) <- c("HomePurchare","HomeImprovement","Other",
                               "NotApplicable","Refinance","CashOut")
levels(fData$OccupancyType) <- c("Principal","Secondary","Investment")
levels(fData$Ethnicity1) <- c("HorL","NotHL","NotProvided","NotApplicable",
                              "Mexican","PuertoRican","Cuban","OtherHL",
                              "MissingData")
#Don't have 8in Website Schema, Removing Fields with CoEthnicity=8
fData <- fData %>%
  filter(CoEthnicity1 != 8) %>%
  droplevels()
levels(fData$CoEthnicity1) <- c("HorL","NotHL","NotProvided","NotApplicable",
                                "NoCoApplicant","Mexican","PuertoRican","Cuban",
                                "OtherHL","MissingData")

levels(fData$Race1) <- c("AIorAN","Asian","BlackAA","NHorOPI","White",
                         "NotProvided","NotApplicable","IndianAsian","Chinese",
                         "Filipino","Japanese","Korean","Vietnamese",
                         "OtherAsian","NH","GuamanianChamorro","Samoan",
                         "OtherPI","MissingData")
levels(fData$CoRace1) <- c("AIorAN","Asian","BlackAA","NHorOPI","White",
                           "NotProvided","NotApplicable","NoCoApplicant",
                           "IndianAsian","Chinese","Filipino","Japanese",
                           "Korean","Vietnamese","OtherAsian","NH",
                           "GuamanianChamorro","Samoan","OtherPI","MissingData")
levels(fData$Sex) <- c("Male","Female","NotProvided","NotApplicable",
                       "SelectedBoth")
levels(fData$CoSex) <- c("Male","Female","NotProvided","NotApplicable",
                         "NoCoApplicant","SelectedBoth")
#fData <- fData %>% 
#  filter(!is.na(DTRatio)) %>%
#  droplevels()

fDataRBkp <- fData #Backup with Renamed Fields with Acutal Text

#Filtering Categories ## MAIN DATASET With 3 and 6 in ActionTaken 
fcData <- fData %>%
  filter(ActionTaken == 3 | ActionTaken ==  6,!is.na(LoanAmount)) %>% 
  #Denied and Purchased Loan & Non Null Loan Amount
  droplevels()

#Target is ActionTaken
require(forcats)

#Filtering Columns - (Merge and Reduce Count):
fcData$PurchaserType <- fct_collapse(fcData$PurchaserType,
                                     Other = c("Private","Commercial",
                                               "AffiliateInstitute","Other",
                                               "CreditUnion","LIC"))
fcData$LoanPurpose <- fct_collapse(fcData$LoanPurpose,
                                   Refinance = c("Refinance","CashOut"))
fcData$Ethnicity1 <- fct_collapse(fcData$Ethnicity1,
                                  HorL = c("Mexican","PuertoRican","Cuban",
                                           "OtherHL"))
fcData$CoEthnicity1 <- fct_collapse(fcData$CoEthnicity1,
                                  HorL = c("Mexican","PuertoRican","Cuban",
                                           "OtherHL"))
fcData$Race1 <- fct_collapse(fcData$Race1,
                             Asian = c("IndianAsian","Chinese","Filipino",
                                       "Japanese","Korean","Vietnamese",
                                       "OtherAsian"),
                             NHorOPI = c("NH","GuamanianChamorro","Samoan",
                                         "OtherPI"))
fcData$CoRace1 <- fct_collapse(fcData$CoRace1,
                             Asian = c("IndianAsian","Chinese","Filipino",
                                       "Japanese","Korean","Vietnamese",
                                       "OtherAsian"),
                             NHorOPI = c("NH","GuamanianChamorro","Samoan",
                                         "OtherPI"))
#Replacing 8888 with NotProvided instead of Deleting the Rows as lot of data is 
#Lost if 8888 is filtered out
fcData$Age <- fct_collapse(fcData$Age,
                           "25-44" = c("25-34","35-44"),
                           "45-64" = c("45-54","55-64"),
                           ">65" = c("65-74",">74"),
                           NotProvided = "8888")

#Repeat Above and Replace 9999 with NoCoApplicant as most of the data (~6.1 B)
#has NoCoApplicant in CoRace1/CoEthnicity1 for 9999
fcData$CoAge <- fct_collapse(fcData$CoAge,
                           "25-44" = c("25-34","35-44"),
                           "45-64" = c("45-54","55-64"),
                           ">65" = c("65-74",">74"),
                           NotProvided = "8888",
                           NoCoApplicant = "9999")

# Replacing NA with NotApplicable (To Match Website which has NA)
levels(fcData$DTRatio) <- c(levels(fcData$DTRatio),"NotApplicable")
fcData$DTRatio[is.na(fcData$DTRatio)] <- "NotApplicable"
#Reassigning New categories for Debt-to-Income Ratio based to Research (GPT)
fcData$DTRatio <- fct_collapse(fcData$DTRatio,
                               Low = c("<20%"),
                               Moderate = c("20%-<30%"),
                               Acceptable = c("30%-<36%","36"),
                               Managable = c("37","38","39","40","41","42","43"),
                               MaxAcceptable = c("44","45"),
                               Elevated = c("46","47","48","49"),
                               HighRisk = c("50%-60%",">60%"))

#Preprocess ####
#One-Hot Encoding and Stuff
require(caret)
# Exploratory Data Analysis ####
#Loan Amount



