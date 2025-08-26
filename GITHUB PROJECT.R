
setwd('C:/Users/E1486728/Desktop/Intermediate R')

#Library installation and setting
install.packages("readxl")

library(readxl)
#importing excel file into R
read_excel('Project for analysis.xlsx')
ADAM_Dataset<- read_excel('Project for analysis.xlsx')
# R does not support spaces in variable name and read_excel doesnt support header=TRUE format

#Library loading
library(dplyr)
library(ggplot2)
library(survival)
library(tidyr)

install.packages("lubricate")
library(lubricate)

# Convert date columns to Date type
date_columns <- c('AESTDTC', 'AEENDTC', 'TRTSDT', 'TRTEDT', 'CMSTDTC', 'CMENDTC')
for (col in date_columns) {
if (col %in% names(data)) {
data[[col]] <- as.Date(data[[col]], format='%Y-%m-%d')
}
}

str(ADAM_Dataset)

#Data Visualization
#listing columns that wanted to be convert and clean
date_columns <- c("AESTDTC", "AEENDTC","TRTEDT", "TRTSDT", "CMSTDTC", "CMENDTC")


#remove all NA's from all columns 
#check how many NA's are in ADAM_dataset
colSums(is.na(ADAM_Dataset))
#remove all NA's only in selected columns
ADAM_Dataset <- ADAM_Dataset %>% drop_na(TRTSDT,TRTEDT,AESTDTC,AEENDTC,CMSTDTC,CMENDTC)
 

View(ADAM_Dataset)
#converting into dates and remove NA's
ADAM_Dataset <- ADAM_Dataset %>%
  mutate(across(all_of(date_columns), ~ as.Date(as.numeric(.), origin = "1899-12-30")))
colSums(is.na(ADAM_Dataset[date_columns])) 
View(ADAM_Dataset)
#creating new dataset with cleaned date values
Cancer_analysis <- ADAM_Dataset[, date_columns]

View(Cancer_analysis)

#Create derived columns
Cancer_analysis <- ADAM_Dataset%>%
  mutate(
    AE_start=as.Date(AESTDTC),
    treatment_start=as.Date(TRTSDT),
    treatment_end=as.Date(TRTEDT),
    time_to_ae=as.numeric(difftime(AE_start, treatment_end, units="days")),
    CM_start=as.Date(CMSTDTC),
    CM_end=as.Date(CMENDTC),
    CM_duration=as.numeric(difftime(CM_end, CM_start, units="days")),
    exposure_days=as.numeric(difftime(as.Date(TRTEDT),as.Date(TRTSDT),units="days"))
  )

View(ADAM_Dataset)
# ANALYSIS1: Adverse Event Incidence

ae_summary <- Cancer_analysis %>%
  filter(SAFFL=="Y") %>%
  group_by(ACTARM,AETERM) %>%
  summarise(N=n_distinct(USUBJID),.groups='drop')%>%
  arrange(desc(N))

#INTERPRETATION: Based on the analysis, patients in the treatment arm AIRIS-101 1 experienced a higher incidence of adverse events such as elevated AST, increased bilirubin levels, and severe fatigue.
#In contrast, patients in the AIRIS-101 2A arm most commonly experienced constipation, diarrhea, and nausea following treatment.
#visualization using DOTPLOT:
library(ggplot2)

ae_summary <- ae_summary %>%
  mutate(highlight= ifelse(N==max(N),"max","other"))


ggplot(ae_summary, aes(x = reorder(AETERM, -N), y = N, fill=highlight)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = N), vjust = -0.3, size = 3.5) +
  scale_fill_manual(values=c("max"="lightgreen",other="orange"))+
  labs(
    title = "Adverse Events in AIRIS-101 1 Treatment Arm",
    x = "Adverse Event Term",
    y = "Number of Patients"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 8),  # Rotate labels
    plot.title = element_text(hjust = 0.5)  # Center the title
  )
#Interpretation: The value higlighted in green shows patients with maximum adverse events while the other one corresponds to less adverse event.

#ANALYSIS2: Time to First AE--

Cancer_analysis <- Cancer_analysis %>%
  mutate(AE_event = ifelse(!is.na(AESTDTC), 1, 0))

surv_obj <- Surv(time=Cancer_analysis$time_to_ae,event= Cancer_analysis$AE_event)

library(survival)
km_fit<- survfit(surv_obj~ ACTARM,data= Cancer_analysis)

plot(km_fit, col = 1:3, main = "Time to First AE", xlab = "Days", ylab = "Survival Probability")
legend("bottomleft", legend = levels(factor(Cancer_analysis$ACTARM)), col = 1:3, lty = 1)
exists("km_fit")

#to find how many ACTARM contributes to the graph
levels(factor(Cancer_analysis$ACTARM))

# to check how many subjects are contributing to each ACTARM group in your survival analysis 
table(Cancer_analysis$ACTARM, !is.na(Cancer_analysis$time_to_ae))

#INTERPRETATION: 
#The survival propability decreases over time as patients experience their first AE
#AIRIS 101 (black)-- patients experiences AE's very soon after treatment
#AIRIS 101 1-A (Green)-- AE occured more slowly and spread out over time. Indicates moderate safety profile
#AIRIS 101 2-A (Red)-- patients remained AE free for longer. Fewer and slower AE [Potentially safer profile]

#ANALYSIS3: Lab Toxicity Grade
tox_summary<-ADAM_Dataset %>%
  filter(!is.na(ATOXGRL)) %>%
  group_by(ACTARM,ATOXGRL) %>%
  summarise(n=n(),.groups='drop_last') %>%
  group_by(ACTARM) %>%
  mutate(percent = n / sum(n) * 100) %>%
  ungroup()
  #mutate(percent= n/sum(n)*100)
  #mutate(percent = n / sum(n[ACTARM == ACTARM]) * 100)

#INTERPRETATION: From results, AIRIS 101-1A may require close monitoring due to the presence of moderate toxicity while AIRIS 101 1 and AIRIS 101 2A shows mild toxicity as this falls under Grade-1 toxicity.

#ANALYSIS4: CONCOMITANT MEDICATION
#to identify the maximum no of patients who have taken each CMDECOD drug

#creating a new variable naming CM_duration from the available CMSTDTC and CMENDTC


ADAM_Dataset <- ADAM_Dataset %>%
  mutate(
    CMSTDTC= as.Date(CMSTDTC),
    CMENDTC= as.Date(CMENDTC),
    CM_duration= as.numeric(CMENDTC-CMSTDTC)
  )

cm_summary<- ADAM_Dataset %>%
  filter(!is.na(CMDECOD)) %>%
  group_by(CMDECOD,ACTARM) %>%
  summarise(avg_duration=mean(CM_duration,na.rm=TRUE),count=n(),.groups='drop')

# Now, find the ACTARM with maximum count for each drug
cm_max_usage <- cm_summary %>%
  group_by(ACTARM,CMDECOD) %>%
  filter(count==max(count)) %>%
  arrange(desc(count))


#INTERPRETATION:The most frequently administered concomitant medications alongside the study drug during the clinical trial were NA and Palonosetron Hydrochloride, each taken by 5 patients. 
#These medications were primarily recorded under the AIRIS-101 2A treatment group.

#VIZUALIZATION- HEAT MAP CORRELATION:

library(ggplot2)
library(dplyr)

# Heatmap of CMDECOD (drug) vs ACTARM (treatment arm)
cm_summary %>%
  filter(!is.na(CMDECOD), !is.na(ACTARM)) %>%
  ggplot(aes(x = ACTARM, y = reorder(CMDECOD, count), fill = count)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "lightblue", high = "red") +
  labs(title = "Drug Usage Count per Treatment Arm",
       x = "Treatment Arm",
       y = "Drug (CMDECOD)",
       fill = "Count") +
  facet_wrap(~ ACTARM, scales = "free_x") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 5),
        strip.text = element_text(size = 10, face = "bold"))

#top 5 drugs per ARM

library(dplyr)

top5_drugs <- cm_summary %>%
  group_by(ACTARM) %>%
  arrange(desc(count)) %>%
  slice_head(n = 5) %>%
  ungroup()

#Plot heatmap for top 5 drugs
library(ggplot2)
ggplot(top5_drugs, aes(x = ACTARM, y = reorder(CMDECOD, count), fill = count)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "lightblue", mid = "white", high = "red", 
                       midpoint = median(top5_drugs$count, na.rm = TRUE)) +
  labs(title = "Top 5 Most Used Drugs per Treatment Arm",
       x = "Treatment Arm",
       y = "Drug (CMDECOD)",
       fill = "Count") +
  facet_wrap(~ ACTARM, scales = "free_x") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 6),
        axis.text.x = element_text(angle = 45, hjust = 1),
        strip.text = element_text(size = 11, face = "bold"))

#INTERPRETATION:AIRIS-101 2A stands out with higher frequency and broader drug usage, suggesting the need for intensive supportive care.

#ANALYSIS5: CHANGE FROM BASELINE CALCULATION

#renaming column name:

library(dplyr)
ADAM_Dataset <- ADAM_Dataset %>%
  rename(AVAL = AVAL...22)

#removing screen failure as that has insufficient data

ADAM_Dataset <- ADAM_Dataset %>%
  filter(ACTARM!= "Screen failure")

#calculate change in baseline for each parameter

parameter_of_interest <- c("Systolic Blood Pressure (mmHg)", 
                           "Diastolic Blood Pressure (mmHg)",
                           "Heart Rate (beats/min)",
                           "Height (cm)",
                           "Temperature (C)",
                           "Weight (kg)",
                           "Body Surface Area (m^2)")


# 1. Extract baseline value (CYCLE 1 DAY 1)
baseline_df <- ADAM_Dataset %>%
  filter(PARAM %in% parameter_of_interest, AVISIT == "CYCLE 1 DAY 1") %>%
  group_by(USUBJID, ACTARM, PARAM) %>%
  summarise(Baseline = max(AVAL), .groups = "drop")  # or first(AVAL) if only one per subject

# 2. Extract post-baseline values (excluding baseline)
post_df <- ADAM_Dataset %>%
  filter(PARAM %in% parameter_of_interest, AVISIT != "CYCLE 1 DAY 1") %>%
  group_by(USUBJID, ACTARM, PARAM) %>%
  summarise(Max_Post = max(AVAL, na.rm = TRUE), .groups = "drop")

# 3. Join and compute PCHG
change_df <- baseline_df %>%
  inner_join(post_df, by = c("USUBJID", "ACTARM", "PARAM")) %>%
  mutate(
    Baseline= as.numeric(Baseline),
    Max_Post= as.numeric(Max_Post),
    Change = Max_Post - Baseline,
    PCHG = (Change / Baseline) * 100
  )


#INTERPRETATION:
# From the interpretation, after performing clinical trial there is fluctuation in blood pressure
#i.e the diastolic pressure has been increased to certain extent which has to be monitored on regular basis as that
#could flag stage 1 hypertension


#BSA has been largely dropped while undergoing treatment arm AIRIS 101-2A that strongly suggest
#significant weight loss due to disease progression or toxicity.


# --- ANALYSIS 7: Exposure-Adjusted Incidence Rates ---
#ea-ir useful when treatment duration varies between subjects or arm
#standardizes the AE rate per year
#calculate the avg_exposure and sd_exposure


# calculating the exposure_days using TRTSDT and TRTEDT
ADAM_Dataset <- ADAM_Dataset %>%
  mutate(
    exposure_days= as.numeric(difftime(as.Date(TRTEDT), as.Date(TRTSDT), units="days"))+1
  )
  
exposure_summary <- ADAM_Dataset %>%
  group_by(ACTARM) %>%
  summarise(
    total_ae=n(),
    total_exposure= sum(exposure_days, na.rm=TRUE),
    ea_ir=(total_ae/ total_exposure)*365,
    average_exposure= mean(exposure_days, na.rm= TRUE),
    sd_exposure= sd(exposure_days, na.rm= TRUE),
    .groups='drop'
  )

#ea-ir= exposure-adjusted incident rate 
#visualization using plot bar chart with error bars(+-)

ggplot(exposure_summary,aes(x= ACTARM,y=average_exposure)) +
  geom_bar(stat="identity",fill="steelblue")+
  geom_errorbar(aes(ymin= average_exposure- sd_exposure,ymax= average_exposure + sd_exposure),
                width= 0.2,color="black")+
  geom_text(aes(label= round(average_exposure, 1)),vjust= -0.5,size= 3.5)+
  labs(title= "Average Exposure Days by Treatment Arm",
       x= "Treatment Arm", y= "Average Exposure Days")+
  theme_minimal() +
  theme(axis.text.x= element_text(angle=45,hjust=1))

#Compare exposure days to adverse event by ARM

AE_with_exposuredays <- ADAM_Dataset %>%
  group_by(ACTARM) %>%
  summarise(n_AE = n_distinct(AEDECOD), n_subjects = n_distinct(USUBJID))

#INTERPRETATION: AIRIS-101 1A appears to have the highest AE diversity per subject.
#Now compute EAIR to adjust for how long subjects were exposed to treatment — this gives a fair comparison across treatment arm

AE_perARM <- ADAM_Dataset %>%
  filter(!is.na(AEDECOD)) %>%
  group_by(ACTARM) %>%
  summarise(
    total_AEs = n()
  )

eair_summary <- left_join(AE_perARM, exposure_summary, by="ACTARM") %>%
  mutate(
    EAIR= (total_AEs / total_exposure) * 365
  )

#INTERPRETATION: So based on the test results, AIRIS 101 1 appears worst in raw EAIR, but due to 
#very short exposure, the rate is inflated. : AIRIS-101 1 may not be inherently more toxic — it’s a data artifact.
#EAIR is more stable and fair for AIRIS 101 1A and 2A due to longer followup.

#Correlation between exposure and AE's occured

#Hypothesis Statement:
#Do longer exposures result in more AE's
#merge exposure days and AE dataset
merged_subject_data <- merge(
  AE_with_exposuredays, exposure_summary,
  by= "ACTARM"
)


cor.test(merged_subject_data$total_exposure, merged_subject_data$n_AE, method= "spearman")

#From the spearman correlation test, we dont have strong evidence of the relationship between exposure time and AE since the sample size is relatively very small (i.e n=4)

#VISUALIZATION PLOT FOR COMPARING EAIR ACROSS TRT ARM
#still needs to work on the visualization part of the above 

#HYPOTHESIS TESTINGS:
#1: CHI SQUARE TEST- For comparing AE incidence between treatment arms

#H₀: The distribution of patients experiencing specific AEs is the same across arms.
#H₁: The distribution differs by treatment arm.


ae_contigency <- table(ADAM_Dataset$ACTARM, ADAM_Dataset$AEDECOD)

fisher.test(ae_contigency, simulate.p.value = TRUE, B = 1e5)

# There is a statistically significant difference in AE distribution between treatment arms (p=0.00056)
#2.ANOVA TEST: Compare mean exposure days, lab values or change from baseline across treatment arm

#H₀: Mean exposure days are equal across arms.
#H₁: At least one arm differs

aov_result <- aov(exposure_days ~ ACTARM, data= ADAM_Dataset)
summary(aov_result)

#Based on the results, the ANOVA test shows a p value of < 2 × 10⁻¹⁶, we reject null hypothesis.
#So, the mean exposure days differ significantly between treatment arms.
#Also the large F-value shows that the variation btw trt arm is much greater than the variation within each arm.

#Performing Tukey HSD to identify which exact pairs of treatment arm have significantly different mean exposure days.

aov_result <- aov(exposure_days ~ ACTARM, data = ADAM_Dataset)

#Tukey honest significant difference test
tukey_result <- TukeyHSD(aov_result)

#view the results
print(tukey_result)

# Based on the results obtained the overall ranking of exposure days as follows:
#AIRIS 101 1A > AIRIS 101 2A > AIRIS 101 1


#This can be corelated with EAIR, i.e arms with much shorter exposure (AIRIS 101 1) will tend to have inflated EAIR.

#3. REPEATED MEASURES ANOVA/ MIXED EFFECTS MODEL
#Analyse change in vitals/labs over time across treatment arms
#converting AVAL in ADAM_Dataset to numeric since the response must be numeric 

#Simplify AVISIT 
ADAM_Dataset$AVISIT<- factor(ADAM_Dataset$AVISIT)

#Drop unused levels in AVISIT and ACTARM
ADAM_Dataset$AVISIT <- droplevels(ADAM_Dataset$AVISIT)
ADAM_Dataset$ACTARM <- droplevels(ADAM_Dataset$ACTARM)

install.packages("lme4")
install.packages("lmerTest")
install.packages("Matrix")

library(lme4)
library(lmerTest)

# Ensure numeric response
ADAM_Dataset$AVAL <- as.numeric(ADAM_Dataset$AVAL)

# Clean factor levels
ADAM_Dataset$AVISIT <- droplevels(factor(ADAM_Dataset$AVISIT))
ADAM_Dataset$ACTARM <- droplevels(factor(ADAM_Dataset$ACTARM))

# Fit mixed model

ADAM_Dataset <- ADAM_Dataset %>%
  mutate(
    AVISIT_CAT = case_when(
      grepl("^CYCLE", AVISIT, ignore.case = TRUE) ~ "Scheduled",
      AVISIT %in% c("END OF TREATMENT", "SAFETY FOLLOWUP", "SCREENING") ~ "Special",
      grepl("^UNSCHEDULED", AVISIT, ignore.case = TRUE) ~ "Unscheduled",
      TRUE ~ "Other"
    ),
    AVISIT_CAT = factor(AVISIT_CAT, levels = c("Scheduled", "Special", "Unscheduled", "Other"))
  )

mixed_model <- lmer(AVAL ~ AVISIT_CAT * ACTARM + (1 | USUBJID), data = ADAM_Dataset)
summary(mixed_model)

#INTERPRETATION: 


#4.KAPLAN-MEIER LOG-RANK TEST
#Statistically compares survival curves across arm

#Ensure both columns are Date type
Cancer_analysis <- Cancer_analysis %>%
  mutate(
    AESTDTC = as.Date(AESTDTC),
    TRTEDT  = as.Date(TRTEDT)
  )

#Calculate time_to_AE and AE_event
time_to_ae_data<- Cancer_analysis %>%
  group_by(USUBJID)%>%
  summarise(
    first_AE_date=min(AESTDTC, na.rm=TRUE),
    TRTEDT= first(TRTEDT)) %>%
  mutate(
    AE_event = ifelse(!is.na(first_AE_date), 1, 0),
    time_to_ae = case_when(
      AE_event == 1 ~ as.numeric(first_AE_date - TRTEDT),  # days to AE
      AE_event == 0 ~ NA_real_                            # no AE
    )
  )

survdiff(Surv(time_to_ae, AE_event) ~ ACTARM, data = Cancer_analysis)
#INTERPRETATION: AIRIS 101 1 experienced AE's earlier with shorter exposure time when compared to other arms.
#AIRIS-101 1A had slower time to first AE while AIRIS 101 2A behaved roughly as expected under the null hypothesis

#VISUALIZATION PLOT :
library(survival)
install.packages("survminer")
library(survminer)

# Fit Kaplan-Meier survival curves
km_fit <- survfit(Surv(time_to_ae, AE_event) ~ ACTARM, data = Cancer_analysis)

# Plot
ggsurvplot(
  km_fit,
  data = Cancer_analysis,
  risk.table = TRUE,           # Show number at risk table
  pval = TRUE,                 # Show log-rank p-value
  conf.int = TRUE,              # Show confidence intervals
  xlab = "Time to First AE (days)",
  ylab = "AE-free Probability",
  surv.median.line = "hv",     # Show median lines
  legend.title = "Treatment Arm",
  legend.labs = levels(Cancer_analysis$ACTARM),
  palette = c("#E64B35", "#4DBBD5", "#00A087"), # Custom colors
  ggtheme = theme_minimal()
)

#5.CORELATION AND REGRESSION ANALYSIS:
#Check for corelation between : Total AE's , Total exposure, Average Exposure, EAIR

correlation_data <- eair_summary %>%
  select(total_AEs, total_exposure, average_exposure, EAIR)

cor(correlation_data, use= "complete.obs")

#INTERPRETATION: 
#Total AEs show a moderate positive correlation with total and average exposure (r ≈ 0.59), meaning more exposure generally leads to more AEs. 
#EAIR is strongly negatively correlated with exposure metrics (r ≈ -0.96) because longer exposure lowers the rate when adjusted for time. 
#Total and average exposure are almost perfectly correlated (r ≈ 1.00), indicating redundancy between these two variables.

#6.LINEAR REGRESSION:
#Does average_exposure predict total AE's or EAIR?

# Linear regression of total AEs on average exposure
model1 <- lm(total_AEs ~ average_exposure, data= eair_summary)
summary(model1)

#INTERPRETATION: There is no statistically reliable evidence that average exposure predicts the no of total AEs
## Linear regression of EAIR on average exposure
model2<- lm(EAIR ~ average_exposure, data=eair_summary)
summary(model2)

#INTERPRETATION:The slope value (-0.97) suggests a negative trend which could be due to the fact that high EAIR may occur in patients with short exposure times and a few rapid AEs, but more data are needed to confirm.

#7.BASELINE MEDICAL HISTORY (MHTERM) BALANCE
#Check whether baseline risk differs by arm (important covariate)

#Top 10 MH term; imbalance by ACTARM
mh_top <- Cancer_analysis |>
  dplyr::filter(!is.na(MHTERM)) |>
  dplyr::count(MHTERM,sort=TRUE) |> dplyr::slice_head(n=10) |> dplyr::pull(MHTERM)

mh_bal <- Cancer_analysis
dplyr::filter(MHTERM %in%mh_top) |>
  dplyr::mutate(hasMH=1) |>
  tidyr::pivot_wider(id_cols=c(USUBJID,ACTARM),
                     names_from=MHTERM,values_from=hasMH,values_fill=0)

#Test one MH term vs arm (Fisher for sparse)
xt <- with(Cancer_analysis |> dplyr::filter(MHTERM%in%mh_top),
           table(ACTARM,MHTERM))
fisher.test(xt,simulate.p.value = TRUE,B=1e4)

#INTERPRETATION: Fisher exact test results indicating that the distribution of the selected medical history
#terms differs significantly across treatment arms.This suggests a potential imbalance in baseline comorbidities or medical history profiles between groups, which could influence safety or efficacy outcomes 
#and may need adjustment or stratification in further analyses.


#Stratification, covariate adjustment, and subgroup analysis ensure your findings are robust, fair, and clinically meaningful.

#STRATIFICATION done using variable sex
#convert ADURN into numeric

Cancer_analysis$ADURN <- as.numeric(Cancer_analysis$ADURN)
#create event indicator: 1 if ADURN >0 (event happened), 0 otherwise

library(survival)
Cancer_analysis$AE_event <- ifelse(!is.na(Cancer_analysis$ADURN) & Cancer_analysis$ADURN > 0, 1, 0)
cox_model <-coxph(Surv(time_to_ae, AE_event) ~ ACTARM+ strata(SEX), data=Cancer_analysis)
summary(cox_model)

#Co-variate adjustment in regression
# Adjust for age and baseline score
Cancer_analysis_clean <- Cancer_analysis %>%
  filter(!is.na(EAIR), !is.nan(EAIR), !is.infinite(EAIR))

model_adj <- lm(EAIR ~ ACTARM + AGE + BASE, data = Cancer_analysis_clean)
summary(model_adj)

#EXPLANATORY ANALYSIS
#SUB-GRP ANALYSIS
# Run analysis for a subgroup (e.g., females only)
female_data <- subset(Cancer_analysis, SEX == "F")
cox_female <- coxph(Surv(time_to_ae, AE_event) ~ ACTARM, data = female_data)
summary(cox_female)

#8. FREQUENCY BAR CHART FOR TOP 10 MHTERM
Cancer_analysis <- Cancer_analysis %>% drop_na(MHTERM)

mh_freq <- Cancer_analysis %>%
  count(MHTERM, sort=TRUE)
#Top 10 most common medical histories
top10_mh <- mh_freq %>% slice_max(n,n= 10)
#Bar chart
# Bar chart
ggplot(top10_mh, aes(x = reorder(MHTERM, n), y = n)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(title = "Top 10 Medical Histories (MHTERM)",
       x = "Medical History Term",
       y = "Frequency")
  
  
#Categorize MHTERM into body-system level groups
Cancer_analysis <- Cancer_analysis %>%
  mutate(MH_Category = case_when(
    grepl("Cardiac|Heart|Hypertension|Atrial|Pacemaker|Coronary", MHTERM, ignore.case = TRUE) ~ "Cardiac",
    grepl("Diarrhea|Nausea|Vomiting|Gastro|GERD|Constipation", MHTERM, ignore.case = TRUE) ~ "Gastrointestinal",
    grepl("Asthma|COPD|Respiratory|Pneumonia", MHTERM, ignore.case = TRUE) ~ "Respiratory",
    grepl("Seizure|Neuropathy|Headache|Stroke", MHTERM, ignore.case = TRUE) ~ "Neurological",
    grepl("Arthritis|Back pain|Bone|Musculoskeletal", MHTERM, ignore.case = TRUE) ~ "Musculoskeletal",
    TRUE ~ "Other"
  ))


#Clean ID's in both datasets
subj <- subj %>%
  mutate(USUBJID = gsub("\\s+", "", USUBJID)) 

Cancer_analysis <- Cancer_analysis %>%
  mutate(USUBJID = gsub("\\s+", "", USUBJID))

library(dplyr)
library(tidyr)
#Create indicator variable for all MH reported:
mh_flags <- Cancer_analysis %>%
  mutate(flag = 1) %>%
  distinct(USUBJID, MH_Category, .keep_all = TRUE) %>%
  pivot_wider(
    names_from = MH_Category,
    values_from = flag,
    values_fill = 0,
    names_prefix = "has_"
  )


# Join them into subj
subj <- subj %>%
  mutate(
    has_CardiacMH = coalesce(has_CardiacMH, has_CardiacMH.x, has_CardiacMH.y),
    has_GI_MH     = coalesce(has_GI_MH, has_GI_MH.x, has_GI_MH.y),
    has_RespMH    = coalesce(has_RespMH.x, has_RespMH.y),
    has_NeuroMH   = coalesce(has_NeuroMH.x, has_NeuroMH.y)
  ) %>%
  select(-ends_with(".x"), -ends_with(".y"))

names(subj)

#PROGNOSTIC EFFECT OF MH ON AE OCCURENCE(ADJUSTED):
#run model for each MH category
model_cardiac <- glm(anyAE ~ ACTARM + has_CardiacMH + offset(log(pmax(exposure,1))),
                     family = poisson, data = subj)

model_gi <- glm(anyAE ~ ACTARM + has_GI_MH + offset(log(pmax(exposure,1))),
                family = poisson, data = subj)

model_resp <- glm(anyAE ~ ACTARM + has_RespMH + offset(log(pmax(exposure,1))),
                  family = poisson, data = subj)

model_neuro <- glm(anyAE ~ ACTARM + has_NeuroMH + offset(log(pmax(exposure,1))),
                   family = poisson, data = subj)


summary(model_cardiac)
summary(model_gi)
summary(model_resp)
summary(model_neuro)  
  
  
#INTERPRETATION: Patients with a history of gastrointestinal conditions showed a significantly lower risk of adverse events (RR ≈ 0.22, p = 0.036), 
#suggesting a potential protective effect when compared to other medical history of other patients 


#Toxicity Grades/ Types(ATOXGRL, AESOC, AEDECOD)
#Severity profile by arm; which arm has more Grade ≥3?

# Ordinal model (if grades coded ordered)
library(MASS)
tox <- Cancer_analysis |> dplyr::filter(!is.na(ATOXGRL))
tox$ATOXGRL <- ordered(tox$ATOXGRL, levels=c("Grade 0","Grade 1","Grade 2","Grade 3","Grade 4","Grade 5"))
fit_ord <- MASS::polr(ATOXGRL ~ ACTARM, data=tox, Hess=TRUE)
summary(fit_ord)

#INTERPRETATION: There is no strong evidence that treatment arms differ in overall distribution of 
#toxicity grades
# Grade ≥3 exposure-adjusted
tox_flag <- tox |> dplyr::mutate(gr3p = as.integer(ATOXGRL %in% paste("Grade",3:5)))
subj_gr3p <- tox_flag |>
  dplyr::group_by(USUBJID, ACTARM) |>
  dplyr::summarise(gr3p_any = as.integer(any(gr3p==1)), .groups='drop') |>
  dplyr::left_join(subj[,c("USUBJID","exposure")], by="USUBJID")

glm(gr3p_any ~ ACTARM + offset(log(pmax(exposure,1))),
    family = binomial(), data = subj_gr3p)

#INTERPRETATION: Grade-3 toxicity occured more often in AIRIS 101 2-A than compared with other trt arm / reference.


#Dose Modifications and AE signals 
#Are dose holds/reductions linked to specific AEs
# Example: probability of any dose modification given AE type/grade
# (replace DOSEMOD flag with your variable if available)
ae_mod <- Cancer_analysis |>
  dplyr::mutate(gr3p = as.integer(ATOXGRL %in% paste("Grade",3:5))) |>
  dplyr::group_by(USUBJID, ACTARM) |>
  dplyr::summarise(gr3p_any = as.integer(any(gr3p==1)),
                   mod_any  = as.integer(any(AEACN %in% c("DRUG INTERRUPTED","DOSE REDUCED","DRUG WITHDRAWN"))),
                   .groups='drop')

glm(mod_any ~ ACTARM + gr3p_any, family=binomial(), data=ae_mod)


#INTERPRETATION:Although Grade ≥3 toxicities were observed in the AIRIS-101 treatment arms, no corresponding dose modifications were recorded. This suggests that, 
#in this dataset, high-grade toxicities did not consistently lead to dose adjustments, likely reflecting small sample size or protocol/data capture limitations


#MULTIPLICITY AND SENSITIVITY:
#Adjust p-values across many AE types
#Sensitivity: repeat key models excluding Screen Failure, or using per-subject first AE only
install.packages("epitools")
library(dplyr)
library(tidyr)
library(epitools)

# --- AE subject counts by treatment arm ---CONTIGENCY TABLE
ae_counts <- Cancer_analysis%>%
  group_by(AEDECOD, ACTARM) %>%
  summarise(n_subj = n_distinct(USUBJID), .groups = "drop") %>%
  pivot_wider(
    names_from  = ACTARM,
    values_from = n_subj,
    values_fill = 0
  )

#Inspect the data to find the single arm events
head(ae_counts)

# Loop directly over AE terms
pvals <- sapply(unique(Cancer_analysis$AEDECOD), function(ae) {
  # Build contingency table: subjects with vs without this AE, across arms
  tab <- Cancer_analysis %>%
    mutate(has_ae = ifelse(AEDECOD == ae, 1, 0)) %>%
    group_by(ACTARM, has_ae) %>%
    summarise(n = n_distinct(USUBJID), .groups = "drop") %>%
    tidyr::pivot_wider(names_from = has_ae, values_from = n, values_fill = 0) %>%
    as.data.frame()
  
  # Drop ACTARM column → 2-column table (no AE vs AE)
  m <- as.matrix(tab[,-1])
  
  # Fisher’s exact test if valid
  if(ncol(m) == 2 && nrow(m) >= 2) {
    tryCatch(fisher.test(m)$p.value, error = function(e) NA_real_)
  } else {
    NA_real_
  }
})

# Multiple testing correction
pvals_adj <- p.adjust(pvals, method = "BH")

results <- data.frame(
  AEDECOD = names(pvals),
  pval_raw = pvals,
  pval_adj = pvals_adj
)

#Viewing the results obtained
head(results, 10)

#INTERPRETATION: pval_raw= raw fisher exact test p-value or difference across treatment arms.
#pval_adj → adjusted p-value after multiple testing correction (Benjamini–Hochberg FDR).
#None of the AE event occured is significant with treatment arms as p value is < 0.05
#No significant differences in adverse event frequencies were observed between treatment arms. 
#After adjustment for multiplicity using the Benjamini–Hochberg procedure, all adjusted p-values remained >0.70. 
#Results were consistent when assessed with Fisher’s exact test, indicating robustness of findings across sensitivity analyses.









  
