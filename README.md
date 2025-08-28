# Clinical Trial Safety and Efficacy Analysis in R  

## 📄 Abstract  
This project demonstrates the application of **statistical programming in R** to analyze clinical trial datasets (SDTM/ADaM). The analysis explores **adverse events (AEs), exposure-adjusted incidence rates (EAIR), survival outcomes, laboratory/vital sign changes, concomitant medication use, and baseline medical history** across treatment arms.  

Findings suggest **no major safety imbalances**, but trends in **AE timing and rescue medication usage** highlight areas for further monitoring. The project serves as a reproducible framework for clinical data analysis in drug development.  

---**Repository Structure ** 
├── data/ # Example/derived datasets (raw data not shared due to confidentiality)
├── scripts/ # R analysis scripts
│ ├── 01_data_preprocessing.R
│ ├── 02_ae_analysis.R
│ ├── 03_survival_analysis.R
│ ├── 04_lab_vitals_analysis.R
│ ├── 05_cm_analysis.R
│ ├── 06_mh_analysis.R
├── outputs/ # Plots, tables, and model outputs
└── README.md # Documentation & publication-style report

## 🔬 Methods  

### Data Sources  
- Derived from **clinical trial ADaM/SDTM datasets**  
- Key domains analyzed: **AE, DM, EX, LB, VS, CM, MH**  

### Statistical Analysis  
- **Adverse Events (AEs):** Incidence compared across arms (Fisher’s Exact Test, multiplicity adjustment)  
- **Exposure Adjustment:** EAIR = (Number of AEs ÷ Total exposure days) × 365  
- **Time-to-Event:** Kaplan–Meier curves and Cox proportional hazards models  
- **Laboratory & Vital Signs:** Change from baseline (ANOVA/repeated measures); Grade ≥3 toxicities flagged  
- **Concomitant Medications (CM):** Usage summarized; class-level heatmaps plotted  
- **Medical History (MH):** Baseline balance across treatment arms  

### Software  
- **R** (tidyverse, dplyr, ggplot2, survival, broom)  
- Visualizations created with **ggplot2** and survival analysis functions  

---

## 📊 Results  

### 1. Adverse Events (AEs)  
- Incidence of treatment-emergent AEs comparable across arms  
- After multiplicity adjustment, **no AE reached statistical significance**  

- [Adverse Events (AEs) in AIRIS-101-1 Treatment Arm](path/to/AE_in_AIRIS-101_1_TRTARM.pdf) 

---

### 2. Exposure-Adjusted Incidence Rate (EAIR)  
- EAIR revealed **one arm with disproportionately higher AE burden relative to exposure**  

- [Average Exposure Days by Treatment Arm (EAIR Analysis)](path/to/Average_Exposure_days_by_TRTARM.pdf)  

---

### 3. Time-to-First AE (Survival Analysis)  
- Kaplan–Meier curves showed **earlier AE onset in one arm**  
- Cox model HR suggested increased AE risk, though not statistically significant  

- [Kaplan–Meier Plot: Time to First AE (Survival Analysis)](path/to/Kaplan-Meier_Plot_Time_to_First_AE.pdf)  

---

### 4. Laboratory & Vital Signs  
- A few **Grade 3+ abnormalities** observed, but not clustered in a single arm  
- Vital signs showed **no clinically significant shifts**  


---

### 5. Concomitant Medications (CM)  
- CM use consistent across arms  
- Slightly higher **rescue medication use** in one arm  

- [Top 5 Most Taken Drugs per Treatment Arm (Concomitant Medications)](path/to/Top5_Most_Taken_Drugs_per_TRTARM.pdf)  
- [Drug Usage Count per Treatment Arm (Concomitant Medications)](path/to/Drug_Usage_Count_per_TRTARM.pdf)  

---

### 6. Medical History (MH)  
- Balanced distribution of baseline medical history across arms  
- Confirms **successful randomization**  


-[Adverse Events (AEs) in AIRIS-101-1 Treatment Arm](https://github.com/YourUsername/AIRIS-101-Clinical-Study-Analysis-Safety-and-Efficacy-Evaluation-in-Advanced-Solid-Tumors/blob/main/AE_in_AIRIS-101_1_TRTARM.pdf)

---


- [Top 10 Medical History (MH) Observed](AIRIS-101-Clinical-Study-Analysis-Safety/Top%2010%20MH%20observed.pdf)  
- [Adverse Events (AEs) in AIRIS-101-1 Treatment Arm](AIRIS-101-Clinical-Study-Analysis-Safety/AE%20in%20AIRIS-101%201%20TRTARM.pdf)  
- [Kaplan–Meier Plot: Time to First AE (Survival Analysis)](AIRIS-101-Clinical-Study-Analysis-Safety/Kaplan-Meier%20Plot%20Time%20to%20First%20AE.pdf)  
- [Average Exposure Days by Treatment Arm (EAIR Analysis)](AIRIS-101-Clinical-Study-Analysis-Safety/Average%20Exposure%20days%20by%20TRTARM.pdf)  
- [Top 5 Most Taken Drugs per Treatment Arm (Concomitant Medications)](AIRIS-101-Clinical-Study-Analysis-Safety/Top%205%20most%20taken%20drugs%20per%20treatment%20Arm.pdf)  
- [Drug Usage Count per Treatment Arm (Concomitant Medications)](AIRIS-101-Clinical-Study-Analysis-Safety/Drug%20Usage%20Count%20per%20treatment%20arm.pdf)

- 
## 🧾 Discussion  
- **Multiplicity adjustment** reduced apparent AE differences → avoids false positives  
- **Exposure-adjusted metrics** uncovered hidden differences not visible from raw counts  
- **Survival analysis** suggested earlier AE onset in one arm, though inconclusive  
- **Lab/vital signs stable**, supporting safety  
- **Rescue medication trends** may warrant further monitoring  

---

## ✅ Conclusion  
- No major safety imbalances across arms  
- Exposure adjustment is critical for fair AE comparison  
- Trends in AE timing & rescue medication use highlight tolerability signals  
- Framework offers a **reproducible R-based pipeline** for clinical trial safety analysis  

## Future Work  
-Planned to work on with the SDTM,ADAM,TLF analysis with the current dataset as an extensive analysis
- Extend analysis to **PK/PD datasets** to explore exposure–response relationships  
- Apply **Bayesian hierarchical models** for robust inference in small sample sizes  
- Develop an **interactive Shiny dashboard** for clinical trial monitoring  
- Automate **data pipelines** for multi-study integration 

