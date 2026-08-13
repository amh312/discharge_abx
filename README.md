This repository houses the code for the academic paper **"Screening hospital discharge letters with scalable natural language processing efficiently detects inappropriate antibiotic use"**, for the purpose of peer review and subsequent open-sourcing.

If you use this code please cite this repository.

***Instructions for use:***

The electronic healthcare record source data can be obtained from PhysioNet at https://physionet.org/content/mimiciv/2.2/ and https://physionet.org/content/mimic-iv-note/2.2/ once the terms of access are met. The csv filenames used in this code match the following default filenames that can be downloaded from the site: "admissions.csv", "patients.csv"*, and "discharge.csv".

PhysioNet MIMIC-IV citations:

*Johnson, A., Pollard, T., Horng, S., Celi, L. A., & Mark, R. (2023). MIMIC-IV-Note: Deidentified free-text clinical notes (version 2.2). PhysioNet. RRID:SCR_007345. https://doi.org/10.13026/1n74-ne17*

*Johnson, A., Bulgarelli, L., Pollard, T., Horng, S., Celi, L. A., & Mark, R. (2023). MIMIC-IV (version 2.2). PhysioNet. https://doi.org/10.13026/6mm1-ek67.*

*Johnson, A.E.W., Bulgarelli, L., Shen, L. et al. MIMIC-IV, a freely accessible electronic health record dataset. Sci Data 10, 1 (2023). https://doi.org/10.1038/s41597-022-01899-x*

*Goldberger, A., Amaral, L., Glass, L., Hausdorff, J., Ivanov, P. C., Mark, R., ... & Stanley, H. E. (2000). PhysioBank, PhysioToolkit, and PhysioNet: Components of a new research resource for complex physiologic signals. Circulation [Online]. 101 (23), pp. e215–e220.*

This code was written and run using *R* version 4.3.2 and Python version 3.11.15 on a MacBook Pro running macOS Tahoe version 26.5.2 with an Apple M5 processor, 32GB random-access memory and 10 cores. Total run time for all scripts was 23 hours, 22 minutes and 59.2 seconds (a breakdown of timings for all scripts can be found in *script_times.csv*). Metal Performance Shaders were used to run the code on the Apple M5 GPU—model scripts (those starting with *BERT* below) will revert to CPU if MPS is unavailable, but if other GPUs are available this preference can be amended in the script.

***Reproducing the study***

This code will exactly reproduce the clinical prediction model results of the study and descriptive data. It will, however, not necessarily sample the same discharge letters that were used for the clinician review exercise. 

Before running the code, the data should be saved into a secure local directory, along with the *aware_classification.csv* file that can be downloaded from this repository. The required package versions are included in the *packages.txt* file within this directory.

To reproduce the analyses, scripts must be run in this order:  

   1. **lang_packages&setup.R**
   2. **lang_disc_cleaning.R**
   3. **BERT_discharges.py**
   4. **BERT_access.py**
   5. **lang_performance.R**
   6. **lang_access_performance.R**
   7. **lang_descriptive.R**
   8. **BERT_stability.py**
   9. **BERT_timesens.py**
   10. **lang_sensitivity.R**
   
To then run **lang_questionnaire.R**, **BERT_SHAP.py**, **BERT_SHAP_Access.py** and **lang_SHAP.R**, questionnaires (produced as *q_1.csv* to *q_6.csv* and *ac_q_1.csv* to *ac_q_6.csv* by the *lang_performance.R* and *lang_access_performance.R* scripts) first need to be undertaken by six suitably qualified participants, each of whom must first have gone through the MIMIC-IV data access steps mandated by PhysioNet. Participants should answer 'Yes' or 'No' in the 'answer' column then save as *r_1.csv* to *r_6.csv* and *ac_r_1.csv* to *ac_r_6.csv* corresponding to their question csv number. The required format for these files is demonstrated in the *r_example.csv* file in this repository.

Question wording for the overall model was *"You are on an antibiotic stewardship team, tasked with identifying inappropriate discharge antibiotic prescriptions. You will see 25 discharge letters - for each case, if you think discharging the patient on an antibiotic is very likely to be INappropriate based on the information provided, please select 'Yes'. Otherwise, please select 'No'. Please note you are only reviewing the appropriateness of the decision WHETHER to discharge on an antibiotic, NOT the choice of antibiotic agent."*.

Question wording for the Access model was *"Your antimicrobial stewardship team is now tasked with reviewing choices of discharge antimicrobial agents for their appropriateness. You will review another 25 discharge letters - for each case, answer 'yes' if you think that discharge on a broad-spectrum antibiotic treatment is very likely to be INappropriate, i.e., that either that a narrower-spectrum (WHO Access) antibiotic would be more appropriate, and/or that no antibiotic therapy is indicated. Otherwise, please select 'no'."*.

***Testing the code***

To test the functionality of the code for the main analysis without requiring download of the real PhysioNet datasets, download all csv files in this repository, and **remove the "_test" suffix from any csv file names where it is present**. All scripts apart from those that are specifically tailored to the engineering and descriptive characteristics of the PhysioNet datasets can then be run in the following order:

   1. **lang_packages&setup.R**
   2. **BERT_discharges.py**
   3. **BERT_access.py**
   4. **lang_performance.R**
   5. **lang_access_performance.R**
   6. **lang_questionnaire.R**
   7. **BERT_SHAP.py**
   8. **BERT_SHAP_Access.py**
   9. **lang_SHAP.R**
   10. **BERT_stability.py**
   11. **BERT_timesens.py**
   12. **lang_sensitivity.R**

The test CSVs contain collections of random words, so the results will be random but can be used to demonstrate the code's functionality.

***Reviewing the code***

R code follows the general structure of function definitions, then data uploads, then code that embeds the functions. Python code follows the same general structure, except that packages are imported at the beginning of each script (for R scripts this is done once in *lang_packages&setup.R*). Sections are denoted with a double hash, subsections with a triple hash, and sections within functions with a single hash. R code was written during a transition between R Studio and Positron IDEs, resulting in a mixture of "%>%" and "|>" to denote pipes for tidyverse functions.
