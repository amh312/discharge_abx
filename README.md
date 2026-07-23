This repository houses the code for the academic paper **"Screening hospital discharge letters with scalable natural language processing efficiently detects inappropriate antibiotic use"**, for the purpose of peer review and subsequent open-sourcing.

If you use this code please cite this repository.

***Instructions for use:***

The electronic healthcare record source data can be obtained from PhysioNet at https://physionet.org/content/mimiciv/2.2/ and https://physionet.org/content/mimic-iv-note/2.2/ once the terms of access are met. The csv filenames used in this code match the following default filenames that can be downloaded from the site: "admissions.csv", "patients.csv"*, and "discharge.csv".

PhysioNet MIMIC-IV citations:

*Johnson, A., Pollard, T., Horng, S., Celi, L. A., & Mark, R. (2023). MIMIC-IV-Note: Deidentified free-text clinical notes (version 2.2). PhysioNet. RRID:SCR_007345. https://doi.org/10.13026/1n74-ne17*

*Johnson, A., Bulgarelli, L., Pollard, T., Horng, S., Celi, L. A., & Mark, R. (2023). MIMIC-IV (version 2.2). PhysioNet. https://doi.org/10.13026/6mm1-ek67.*

*Johnson, A.E.W., Bulgarelli, L., Shen, L. et al. MIMIC-IV, a freely accessible electronic health record dataset. Sci Data 10, 1 (2023). https://doi.org/10.1038/s41597-022-01899-x*

*Goldberger, A., Amaral, L., Glass, L., Hausdorff, J., Ivanov, P. C., Mark, R., ... & Stanley, H. E. (2000). PhysioBank, PhysioToolkit, and PhysioNet: Components of a new research resource for complex physiologic signals. Circulation [Online]. 101 (23), pp. e215–e220.*

This code was written and run using *R* version 4.3.2 and Python version 3.11.15 on a MacBook Pro running macOS Tahoe version 26.5.2 with an Apple M5 processor, 32GB random-access memory and 10 cores. The code may need to be run in chunks, depending on application memory. The typical run time of all code including sensitivity analyses was approximately 3 days.

***Reproducing the study***

This code will exactly reproduce the clinical prediction model results of the study and descriptive data. It will, however, not necessarily sample the same discharge letters that were used for the clinician review exercise. 

Before running the code, the data should be saved into a secure local directory. The required package versions are included in the *packages.txt* file within this directory.

To reproduce the analyses, scripts must be run in this order:  

   1. **lang_packages&setup.R***
   2. **lang_disc_cleaning.R**
   3. **BERT_discharges.py**
   4. **BERT_access.py**
   5. **lang_performance.R**
   6. **lang_access_performance.R**
   7. **lang_descriptive.R**
   8. **BERT_SHAP.py**  
   9. **BERT_SHAP_Access.py**
   10. **lang_SHAP.R**
   11. **BERT_stability.py**
   12. **BERT_timesens.py**
   13. **lang_sensitivity.R**
   
To run **lang_questionnaire.R**, questionnaires first need to be undertaken by six suitably qualified participants, each of whom must first have gone through the MIMIC-IV data access steps mandated by PhysioNet. We engineered a user-friendly version of the questionnaire using the Shiny app *app.R* for the overall model and *app_2.R* for the Access model, with manual editing of the specific csv file name required for each exercise in the format "q_1.csv", "q_2.csv", etc and "ac_q_1.csv", "ac_q_2.csv", etc. However, questions could also be provided as .csv files with the column headings "page" with values 2-26, "question" for the discharge letter text and "answer" with instructions for participants to input either "Yes" or "No" in this column.
