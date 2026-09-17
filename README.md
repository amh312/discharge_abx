This repository houses the code for the academic paper **"Screening hospital discharge letters with scalable natural language processing efficiently detects inappropriate antibiotic use"**, for the purpose of peer review and subsequent open-sourcing.

If you use this code please cite this repository.

***Instructions for use:***

The electronic healthcare record source data can be obtained from PhysioNet at https://physionet.org/content/mimiciv/2.2/ and https://physionet.org/content/mimic-iv-note/2.2/ once the terms of access are met.

PhysioNet MIMIC-IV citations:

*Johnson, A., Pollard, T., Horng, S., Celi, L. A., & Mark, R. (2023). MIMIC-IV-Note: Deidentified free-text clinical notes (version 2.2). PhysioNet. RRID:SCR_007345. https://doi.org/10.13026/1n74-ne17*

*Johnson, A., Bulgarelli, L., Pollard, T., Horng, S., Celi, L. A., & Mark, R. (2023). MIMIC-IV (version 2.2). PhysioNet. https://doi.org/10.13026/6mm1-ek67.*

*Johnson, A.E.W., Bulgarelli, L., Shen, L. et al. MIMIC-IV, a freely accessible electronic health record dataset. Sci Data 10, 1 (2023). https://doi.org/10.1038/s41597-022-01899-x*

*Goldberger, A., Amaral, L., Glass, L., Hausdorff, J., Ivanov, P. C., Mark, R., ... & Stanley, H. E. (2000). PhysioBank, PhysioToolkit, and PhysioNet: Components of a new research resource for complex physiologic signals. Circulation [Online]. 101 (23), pp. e215–e220.*

This code was written and run using *R* version 4.3.2 and Python version 3.11.15 on a MacBook Pro running macOS Tahoe version 26.5.2 with an Apple M5 processor, 32GB random-access memory and 10 cores. A breakdown of timings for all scripts can be found in *script_times.csv*. Metal Performance Shaders were used to run the code on the Apple M5 GPU—model scripts (those starting with *DistilBERT* or *baseBERT*) will revert to CPU if MPS is unavailable, but if other GPUs are available this preference can be amended in the script.

***Reproducing the study***

This code will exactly reproduce the clinical prediction model results of the study and descriptive data. It will, however, not necessarily sample the same discharge letters that were used for the clinician review exercise. To reproduce the main analysis:

   1. **Save *admissions.csv*, *patients.csv*, (MIMIC-IV) and *discharge.csv* (MIMIC-IV-Note) into a secure local directory**
   2. **Download *aware_classification.csv* from this repo into the same directory**
   3. **Install the required package versions listed in *packages.txt***
   4. **Run scripts in the initial_scripts folder (A-B)**
   5. **Run scripts in the main_analysis folder (C-K)**

Scripts must be run in **alphabetical order**. Note that **scripts H, I, J and K will not run** until questionnaires (produced as *q_1.csv* to *q_6.csv* and *ac_q_1.csv* to *ac_q_6.csv* by the scripts E and F) have been undertaken by six suitably qualified participants, each of whom must first have gone through the MIMIC-IV data access steps mandated by PhysioNet. Participants should answer 'Yes' or 'No' in the 'answer' column then save as *r_1.csv* to *r_6.csv* and *ac_r_1.csv* to *ac_r_6.csv* corresponding to their question csv number. The required format for these files is demonstrated in the *r_example.csv* file in this repository.

Question wording for the overall model was *"You are on an antibiotic stewardship team, tasked with identifying inappropriate discharge antibiotic prescriptions. You will see 25 discharge letters - for each case, if you think discharging the patient on an antibiotic is very likely to be INappropriate based on the information provided, please select 'Yes'. Otherwise, please select 'No'. Please note you are only reviewing the appropriateness of the decision WHETHER to discharge on an antibiotic, NOT the choice of antibiotic agent."*.

Question wording for the Access model was *"Your antimicrobial stewardship team is now tasked with reviewing choices of discharge antimicrobial agents for their appropriateness. You will review another 25 discharge letters - for each case, answer 'yes' if you think that discharge on a broad-spectrum antibiotic treatment is very likely to be INappropriate, i.e., that either that a narrower-spectrum (WHO Access) antibiotic would be more appropriate, and/or that no antibiotic therapy is indicated. Otherwise, please select 'no'."*.

Then, to reproduce the other analyses:

   1. **Run scripts in the *sensitivity_analysis* folder (L-R) for the sensitivity analysis**
   2. **Run scripts in the *comparator_models* folder (S-Y) for the baseBERT and bag-of-words models**

Scripts must again be run **in alphabetical order**.

***Testing the code***

To test the functionality of the code for the main analysis without requiring download of the real PhysioNet datasets: 

   1. **Install the packages listed in *packages.txt***
   2. **Download all csv files in the *test_data* folder and **remove the "_test" suffix****
   3. **Run all scripts in the *main_analysis* folder **except script G****
   4. **Run all scripts in the *sensitivity_analysis* folder**

The scripts must again be run in **alphabetical order** (omitting script G). The test CSVs contain collections of random words, so the results will be random but can be used to demonstrate the code's functionality.

***Reviewing the code***

R code follows the general structure of function definitions, then data uploads, then code that embeds the functions. Python code follows the same general structure, except that packages are imported at the beginning of each script (for R scripts this is done once in *lang_packages&setup.R*). Sections are denoted with a double hash, subsections with a triple hash, and sections within functions with a single hash. R code was written during a transition between R Studio and Positron IDEs, resulting in a mixture of "%>%" and "|>" to denote pipes for tidyverse functions.
