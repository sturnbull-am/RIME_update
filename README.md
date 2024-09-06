# RIME pipeline
RIME (Rapid Immunoprecipitation Mass spectrometry of Endogenous proteins) 

This repository contains all the required files (except for data) to make the RIME Deliverables for Antibody Validation and Full Scale RIME

## For both Full scale or AbQ
1. Clone this repo and name it as you want
2. Download Deliverables from [Spectrus](https://spectrus.operend.com/admin/inventory) 
  a) xxx.zip (Peak Studio folder) 
  b) xxx_csv.zip (need db.proteins.csv) 
  c) xxx_html.zip (need unziped folder where it should have db.summary.html & db.proteins.html & img/) 
3. Make sure to put db.proteins.csv and html folder (usually named "Analysis 1" but Spectrus may not be consistent)

   
## Full Scale
  ```
  bash rime_fullscale.sh -b BAIT -q QUOTENB -p PROJECTID -f HTMLFOLDER -o ORGANISM
  ```
  * `-b` : protein bait
  * `-q` : quote number
  * `-p` : project ID from file maker
  * `-f` : html folder name
  * `-o` : organism (ex/ human, mouse)

This will create the deliverable folder named as **Q-`QUOTENB`_RIME**
All information on input/output/process can be found in `rime_fullscale.R` 
4. Check deliverables **rime_report_abq.html** and **Sample Reportxxx.xlsx** and upload to ftp with **Peaks Studio folder**


## Antibody Validation
  ```
  bash rime_abq.sh -b BAIT -q QUOTENB -p PROJECTID -i INSTITUTION -f HTMLFOLDER -o ORGANISM
  ```
  * `-b` : protein bait
  * `-q` : quote number
  * `-p` : project ID from file maker
  * `-i` : project institution
  * `-f` : html folder name
  * `-o` : organism (ex/ human, mouse)  

This will create the deliverable folder named as **Q-`QUOTENB`_RIMEabq**
4. Check deliverables **rime_report_abq.html** and **Sample Reportxxx.xlsx** and upload to ftp with **Peaks Studio folder**
