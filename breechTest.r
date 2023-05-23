#
# headless submission of 12hr breech datix
#
.libPaths( c( .libPaths(), "/home/marlowr@ubht.nhs.uk/R/x86_64-pc-linux-gnu-library/4.2") )
library(dplyr)
library(lubridate)
library(janitor)
library(tidyr)
library(purrr)
library(httr)
library(glue)
library(patientcounter)
library(toOrdinal)
library(cli)      # for pluralize (NB don't use the pluralize package, as this is completely different!)
library(webdriver)
library(data.table)
library("pushoverr")

set_pushover_app(token='')
set_pushover_user(user='')

path='/home/marlowr@ubht.nhs.uk/performanceMonitoring/'

#past_reports<-breech_data[1:3,]
load(paste0(path,'breech_database.rdata'))
cat(glue("\n\n-------------------------------------------------------\n\n"))
cat(glue("{now()}\n\n"))

if (file.exists(paste0(path,'breechTest_STOP'))){
  stop(cat(glue("*** Emergency STOP - breechTest_STOP exists! ***\n\n")))
}

cat(glue("{nrow(past_reports)} breeches already reported in database\n\n"))


startdate=now('UTC')-days(5)
#startdate=dmy_hm('26-02-2023 08:00')
#to catch any long stays before
startdate_pull=startdate-days(2)
enddate=now('UTC')

source(paste0(path,".UBHT_credentials.r"))
source(paste0(path,"data_pull.r"))

new_breeches<-breech_data%>%
  drop_na(disch_date)%>%
  filter(!CAS_ID %in% past_reports$CAS_ID)

number_to_report<-nrow(new_breeches)

if(number_to_report<1){ 
  pushover('No new 12hr breeches to report')
  stop(glue("No new 12hr breeches to report"))
}else{
  pushover(glue("{number_to_report} new 12hr breeches to report"))
  cat(glue("{number_to_report} new 12hr breeches to report\n\n"))
}


# Datix Data to fill ------------------------------------------------------

breech_12hr<-
  tribble(
    ~field, ~type, ~value,
    '#inc_dincident',         'normal', '11/10/2022',
    '#inc_time',              'normal', '19:25',
    '#inc_notes' ,            'normal', "20 hour trolley wait in Children's ED after DTA",
    '#inc_actiontaken',       'normal', 'Completed for audit purposes',
    '#inc_type_title',        'pause',  'Incident affecting Patient',
    '#inc_category_title',    'pause',  'Service Provision',
    '#inc_subcategory_title', 'pause',  '12 Hour Trolley Breach (Paediatric Only)',
    '#UDF_T_0_108_values > ul > li:nth-child(26) > input',     'click',  'associated factors',
    '#con_type_6_title',      'pause',  'Patient',
    '#con_forenames_6',       'normal', 'Robin',
    '#con_surname_6',         'normal', 'Marlow',
    '#UDF_Y_0_346_6_title',   'pause',  'Yes',
    '#con_number_6',          'normal', 'T8056957',
    '#con_dob_6',             'normal', '03/04/2018',
    '#con_gender_6_title',    'pause',  'Not Stated/Unknown Gender',
    '#con_ethnicity_6_title', 'pause',  'Not stated',
    '#show_injury_6_title',   'pause',  'No',
    '#UDF_C_0_1422_6_title',  'pause',  'Yes',
    '#inc_unit_title',        'pause',  'Bristol Royal Hospital For Children',
    '#inc_clingroup_title',   'pause',  "Children's Emergency Department E308 (Children's ED & Ward 39 BRHC)",
    '#inc_directorate_title', 'pause',  'Womens and Childrens',
    '#inc_specialty_title',   'pause',  'Paediatric Accident and Emergency',
    '#inc_result_title',      'pause',  'Harm caused to a person or the organisation',
    '#inc_severity_title',    'pause',  'Major',
    '#button_0',              'click',  'dismiss_popup',
    '#UDF_Y_0_764_title',     'pause',  'No',
    '#UDF_Y_0_1028_title',    'pause',  'No',
    '#con_forenames_3',       'normal',  'Robin',
    '#con_surname_3',         'normal',  'Marlow',
    '#UDF_C_0_282_3_title',   'pause',   'Medical',
    '#UDF_C_0_302_3_title',   'pause',   'Consultant',
    '#con_tel1_3',            'normal',  '28666',
    '#con_email_3',           'normal',  'test@test.nhs.uk',
    '#btnSubmit',             'submit',   'submit_form'
  )


# Datix field filler function ---------------------------------------------
fill_field<-function(field,type,value){
  if (type=='normal'){
    webElem <- remDr$findElement(css = field)
    webElem$sendKeys(value)
  }
  else if(type=='pause'){
    webElem <- remDr$findElement(css = field)
    webElem$sendKeys(value)
    Sys.sleep(5)  
    webElem$sendKeys(webdriver::key$tab)
  }
  else if(type=='click'){
    webElem <- remDr$findElement(css  = field)
    webElem$click()
  }
  else if(type=='submit'){
    webElem <- remDr$findElement(css = field)
    webElem$click()
  }
}

# Now report it -----------------------------------------------------------
#install_phantomjs()
source(paste0(path,"load_phantomjs_patched.r"))
pjs <- run_phantomjs()
#pjs

remDr <- Session$new(port = pjs$port)
#remDr

remDr$go("http://datix/datix/live/index.php")
## Check that can access datix here
if(!remDr$getTitle()=="Datix: Incident Report Form"){
  stop(cat(glue("Couldn't access Datix site\n\n")))
}else{
  cat(glue("successfully connected to Datix\n\n"))
}

no_to_report<-nrow(new_breeches)
if (no_to_report>5){no_to_report<-5}

for( i in c(1:no_to_report))
{
  cat(glue("({i}/{number_to_report})\t{new_breeches[i,]$disch_date}\t{new_breeches[i,]$CAS_ID}\t{new_breeches[i,]$FORENAME} {new_breeches[i,]$SURNAME}\n\n"))

  if(new_breeches[i,]$CAS_ID %in% past_reports$CAS_ID){
    stop(glue("{new_breeches[i,]$CAS_ID} has already been reported"))
  }
  
  breech_12hr$value[c(1,2,3,10,11,13,14)]<-new_breeches[i,c(1,2,3,4,5,6,7)]
  remDr$go("https://datix/datix/live/index.php")
  
  pmap(breech_12hr, fill_field, .progress = list(
    type = "iterator", 
    format = "({i}/{number_to_report}): {new_breeches[i,]$CAS_ID} {cli::pb_bar} {cli::pb_percent}",
    clear = TRUE))
  Sys.sleep(10)
  
  html<-remDr$getSource()
  if(!grepl('Incident has been saved',html)){
    remDr$takeScreenshot(file=paste0(format(now(), "%Y%m%d_%H%M%S_"), "error.png"))
    file.create(paste0(path,"breechTest_STOP"))
    stop(cat(glue("Failed to report at patient {new_breeches[i,]$CAS_ID}\n\n")))
  }else{
    cat(glue("({i}/{number_to_report})\t{new_breeches[i,]$disch_date}\t{new_breeches[i,]$CAS_ID}\t{new_breeches[i,]$FORENAME} {new_breeches[i,]$SURNAME}\t SUCCESS!\n\n"))
    #pushover(glue("new 12hr breech reported!"))
    past_reports<-rbind(past_reports,new_breeches[i,])
  }
  
}

save(past_reports, file=paste0(path,'breech_database.rdata'))

remDr$delete()

#file.create(paste0(path,"breechTest_STOP"))