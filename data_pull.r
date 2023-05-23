if(nchar(Sys.getenv("UBHT_username"))==0) 
  stop("Make sure you have set your username with Sys.setenv(UBHT_username = \"xxxxx\")")

if(nchar(Sys.getenv("UBHT_password"))==0) 
  stop("Make sure you have set your password with Sys.setenv(UBHT_password = \"xxxxx\")")


url=paste('secret_BI_Address',
          'StartDate=',format(startdate_pull,format='%m/%d/%Y'),'%2000:00:00','&',
          'EndDate=',format(enddate,format='%m/%d/%Y'),'%2000:00:00',
          '&rs:ParameterLanguage=&rs:Command=Render&rs:Format=CSV&rc:ItemPath=Tablix1',sep="")

r<-RETRY("GET",url,authenticate(Sys.getenv("UBHT_username"),
                                Sys.getenv("UBHT_password"),
                                type="ntlm"),timeout(60))

#status<-r$status_code
#stopifnot(r$status_code=200)
if(r$status_code!="200") 
  stop("Error: Unable to download from BI!")

attendances_plus<-readr::read_csv( content(r,"text"), skip=2 )%>%
  #clean_names()%>% 
  distinct(CAS_ID, .keep_all = TRUE)%>%
  mutate(dta_time     = time_length(interval(dmy_hms(Arrival_DTTM),dmy_hms(Decision_to_Admit_Datetime)), "minute"),
         resus_time   = time_length(interval(dmy_hms(Resus_Start_Datetime),dmy_hms(Resus_End_Datetime)), "minute"),
         age_months   = round(time_length(interval(dmy_hms(DoB),dmy_hms(Arrival_DTTM)), "month")),
         Arrival_DTTM = dmy_hms(Arrival_DTTM),
         Resus_Start_Datetime= dmy_hms(Resus_Start_Datetime),
         Resus_End_Datetime=dmy_hms(Resus_End_Datetime),
         Discharge_DTTM = Arrival_DTTM + minutes(ED_Length_of_Stay_Mins),
         Decision_to_Admit_Datetime = dmy_hms(Decision_to_Admit_Datetime),
         DTA_plus = Decision_to_Admit_Datetime+minutes(90),
         Disposal_time = Arrival_DTTM+minutes(ED_Length_of_Stay_Mins),
         DoB=format(dmy_hms(DoB),format='%d/%m/%Y'),
         dta_time=time_length(interval(Arrival_DTTM,Decision_to_Admit_Datetime), "minute"),
         Time_Seen=dmy_hms(Time_Seen),
         seen_time=time_length(interval(Arrival_DTTM,Time_Seen), "minute"),
         seen_to_dta=time_length(interval(Time_Seen,Decision_to_Admit_Datetime), "minute"),
         Triage_DTTM=dmy_hms(Triage_Start_Datetime),
         tbs=time_length(interval(Arrival_DTTM,Time_Seen), "minute"),
         ttt=time_length(interval(Arrival_DTTM,Triage_DTTM), "minute"),
         first_contact=coalesce(ttt,tbs),
         first_contact_DTTM=coalesce(Triage_DTTM,Time_Seen),
         First_Ward_Start_Date=dmy_hms(First_Ward_Start_Date),
         Second_Ward_Start_Date=dmy_hms(Second_Ward_Start_Date),
         finalDischarge_DTTM=dmy_hms(Discharge_Datetime),
         obs_dc_DTTM=coalesce(Second_Ward_Start_Date,finalDischarge_DTTM),
         Arrival_shift_day = case_when( hour(Arrival_DTTM) > 7 ~ as.Date(Arrival_DTTM),
                                        hour(Arrival_DTTM) < 8 ~ as.Date(Arrival_DTTM)- days(1)),
         DNW = if_else(Discharge_Description %in% c("BRHC Did Not Wait","BRHC Self Discharge","Did Not Wait","Self Discharge"),TRUE,FALSE),
         disposition = case_when(Discharge_Description=="Admitted" & First_Ward=='BCH E307 (The Observatory) x28339/28365' ~ 'Obs Patient',
                                 Discharge_Description=="Admitted" & First_Ward!='BCH E307 (The Observatory) x28339/28365' ~ 'Inpatient',
                                 TRUE ~ 'Discharged'))

attendances<-
  attendances_plus%>%
  filter(Arrival_DTTM %within% interval(startdate,enddate))
#
#
# figure out the by minute counts -----------------------------------------
#
#

library(data.table)
timeblock_fast<-data.frame(
  times=seq(startdate,enddate,by="1 mins"))%>%
  mutate(base_date = as.Date(times),
         base_hour=hour(times),
         shift_date=case_when(base_hour<8 ~ base_date-days(1),
                              TRUE~base_date))%>%
  setDT()

#start_time <- Sys.time()
timeblock<-timeblock_fast[, ':=' (
  triageQueue=sum(between(times,attendances_plus$Arrival_DTTM, attendances_plus$first_contact_DTTM,NAbounds=NA),na.rm=TRUE),
  deptNo     =sum(between(times,attendances_plus$Arrival_DTTM, attendances_plus$Discharge_DTTM,NAbounds=NA),na.rm=TRUE),
  DTANo      =sum(between(times,attendances_plus$DTA_plus    , attendances_plus$Discharge_DTTM,NAbounds=NA),na.rm=TRUE),
  ResusNo    =sum(between(times,attendances_plus$Resus_Start_Datetime,attendances_plus$Resus_End_Datetime,NAbounds=NA),na.rm=TRUE))
  ,by=times]
#end_time <- Sys.time()
#end_time - start_time

##
#



summary_day<-  
  timeblock%>%
  group_by(base_date)%>%
  summarise(max_triage_queue=max(triageQueue), 
            max_census=max(deptNo),
            max_DTA=max(DTANo)
  )

summary_shift<-  
  timeblock%>%
  group_by(shift_date)%>%
  summarise(max_triage_queue=max(triageQueue), 
            max_census=max(deptNo),
            max_DTA=max(DTANo)
  )

summary_hour<-  
  timeblock%>%
  group_by(base_date,base_hour)%>%
  summarise(max_triage_queue=max(triageQueue), 
            max_census=max(deptNo),
            max_DTA=max(DTANo),
            max_inResus=max(ResusNo)
  )%>%
  mutate(base_date=case_when(base_hour<8 ~ base_date-days(1),
                             TRUE~base_date))%>%
  mutate(weekday=lubridate::wday(base_date,label=TRUE, week_start=1))%>%
  mutate(weekday=factor(as.character(weekday), levels=rev(levels(weekday))))%>%
  mutate(week=floor_date(base_date,unit="week", week_start=1))%>%
  mutate(week=paste("w/c:",(format(week, format="%d %b"))))%>%
  mutate(week=forcats::as_factor(week))%>%
  mutate(base_hour=factor(base_hour, levels=c(8:23,0:7)))

#
#
# create breech data reports ----------------------------------------------
#
#

breech_data<-
  attendances%>%
  filter(ED_Length_of_Stay_Mins >= 12*60)%>%
  #filter(ED_Length_of_Stay_Mins -dta_time > 12*60)%>%
  mutate(Disposal_time = Arrival_DTTM+minutes(ED_Length_of_Stay_Mins))%>%
  mutate(disch_date=format(Disposal_time,format='%d/%m/%Y'),
         disch_time=format(Disposal_time,format='%H:%M'),
         ED_hrs_post_DTA = round((ED_Length_of_Stay_Mins-dta_time)/60, digits=0),
         ED_hrs = round(ED_Length_of_Stay_Mins/60, digits=0),
         child_age = if_else(age_months<12,"baby","child"),
         description = glue("This {child_age} and their parent spent a total of {ED_hrs} hours in the Children's Emergency Department, {ED_hrs_post_DTA} hours after the decision to admit. From {format(Arrival_DTTM,format='%H:%M %d/%m/%Y')} to {format(Disposal_time,format='%H:%M %d/%m/%Y')}"))%>%
  select(disch_date, disch_time, description,FORENAME,SURNAME, Trust_Number, DoB, CAS_ID)

#
#
# create performance reports ----------------------------------------------
#
#

performance_report<-data.frame(
  times=seq(startdate,enddate,by="day"))%>%
  mutate(base_date = format(times,format='%d/%m/%Y'))%>%select(-times)

performance <- function(basedate){
  #print(basedate)
  nicedate<-paste0(lubridate::wday(dmy(basedate),label=TRUE,abbr=FALSE),
                   " ",
                   toOrdinalDate(dmy(basedate)))
  
  #this gets the patient level data
  attendances_day<-subset(attendances, Arrival_shift_day==dmy(basedate))
  #this gets the queue lengths
  summary_day<-subset(summary_shift, shift_date==dmy(basedate))
  
  admissions_day<-subset(attendances_day,Discharge_Description=="Admitted")
  admissions_obs<-subset(attendances_day,Discharge_Description=="Admitted" & 
                           First_Ward=="BCH E307 (The Observatory) x28339/28365")
  admissions_ward<-subset(attendances_day,Discharge_Description=="Admitted" & 
                            !First_Ward=="BCH E307 (The Observatory) x28339/28365")%>%
    mutate(timeToWard=ED_Length_of_Stay_Mins-dta_time)
  
  timeToWard_mean<-mean(admissions_ward$timeToWard,na.rm=TRUE)
  timeToWard_mean<-pluralize("{timeToWard_mean %/% 60} hour{?s} {round(timeToWard_mean %% 60)} minute{?s}")
  timeToWard_max<-max(admissions_ward$timeToWard,na.rm=TRUE)
  timeToWard_max<-pluralize("{timeToWard_max %/% 60} hour{?s} {round(timeToWard_max %% 60)} minute{?s}")
  
  los_4h<-subset(attendances_day, ED_Length_of_Stay_Mins > (60*4))
  los_12h<-subset(attendances_day, ED_Length_of_Stay_Mins > (60*12))
  breech12h<-subset(attendances_day,(ED_Length_of_Stay_Mins -dta_time) > 12*60)
  breech12h_was_were<-if_else(nrow(breech12h)>0,"was","were")
  
  los_max<-max(admissions_ward$ED_Length_of_Stay_Mins)
  
  
  attendances_p2<-na.omit(subset(attendances_day,Triage_Category_Code==2)$tbs)
  p2_tbs_percent= round(length(na.omit(subset(attendances_day,Triage_Category_Code==2 & tbs < 11 )$tbs))/
                          length(na.omit(subset(attendances_day,Triage_Category_Code==2 )$tbs))*100)
  p2_tbs_mean<-mean(attendances_p2,na.rm=TRUE)
  p2_tbs_mean<-if_else(p2_tbs_mean<60,
                       pluralize("{round(p2_tbs_mean %% 60)} minute{?s}"),
                       pluralize("{p2_tbs_mean %/% 60} hour{?s} {round(p2_tbs_mean %% 60)} minute{?s}")
  )
  p2_tbs_max<-max(attendances_p2,na.rm=TRUE)
  p2_tbs_max<-if_else(p2_tbs_max<60,
                      pluralize("{round(p2_tbs_max %% 60)} minute{?s}"),
                      pluralize("{p2_tbs_max %/% 60} hour{?s} {round(p2_tbs_max %% 60)} minute{?s}")
  )
  
  attendances_p3<-na.omit(subset(attendances_day,Triage_Category_Code==3)$tbs)
  p3_tbs_percent= round(length(na.omit(subset(attendances_day,Triage_Category_Code==3 & tbs < 61 )$tbs))/
                          length(na.omit(subset(attendances_day,Triage_Category_Code==3 )$tbs))*100)
  p3_tbs_mean<-mean(attendances_p3,na.rm=TRUE)
  p3_tbs_mean<-if_else(p3_tbs_mean<60,
                       pluralize("{round(p3_tbs_mean %% 60)} minute{?s}"),
                       pluralize("{p3_tbs_mean %/% 60} hour{?s} {round(p3_tbs_mean %% 60)} minute{?s}")
  )
  p3_tbs_max<-max(attendances_p3,na.rm=TRUE)
  p3_tbs_max<-if_else(p3_tbs_max<60,
                      pluralize("{round(p3_tbs_max %% 60)} minute{?s}"),
                      pluralize("{p3_tbs_max %/% 60} hour{?s} {round(p3_tbs_max %% 60)} minute{?s}")
  )
  
  
  ttt<-subset(attendances_day, ttt<15)
  dnw<-subset(attendances_day, DNW)
  
  perf_4h<-100-round((nrow(los_4h)/nrow(attendances_day))*100)
  perf_triage<-round((nrow(ttt)/nrow(attendances_day))*100)
  perf_dnw<-round((nrow(dnw)/nrow(attendances_day))*100)
  
  output<-pluralize("Unacceptable 24hrs of performance in Children's ED due to overwhelming demand and exit block.
  
  On {nicedate} the Children's Emergency department had {nrow(attendances_day)} attendances.
We admitted a total of {nrow(admissions_day)} patient{?s}, {nrow(admissions_obs)} to the Observatory.

Our 4hr performance was {perf_4h}%. There were {nrow(los_4h)} 4hr breech{?es} and {no(nrow(breech12h))} 12hr breech{?es}. {no(nrow(los_12h))} child{?ren} had a >12hr length of stay.  
")
  
  
  if(perf_triage >10 | round(max(attendances_day$ttt, na.rm=TRUE))){
    output<-pluralize("{output}
  
  We did not meet our triage standards: {perf_triage}% of children waited longer than 15 minutes for their initial assessment. 
  The triage queue reached {summary_day$max_triage_queue} patients,  with a longest wait of {round(max(attendances_day$ttt, na.rm=TRUE))} minutes.
              ")
  }
  
  if(max(attendances_p2,na.rm=TRUE) >10 ){
    output<-pluralize("{output}
  
  We saw {p2_tbs_percent}% of category 2 patients within 10 minutes.
  The average time for a P2 patient to be seen was {p2_tbs_mean} and maximum was {p2_tbs_max}.
              ")
  }
  
  if(max(attendances_p3,na.rm=TRUE) >60 ){
    output<-pluralize("{output}
  
  We saw {p3_tbs_percent}% of category 3 patients within 60 minutes.
  The average time for a P3 patient to be seen was {p3_tbs_mean} and maximum was {p3_tbs_max}.
              ")
  }
  
  output<-pluralize("{output}
{nrow(dnw)} ({perf_dnw}%) patients left before being seen.")
  
  if(summary_day$max_census >39 ){
    output<-pluralize("{output}
  
  We did not meet our crowding standards: with a maximum of {summary_day$max_census} patients in the department.
  ")
  }
  
  if(summary_day$max_DTA >5 ){
    output<-pluralize("{output}
  
  We had exit block: with a peak of {summary_day$max_DTA} patients still in the department 90 minutes after a decision to admit.")
  }
  
  output<-pluralize("{output}
  Patients spent on average a further {timeToWard_mean} in the department after decision to admit.
  The longest wait for an inpatient bed was {timeToWard_max}.
  ")
  
  return(output) 
}


#performance_report$output<-map_chr(performance_report$base_date, performance)
#view(performance_report)
