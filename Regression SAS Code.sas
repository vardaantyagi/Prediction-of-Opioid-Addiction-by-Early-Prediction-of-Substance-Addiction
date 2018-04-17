
Proc import datafile = 'C:\Users\Tarun Aggarwal\Desktop\video_games_sales_with_reviews2.csv' out = vgs_reviews1;
run;
data 'C:\Users\Tarun Aggarwal\Desktop\vgs_reviews1';
set vgs_reviews1;
run;





libname sas_data "C:\SAS\Regression";
libname c 'c:\SAS';
run;


data sas_data.kddr;

set c.mydataset_sas2;

run;
/* Creating new columns alc,mar,her,coc*/
data sas_data.kddr1;
set c.mydataset_sas2;
if Substance="alcohol" then alc=1;
	else alc=0;
if Substance="marijua" then mar=1;
	else mar=0;	
if Substance="heroine" then her=1;
	else her=0;
if Substance="cocaine" then coc=1;
	else coc=0;
	run;

	
/* Running regression on subdays subrec*/
proc reg data=sas_data.kddr1;
model  alc=subdays subrec /dw dwprob vif ; 
run;
/* Running correlation between subdays subrec */
proc corr data = sas_data.kddr1;
var subdays subrec;
run;





proc sgplot data=sas_data.kddr1 ;
  scatter     x= subdays y=subrec;
  ellipse     x= subdays y=subrec ;
   
run;
/* Normalizing the data */
PROC STANDARD DATA=sas_data.kddr1(keep= subrec subdays substance alc mar her coc) MEAN=0 STD=1 
             OUT=sas_data.kddr2(rename=(subrec=subrec_z subdays=subdays_z));
  VAR  subrec subdays ;
RUN;
/* Running Principal Component Analysis */
proc princomp   data=sas_data.kddr2 out=sas_data.kddr3;
   var  subdays_z  subrec_z  ;
run;

data sas_data.kddr4;
  set sas_data.kddr2;
     compz_1=0.707107*subdays_z+0.707107*subrec_z;
     compz_2=0.707107*subdays_z-0.707107*subrec_z;
run;
/* Correlation between compz_1 compz_2*/
proc corr data = sas_data.kddr4;
var compz_1 compz_2;
run;
/* Running Logistic regression on alcohol */
proc LOGISTIC data=sas_data.kddr4 descending;
	model     alc= compz_1 compz_2;
	run;
/* Running Logistic regression on marijuana */
proc LOGISTIC data=sas_data.kddr4 descending;
	model     mar= compz_1 compz_2;
	run;
/* Running Logistic regression on heroine */
proc LOGISTIC data=sas_data.kddr4 descending;
	model     her= compz_1 compz_2;
	run;
/* Running Logistic regression on cocaine */
proc LOGISTIC data=sas_data.kddr4 descending;
	model     coc= compz_1 compz_2;
	run;
quit;
