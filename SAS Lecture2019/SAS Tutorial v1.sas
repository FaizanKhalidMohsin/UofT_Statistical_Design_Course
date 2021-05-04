/**************************************************************************************/
*Purpose: Introductory SAS Tutorial;
/**************************************************************************************/

*Date: 27SEP2018;

options nocenter;

libname TUTORIAL "M:\SAS Teaching\SAS Tutorial\Working"; *specifies permanent director we will use;

/**************************************************************************************/

/*Import data*/
proc import datafile="M:\SAS Teaching\SAS Tutorial\Data\Tutorial Socio.csv"
	out=SOCIO
	dbms=csv replace;
	guessingrows=100;
run; *165 obs;

proc import datafile="M:\SAS Teaching\SAS Tutorial\Data\Tutorial Additional.csv"
	out=ADDITIONAL
	dbms=csv replace;
	guessingrows=100;
run; *21 obs;

proc import datafile="M:\SAS Teaching\SAS Tutorial\Data\Tutorial Health.csv"
	out=HEALTH
	dbms=csv replace;
	guessingrows=100;
run; *153 obs;

proc import datafile="M:\SAS Teaching\SAS Tutorial\Data\Tutorial HPV.csv"
	out=HPV
	dbms=csv replace;
	guessingrows=100;
run; *69 obs;

/*Append additional data to socio data - first explore datasets*/
proc contents data=SOCIO order=varnum;
run;

proc contents data=ADDITIONAL order=varnum;
run;

/*Append data - ensure values are not truncated*/
data COMBINE;
	length	FNAME	$14.
			LNAME	$18.
			CITY	$13.
			MARITAL_STATUS	$7.;
	set SOCIO ADDITIONAL;
run; *186 obs;

/*Print values*/
proc print data=COMBINE (obs=10);
run; *values clearly not standardized, also notice the duplicates;

/*Look at categorical values*/
proc freq data=COMBINE;
	tables CITY SEX EDUCATION MARITAL_STATUS;
run;

/*Clean data - to keep things simple let's turn unknowns to missing*/
data COMBINE2 (drop=EDUCATION rename=(EDUCATION2=EDUCATION)); *drop original education variable, rename second to original;
	set COMBINE;
	length EDUCATION2 $25.; *prevent truncation;

	if missing(FAKE_HCN) then HCN_MISSING=1; *flag missing HCN;
	else HCN_MISSING=0;

	if missing(DOB) then DOB_MISSING=1; *flag missing DOB;
	else DOB_MISSING=0;

	FNAME=propcase(FNAME); *propercase for first name;
	LNAME=propcase(LNAME); *propercase last name;

	if upcase(CITY) in: ('6IX','TOR') then CITY='Toronto';
		*makes all values in city capitalized to make searching easier, 
		then look for values that start with '6IX' or 'TOR', and if found,
		change value to 'Toronto';
	if CITY in ('?','Unknown') then CITY='';
	if CITY='NorthYork' then CITY='North York';

	if SEX='U' then SEX='';
	
	if upcase(MARITAL_STATUS) in: ('U') then MARITAL_STATUS='';

	if EDUCATION=1 then EDUCATION2='1 - Less than high school';
	else if EDUCATION=2 then EDUCATION2='2 - High school';
	else if EDUCATION=3 then EDUCATION2='3 - Undergraduate';
	else if EDUCATION=4 then EDUCATION2='4 - Post-graduate';
	else EDUCATION2='';
run; *186 obs;

/*Check standardized values*/
proc freq data=COMBINE2;
	tables HCN_MISSING DOB_MISSING CITY SEX EDUCATION MARITAL_STATUS;
run; *notice 3 (1.61%) missing DOB;

/*Look at data with missing DOB*/
data MISSING_DOB;
	set COMBINE2;
	if missing(DOB) then output;
run; *3 obs;

proc print data=MISSING_DOB;
run;

/*Remove duplicates*/
proc sort data=COMBINE2 out=COMBINE3 nodupkey;
	by FAKE_HCN;
run; *161 obs;

/*Calculate age*/
data COMBINE4;
	set COMBINE3;
	format TODAY date9.;
	where DOB_MISSING=0; *remove records with missing DOB;
	TODAY=today(); *uses today's date;
	AGE=int((TODAY-DOB)/365.25); *manually calculate age;
run; *158 obs;

proc freq data=COMBINE4;
	tables AGE;
run;

/*Save permanent copy*/
data TUTORIAL.COMBINE4; *TUTORIAL. is the name of the permanent directory specified earlier;
	set COMBINE4;
	drop HCN_MISSING DOB_MISSING;
run; *158 obs;

/*Merge combined data with health data - first need to sort data the same way*/
proc sort data=TUTORIAL.COMBINE4 out=COMBINE4;
	by FAKE_HCN; *order data by HCN;
run; *158 obs;

proc sort data=HEALTH;
	by FAKE_HCN;
run; *153 obs;

/*Merge data on HCN*/
data COMBINE_HEALTH;
	merge COMBINE4 (in=a) HEALTH (in=b); *specifies a=COMBINE4 and b=HEALTH;
	by FAKE_HCN;
	if a=1 and b=1; *tells SAS to merge only if FAKE_HCN in both datasets;
run; *150 obs;

/*Explore new variables - need to standardize again*/
proc freq data=COMBINE_HEALTH;
	tables DIABETES HYPER OBE CANCER DEP DIET SUPPLEMENTS DAIRY CAFFEINE SODA WEIGHT_LIFTING CARDIO /missing;
run;

/*Standardize data - again, let's keep it simple and turn unknown to missing*/
data COMBINE_HEALTH2 (drop=DIET OBE DEP rename=(DIET2=DIET));
	length DIET2 $10.;
	set COMBINE_HEALTH;
	if DIABETES='U' then DIABETES='';
	if HYPER='U' then HYPER='';
	OBESITY=OBE;
	if OBESITY='U' then OBESITY='';
	if CANCER='U' then CANCER='';
	DEPRESSION=DEP;
	if DEPRESSION='U' then DEPRESSION='';
	if DIET=1 then DIET2='MIXED';
	else if DIET=2 then DIET2='VEGETARIAN';
	else if DIET=3 then DIET2='VEGAN';
	else DIET2='';
	if SUPPLEMENTS='U' then SUPPLEMENTS='';
	if DAIRY='U' then DAIRY='';
run; *150 obs;

/*Check standardization*/
proc freq data=COMBINE_HEALTH2;
	tables DIABETES HYPER OBESITY CANCER DEPRESSION DIET SUPPLEMENTS DAIRY CAFFEINE SODA WEIGHT_LIFTING CARDIO /missing;
run;

/*Descriptive stats for continuous variables*/
proc means data=COMBINE_HEALTH2 n mean median min max clm std p25 p50 p75 maxdec=2;
	var AGE EXERCISE TV;
run;

/*Same as above but by sex now*/
proc means data=COMBINE_HEALTH2 n mean median min max clm std p25 p50 p75 maxdec=2;
	var AGE EXERCISE TV;
	class DIABETES;
run;


/*Alternative function that also shows "extreme" values*/
proc univariate data=COMBINE_HEALTH2 plot;
	title "Analysis of Age, Exercise, & TV by Sex";
	var AGE EXERCISE TV;
	class DIABETES;
run;

/*Can also show a side-by-side comparison with the BY statement - first need to sort data*/
proc sort data=COMBINE_HEALTH2;
	by DIABETES;
run;

proc univariate data=COMBINE_HEALTH2 plot;
	title "Analysis of Age by Sex";
	by DIABETES;
	var AGE EXERCISE TV;
	where DIABETES^='';
run;

/*Can use logical operators in data step to create new variables*/
data COMBINE_HEALTH3;
	set COMBINE_HEALTH2;
	if AGE>=65 then SENIOR=1;
	else SENIOR=0;

	if (WEIGHT_LIFTING='Y' or CARDIO='Y') and EXERCISE>15 then ACTIVE=1; *let's assume researcher interested in combining these;
	else ACTIVE=0; 
run; *150 ob;

/*Can cross-tabulate as well*/
proc freq data=COMBINE_HEALTH3;
	tables DIABETES*SENIOR /chisq fisher norow nocol; *specify chi-square and Fischer's exact test;
run;

proc freq data=COMBINE_HEALTH3;
	tables DIABETES*ACTIVE /chisq fisher norow nocol; 
run;

ods pdf file="M:\SAS Teaching\SAS Tutorial\Presentation\Logistic Regression Example.pdf";
/*Example logistic regression*/
proc logistic data=COMBINE_HEALTH3;
	class 	SENIOR (ref="0") ACTIVE (ref="0") /param=ref;
	model	DIABETES (event='Y') = TV SENIOR ACTIVE /clodds=wald cl; *shows odds ratio and confidence intervals;
	units	TV=1;
run; 
ods pdf close;

/*Export dataset*/
proc export data=COMBINE_HEALTH3 outfile='M:\SAS Teaching\SAS Tutorial\Data\test export.csv'
         dbms=csv replace;
run; 


/*Alternative way of merging data is to use PROC SQL - let's use the HPV screening data, don't need to sort data first*/
proc sql;
	create table COMBINE_HPV as
	select	a.FAKE_HCN,
			b.TEST_ID,
			b.HPV_TEST_DATE,
			b.HPV_RESULT,
			b.BIOPSY as CERVICAL_BIOPSY,
			count(*) as TEST_COUNT, /*this counts the number of records by the stated group further below*/
			sum(HPV_RESULT) as POSITIVE_HPV, /*this sums the number of positive HPV results by stated group below*/
			sum(CERVICAL_BIOPSY) as POSITIVE_BIOPSY /*same as above but for cervical biopsy*/
	from	COMBINE_HEALTH3 as a inner join HPV as b /*inner join means both datasets must have the specified joining variable*/
	on		a.FAKE_HCN=b.FAKE_HCN /*tells SAS to join on HCN*/
	group by	a.FAKE_HCN /*group by statement tells SAS how to group counts and sums, in this case by HCN*/
	order by	a.FAKE_HCN, b.HPV_TEST_DATE /*this just sorts the resulting output, but is not necessary*/
	;
quit; *69 obs;

/*Create a sequence number for HPV screening tests*/
data COMBINE_HPV2;
	set COMBINE_HPV;
	by FAKE_HCN; *tells SAS to group by HCN;
	if first.FAKE_HCN then TEST_NUM=0; *if first HCN, then TEST_NUM=0;
	TEST_NUM=TEST_NUM+1; *TEST_NUM=1;
	retain TEST_NUM; *retain the previous value so the next observatio with the same HCN will have TEST_NUM=2;
run; *69 obs;

/*Assume researcher wants to look at screening tests that are not the very first test, where it was either
HPV positive or the biopsy was positive, and must be at least 30+ days after a previous test but not more than 2 years after*/
proc sql;
	create table COMBINE_HPV3 as
	select	a.FAKE_HCN,
			a.TEST_ID,
			a.TEST_NUM,
			a.HPV_TEST_DATE,
			b.TEST_ID as TEST_ID2,
			b.TEST_NUM as TEST_NUM2,
			b.HPV_RESULT as HPV_RESULT2,
			b.CERVICAL_BIOPSY as CERVICAL_BIOPSY2,
			b.HPV_TEST_DATE as HPV_TEST_DATE2, 
			b.HPV_TEST_DATE-a.HPV_TEST_DATE as DATE_DIFF
	from	COMBINE_HPV2 as a inner join COMBINE_HPV2 as b /*join dataset against itself*/
	on		a.FAKE_HCN=b.FAKE_HCN
	where	(b.TEST_NUM>1 and (b.HPV_RESULT=1 or b.CERVICAL_BIOPSY=1)) and 30<=(b.HPV_TEST_DATE-a.HPV_TEST_DATE)<=730 /*only keep records that match criteria*/
	order by	a.FAKE_HCN, a.HPV_TEST_DATE, b.HPV_TEST_DATE
	;
quit; *49 obs;

/*Identify patients who had screening tests that the researcher was interested in*/
proc sort data=COMBINE_HPV3 (drop=TEST_NUM HPV_TEST_DATE2 DATE_DIFF) out=COMBINE_HPV_PATIENT nodupkey;
	by FAKE_HCN;
run; *7 obs;

/*Convert data from long to wide - first sort data*/
proc sort data=HPV;
	by FAKE_HCN HPV_TEST_DATE;
run; *69 obs;

/*Can use arrays to transpose data (there are many other ways though)*/
data HPV_WIDE (drop=a TEST_ID HPV_TEST_DATE HPV_RESULT BIOPSY);
	retain FAKE_HCN;

	array	TEST_ID_[21];
	array	HPV_TEST_DATE_[21];
	array	HPV_RESULT_[21];
	array	BIOPSY_[21];
	format	HPV_TEST_DATE_1-HPV_TEST_DATE_21 date9.;

	do a=1 to 21 until (last.FAKE_HCN);
		set HPV;
		by FAKE_HCN;
		TEST_ID_[a]=TEST_ID;
		HPV_TEST_DATE_[a]=HPV_TEST_DATE;
		HPV_RESULT_[a]=HPV_RESULT;
		BIOPSY_[a]=BIOPSY;
	end;
	POSITIVE_HPV=sum(of HPV_RESULT_1-HPV_RESULT_21);
	POSITIVE_BIOPSY=sum(of BIOPSY_1-BIOPSY_21);
run; *17 obs;

/*Transpose data from wide to long*/
data HPV_LONG;
	retain TEST_ID FAKE_HCN HPV_TEST_DATE HPV_RESULT BIOPSY;
	set HPV_WIDE;
	format HPV_TEST_DATE date9.;

	array	_TEST_ID [21] TEST_ID_1-TEST_ID_21;
	array	_HPV_TEST_DATE [21] HPV_TEST_DATE_1-HPV_TEST_DATE_21;
	array	_HPV_RESULT [21] HPV_RESULT_1-HPV_RESULT_21;
	array	_BIOPSY [21] BIOPSY_1-BIOPSY_21;

	do a=1 to 21;
		if _TEST_ID[a]^=. then do;
			TEST_ID=_TEST_ID[a];
			HPV_TEST_DATE=_HPV_TEST_DATE[a];
			HPV_RESULT=_HPV_RESULT[a];
			BIOPSY=_BIOPSY[a];
			output;
		end;
	end;
	keep TEST_ID FAKE_HCN HPV_TEST_DATE HPV_RESULT BIOPSY;
run; *69 obs;

/*Expore data to a CSV file*/
proc export data=HPV_LONG outfile='M:\SAS Teaching\SAS Tutorial\Data\HPV_LONG.csv'
         dbms=csv replace;
run; *69 obs;
