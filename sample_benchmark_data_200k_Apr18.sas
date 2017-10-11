%let type1 = I1_I2_I3;
%let type2 = "I1","I2","I3";
options symbolgen mprint;

libname indir "F:\shire_followup\data"; 
libname outdir "F:\shire_followup\data\200K_2.3M\95M_&type1.";
libname tempdir "D:\shire_followup\data\95M_&type1.";

proc sql;
	select count(*) from outdir.Nonhae_3m_95m_&type1.;
quit;
/*2301458*/

/*get the pure nonHae after remove the patid in LHAE_9692*/
proc import out=outdir.HAE_ptid_days
	datafile="F:\shire_followup\data\200K_2.3M\95M_&type1.\hae_ptid_95M_&type1..csv"
	dbms=csv replace;
run;

proc sort data=outdir.hae_ptid_days out=outdir.hae_ptid_days_ord;
	by lookback_days;
run;
data outdir.hae_ptid_days_ord;
	set outdir.hae_ptid_days_ord;
	by lookback_days;
	retain count;
	if first.lookback_days then count=0;
	count + 1;
	rename patient_id=hae_patient_id;
run;

proc sql noprint;
	select max(count) into :cnt from outdir.hae_ptid_days_ord;
quit;
%put &cnt;

/*exclude the 2.3M patient_id from the outdir.nonhae_pt;*/
proc sql;
	create table pat_exc as
		select patient_id from outdir.Nonhae_3m_index_95m_&type1. 
		union 
		select patient_id from indir.LHAE_ptid;
quit;

proc sql;
create table outdir.nonhae_pt_for_exc3M as
	select
		a.patient_id as patient_id, 
		a.lookback_days as lookback_days,
		a.age,
		b.patient_id as exc_patient_id,
		case 
			when b.patient_id = . then 0
			else 1
		end as excl_flag
	from 
	(select patient_id, lookback_days, age from indir.Dataset_ii where datasource in ("95M", &type2.)) a 
	left join pat_exc b
	on a.patient_id = b.patient_id;
quit;

data outdir.nonhae_pt_for_exc3M;
	set outdir.nonhae_pt_for_exc3M;
	if excl_flag = 0;
run;

proc sql;
	select count(*) from outdir.nonhae_pt_for_exc3M a inner join
	outdir.Nonhae_3m_index_95m_&type1. b on
	a.patient_id = b.patient_id;
quit;

proc sql noprint;
select a.cnt1/b.cnt format=8.2 length=8 into :pct  from
	(select count(*) as cnt1 from outdir.nonhae_pt_for_exc3M where age gt 12) a,
	(select count(*) as cnt from outdir.nonhae_pt_for_exc3M) b;
quit;
%put &pct;

data outdir.temp1_1;
	set outdir.nonhae_pt_for_exc3M;
run;
/**/
proc printto log = null;
run;

/*%let i=1;*/
/*%let pat=1;*/
%macro smp_200K;
	%do i= 1 %to &cnt.;
		data tempdir.hae_patid_days_ord_t;
			set outdir.hae_ptid_days_ord;
			if count=&i.;
		run;

		data tempdir.hae_patid_days_ord_t;
			set tempdir.hae_patid_days_ord_t;
			pat_id = _n_;
		run;
		
		proc sql noprint;
			select max(pat_id) into :pat_cnt from tempdir.hae_patid_days_ord_t;
		quit;
		%put &pat_cnt;

		data tempdir.temp1_&i._1;
			set outdir.temp1_&i.;
		run;		
		%do pat=1 %to &pat_cnt;
			proc sql;
				create table tempdir.nonhae_matched_u_&i.&pat. as
				select 
						a.patient_id
						, a.lookback_days as nonhae_lookback_days
						, a.age
						, b.hae_patient_id
						, b.lookback_days as lookback_days
				from 
					tempdir.temp1_&i._&pat. a inner join 
					(select * from tempdir.hae_patid_days_ord_t where pat_id = &pat.) b
					on a.lookback_days between (b.lookback_days-90) and (b.lookback_days + 90);
			quit;

			/*sampling 200k*/
			proc sort data=tempdir.nonhae_matched_u_&i.&pat.;
				by hae_patient_id;
			run;
			%put %sysevalf(&pct eq 1);

			%if &pct ne 1.00 %then %do;
				proc surveyselect data=tempdir.nonhae_matched_u_&i.&pat.(where=(age gt 12))
								out=tempdir.smp_nonhae_u_&i.&pat._1 method=srs seed=1234 SAMPSIZE = %sysevalf(200 * &pct) noprint;
					strata hae_patient_id;
				run;
				
/*				%put %sysevalf(200 * &pct);*/
/*				%put %eval(200 - %sysevalf(200 * &pct));*/

				proc surveyselect data=tempdir.nonhae_matched_u_&i.&pat.(where=(age le 12)) 
								out=tempdir.smp_nonhae_u_&i.&pat._2 method=srs seed=1234 SAMPSIZE = %eval(200 - %sysevalf(200 * &pct)) noprint;
					strata hae_patient_id;
				run;			
				data tempdir.smp_nonhae_u_&i.&pat.;
					set tempdir.smp_nonhae_u_&i.&pat._1 tempdir.smp_nonhae_u_&i.&pat._2;
				run;
				
				proc datasets library = tempdir noprint;
					delete smp_nonhae_u_&i.&pat._1 smp_nonhae_u_&i.&pat._2;
				run;
			%end; 
			%else  %do;
				proc surveyselect data=tempdir.nonhae_matched_u_&i.&pat.
								out=tempdir.smp_nonhae_u_&i.&pat._1 method=srs seed=1234 SAMPSIZE = 200 noprint;
					strata hae_patient_id;
				run;
				
				data tempdir.smp_nonhae_u_&i.&pat.;
					set tempdir.smp_nonhae_u_&i.&pat._1;
				run;

				proc datasets library=tempdir noprint;
					delete smp_nonhae_u_&i.&pat._1;
				run;
			%end;

			%let patadd = %eval(&pat.+1);
			%put &patadd;

			proc sql;
				create table tempdir.temp1_&i._&patadd. as
					select a.*,
							b.patient_id as mat_patient_id
					from tempdir.temp1_&i._&pat. a 
					left join tempdir.smp_nonhae_u_&i.&pat. b
					on a.patient_id = b.patient_id;
			quit;

			data tempdir.temp1_&i._&patadd.;
				set tempdir.temp1_&i._&patadd.;
				if mat_patient_id = .;
				drop mat_patient_id;
			run;

			proc datasets library=tempdir noprint;
				delete temp1_&i._&pat. nonhae_matched_u_&i.&pat.;
			run;
		%end;

		data tempdir.smp_nonhae_u_all_&i.;
			set tempdir.smp_nonhae_u_&i.:;
		run;

		proc datasets library=tempdir noprint;
			delete smp_nonhae_u_&i.:;
		run;
		
		%let j=%eval(&i+1);
		proc sql;
			create table outdir.temp1_&j. as
			select
				a.patient_id
				,a.lookback_days as lookback_days
				,a.age
				, b.patient_id as matched_hae_ptid
			from	
				outdir.temp1_&i. a left join tempdir.smp_nonhae_u_all_&i. b
			on
				a.patient_id = b.patient_id;
		quit;

		data outdir.temp1_&j. ;
			set outdir.temp1_&j. ;
			if matched_hae_ptid = .;
			drop matched_hae_ptid;
		run;
	%end;

	proc datasets library=outdir noprint;
		delete temp1:;
	run;
%mend;
%smp_200K;

data outdir.nonhae_200k_index;
	set tempdir.Smp_nonhae_u_all_:;
run;

proc sql;
	select count(*) from outdir.nonhae_200k_index
	where patient_id in
	(select patient_id from outdir.Nonhae_3m_index_95m_&type1.);
quit;

proc sql;
create table outdir.nonhae_200k as
	select a.*, 
		   b.hae_patient_id from 
	(select * from indir.Dataset_ii where datasource in ("95M", &type2.)) a 
	inner join outdir.nonhae_200k_index b
	on a.patient_id = b.patient_id;
quit;

proc export data=outdir.nonhae_200K
	outfile="F:\shire_followup\data\200K_2.3M\95M_&type1.\nonhae_200K.csv"
	dbms=csv replace;
run;
	

proc sql;
	select count(*) as cnt1, count(distinct patient_id) as cnt2 ,count(*) /200 as cnt3 from outdir.nonhae_200k ;
quit;

proc sql;
	select count(*) from outdir.hae_ptid_days_ord;
quit;



