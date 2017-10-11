/*
This macro is used to stratfy the data in each response based on the number of fods.
The followings are description of input parameters:
	1. table:    this is the raw data used to split. there must be 2 variables: unique id and response
	2. res:      this is response variable.
	3. uniqueID: this is the unique id of each row.
	4. folds:    the number of folds. 
*/

%macro kfolds(table, res, uniqueId, folds);
	data t1;
		set &table.;
		response = &res;
		uniqueId = &uniqueId;
		foldId = 0;
		drop &res &uniqueId;
	run;
	%let rate = %sysevalf(1/&folds);
	proc sql noprint;
		select ceil(count(*) * &rate) into: posCnt from t1 where response = 1;
		select ceil(count(*) * &rate) into: negCnt from t1 where response = 0;
	quit;
	%put &posCnt;
	%put &negCnt;

	%do i = 1 %to %sysevalf(&folds - 1);
		proc surveyselect data = t1(where = (foldId =0 and response = 0)) out = _neg_sample method = sys sampsize = &negCnt.  noprint seed = 1234;
		run;
		proc surveyselect data = t1(where = (foldId =0 and response = 1)) out = _pos_sample method = sys sampsize = &posCnt.  noprint seed = 1234;
		run;
		data _t_sample;
			set _neg_sample _pos_sample;
		run;

		proc sort data = t1;
			by uniqueId;
		run;
		proc sort data = _t_sample;
			by uniqueId;
		run;
		data t1;
			merge t1(in = in1) _t_sample(in = in2 keep=uniqueId);
			by uniqueId;
			if in1;
			if in1 and in2 then foldId = &i;
		run;
	%end;
	data outFolds;
		set t1;
		if foldId = 0 then foldId = &folds;
	run;
	proc freq data = outFolds;
		table response * foldId/nocol nocum nopercent norow;
	run;
	proc datasets library = work noprint;
		delete t1 _t_sample _neg_sample _pos_sample;
	quit;
%mend;

/*
%kfolds(test, f_sex, id, 5);
*/


