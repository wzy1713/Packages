
option symbolgen mprint;

PROC IMPORT OUT= WORK.TEST
            DATAFILE= "D:\chileancredit.csv" 
            DBMS=CSV REPLACE;
     GETNAMES=YES;
     DATAROW=2; 
RUN;

data raw_tb;
	set TEST;
	response = fgood;
	keep response dep cblineut;
run;

%macro en(target_tb);
	data test;
		set &target_tb.;
	run;
	
	proc sql noprint;
		select count(*) into :pos from test where response = 1;
		select count(*) into :neg from test where response = 0;
	quit;

	/*删除因变量*/
	data test_num;
		set test(drop = response);
	run;

	/*建立数值型数据集*/
	data num;
		set sashelp.vcolumn;
		where memname = "TEST_NUM" and libname = "WORK" and type = "num";
	run;

	/*把数值型变量定义为宏变量*/
	proc sql noprint;
		select name into:numname separated by " " from num;
	quit;

	%put &numname.;
	%let i = 1;
	%do %until(%scan(&numname.,&i.," ")=);
		%let numnamei = %scan(&numname.,&i.," ");
		data test_0;
			set test;
			_Node_ = -1;
			_Leaf_ = -1;
			P_response1 = 0;
			P_response0 = 0;
			if &numnamei. = .;
			drop &numname.;
		run;

		data test_1;
			set test;
			if &numnamei. ne .;
		run;

		ods graphics on;
		proc hpsplit data= test_1 leafsize=10 maxbranch=2 Missing=SIMILARITY;
			target response; 
			input &numnamei./ level = int; *需分箱的连续变量;
			CRITERION GINI;
			output out =tem1_&numnamei.
			importance=temp2_&numnamei.
			nodestats=temp3_&numnamei. /*temp3中存了分箱的结果;*/
			prunesubtree=temp4_&numnamei.;
			rules file = "d:\rules.txt";
		run;
		
      /*计算IV值和WOE*/
		data test_gp;
			set tem1_&numnamei. test_0;
		run;

		proc sql;
			create table summarize as 
				select _Leaf_ as group,
					 response,
					 count(*) as cnt
			from test_gp
			group by _Leaf_, response
			order by 1,2,3;
		quit;

		proc transpose data = summarize out = summarize_tran(drop=_NAME_) prefix = r_;
			by group;
			id response;
			var cnt;
		run;

		data IV_&numnamei.;
			set summarize_tran;
			total_pos = &pos.;
			total_neg = &neg.;
			WOE = log((r_1 * &neg.)/(r_0 * &pos.));
			IV = (r_1/&pos. - r_0/&neg.) * log((r_1 * &neg.)/(r_0 * &pos.));
		run;

	%let i = %eval(&i.+1);
	%end;
%mend;
%en(raw_tb);

