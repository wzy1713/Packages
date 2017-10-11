/*
Notes:  This macro is used to convert category variables into dummy variables, each level is one dummy variable excluding reference level.
	    The level that you want to be reference level should be the last one if you order all of the levels in this category.

Sample: %dummy(china, Final_persistency_3m_pred, ("ID"));
		data _null_;
			set china.dummyes ;
				CALL EXECUTE(texto);
		run;
*/

%macro dummy(library, table, dropVar);
%let lib = %upcase(&library.);
%let tb = %upcase(&table.);
%put &lib.;
%put &tb.;

proc sql;
create table tipos_char as 
	select name, type, length  from dictionary.columns where libname="&lib." and memname="&tb."
			and type="char" and name not in &dropVar.;
quit;

proc sql noprint;
	select name, count(name) into:names separated by ' ',:n_names  from work.tipos_char;
quit;
%put &names;
%put &n_names;

data &lib..dummyes;
	format valor $40. count 8. percent 8.3 variable $32. id 8. reference $1. texto $400. _count 8.;
	texto="data &lib..&tb._To_Dummy;"; output;
	texto="set &lib..&tb.;"; output;
run;

%do i=1 %to &n_names;
	%put %scan(&names,&i);

	proc freq data=&lib..&tb. noprint;
		tables %scan(&names,&i) / /*missing*/ out=&lib..dummy (rename=(%scan(&names,&i)=valor) );
	run;

	data &lib..dummy; 
		set &lib..dummy; 
		where valor ne ""; 
	run;

	proc sql noprint;
		select distinct upcase(valor) into :valor separated by ' ' from &lib..dummy;
	quit;
 
	%put &valor.;

	proc sort data=&lib..dummy; 
		by valor; 
	run;

	data &lib..dummy;
		set &lib..dummy;
		id = _n_;
	run;

	data &lib..dummyes;
		set  &lib..dummyes (drop= _count) &lib..dummy (in=in_act) end=fin;
		retain _count 0;
		if _n_=1 then _count=0;
		if in_act=1 then do;
				_count=_count+1;
					variable="%scan(&names,&i)";
					texto="if "||trim(variable)||" ne '' then dummy%scan(&names,&i)"||compress(put(_count,8.))||"=0;"
						||"if "||trim(variable)||"='"||trim(valor)||"' then dummy%scan(&names,&i)"||compress(put(_count,8.))||"=1;";
		end;
		if fin = 1 then reference = "Y";
		output;
	run;

	proc sql; 
		drop table &lib..dummy; 
	quit;
%end;

data &lib..dummyes;
	set &lib..dummyes end=fin;
	output;
	if fin=1 then do;
		valor=""; 
		count=.; 
		percent=.; 
		variable="";
		texto="drop &names. ;"; output;
		texto="run;"; output;
	end;
run;

%mend;








