

/*Code for AUC and Gini values*/
ods select none;
ods output WilcoxonScores=WilcoxonScore;
 
proc npar1way wilcoxon data= {score dateset name} ; /** change the score dataset name*/
  where response ne . ;
  class response;
  var  p_1; /*p_1 : put variable of the probability of the output*/
run;
ods select all;
 
data AUC;
 set WilcoxonScore end=eof;
 retain v1 v2 1;
 if _n_=1 then v1=abs(ExpectedSum - SumOfScores);
 v2=N*v2;
 if eof then do;
 d=v1/v2;
 Gini=d * 2;    AUC=d+0.5;   
 put AUC=  GINI=;
 keep AUC Gini;
 output;
 end;
run;
 
 
 
/*Code to calculate lift*/
 
/************/
/*Craft Lift*/
/************/
%let target = response;
%macro lift0(dsname);
 title1 "Validation Data Set : &dsname ";
 proc sort data=&dsname(keep=p &target) out=lift;
  by descending p descending &target;
 run;
 
 data _null_;
  set lift nobs=n;
  call symput('n',n);
  call symput('obs', ceil(n/10));
 run;
 
 %put &n &nbin;
 
 data lift;
  set lift;
  if _n_ <= &obs*1 then bin = "Bin "||put(1, z2.);
  %do k=2 %to 10;
    else if _n_ <= &obs*&k then bin = "Bin "||put(&k, z2.);
  %end;
  else if bin="Bin "||put(&k, z2.);
 run;
 
 proc freq data=lift;
  table bin;
  table bin * &target / out=tmp outpct;
 run;
 
 proc transpose data=tmp(keep= bin &target count) out=t_tmp prefix=_;
        id &target;
        var count;
        by bin;
 run;
 
 proc freq data = &dsname(keep = &target) noprint ;
        tables &target / out = _Rate ; 
 run ;
 
 data _null_ ;
        set _rate(where=(&target = 1)) ;
        call symput('r',percent) ;
 run ;
 
 data tt;
        set t_tmp(drop=_name_ _label_);
        if _1=. then _1=0;
 
        r_0 + _0;
        r_1 + _1;
 
        resp   =  _1 / (  _1 + _0);
        c_resp = r_1 / ( r_1 + r_0);
 
 
        lift   =  resp / (&r * 0.01) ;
        c_lift  = c_resp/(&r * 0.01) ;
 run;
 
 
 data _null_;
        set tt;
        call symput('tc',r_1);
 run;
 
 data t_f;
        set tt;
        capr = _1 / &tc ;
        c_capr = r_1 / &tc;
 run;
 
 options nodate;
 proc print data=t_f noobs;
        var bin _0 _1 r_1 resp capr lift c_resp c_capr c_lift;
 run;
%mend lift0 ;
%lift0(scorev);
%lift0(scoret);
