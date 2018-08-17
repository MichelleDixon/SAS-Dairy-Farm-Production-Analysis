data milk;
input month $ pounds;
cards;
1962-01 589
1962-02 561
1962-03 640
1962-04 656
1962-05 727
1962-06 697
1962-07 640
1962-08 599
1962-09 568
1962-10 577
1962-11 553
1962-12 582
1963-01 600
1963-02 566
1963-03 653
1963-04 673
1963-05 742
1963-06 716
1963-07 660
1963-08 617
1963-09 583
1963-10 587
1963-11 565
1963-12 598
1964-01 628
1964-02 618
1964-03 688
1964-04 705
1964-05 770
1964-06 736
1964-07 678
1964-08 639
1964-09 604
1964-10 611
1964-11 594
1964-12 634
1965-01 658
1965-02 622
1965-03 709
1965-04 722
1965-05 782
1965-06 756
1965-07 702
1965-08 653
1965-09 615
1965-10 621
1965-11 602
1965-12 635
1966-01 677
1966-02 635
1966-03 736
1966-04 755
1966-05 811
1966-06 798
1966-07 735
1966-08 697
1966-09 661
1966-10 667
1966-11 645
1966-12 688
1967-01 713
1967-02 667
1967-03 762
1967-04 784
1967-05 837
1967-06 817
1967-07 767
1967-08 722
1967-09 681
1967-10 687
1967-11 660
1967-12 698
1968-01 717
1968-02 696
1968-03 775
1968-04 796
1968-05 858
1968-06 826
1968-07 783
1968-08 740
1968-09 701
1968-10 706
1968-11 677
1968-12 711
1969-01 734
1969-02 690
1969-03 785
1969-04 805
1969-05 871
1969-06 845
1969-07 801
1969-08 764
1969-09 725
1969-10 723
1969-11 690
1969-12 734
1970-01 750
1970-02 707
1970-03 807
1970-04 824
1970-05 886
1970-06 859
1970-07 819
1970-08 783
1970-09 740
1970-10 747
1970-11 711
1970-12 751
1971-01 804
1971-02 756
1971-03 860
1971-04 878
1971-05 942
1971-06 913
1971-07 869
1971-08 834
1971-09 790
1971-10 800
1971-11 763
1971-12 800
1972-01 826
1972-02 799
1972-03 890
1972-04 900
1972-05 961
1972-06 935
1972-07 894
1972-08 855
1972-09 809
1972-10 810
1972-11 766
1972-12 805
1973-01 821
1973-02 773
1973-03 883
1973-04 898
1973-05 957
1973-06 924
1973-07 881
1973-08 837
1973-09 784
1973-10 791
1973-11 760
1973-12 802
1974-01 828
1974-02 778
1974-03 889
1974-04 902
1974-05 969
1974-06 947
1974-07 908
1974-08 867
1974-09 815
1974-10 812
1974-11 773
1974-12 813
1975-01 834
1975-02 782
1975-03 892
1975-04 903
1975-05 966
1975-06 937
1975-07 896
1975-08 858
1975-09 817
1975-10 827
1975-11 797
1975-12 843
1976-01 .
1976-02 .
1976-03 .
1976-04 .
1976-05 .
1976-06 .
1976-07 .
1976-08 .
1976-09 .
1976-10 .
1976-11 .
1976-12 .
;
run;

*Section 1: The following section of code is part of a time regression model assessment of the above dataset;

data sganno;
   retain function 'text' x1space 'datavalue' y1space 'datapercent' 
          rotate 90 anchor "right" width 30;
   set milk;
   label=month;
   xc1=month;
   y1=-5;
run;

*Is there a trend of the sales of the dietary 
supplement over time?;
proc sgplot data=milk sganno=sganno pad=(bottom=15%); 
series x=month y=pounds / markers;
xaxis display=(nolabel novalues);
run;
*There is an increasing linear trend;

*Appropriate model for the sales of 
the dietary supplement:
There is constant long-run growth, so:
y(t) = TR(t) + e(t) --> y(t) = Beta_0 + Beta_1*t + e(t)


*Accounting for seasonal variation with dummy variables;
%macro dumvar;
data milk3;
set milk;
time=_n_;
month2 = SUBSTR(month,6,2)*1; %do i=1 %to 11;
Mdum&i. = 0;
if month2=&i. then Mdum&i.=1; %end;
run;
%mend;
%dumvar;

proc reg data=milk3 plots=none;
model pounds = time Mdum1-Mdum11; 
run;

*pounds(hat) = 565.50137 + 1.72776*time + 25.07682M1 - 14.43666M2 + 78.04986M3 + 92.89353M4 + 153.88005M5 + 125.50943M6 + 75.71024M7 + 33.41105M8 - 9.17386M9 - 5.68733M10 - 36.70081M11
;

*Are any of the regression assumptions violated?;
proc reg data=milk3 plots(only)=(ResidualbyPredicted QQPlot ResidualPlot);
model pounds = time Mdum1-Mdum11 / dwprob;
run;
*It seems the independence assumption is violated.;

proc reg data=milk3 plots = none;
model pounds = time Mdum1-Mdum11/ dwprob;
run;
*Through Durbin Watson test it was found there is strong positive first-order autocorrelation.;


*What is appropriate model?;
*Linear trend, constant seasonal variation with positive first order autocorrelation;
*pounds(t) = TR(t) + SN(t) + error(t)
		   = Beta0 + Beta1*time + Beta2*M1 + ... + Beta12*M11 + error(t)
		   					where e(t) = phi(1)error(t-1) + a(t)
;



*QUESTION: What is the prediction equation for the most appropriate model?;
proc arima data=milk3 plots=none;
identify var=pounds crosscor=(time Mdum1 Mdum2 Mdum3 Mdum4 Mdum5 Mdum6 Mdum7 Mdum8 Mdum9 Mdum10 Mdum11) noprint;
estimate input=(time Mdum1 Mdum2 Mdum3 Mdum4 Mdum5 Mdum6 Mdum7 Mdum8 Mdum9 Mdum10 Mdum11) p=(1); 
run;
*yhat(t) = 564.39290 + 1.72881*time + 24.41782*M1 - 14.94607M2 + 77.66814M3 + 92.61943M4 + 153.69507M5 + 125.39636M6 + 75.65291M7 + 33.39412M8 - 9.16514M9 - 5.66734M10 - 36.68371M11 + .88737*errorhat(t-1)
All of the variables t values are significant with the new changes;


*Checking if any regression assumptions are violated;
proc arima data=milk3 plots(only)=Residual(QQ SMOOTH); 
identify var=pounds crosscor=(time Mdum1 Mdum2 Mdum3 Mdum4 Mdum5 Mdum6 Mdum7 Mdum8 Mdum9 Mdum10 Mdum11) noprint;
estimate input=(time Mdum1 Mdum2 Mdum3 Mdum4 Mdum5 Mdum6 Mdum7 Mdum8 Mdum9 Mdum10 Mdum11) p=(1); forecast out=milk3pred lead=0 noprint;
run;
quit;
proc sgplot data=milk3pred; 
scatter y=residual x=forecast; 
run;
*With the adjustment for autocorrelation the positive autocorrelation is no longer present, the rest of the assumptions hold;



*Prediction intervals over the next year;
proc arima data=milk3 plots=none;
identify var=pounds crosscor=(time Mdum1 Mdum2 Mdum3 Mdum4 Mdum5 Mdum6 Mdum7 Mdum8 Mdum9 Mdum10 Mdum11) noprint;
estimate input=(time Mdum1 Mdum2 Mdum3 Mdum4 Mdum5 Mdum6 Mdum7 Mdum8 Mdum9 Mdum10 Mdum11) p=(1); forecast lead=12 interval=month id=date out=milk3pred2;
run; 
quit;
data milk3pred2; 
set milk3pred2; 
plen = U95-L95; 
run;

*-----------------------------------------------------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------------------------------------------------------;

*Section 2: The following section of code is part of a Box Jenkins model assessment of the above dataset;


data sganno;
   retain function 'text' x1space 'datavalue' y1space 'datapercent' 
          rotate 90 anchor "right" width 30;
   set milk;
   label=month;
   xc1=month;
   y1=-5;
run;
proc sgplot data=milk sganno=sganno pad=(bottom=15%); 
series x=month y=pounds / markers;
xaxis display=(nolabel novalues);
run; 

proc arima data=milk plots(only)=series(ACF); 
identify var=pounds;
run;

*To observe whether the series of first differences seem stationary;
data boxmilk2; 
set milk;
z = dif1(pounds); *creates variable of first differences;
run;

proc sgplot data=boxmilk2 sganno=sganno pad=(bottom=15%); 
series x=month y=z / markers;
xaxis display=(nolabel novalues);
run; 
*This makes the data almost stationary, slightly increasing;

proc arima data=boxmilk2 plots(only)=series(ACF); 
identify var=z;
run;


*for second differences:;
data boxmilk3; 
set boxmilk2; 
z2 = dif1(z); 
*The second differences are the first differences OF the first differences;
run;

proc sgplot data=boxmilk3 sganno=sganno pad=(bottom=15%); 
series x=month y=z2 / markers;
xaxis display=(nolabel novalues);
run;
*Seems stationary;

proc arima data=milk plots(only)=series(ACF PACF); 
identify var=pounds(1,1); *produces second differences;
run;
*(SAC dies down a  bit more but still slowly. SPAC dies down fairly slowly
spikes:
SAC: 1, 6, 7?, 11, 12, 13, 24, 25
SPAC: 1, 9?, 11, 12, 13
;


*---------------------------------Only use below code for box Jenkins-----------------------------------------------------------------------------;

data sganno;
   retain function 'text' x1space 'datavalue' y1space 'datapercent' 
          rotate 90 anchor "right" width 30;
   set milk;
   label=month;
   xc1=month;
   y1=-5;
   m2 = substr(month,6,2);
   if m2 in (1,2,4,5,7,8,10,11) then label='';
run;

*Does the series of energy generation have constant seasonal variation?;
proc sgplot data=milk sganno=sganno pad=(bottom=15%); 
series x=month y=pounds / markers;
xaxis display=(nolabel novalues);
run;
*yes;

*Is the series of energy generation stationary?;
proc arima data=milk plots(only) = series(acf); 
identify var=pounds;
run;
*Seasons are months, so L=12.
Series is not stationary.
The SAC does not cut off or die down quickly at the nonseasonal level
The SAC does not cut off or die down fairly quickly at the seasonal level;

proc arima data=milk plots(only) = series(acf); 
identify var=pounds(1); *first Regular differencing;
identify var=pounds(12); *first seasonal differencing;
identify var=pounds(1,12); *first regular and seasonal differencing;
run;
*the series of first regular and seasonal differences are stationary.
the SAC dies down fairly quickly at the nonseasonal level
additionally the SAC cuts off at the seasonal level
;


proc arima data=milk plots(only) = series(acf pacf); 
identify var=pounds(1,12);
run;
*The SPAC cuts off at p=1 and dies down quicker at the nonseasonal level
The SAC cuts off at Q=12 and dies down quicker at the nonseasonal level;
;

*tentatively identified model:
z(t) = [y(t) - y(t-1)] - [y(t-12) - y(t-11)] 
Step 1: nonseasonal model ==> z(t) = delta + phi_1*z(t-1) + a(t)
Step 2: seasonal model ==> z(t) = delta + a(t) - theta_1,L*a(t-L)
Step 3: combined final ==> z(t) = delta + phi_1*z(t-1) - theta_1,L*a(t-L) + a(t)
;

*Checking if the currently identified model can be improved;
proc arima data=milk plots(only)=residual(acf pacf); 
identify var=pounds(1,12);
estimate p=(1) q=(12) noconstant;
run;
*No improvements can be made, there are no spikes in the RSAC or RSPAC;

*Checking if the residuals of the temporarily identified model 
satisfy the assumptions of the random shock;
proc arima data=milk plots(only)=residual(qq smooth); 
identify var=pounds(1,12);
estimate p=(1) q=(12) noconstant;
run;
*No violations in the assumptions of the random shock;


*Forecasting the following 12 months after the data set ended;
proc arima data=milk plots(only)=forecast(forecast); 
identify var=pounds(1,12);
estimate p=(1) q=(12) noconstant;
forecast lead=12;
run;
