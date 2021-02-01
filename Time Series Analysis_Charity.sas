/*Time Series Analysis in SAS - A Pred Mod Demo*/

/*Problem stmt: Following is the Sales data for certain period which is created explicitly, predict the sales
for next 10 months*/

Data past;
Input date :monyy. Sales @@; /*@@ - if multiple inputs are written in single row, this will help  inputs as pair 
                                    which is part of one row/observation. If we have more than 2 vars also
                                    write all the var names before @@ to treat them as single row*/
Format date monyy.; /*To write data in this format in the o/p*/
Datalines;
Jul-89  9.5161
Aug-89  9.6994
Sep-89  9.2644
Oct-89  9.683
Nov-89  10.8635
Dec-89  9.9005
Jan-90  10.2375
Feb-90  10.694
Mar-90  10.629
Apr-90  11.0332
May-90  11.027
Jun-90  11.4165
Jul-90  11.2913
Aug-90  11.3475
Sep-90  11.2913
Oct-90  11.3771
Nov-90  11.5457  
Dec-90  11.6433
Jan-91  11.9293
Feb-91  11.9752
Mar-91  11.9283
Apr-91  11.8995
May-91  12.0419
Jun-91  12.3537
Jul-91  12.4546
;
run;

proc print data=past;
run;

/*Time series Analysis Model*/
proc forecast data=past out=PredSales lead=10 interval=month;
var sales;
id date;
run;

proc print data=PredSales;
run;