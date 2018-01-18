*Syntax to create a lookup indicator file for Shiny profile app.
*Jaime Villacampa January 18

cd '/conf/phip/Projects/Profiles/R Shiny/ScotPHO_profiles/data'.
Insert file = '/home/jamiev01/ScotPHO_pass.sps'.

GET DATA
  /TYPE=ODBC
  /CONNECT=!connect
  /SQL="select PI_P_ID profile_id,  PI_ID ind_id, PI_NAME indicator, PI_TYPE type_id, PI_HIGHER_NUMBER interpret, "+
      "PI_SUPPRESS supression, PI_SUPPRESS_LESS_THAN supress_less_than, PI_DEFINITION_TEXT definition, "
     " PI_DEFINITION_SOURCE source, PI_RANK_TREND_Y_AXIS_TEXT type_definition, PI_TREND_CHART_X_AXIS_TEXT time_period_label "
    "from SCOTPHO.TBL_PROFILE_INDICATORS T3 " 
   "where PI_P_ID in ('23')"
  /ASSUMEDSTRWIDTH=500.
CACHE.
EXECUTE.

recode interpret ('W'= 'L') ('B'= 'H').
execute.

*Maybe need to add profile name, domains and domain position.
*There is an issue with the definition of a couple of indicators, as it goes in different lines when saved.
*Don't know how to fix it, apart from manually.
save translate outfile = 'indicator_lookup.csv'
/type = csv /replace    /FIELDNAMES.


