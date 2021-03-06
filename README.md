# 2021_RocaBarceloA_Rcode
## Trends in temperature-mortality association in Sao Paolo 2000-2018

Exploration of the temporal variations in the temperature-mortality association by gender, age, and ethnic group, in the municipality of São Paulo, Brazil, between 2000 and 2018. We fitted a time varying distributed lag non-linear model (tv-DLNM)11 to assess the association between daily mean temperature and mortality while allowing for time-varying exposure-response and lag-response associations. 

This code is used in:
[PAPER]


### The code
The code has to be run in the following order:
- 01_Master_Main_Model - Runs the main model for the results reported in the manuscript
- 02_Master_Sensitivty Analyses - Contains the sensitivity analyses as reproted in Table S2 of the manuscript
- 03_Figures_Manuscript - uses the exports of the previous file to generate the figures in the manuscript
- (DO NOT RUN)DLNM_source_code - is the source code called by 01 and 02 where the parametrization is defined



Download as a ZIP file using the green button Clone or download above.

### The data
Mortality counts should be saved in the same folder as the code. We do not provide this data, yet it can be downloaded from the [Public Health System database (Sistema Único de Saúde, SUS)](https://datasus.saude.gov.br/mortalidade-desde-1996-pela-cid-10), run by the Ministry of Health of Brazil. Alternatively, if you would like a file containing simulated numbers that allow you to test the code, please contact a.roca-barcelo@imperial.ac.uk .

A template with the structure and format of the mortality dataset needed to run the model is provided in "DataFormat.csv". Additionally, a synthetic dataset based on a modified version of the NMMAPS publicaly availble chicago data is provided in "SimulatedDF.csv". The code to create the synthetic data is provided in "00_SyntheticDataset.R".

##ATTENTION! ##
This is NOT a real dataset and so, results should NOT be interpreted as being real. The purpose of this sythetic dataset is purely for illustrative of the code functions.

### The model

The general specification of the model is defined below:
<p align="center"><i>
Y<sub>t,i</sub>~ quasiPoisson(μ<sub>t,i</sub>)  <br/>
Log(μ<sub>t,i</sub>) = α + cb<sub>temp,t</sub>  +  βdow<sub>t</sub> + γ holidays<sub>t</sub> + δPM<sub>10lag0-2<sub>t</sub></sub> + ns(RH<sub>lag0-2<sub>t</sub></sub>, 3df) + ns(time<sub>t</sub>, 10df x year)  + cb<sub>temp,t</sub> x time<sub>t</sub>    <br/>
for t=1, ..., T=6,940  </p> </i>

Where Y<sub>t,i</sub> denotes the daily mortality counts at day t for population group i; *α* is the intercept; cb<sub>temp,t</sub> is the bi-dimensional cross basis matrix; *dow* is the day-of-the-week; *holidays* is a binary indicator for the national and estate wide holidays for São Paulo during the years studied; *ns* refers to the natural cubic spline of time used to adjust for seasonality and long-term trends, and *time* is the time index with centring at 1<sup>st</sup> August of each year.Finally, cb<sub>temp<sub>t</sub></sub> ×time  is the interaction term between time and the cb of temperature variable, using August 15<sup>th</sup>  of each year as the centring point.



