## Trends in Temperature-associated Mortality in São Paulo (Brazil) between 2000 and 2018: an Example of Disparities in Adaptation to Cold and Heat

**Abstract**: Exposure to non-optimal temperatures remains the single most deathful direct climate change impact to health. The risk varies based on the adaptation capacity of the exposed population which can be driven by climatic and/or non-climatic factors subject to fluctuations over time. We investigated temporal changes in the exposure–response relationship between daily mean temperature and mortality by cause of death, sex, age, and ethnicity in the megacity of São Paulo, Brazil (2000–2018). We fitted a quasi-Poisson regression model with time-varying distributed-lag non-linear model (tv-DLNM) to obtain annual estimates. We used two indicators of adaptation: trends in the annual minimum mortality temperature (MMT), i.e., temperature at which the mortality rate is the lowest, and in the cumulative relative risk (cRR) associated with extreme cold and heat. Finally, we evaluated their association with annual mean temperature and annual extreme cold and heat, respectively to assess the role of climatic and non-climatic drivers. In total, we investigated 4,471,000 deaths from non-external causes. We found significant temporal trends for both the MMT and cRR indicators. The former was decoupled from changes in AMT, whereas the latter showed some degree of alignment with extreme heat and cold, suggesting the role of both climatic and non-climatic adaptation drivers. Finally, changes in MMT and cRR varied substantially by sex, age, and ethnicity, exposing disparities in the adaptation capacity of these population groups. Our findings support the need for group-specific interventions and regular monitoring of the health risk to non-optimal temperatures to inform urban public health policies.


**Read our paper here:**

**Check our website here:** https://ainarb.github.io/climate_and_health/

### The study area: São Paulo (Brazil)

<img src=https://github.com/AinaRB/2021_RocaBarceloA_TrendsTempMort__Rcode/blob/a49f88e7677fe214bc9eec38ea64c316cfd2b71d/StudyArea.png>

### The data
**Mortality data:** We do not provide this data, yet it can be downloaded from the [Public Health System database (Sistema Único de Saúde, SUS)](https://datasus.saude.gov.br/mortalidade-desde-1996-pela-cid-10), run by the Ministry of Health of Brazil. A template with the structure and format of the mortality dataset needed to run the model is provided in "DataFormat.csv". Please, note that for the code to run, the mortality dataset should be saved in the same folder as the code. 

Alternatively, a synthetic dataset based on a modified version of the NMMAPS publicaly availble chicago data is provided in "SimulatedDF.csv". The code to create the synthetic data is provided in "00_SyntheticDataset.R". ##ATTENTION! ## This is NOT a real dataset and so, results should NOT be interpreted as being real. The purpose of this sythetic dataset is purely to illustrate the code functions.


**Meterological data**: Temperature and relative humidity data were obtained from the Institute of Astronomy Geophysics and Atmospheric Sciences and University of São Paulo (IAG-USP) meteorological station (coordinates: 23,6512°S, 46,6224°W; elevation: 799.2 m). Data was obtained hourly and averaged to obtain a daily mean. The data is open access and can be downloaded from the [IAG-USP platform](http://www.estacao.iag.usp.br/). 


### The model

In order to capture temporal changes in the temperature-mortality association we apply the time-varying distributed lag non-linear model (tv-DLNM) framework. Please, refer to [Gasparrini 2016 et al.](https://doi.org/10.1093/aje/kwv260) for a detailed description. The general specification of the model is defined below:
<p align="center"><i>
Y<sub>t,i</sub>~ quasiPoisson(μ<sub>t,i</sub>)  <br/>
Log(μ<sub>t,i</sub>) = α + cb<sub>temp,t</sub>  +  ∑<sup>7</sup><sub>k=1</sub>β<sub>k</sub>I(dow<sub>t</sub>=k)   βdow<sub>t</sub> + γ holidays<sub>t</sub> + δPM<sub>10lag0-2<sub>t</sub></sub> + ns(RH<sub>lag0-2<sub>t</sub></sub>, 3df) + ns(time<sub>t</sub>, 10df x year)  + cb<sub>temp,t</sub> x time<sub>t</sub>    <br/>
for t=1, ..., T=6,940  </p> </i>

where Y<sub>t</sub> denotes the mortality counts at day t, α is the intercept, and cbtemp is the bi-dimensional crossbasis matrix. The crossbasis function of daily mean temperature was composed of a natural cubic spline (ns) with one knot at the 75th percentile of the temperature distribution for the exposure dimension, and a ns with an intercept and three knots equally spaced over the log scale of the temperature distribution for the lag dimension. We set the maximum lag effect to expand over 21 days to capture the delayed effects and short-term harvesting.To eliminate potential confounding by time-varying factors, we included the following covariates: a time variable to account for long-term trends and seasonality modeled as a ns with 10 df per year [ns(time<sub>t</sub>, 10df x year)]; categorical variables for day-of-the-week [∑<sup>7</sup><sub>k=1</sub>β<sub>k</sub>I(dow<sub>t</sub>=k) ], and national and statewide holidays [γ holidays<sub>t</sub>], and the ns of the 2-day moving average of the daily mean RH with 3df [ ns(RH<sub>lag0-2<sub>t</sub></sub>, 3df)] and the 2-day moving average of the daily mean PM10 value [δPM<sub>10lag0-2<sub>t</sub></sub>].

Finally, to allow for time-varying exposure-lag-response associations, we added a linear interaction term between time and the temperature crossbasis variable [
cb<sub>temp,t</sub> x time<sub>t</sub>]. It represents the exposure-lag-response relationship at the centring point of the time variable used in the interaction term. Here, we were interested in annual changes in the temperature-lag-mortality association; thus, we used the central day of each year (1st July) to obtain equidistant estimates summarizing each year.


### The code
The code has to be run in the following order:
- 01_Master_Main_Model - Runs the main model for the results reported in the manuscript
- 02_Master_Sensitivty Analyses - Contains the sensitivity analyses as reproted in Table S2 of the manuscript
- 03_Figures_Manuscript - uses the exports of the previous file to generate the figures in the manuscript
- (DO NOT RUN)DLNM_source_code - is the source code called by 01 and 02 where the parametrization is defined

Download as a ZIP file using the green button Clone or download above.

## Cite us
