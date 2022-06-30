## Exploring Optimal Vaccination Allocation
Coronavirus disease 2019 (COVID-19) is an infectious disease caused by severe acute respiratory syndrome coronavirus 2 (SARS-COV-2). It killed millions of people and isolated or quarantined hundreds of millions. The pandemic, despite its continued existence, is infecting and killing new people around the world every day.

In this project we utilize the SEIR (Susceptible-Exposed-Infectious-Removed) model to analyze the COVID-19 trends, and predict the trajectory for the future. Also, we will give the optimal parameter for the model.

Our shiny app provides users an overall understanding of COVID-19 Trend all over the world. Through a variety of interactions in the app, users can learn about the overall trend of the COVID-19 from the start. Besides, users can fit the SEIR model by selecting some initial values and then get the fitted plot as well as the optimal parameter.

The link to the app is [here](https://2560finalseirmodel.shinyapps.io/final/).

## Datasets
``` source
  Data/
  time_series_covid19_recovered_global.cav
  time_series_covid19_confirmed_global.csv
  time_series_covid19_deaths_global.csv

  ```

### Data Generating Process

COVID-19 Data Repository by the Center for Systems Science and Engineering (CSSE) at Johns Hopkins University: https://github.com/CSSEGISandData/COVID-19

This is the data repository for the 2019 Novel Coronavirus Visual Dashboard operated by the Johns Hopkins University Center for Systems Science and Engineering (JHU CSSE), supported by ESRI Living Atlas Team and the John Hopkins University Applied Physics Lab (JHU APL).

## Terms of Use:
This data set is licensed under the Creative Commons Attribution 4.0 International (CC BY 4.0) by the Johns Hopkins University on behalf of its Center for Systems Science in Engineering. Copyright Johns Hopkins University 2020.

## Packages
Require the following libraries: `shiny`, `ggthemes` for generating interactive dashboards; `tidyverse` for cleaning data and manipulating our datasets; `ggplot2` and `cowplot` for creating interactive visualizations;
`stringr` for dealing with some character strings; `reshape` for transforming some data; `knitr` for providing
text report for our simulations and statistical analysis, `deSolve` for solving the SEIR equations.

## Model setting and mehods

First, we processed the dataset to calculate the number of new daily cases in each country. And we used the SEIR model to estimate parameter values and give trending forecast of each 50-day new diagnosis in any selected country we. In order to improve the accuracy of parameter fitting and prediction, we used the data of 150 days at most for simulation, in which 0-50 days ahead was classified as the first group, 51-100 days ahead was the second group, 101-150 days ahead was the third group. We used the function Isoda( from `deSolve` package to solve our ODEs with given initial values.


## In-App
1. In tab **Current Trajectory**, users can choose a country and a cases type (confirmed, recover and death cases), the app will create a trajectory plot.

2. In tab **SEIR Model**, users can choose the country they want to research, set the E0 represented the
number of exposed people initially, choose the start time point to select a time that 150 days, 100 days
and 50 days before the newest day, and input the population of the country they want to study. And
the application will display a picture of the confirmed data in the future and a table of parameters in SEIR model, including contact _rate, transmission probability, infectious_period, latent _period, and the calculated beta = contact _rate * transmission_probability, gamma = 1 / infectious_ period, delta = 1 / latent period.

## Functions



```{r}
  data_processing(df)
 ```
Return a updated version of Covid-19 daily cases data

```{r}
  date_format (df_update)
 ```

Return a dataframe with format date for display
```{r}
seir_model (current_timepoint, state_values, parameters)
```
Return a desolve matrix with vector of time, susceptibles, exposes, infections, recovers. This function is
called in the soda to fit SEIR at 51 time points by modeling contact rate, transmission probability, infectious period, and latent period.
```{r}
parameter_func (var)
```
Return a vector of Beta, alpha, and gamma value. This function is called in apply function to calculate these three values in grid simulation.
```{r}
cost_function (param)
```
Return a value of mean square error. This function is called in apply function to get mean sqared error of each row of simulated Beta, alpha, and gamma parameters.
```{r}
optimal_func (df)
```
Return a tibble of optimal parameters(i.e., contact rate, transmission probability, infectious period, and latent period, Beta, alpha, and gamma) with minimal mean squared error. This function is called in the input for country tab to find the best parameters of contact rate, transmission probability, infectious period, and latent period fitting SIR model for the country.
```{r}
get_table (df)
```
Return the table of optimal parameters. The function is called to dispaly the table of optimal parameters in the shinyapp.
```{r}
country_func (start_time_point)
```
Return the dataframe of interested country at selected start time point. This function is called in country tab to get the data of interested country.
```{r}
initial_func (start_time_point,E0)
```
Return the initial value of susceptibles, infections, recovers at a selected start time point. This function is called to set intial values of our paramter for fitting model in interested time interval.
```{r}
plot_trajectory (df_show, country)
```
Returns a plot of the Covid-19 cases trajectory
