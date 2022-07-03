#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

#load library----------------------------
library(cowplot)
library(dplyr)
library (deSolve)
library(ggplot2)
library(shiny)
library(tidyverse)
library(ggthemes)
library(stringr)
library(reshape)
library(knitr)


#load datasets----------------------------
Recover_Daily <- read.csv('https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv')
#Recover_Daily <- read.csv("./time_series_covid19_recovered_global.csv")

Confirmed_Daily <- read.csv('https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv')
#Confirmed_Daily <- read.csv("./time_series_covid19_confirmed_global.csv")

Death_Daily <- read.csv('https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv')
#Death_Daily <- read.csv("./time_series_covid19_deaths_global.csv")


#storage of some variables----------------------------

country_list <- unique(Confirmed_Daily$Country.Region)

#data preprocessing function----------------------------

data_processing <- function(df){
  #' data preprocessing
  #' @df Covid-19 cases dataset
  #' return a updated version of dataset
  
  df_update <- df %>%
    select(-c("Province.State","Lat","Long")) %>%
    group_by(Country.Region) %>%
    summarise_each(funs(sum))
  
  #change cumulative cases to daily cases
  df_update[,-c(1:2)] <- t(apply(df_update[,-1], 1, diff))
  
  return(df_update)
  
}

date_format <- function(df_update){
  #' change the date format and select one country
  #' @df_update Updated Covid-19 cases dataset
  #' @country country selected by users
  #' return an dataset with standardized date in selected country
  
  #transpose the dataset
  rownames(df_update) <- t(df_update[,1])
  df_show <- df_update %>%
    t() %>%
    as.data.frame()
  df_show <- df_show[-1,]
  
  #date standardization
  df_show$time <- as.character(rownames(df_show))
  for (i in 1:nrow(df_show)) {
    df_show$time[i] <- substr(df_show$time[i], 2, nchar(df_show$time[i]))
  }
  df_show$date <- as.Date(as.vector(df_show$time), "%m.%d.%y")
  
  return(df_show)
  
}


#main functions----------------------------

#setting values
timepoints <- seq(0, 50, by=1)

seir_model = function (current_timepoint, state_values, parameters){
  #' get the mathmatical formulas of describing epidemic cases
  #' @current_timepoint vector a sequence of time points
  #' @state_values vector of susceptibels, exposed, infections,and recovers value.
  #' @parameters vector of beta, delta, and gamma
  #' return a list of mathematical algorithm
  
  # create state variables (local variables)
  S = state_values [1]        # susceptibles
  E = state_values [2]        # exposed
  I = state_values [3]        # infectious
  R = state_values [4]        # recovered
  
  with ( 
    as.list (parameters),     # variable names within parameters can be used 
    {
      # compute derivatives
      dS = (-beta * S * I)
      dE = (beta * S * I) - (delta * E)
      dI = (delta * E) - (gamma * I)
      dR = (gamma * I)
      
      # combine results
      results = c (dS, dE, dI, dR)
      list (results)
    }
  )
}


parameter_funct<-function(var)
{
  #' get the values of beta gamma and delta given the specific contact_rate, transmission_probability, infectious_period and latent_period
  #' @var numeric values
  #' return the numeric values of gamma, delta, and beta
  contact_rate<-var[1]
  transmission_probability<-var[2]
  infectious_period<-var[3]
  latent_period<-var[4]
  beta_value <- contact_rate * transmission_probability
  gamma_value <- 1 / infectious_period
  delta_value <- 1 / latent_period
  #Ro = beta_value / gamma_value
  value<-c(beta=beta_value, gamma=gamma_value, delta=delta_value)
  return(value)
}


get_table<-function(df){
  #' get a table of optimal parameter
  #' @df dataset
  #' return a table optimal paratmeter "contact_rate"	"transmission_probability",
  #' "infectious_period",	"latent_period", "beta",	"gamma	delta	MSE"
  min<-df[which.min(df$MSE),]
  return(min[1:7])
}

optimal_func<-function(df){
  #' get the optiaml values which is find the combination of paramters minimize our mean squared error
  #' @df dataframe
  #' return numeric values
  optimal<-df[which.min(df$MSE),]
  optimal_param<-optimal[c("beta", "gamma","delta")]
  return(optimal_param)
}

#interface functions----------------------------
plot_trajectory <- function(df_show, country){
  #' show the trajectory of Covid-19 cases
  #' @df_show updated Covid-19 cases dataset
  #' @country country selected by users
  #' return a plot showing cases trend
  
  df_plot <- df_show %>% 
    select(country, date)
  
  colnames(df_plot) <- c("cases", "date")
  
  df_plot$cases <- as.numeric(as.character(df_plot$cases))
  
  #show the trajectory
  trajectory <- ggplot(aes(x = date, y = cases), 
                       data = df_plot) +
    geom_line(color = "red", lwd = 0.8) +
    ylim(0,max(df_plot$cases)+5000) +
    labs(x = "Date", y = "daily cases") +
    theme_fivethirtyeight()
  
  return(trajectory)
  
}

# define UI for application----------------------------
ui <- fluidPage(
  
  # Application title
  titlePanel("Covid-19 Trajectory and SEIR Model Simulation"),
  
  navbarPage("Content",
    
    tabPanel("Current Trajectory",
             
    # Sidebar with a slider input for number of bins 
      sidebarLayout(
    
        # Sidebar panel for inputs
        sidebarPanel(
          
          # Input: Select the country
          selectInput(inputId = "country",
                      label = "Select a country",
                      choices = as.list(country_list)),
          
          # Input: Select the dataset
          selectInput(inputId = "dataset",
                      label = "Select daily cases type",
                      choices = list(Confirmed = "Confirmed_Daily", 
                                     Deaths = "Death_Daily",
                                     Recover = "Recover_Daily")),
          
          submitButton(text = "Show trajectory")
          
        ),
        
        mainPanel(
          # Show current cases trajectory 
          plotOutput("currentPlot")
        )
     ),
   ),
   
   tabPanel("SEIR Model",
            
      # Sidebar with a slider input for number of bins 
      sidebarLayout(
        
        # Sidebar panel for inputs
        sidebarPanel(
          
          # Input: Set initial value for the model
          selectInput(inputId = "country",
                      label = "Select a country",
                      choices = as.list(country_list),
                      selected = "US"),
          sliderInput(inputId = "E0",
                      label = "Set E0",
                      min = 1,
                      max = 10,
                      value = 10),
          radioButtons(inputId = "stp",
                      label = "Set start time point",
                      choices = c("one" = "1", "two" = "2", "three" = "3"),
                      selected = 1),
          textInput(inputId = "Pop_N",
                    label = "Input Population",
                    value = "30000",
                    placeholder = "30000"
                    ),
          
          submitButton(text = "Fit your model!")
        ),
        
        mainPanel(
          # show prediction trajectory
          plotOutput("predictPlot"),
          # show a table optimal parameter
          tableOutput("optimalTable")
          
          
        )
      ),
    ),
   
   tabPanel("Reference",
            
            br(),
            fluidRow(
              column(8,
                     h4("Reference:"),
                     p("Our data is mainly webscraped from: ", 
                       em(a("https://github.com/CSSEGISandData/COVID-19")),", ",
                       "All data usage complies with respective terms of use."
                     )),
              column(4,
                     h4("Contact Us:"),
                     p("Yufei Yan: ",
                       em(a("yufei_yan@brown.edu", href="mailto:yufei_yan@brown.edu"))),
                     p("Yanru Liao: ",
                       em(a("yanru_liao@brown.edu", href="mailto:yanru_liao@brown.edu"))),
                     p("Zexuan Yu: ",
                       em(a("zexuan_yu@brown.edu", href="mailto:zexuan_yu@brown.edu"))),
              )
            )
            
   )
  )
)

# define server logic----------------------------
server <- function(input, output) {
  
  run_model <- reactive({
    
    #import dataset to fit the model
    country_confirmed <- data_processing(Confirmed_Daily) %>% filter(Country.Region==input$country)
    country_recover <- data_processing(Recover_Daily) %>% filter(Country.Region==input$country)
    
    #setting by users
    E0<-as.numeric(input$E0)
    stp<-as.numeric(input$stp)
    Pop_N<-as.numeric(input$Pop_N)
    
    initial_func<-function(start_time_point, E0){
      #' get the initial values of S0, E0,I0,and R0
      #' @start_time_point character 
      #' @E0 numeric value of initial value of exposes
      #' return vector
      
      #the first period 
      if (start_time_point==1){
        country_first<-country_confirmed[,(length(country_confirmed)-50):length(country_confirmed)]
        recover_first<-country_recover[,(length(country_recover)-50):length(country_recover)]
        I0_first<-country_first[1]/10000
        R0_first<-recover_first[1]/10000
        S0_first<-Pop_N-R0_first-I0_first/10000
        initial_values<-c(S0_first,E0, R0_first,I0_first) 
        #the second period
      } else if(start_time_point==2){
        country_second<-country_confirmed[,(length(country_confirmed)-100):(length(country_confirmed)-50)]
        recover_second<-country_recover[,(length(country_recover)-100):(length(country_recover)-50)]
        I0_second<-country_second[1]/10000
        R0_second<-recover_second[1]/10000
        S0_second<-Pop_N-R0_second-I0_second/10000
        initial_values<-c(S0_second,E0, R0_second,I0_second) 
        #the second period
      }  else if(start_time_point==3){
        country_third<-country[,(length(country_confirmed)-150):(length(country_confirmed)-100)]
        recover_third<-country_recover[,(length(country_recover)-150):(length(country_recover)-100)]
        I0_third<-country_third[1]/10000
        R0_third<-recover_third[1]/10000
        S0_third<-Pop_N-R0_third-I0_third/10000
        initial_values<-c(S0_third,E0, R0_third,I0_third)
      }
      return(initial_values)
    }
    
    #set initial values
    initial_values<-as.numeric(initial_func(stp, E0))
    
    #get actual cases
    country_func<-function(start_time_point){
      #' get the data of interested country
      #' @start_time_point character 
      #' return dataframe
      if (start_time_point==1){
        country_update<-country_confirmed[,(length(country_confirmed)-50):length(country_confirmed)]
      }else if (start_time_point==2){
        country_update<-country_confirmed[,(length(country_confirmed)-100):(length(country_confirmed)-50)]
      } else if(start_time_point==3){
        country_update<-country_confirmed[,(length(country_confirmed)-150):(length(country_confirmed)-100)]
      }
      return(country_update)
    }
    
    country_51<-country_func(stp)
    
    #grid search
    para_simu<-expand.grid(contact_rate<-c(5,6,7,8,9,10),
                           transmission_probability<-seq(0.01,0.07,0.01),
                           infectious_period<-c(5,6,7,8,9,10),
                           latent_period<-c(7,8,9,10,11,12,13,14))
    
    parameter_list<-t(round(apply(as.matrix(para_simu), 1, parameter_funct),4))
    colnames(parameter_list) <- c("beta","gamma","delta")
    
    #calculate MSE of each combinations of four parameters
    
    cost_function<-function(param){
      #' get the mean squared error of SEIR model parameters
      #' @param numeric values of contact_rate transmission_probability infectious_period latent_period
      #' return the numeric value 
      fit<-lsoda(initial_values, timepoints, seir_model, param)
      infectious<-fit[,4]
      error<-infectious-as.numeric(country_51)
      mse<-mean((error^2))
      return(mse)
    }
    
    MSE<-apply(parameter_list, 1, cost_function)
    
    #combine our paramters and mathematical variables(i.e., beta, gamma, and delta) with the MSE
    parameter<-as.data.frame(cbind(para_simu, parameter_list,MSE))
    colnames(parameter)[1:4]<-c("contact_rate","transmission_probability", "infectious_period", "latent_period")
   
    return(parameter)
  })
  
  # You should define all the rendered output in the server
  output$currentPlot <- renderPlot({
    
    #select different dataset
    if(input$dataset == "Recover_Daily"){
      df <- data_processing(Recover_Daily)
    } else if (input$dataset == "Confirmed_Daily"){
      df <- data_processing(Confirmed_Daily)
    } else if (input$dataset == "Death_Daily"){
      df <- data_processing(Death_Daily)
    }
    
    return(plot_trajectory(date_format(df), input$country))
  })
  
  output$predictPlot <- renderPlot({
    
    #import dataset to fit the model
    country_confirmed <- data_processing(Confirmed_Daily) %>% filter(Country.Region==input$country)
    country_recover <- data_processing(Recover_Daily) %>% filter(Country.Region==input$country)
    
    #setting by users
    E0<-1700*(input$E0)
    stp<-as.numeric(input$stp)
    Pop_N<-as.numeric(input$Pop_N)
    
    initial_func<-function(start_time_point, E0){
      #' get the initial values of S0, E0,I0,and R0
      #' @start_time_point character 
      #' @E0 numeric value of initial value of exposes
      #' return vector
      
      #the first period 
      if (start_time_point==1){
        country_first<-country_confirmed[,(length(country_confirmed)-50):length(country_confirmed)]
        recover_first<-country_recover[,(length(country_recover)-50):length(country_recover)]
        I0_first<-country_first[1]
        R0_first<-recover_first[1]
        S0_first<-Pop_N-R0_first-I0_first
        initial_values<-c(S0_first,E0, R0_first,I0_first) 
        #the second period
      } else if(start_time_point==2){
        country_second<-country_confirmed[,(length(country_confirmed)-100):(length(country_confirmed)-50)]
        recover_second<-country_recover[,(length(country_recover)-100):(length(country_recover)-50)]
        I0_second<-country_second[1]
        R0_second<-recover_second[1]
        S0_second<-Pop_N-R0_second-I0_second
        initial_values<-c(S0_second,E0, R0_second,I0_second) 
        #the second period
      }  else if(start_time_point==3){
        country_third<-country[,(length(country_confirmed)-150):(length(country_confirmed)-100)]
        recover_third<-country_recover[,(length(country_recover)-150):(length(country_recover)-100)]
        I0_third<-country_third[1]
        R0_third<-recover_third[1]
        S0_third<-Pop_N-R0_third-I0_third
        initial_values<-c(S0_third,E0, R0_third,I0_third)
      }
      return(initial_values)
    }
    
    #set initial values
    initial_values<-as.numeric(initial_func(stp, E0))
    
    #run the model with optimal parameter
    parameter <- run_model()
    optimal_parameter<-optimal_func(parameter)
    df_predict <- as.data.frame(lsoda(initial_values, timepoints, seir_model, optimal_parameter))
    
    #plot the fitted cases trend
    plot_predict <- ggplot() +
      geom_line(aes(x = time, y = df_predict[,4]), 
                data = df_predict, color = "blue", lwd = 0.8) +
      labs(x= "time", y = "cases") +
      theme_fivethirtyeight()
    
    return(plot_predict)
    
  })
  
  output$optimalTable <- renderTable({
    
    #show the table of optimal parameter
    parameter <- run_model()
    table <- get_table(parameter)
    
    return(table)
    
  })
  
}

# run the application----------------------------
shinyApp(ui = ui, server = server)