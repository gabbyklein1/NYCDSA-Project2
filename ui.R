#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(plyr)

sidebar =
    dashboardSidebar(
    sidebarMenu(
        
        menuItem("Overview", tabName = "overview" ),
        menuItem("Attribute Exploration", tabName = "attributes"),
        menuItem("Logistic Regression",  tabName = "logisticregression")
        
    )
)

body <- dashboardBody(
    tabItems(
        tabItem(tabName = "attributes",
                h1('Get to Know the Dataset\'s Attributes'),
               
                sidebarLayout(
                    sidebarPanel(
                        selectInput("facat",
                                    "Attribute:",
                                    c("GENDER",'INCOME',"TRAVTIME","HOMEKIDS","PARENT1","MSTATUS","KIDSDRIV", "EDUCATION" ,'EDUCATIONBINARY','OLDCLAIMBINARY','KIDSDRIVBINARY','HOMEBINARY',"OCCUPATION" ,"CAR_USE" ,"CAR_TYPE","RED_CAR","REVOKED" , "URBANICITY" ,"AGE","YOJ","BLUEBOOK","TIF"  ,"MVR_PTS","CAR_AGE")),
                         #checkboxInput('woutliers', 'See Outliers', value = FALSE, width = NULL)
                        ),
                    
                    # Show a plot of the generated distribution
                    mainPanel(splitLayout(
                                          plotOutput("countplot"),
                                          plotOutput("claimplot1")
                    )
                    ))),
                #,
                # sidebarLayout(
                #     sidebarPanel(
                #         selectInput("facatcts",
                #                     "Attribute:",
                #                     c("AGE","YOJ","INCOME","HOME_VAL","TRAVTIME","BLUEBOOK","TIF" ,"OLDCLAIM" ,"MVR_PTS","CAR_AGE")),
                #         checkboxInput('woutlierscts', 'See Outliers', value = FALSE, width = NULL)),
                #     
                #     # Show a plot of the generated distribution
                #     mainPanel(splitLayout(
                #         plotOutput("ctsplotscatter"),
                #         plotOutput("ctsplothist")
                #         
                #     )
                #     ))
                
        
        tabItem(tabName = "overview",
                h2("Introduction to the Dataset"),
                'This project analyses a car insurance dataset found on Kaggle. The dataset includes attributes of claims received including personal details about the driver and whether the claim resulted from an accident. The goal is to use the provided information about the driver to assess what characteristics make a person more likely to have had a crash. This type of analysis would be useful for an insurance company when determining customer risk and pricing. ',
                h3("Attributes"),
                htmlOutput("text")
                
        ),
        tabItem(tabName = "logisticregression",
                h2("Logistic Regression"),
                sidebarLayout(
                    sidebarPanel(
                        checkboxGroupInput("checkGroup", label = h3("Attribute to Include:"), 
                                           choices = c("GENDER",'INCOME',"TRAVTIME","HOMEKIDS","PARENT1","MSTATUS","KIDSDRIV", "EDUCATION" ,'EDUCATIONBINARY','OLDCLAIMBINARY','KIDSDRIVBINARY','HOMEBINARY',"OCCUPATION" ,"CAR_USE" ,"CAR_TYPE","RED_CAR","REVOKED" , "URBANICITY" ,"AGE","YOJ","BLUEBOOK","TIF"  ,"MVR_PTS","CAR_AGE"),
                                           selected = "GENDER")),
                    
                    # Show a plot of the generated distribution
                    mainPanel(verbatimTextOutput("summary"),verbatimTextOutput("expcoefs")
                    )
                ),sidebarLayout(
                    sidebarPanel(
                        sliderInput("obs", "Cutoff for Classifying a 1:",
                                    min = 0, max = 1, value = .5
                        )),
                    
                    # Show a plot of the generated distribution
                    mainPanel(splitLayout(  verbatimTextOutput("confusionmat")
                    ))
                ) )
))


# Put them together into a dashboardPage
dashboardPage(
    dashboardHeader(title = "Menu"),
    sidebar,
    body)


