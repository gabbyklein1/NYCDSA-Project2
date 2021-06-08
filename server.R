#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(plyr)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    
    output$distPlot <- renderPlot(
        {if(input$woutliers){ylimit=120000
        }else{ylimit=10000}
        ggplot((df), aes(x=get(input$facat),y = CLM_AMT))  + geom_boxplot(aes(group = get(input$facat)))+theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
        ggtitle('Claim Amount vs Selected Attribute') +
        xlab('Factor Attribute')+ # for the x axis label
        ylab('Claim Amount')+
        coord_cartesian(ylim = c(0, ylimit))})
   
    output$ctsplothist <- renderPlot({if(input$woutlierscts){dflimited=df
    }else{dflimited=df[(df$CLM_AMT<summary(df$CLM_AMT)[[5]]) & (df$CLM_AMT>summary(df$CLM_AMT)[[2]]),]}
        ggplot(dflimited, aes(x=get(input$facatcts))) + 
            geom_histogram()+
            ggtitle('Claim Amount vs Selected Attribute') +
            xlab('Factor Attribute')+ # for the x axis label
            ylab('Claim Amount')})
    
    output$countplot <- renderPlot({
        if((input$facat)=='INCOME'|(input$facat)=='HOMEVAL'|(input$facat)=='BLUEBOOK'|(input$facat)=='AGE'){
            ggplot(df, aes(x=get(input$facat))) + 
                geom_histogram()+
                ggtitle('Count of Attribute') +
                xlab('Attribute')+ # for the x axis label
                ylab('Count Amount')
        }else{
        ggplot(df, aes(x=get(input$facat)))+theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+ 
            geom_bar()+ggtitle('Count of Selected Attribute') +
            xlab('Attribute')+
            ylab('Count')
            }})
    output$claimplot <- renderPlot(
        {t=df%>% 
            group_by_(input$facat)%>%
            summarise_each(funs(mean),CLAIM_FLAG)
        
        ggplot(t) + geom_bar(aes(x=get(input$facat),y=CLAIM_FLAG), stat="identity")+theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
                })
    output$claimplot1 <- renderPlot({
        if((input$facat)=='INCOME'|(input$facat)=='HOMEVAL'|(input$facat)=='BLUEBOOK'|(input$facat)=='AGE'|(input$facat)=='TRAVTIME'|(input$facat)=='HOME_VAL'){
            ddf=df
            ddf$CLAIM_FLAG=as.factor(df$CLAIM_FLAG)
            ggplot(ddf, aes(x = CLAIM_FLAG,y = get(input$facat)))  + geom_boxplot(aes(group = CLAIM_FLAG))+
            ggtitle('Atrribute for Crash and No Crash Groups') +
            xlab('Crash n/y')+ # for the x axis label
            ylab('Attribute Values')
    }else{t=df%>% 
            group_by_(input$facat)%>%
            summarise_each(funs(mean),CLAIM_FLAG)
        
        ggplot(t) + geom_bar(aes(x=get(input$facat),y=CLAIM_FLAG), stat="identity")+theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
        ggtitle('Crash Proportion for Attribute ') +
            xlab('Attribute')+ # for the x axis label
            ylab('Crashes')
        }
    })
    output$text <- renderText({
        paste('GENDER- 0=Female,1=Male',
                'INCOME- Dollar amount, yearly earnings',
                'TRAVTIME- Time travel to work',
                'HOMEKIDS- How many kids at home',
                'PARENT1- 0=No, 1=Yes',
                'MSTATUS-0=No, 1=Yes',
                'KIDSDRIV-Number of driving age kids ',
                'EDUCATION- five levels from no high school degree to PhD',
                'EDUCATIONBINARY-Do they have a bachelor\'s or above',
                'OLDCLAIMBINARY-Have they had a previous claim',
                'KIDSDRIVBINARY-Any driving age children 0=No, 1=Yes', 
                'HOMEBINARY- Do they own a home',
               'OCCUPATION-Job category',
                'CAR_USE-Commercial or Private ',
               'CAR_TYPE-Type of Car',
                'RED_CAR-0=No, 1=Yes',
                'REVOKED-Have they had their license revoked ',
                'URBANICITY-Urban or rural, 0=Rural',
                'AGE-Customer Age',
                'YOJ-Years on Job- How long they\'ve been at their job ',
                'BLUEBOOK-Value of the car',
                'TIF-Years of being a customer ',
                'MVR_PTS-Do they have Motor Vehicle Record Points'
                ,'CAR_AGE-Age of vehicle', sep="<br/>")
    })
    output$summary <- renderPrint({
        
        datatouse=data.frame(df[, c("CLAIM_FLAG", input$checkGroup)])
        set.seed(234)
        split <- sample.split(datatouse['CLAIM_FLAG'], SplitRatio = 0.70)
        dfbinarytrain <- subset(datatouse, split == TRUE)
        dfbinarytest <- subset(datatouse, split == FALSE)
        logit.overall = glm(CLAIM_FLAG ~ .,
                            family = "binomial",
                            data = dfbinarytrain )
        summary(logit.overall)
        
    })
    output$expcoefs<- renderPrint({
        datatouse=data.frame(df[, c("CLAIM_FLAG", input$checkGroup)])
        set.seed(234)
        split <- sample.split(datatouse['CLAIM_FLAG'], SplitRatio = 0.70)
        dfbinarytrain <- subset(datatouse, split == TRUE)
        dfbinarytest <- subset(datatouse, split == FALSE)
        logit.overall = glm(CLAIM_FLAG ~ .,
                            family = "binomial",
                            data = dfbinarytrain )
      
        exp(logit.overall$coefficients)
        
        
    })
    output$confusionmat<- reactive({
        datatouse=data.frame(df[, c("CLAIM_FLAG", input$checkGroup)])
        
        set.seed(234)
        split <- sample.split(datatouse['CLAIM_FLAG'], SplitRatio = 0.70)
        dfbinarytrain <- subset(datatouse, split == TRUE)
        dfbinarytest <- subset(datatouse, split == FALSE)
        logit.overall = glm(CLAIM_FLAG ~ .,
                            family = "binomial",
                            data = dfbinarytrain )
        
        
        t=data.frame(predict(logit.overall, dfbinarytest,type='response'))
        # print(dfbinarytest)
        table(dfbinarytest$CLAIM_FLAG, t>.5)
        # 
    })
    
       
        
    
})
