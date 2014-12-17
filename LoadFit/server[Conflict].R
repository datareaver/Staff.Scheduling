library(dplyr)
library(ggplot2)
library(reshape2)
library(RColorBrewer)
library(shiny)


shinyServer(function(input, output) {
    
    #if (!is.integer(input$numShifts) | !(input$numShifts > 0)) stop("Number of shifts must be an integer > 0")
    
    inputData <- reactive({
        
        hours <- paste0("input$hour",1:30)
        weekdays <- paste0("input$weekday",1:30)
        shifts <- paste0("input$shift",1:30)
        
        inputs <- data.frame(hours = sapply(hours,function(x) {eval(parse(text = x))}),
                   weekdays = sapply(weekdays,function(x) {eval(parse(text = x))}),
                   shifts = sapply(shifts,function(x) {eval(parse(text = x))}))
        
        if (sum(inputs$shifts) > 0) {
            shift.length <- 7
            
            schedule <- data.frame(Start = (weekdays-1)*24 + (hours-1),N = shifts)[shifts > 0,]
            
            count <- data.frame(table(unlist(lapply(schedule$Start,function(x) {seq(x,x+shift.length)}))))
            count$Var1 <- as.numeric(as.character(count$Var1))
            count <- rbind(filter(count,Var1 < 168),filter(count, Var1 >= 168) %>% transform(Var1 = Var1-168)) %>%
                group_by(Var1) %>%
                summarize(Freq = sum(Freq))
            
            sched_load <- anti_join(
            
            inputData <- data.frame(Time = test$Var1,Load = rep(1:24,length.out = 168))#,
            #Schedule = test$Freq[1:168]+c(test$Freq[168:nrow(test)],rep(0,168-(nrow(test)-167))))
            inputData 
        
        }
        else {
            inputData <- data.frame(Time = 0:167,Load = rep(1:24,length.out = 168),Schedule = rep(0,168))
        }
        anti_join(data.frame(Var1 = 0:167,Freq = 0),data.frame(table(unlist(lapply(schedule$Start,function(x) {seq(x,x+7)})))))

    })

    output$loadPlot <- renderPlot({
        #labels <- paste(c(rep(seq(0,23,12),7),0),labels,sep = '\n')
        colors <- brewer.pal(12,'Set3')[c(5,12)]        
        
        ggplot(inputData(),aes(x = Time,y = Load)) + geom_step()
    })
        
        
})
  

# Generate Inputs ---------------------------------------------------------


# i <- 30
# wd <- ',label = NULL,choices = list("Sunday" = 1, "Monday" = 2, "Tuesday" = 3,
#                                   "Wednesday" = 4, "Thursday" = 5, "Friday" = 6,
#                                   "Saturday" = 7),selected = 1)),'
# wds <- sapply(1:i,function(i) {paste0("\ncolumn(3,\nselectInput(\"weekday",i,"\"",wd,collapse = "")})
# 
# hour <- ',label = NULL,choices = list("0:00" = 1, "1:00" = 2, "2:00" = 3, "3:00" = 4,
#                                 "4:00" = 5, "5:00" = 6, "6:00" = 7, "7:00" = 8,
#                                 "8:00" = 9, "9:00" = 10, "10:00" = 11, "11:00" = 12,
#                                 "12:00" = 13, "13:00" = 14, "14:00" = 15, "15:00" = 16,
#                                 "16:00" = 17, "17:00" = 18, "18:00" = 19, "19:00" = 20,
#                                 "20:00" = 21, "21:00" = 22, "22:00" = 23, "23:00" = 24),
#                                 selected = 1)),'
# 
# hours <- lapply(1:i,function(i) {paste0("\ncolumn(3,\nselectInput(\"hour",i,"\"",hour,collapse = "")})
# 
# num <- ',label = NULL,value = 0))'
# nums <- sapply(1:i,function(i) {paste0("\ncolumn(3,\nnumericInput(\"shifts",i,"\"",num,collapse = "")})
# 
# code <- paste0('fluidRow(',wds,hours,nums,"),\n",collapse = '')
# HTML(code)  

# output$selectInputs <- renderUI({
#     i <- input$numShifts
#     wd <- ',label = NULL,choices = list("Sunday" = 1, "Monday" = 2, "Tuesday" = 3,
#                                   "Wednesday" = 4, "Thursday" = 5, "Friday" = 6,
#                                   "Saturday" = 7),selected = 1)'
#     wds <- paste0("column(3,",paste0(sapply(1:i,function(i) 
#     {paste0("\nselectInput(\"weekday",i,"\"",wd,collapse = "")}),c(rep(",\n",i-1),"\n"),
#     collapse = ''),"\n),\n",collapse = '')
#     
#     hour <- ',label = NULL,choices = list("0:00" = 1, "1:00" = 2, "2:00" = 3, "3:00" = 4,
#                                 "4:00" = 5, "5:00" = 6, "6:00" = 7, "7:00" = 8,
#                                 "8:00" = 9, "9:00" = 10, "10:00" = 11, "11:00" = 12,
#                                 "12:00" = 13, "13:00" = 14, "14:00" = 15, "15:00" = 16,
#                                 "16:00" = 17, "17:00" = 18, "18:00" = 19, "19:00" = 20,
#                                 "20:00" = 21, "21:00" = 22, "22:00" = 23, "23:00" = 24),
#                                 selected = 1)'
#     hours <- paste0("column(3,",paste0(sapply(1:i,function(i) 
#     {paste0("\nselectInput(\"hour",i,"\"",hour,collapse = "")}),c(rep(",\n",i-1),"\n"),
#     collapse = ''),"\n),\n",collapse = '')
#     
#     num <- ',label = NULL,value = 0)'
#     nums <- paste0("column(3,",paste0(sapply(1:i,function(i) 
#     {paste0("\nnumericInput(\"shifts",i,"\"",num,collapse = "")}),c(rep(",\n",i-1),"\n"),
#     collapse = ''),"\n)\n",collapse = '')
#     
#     code <- paste0(wds,hours,nums,collapse = '')
#     #return(HTML(code))
#     #HTML(parse(text = code))
#     str(HTML(code))
#     return(tag(code))
#     #paste0('textInput("textbox","Text","test")','textInput("textbox","Text","test2")',collapse = '')
# })
# })
