## Shiny application ##
## Discrete event simulation model - Operating rooms Nijmegen ##
## by Stan Wijn ##
## Disclamer: This model was build to act as an example. Results have not been verified or represent reality.

library(shiny)
library(simmer)
library(simmer.plot)
library(svDialogs)

#Build data.frame
ui <-  fluidPage(
  titlePanel("DES OR, for education purposes only!              
             Build by Stan Wijn"),
  inputPanel(
    actionButton("go", "Run simulation"),
    sliderInput("OR", label = "Operating rooms (n=20):",
              min = 0, max = 40, value = 20, step = 1),
   sliderInput("surgeon", label = "Surgeon (n=18):",
              min = 8, max = 28, value = 18, step = 1),
   sliderInput("surgeontraining", label = "Surgeon-in-Training (n=9) :",
              min = 0, max = 20, value = 9, step = 1),
   sliderInput("ORassistant", label = "OR-assistant (n=45):",
              min = 30, max = 70, value = 45, step = 1),
   sliderInput("anesthesiologist", label = "Anesthesiologist (n=9):",
               min = 0, max = 20, value = 9, step = 1),
   sliderInput("anasthesiologistassistant", label = "Anesthesiologist-assistant (n=18):",
              min = 8, max = 28, value = 18, step = 1),
   sliderInput("patient", label = "Patients (n=63):",
              min = 40, max = 80, value = 63, step = 1),
   sliderInput("runs", label = "Simulation days (n=10):",
              min = 1, max = 100, value = 10, step = 5)
),
  mainPanel(
    textOutput("selected_var"),
    textOutput("overtime"),
    textOutput("time"),
    plotOutput("plot"),
    plotOutput("plot2"),
    plotOutput("plot3")
    
  )
)

server <- function(input, output) {
  observeEvent(input$go, {
    data <- data.frame(Cost= rep(NA, input$runs), TotalTime= rep(NA, input$runs))
    cancel <- 0
    for(x in 1:input$runs){
      env <- simmer("OR Nijmegen")
      env 
      patient <- trajectory("Surgery") %>%
        renege_in(900,  #cancel if surgery will start 30min before end of day (480-30 * 2) = 900
                  out= trajectory("cancel") %>% 
                    timeout(function() {cancel <<- cancel + 1})) %>%
        seize("OR", 2) %>%
        renege_abort()%>%  
      
        seize("OR assistant", 5) %>%
        seize("anasthesiologist-assistant", 2)%>%
        
        timeout(function()rgamma(1, shape = (19.6/10.2)^2, scale = (10.2^2)/19.6)) %>%
      
        seize("anesthesiologist", 1) %>%
        seize("surgeon", 2) %>%
        seize("surgeon-in-training", 1) %>%
        timeout(function()rgamma(1, shape = (30.3/13.6)^2, scale = (13.6^2)/30.3)) %>%   
        
        timeout(function()rgamma(1, shape = (56.88/28.44)^2, scale = (28.44^2)/56.88)) %>%
        release("surgeon", 2) %>%
        release("surgeon-in-training", 1) %>%      
        
        timeout(function()rgamma(1, shape = (14.82/7.41)^2, scale = (7.41^2)/14.82)) %>%
        release("anesthesiologist", 1) %>% 
        
        timeout(function()rgamma(1, shape = (7.5/3.75)^2, scale = (3.75^2)/7.5)) %>%
        release("OR assistant", 5) %>%
        release("anasthesiologist-assistant", 2) %>%
        release("OR", 2)
  
      #### Add model resources ####
      env %>%
        
        add_resource("OR", capacity=(input$OR*2), queue_size=Inf) %>%
        add_resource("surgeon", capacity=(input$surgeon*2), queue_size=Inf) %>%
        add_resource("surgeon-in-training", capacity=(input$surgeontraining*2), queue_size=Inf) %>%
        add_resource("OR assistant", capacity=(input$ORassistant*2), queue_size=Inf) %>%
        add_resource("anesthesiologist", capacity=(input$anesthesiologist*2), queue_size=Inf) %>%
        add_resource("anasthesiologist-assistant", capacity=(input$anasthesiologistassistant*2), queue_size=Inf) %>%
        add_generator("patient", patient, at(c(rep(0,(input$patient*2))))) #assume 16269 each year = 62.5 per day ->  2*62.5 = 125
      
      env %>% run()

      
      #Read attributes
      data[x,1] <- sum(get_mon_arrivals(env)[3]*14.12) #average cost are 14.12 per minute
      data[x,2] <- max(get_mon_arrivals(env)[3])
    }
    data <- data/2
    cancel <- cancel/2
    resources<- get_mon_resources(env)
    arrivals <- get_mon_arrivals(env)
    attributes <- get_mon_attributes(env)
 
  
  output$plot <- renderPlot({
    plot(resources, metric="utilization")})
  output$plot2 <- renderPlot({
    plot(resources, metric = "usage", c("surgeon", "OR assistant", "anesthesiologist", "OR"), items= c("server", "queue"))})
  output$plot3 <- renderPlot({plot(arrivals, metric = 'activity_time')})
  output$selected_var <- renderText({paste(cancel, " surgeries have been canceled.")})
    output$time <- renderText({paste(round(mean(data$TotalTime)), "minutes was the average time of one day.    
                                     " , round((mean(data$TotalTime/60)),2), "= hours")})
    output$overtime <- renderText({
      paste("The average overtime was", round(mean(data$TotalTime),2)-480, "minutes")
    })
  
      
  })
}

shinyApp(ui, server)
