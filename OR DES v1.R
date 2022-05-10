## Discrete event simulation model - Operating rooms Nijmegen ##
## by Stan Wijn ##
## Disclamer: This model was build to act as an example. Results have not been verified or represent reality.

library(simmer)
library(simmer.plot)
library(svDialogs)
rm(list=ls())


## Define the number of runs:
runs <- as.numeric(dlgInput("Enter the number of days to simulate", Sys.info()["0"])$res)
#Build data.frame
data <- data.frame(Cost= rep(NA, runs), TotalTime= rep(NA, runs))


#### Input parameters ####
# OR time : https://www.ncbi.nlm.nih.gov/pubmed/19444562
# Average times, can be specified for different departments
# Using gamma distributions to avoid 0
#OR phases
#t.setup <- rgamma(1, shape = (19.6/10.2)^2, scale = (10.2^2)/19.6)
#t.induction <-rgamma(1, shape = (30.3/13.6)^2, scale = (13.6^2)/30.3)
# average radboud = 102 min from pat enter to finish
# 102 - 30.3 = 71.7, assumption: 80% is procedure, SD = half)
#t.procedure <- rgamma(1, shape = (56.88/28.44)^2, scale = (28.44^2)/56.88)
#t.reversal <- rgamma(1, shape = (14.82/7.41)^2, scale = (7.41^2)/14.82)
# cleanup takes 5-10 minutes (http://www.imse.iastate.edu/files/2015/04/Alexandra-Olsen_Thesis.pdf)
#t.cleanup <- rgamma(1, shape = (7.5/3.75)^2, scale = (3.75^2)/7.5)
#t.total <- t.setup + t.induction + t.procedure + t.reversal + t.cleanup


#### Build model trajectory ####
for(x in 1:runs){
  cancel <- 0
env <- simmer("OR Nijmegen")

env 
patient <- trajectory("Surgery") %>%
     
  log_("Patient planned") %>%
        renege_in(900,  #cancel if surgery will start 30min before end of day (480-30 * 2) = 900
            out= trajectory("cancel") %>% 
              timeout(function() {cancel <- cancel + 1}) %>%
              log_("End of day. Surgery canceled")
          ) %>%
        seize("OR", 2) %>%
        renege_abort()%>%  
        log_("Start surgery") %>%
        seize("OR assistant", 5) %>%
        seize("anasthesiologist-assistant", 2)%>%
        log_("Case Start") %>%
        timeout(function()rgamma(1, shape = (19.6/10.2)^2, scale = (10.2^2)/19.6)) %>%
        log_("Patient in room") %>%
        seize("anesthesiologist", 1) %>%
        seize("surgeon", 2) %>%
        seize("surgeon-in-training", 1) %>%
        timeout(function()rgamma(1, shape = (30.3/13.6)^2, scale = (13.6^2)/30.3)) %>%   
        log_("Procedure Start")%>%
        timeout(function()rgamma(1, shape = (56.88/28.44)^2, scale = (28.44^2)/56.88)) %>%
        release("surgeon", 2) %>%
        release("surgeon-in-training", 1) %>%      
        log_("Procedure Finish")%>%
        timeout(function()rgamma(1, shape = (14.82/7.41)^2, scale = (7.41^2)/14.82)) %>%
        release("anesthesiologist", 1) %>% 
        log_("Patient out of room")%>%
        timeout(function()rgamma(1, shape = (7.5/3.75)^2, scale = (3.75^2)/7.5)) %>%
        log_("Case end") %>%
        release("OR assistant", 5) %>%
        release("anasthesiologist-assistant", 2) %>%
        release("OR", 2)

  #trajectory("Stop") %>%
   # log_("End of day. Surgery canceled"))

#### Add model resources ####
env %>%
  
  add_resource("OR", capacity=40, queue_size=Inf) %>% #40
  add_resource("surgeon", capacity=16, queue_size=Inf) %>% #36
  add_resource("surgeon-in-training", capacity=18, queue_size=Inf) %>% #18
  add_resource("OR assistant", capacity=1, queue_size=Inf) %>% #90
  add_resource("anesthesiologist", capacity=18, queue_size=Inf) %>% #18
  add_resource("anasthesiologist-assistant", capacity=36, queue_size=Inf) %>% #36
  #add_generator("patient", patient, at(c(1:125))) #assume 16269 each year = 62.5 per day ->  2*62.5 = 125
  add_generator("patient", patient, at(c(rep(0,125))))

env %>% run() #%>% log_(paste("End of day", x)) 
#%>% #Run the entire program (average of 5 patients per active OR) for 1 day.
#get_mon_arrivals() #(disabled printing to improve speed)

#Read attributes
data[x,1] <- sum(get_mon_arrivals(env)[3]*14.12) #average cost are 14.12 per minute
data[x,2] <- max(get_mon_arrivals(env)[3])
data[x,3] <- cancel

print(paste("End of day", x))
}
data <- data/2
cancel <- cancel/2
resources<- get_mon_resources(env) #last simulation only
arrivals <- get_mon_arrivals(env) #last simulation only
attributes <- get_mon_attributes(env) #last simulation only
#plots
plot(resources, metric="utilization")
plot(resources, metric = "usage", c("surgeon", "OR assistant", "anesthesiologist", "OR"), items= c("server", "queue"))
plot(arrivals, metric = 'activity_time')
#average time and cost
mean(data$TotalTime)
mean(data$Cost)
cancel

# delay voor release om 'loop' tijd te simuleren. 

