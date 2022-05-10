# Example Disrete event simulation (DES) shiny application

Discrete event simulation (DES) is a method used to model real world systems that can be decomposed into a set of logically separate processes that autonomously progress through time. Each event occurs on a specific process, and is assigned a logical time (a timestamp).

In health-economics, a DES can help to simulate patient trajectories or resource use. In this repository I show an example for a shiny application that utilizes the simmer-package in R to build a simple DES. Using the shiny application, users can change the input parameters of the model and visualize the output of the model.

The concept DES model was built to simulate the operating rooms of a hospital. The goal is to search for an optimal use of operating rooms and personel. 

The user can alter: 
-The number of operating rooms
-Surgeons available
-Surgeon-in-training available
-OR assistants available
-Anesthesiologists available
-Anesthesiologist-assistant available
-Patients on schedule for each day
-Days to simulate


The DES OR Shiny represents an older version. The OR DES Stan v1.R is the most advanced DES model in this example (without shiny application). 
NOTE: This model only uses fictional data and is for education purposes only! Results have not been verified or represent reality.
