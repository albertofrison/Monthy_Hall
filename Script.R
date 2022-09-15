######################################################
# This project implements a Monte Carlo method to solve the Monthy Hall Problem
# The Problem: Suppose you're on a game show, and you're given the choice of three doors: Behind one door is a car; behind the others, goats.
# You pick a door, say No. 1, and the host, who knows what's behind the doors, opens another door, say No. 3, which has a goat.
# He then says to you, "Do you want to pick door No. 2?" Is it to your advantage to switch your choice?
# 
# Source: https://en.wikipedia.org/wiki/Monty_Hall_problem
# Source: https://datasciencedojo.com/blog/monte-carlo-simulation-python/
# Created with ♥ by Alberto Frison - September 2022
######################################################


##### 
# Section 00 - Initialization and Load of Packages
rm (list=ls())   # clears all variables in the workspace
library (tidyverse)

#####
# Section 01 - Monte Carlo Models

doors <- c("Car", "Goat", "Goat") # the 3 alternatives behind the 3 doors, the order here does not matter
alternatives <- c(1:3) # the player makes a random choice between doors 1 to 3

# Monte Carlo simulation for the STICK STRAGEGY --> 1/3 of success
Monthy_Hall_Stick <- function (B) {
  return (mean(replicate (B, {
    doors_shuffled <- sample(doors, 3, replace = FALSE) # alternatives get shuffled and stored in doors_shuffled - the GAME can start
    my_choice <- sample (alternatives, 1) # participant chooses randomly ONE door
    doors_shuffled[my_choice] == "Car" # the participant sticks with his choice, if it is correct (1/3 of the time it is) he wins
    } )))
}

# Monte Carlo simulation for the CHANGE STRAGEGY --> 2/3 of success
Monthy_Hall_Change <- function (B) {
  return (mean(replicate (B, {
    
    doors_shuffled <- sample(doors, 3, replace = FALSE) # alternatives get shuffled and stored in doors_shuffled - the GAME can start
    my_choice <- sample (alternatives, 1) # participant chooses randomly ONE door
    
    presentator_selection <- # the host will pick a door with a GOAT behind and show it to the participant
      if (doors_shuffled[my_choice] == "Car") {
        sample (alternatives[-my_choice],1)} else {
        alternatives[-c(my_choice,which(doors_shuffled[alternatives]=="Car"))]
      }
          
    doors_shuffled[-c(my_choice,presentator_selection)] == "Car" # the participant always changes his decision, choosing another door from the one he initially chose andh the one showed by the presenter
    
    })))
}  

#####
# Section 02 - Running the Simulations

# how many Simulations?
B <- trunc(10^seq(1,5, len = 100)) # number of trials

# Running the two simulations, Stick and Change, storing the results - you need sapply to run the simulations against a vector of number of simulations
stick_results <- sapply (B, Monthy_Hall_Stick)
change_results <- sapply (B,Monthy_Hall_Change)

# Storing the results into a DF for clear plotting
results_df <- data.frame (x = B, Stick = stick_results, Change = change_results)

# Building a tidy version of the data.frame
results_df <- pivot_longer(results_df, c("Stick", "Change"), names_to = "type", values_to = "val")

# TIDY DATA PLOTTING
ggplot(data = results_df, aes(x = log10(x), y = val, color = type, group = type)) +
  geom_hline (aes(yintercept = 1/3), color = "black", linetype="dashed") +
  geom_hline (aes(yintercept = 2/3), color = "black", linetype="dashed") +
  geom_line () +
  geom_point () +
  theme_bw() +
  labs (x = "Number of MC Simulations (log10)",
        y = "Probability of Winning",
        title = "Monthy Python Problem",
        subtitle = "Comparison of the Stick vs the Change Strategy",
        caption = "Made with ♥ by Alberto Frison",
        colour = "Strategy")

ggsave (filename = "charts/Chart01MCSimulation.png", device = "png", dpi = "retina", height = 1018/96, width = 1920/96)

###### - SPARE CODE
# A WAY TO PLOY
# ggplot(data = results_df) +
#   geom_hline (aes(yintercept = 1/3), color = "black", linetype="dashed") +
#   geom_line (aes(x = log10(x), y = stick), color = "blue") +
#   geom_point (aes(x = log10(x), y = stick), size = 0.8, color = "blue") +
#   geom_hline (aes(yintercept = 2/3), color = "black", linetype="dashed") +
#   geom_line (aes(x = log10(x), y = change), color = "red") +
#   geom_point (aes(x = log10(x), y = change), size = 0.8, color = "orange") +
#   theme_bw() +
#   labs (x = "Number of MC Simulations (log10)",
#         y = "Probability of Winning",
#         title = "Monthy Python Problem",
#         subtitle = "Comparison of the Stick Strategy vs the Change choice Strategy",
#         caption = "Made with ♥ by Alberto Frison",
#         colour = "c")
