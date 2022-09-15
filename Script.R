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
rm (list =ls())   # clears all variables in the workspace


#####
# Section 01

doors <- c("Car", "Goat", "Goat") # the 3 alternatives
alternatives <- c(1:3) # the player makes a random choice between doors 1 to 3
B <- 100000 # number of trials

Monthy_Hall_Stick <- function (B) {
  return (mean(replicate (B, {
    doors_shuffled <- sample(doors, 3, replace = FALSE) # alternatives get shuffled and stored in doors_shuffled - the GAME can start
    my_choice <- sample (alternatives, 1)
    doors_shuffled[my_choice] == "Car" } )))
}

Monthy_Hall_Change <- function (B) {
  return (mean(replicate (B, {
    doors_shuffled <- sample(doors, 3, replace = FALSE) # alternatives get shuffled and stored in doors_shuffled - the GAME can start
    my_choice <- sample (alternatives, 1)
    doors_shuffled[sample(alternatives[-my_choice],1)] == "Car" } )))
}  

Monthy_Hall_Stick(B)
Monthy_Hall_Change(B)

