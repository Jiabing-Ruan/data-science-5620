Roulette
========

<img src="./assets/roulette-table.svg" style="width:50.0%" />

A roulette table composed of 38 (or 37) evenly sized pockets on a wheel.
The pockets are colored red, black, or green. The pockets are also
numbered. Roulette is a game of chance in which a pocket is randomly
selected. Gamblers may wager on several aspects of the outcome. For
example, one may place a wager that the randomly selected pocket will be
red or odd numbered or will be a specific number.

For this assignment, all one needs to know is that there are 38 pockets
of which 2 are green, 18 are red, and 18 are black. The payout for a bet
on black (or red) is $2 for each $1 wagered. This means that if a
gambler bets $1 on black and the randomly selected pocket is black, then
the gambler will get the original $1 wager and an additional $1 as
winnings.

Roulette strategies
-------------------

There are several *strategies* for playing roulette. (*Strategies* is in
italics because the house always wins, in the long run.) Consider one
such strategy:

``` r
grViz("
digraph boxes_and_circles {

  # a 'graph' statement
  graph [overlap = true, fontsize = 10, rankdir = LR]

  # several 'node' statements
  node [shape = box, fontname = Helvetica, label='Start\nWager $1 on red'] A;
  node [shape = circle, label = 'Play game'] B;
  

subgraph {
  subgraph {
    rankdir = LR
    node [shape = box, label = 'Red', width = 1] E;
    node [shape = box, label = 'Wager $1 on red', width = 2] C;
  }
  subgraph {
  rankdir = LR
  node [shape = box, label = 'Black', width = 1] F;
  node [shape = box, label = 'Wager twice the losses\nfrom previous wager', width = 2] D;
  }
}

  # several 'edge' statements
  A->B B:e->E:w
  B:e->F:w
  E:e->C:w
  F:e->D:w
  C:n->B:n 
  D:s->B:s
}
") %>% export_svg %>% HTML %>% cat(file = "./assets/martingale-strategy.svg")
```

![](./assets/martingale-strategy.svg)

Assignment
----------

In this assignment, you will write a blog post to explain how you used
computer simulation to understand the operating characteristics of the
above strategy.

-   The audience of your blog post is a sharp college freshman with
    little to no background in data science.

-   You should explain how you used computer simulation to calculate the
    average earnings of a gambler that uses this strategy. As part of
    the explanation, provide a figure (or a series of figures) that show
    how the gamblers earnings (or losses) evolve over a series of wagers
    at the roulette wheel. (The x-axis will be the wager number, the
    y-axis will be earnings.) The code below provides all the functions
    you’ll need to calculate average earnings.

-   Show your audience how changing a parameter of the simulation (see
    table below) does or does not have an impact on average earnings. A
    figure would be helpful.

-   See the stopping rule below. Explain to your audience how you used
    computer simulation to estimate the average number of plays before
    stopping. The code below will need to be modified to calculate this
    quantity.

-   Be sure to explain the limitations of the simulation; identify
    simplifications or other sources of uncertainty.

### Submission instructions

1.  Create a private Github repository with the following name:
    `Probability and Inference Portfolio [Lastname, Firstname]`
2.  Within the repo, create a folder called `01-roulette-simulation`
3.  Within the folder, create an .html or .md file with the name
    `writeup`
4.  Be prepared to share your blog post with the class when the
    deliverable is due
5.  The deliverable should be your own work. You may **discuss**
    concepts with classmates, but you may **not share** code or text.

Additional assumptions for the simulation.
------------------------------------------

### Stopping rule

A player will use the above strategy and play until

1.  the player wins **W** dollars
2.  the player goes bankrupt
3.  the player completes **L** wagers

### Budget

The player starts with **B** dollars. The player cannot wager more money
than he/she has.

### Maximum wager

Some casinos have a maximum bet. Call this parameter **M**. If the
strategy directs the player to wager more than M dollars, then the
player will only wager M dollars.

Summary of parameters
---------------------

| Parameter | Description                     |         Starting value        |
|:---------:|:--------------------------------|:-----------------------------:|
|   **B**   | Starting budget                 |              $200             |
|   **W**   | Winnings threshold for stopping | $300 (Starting budget + $100) |
|   **L**   | Time threshold for stopping     |           1000 plays          |
|   **M**   | Casino’s maximum wager          |              $100             |

``` r
one_play <- function(state){
  
    # Wager
    proposed_wager <- ifelse(state$previous_win, 1, 2*state$previous_wager)
    wager <- min(proposed_wager, state$M, state$B)
    
    # Spin of the wheel
    red <- rbinom(1,1,18/38)
    
    # Update state
    state$plays <- state$plays + 1
    state$previous_wager <- wager
    if(red){
      # WIN
      state$B <- state$B + wager
      state$previous_win <- TRUE
    }else{
      # LOSE
      state$B <- state$B - wager
      state$previous_win <- FALSE
    }
  state
}

stop_play <- function(state){
  if(state$B <= 0) return(TRUE)
  if(state$plays >= state$L) return(TRUE)
  if(state$B >= state$W) return(TRUE)
  FALSE
}

one_series <- function(
    B = 200
  , W = 300
  , L = 1000
  , M = 100
){

  # initial state
  state <- list(
    B = B
  , W = W
  , L = L
  , M = M
  , plays = 0
  , previous_wager = 0
  , previous_win = TRUE
  )
  
  # vector to store budget over series of plays
  budget <- rep(NA, L)
  
  # For loop of plays
  for(i in 1:L){
    new_state <- state %>% one_play
    budget[i] <- new_state$B
    if(new_state %>% stop_play){
      return(budget[1:i])
    }
    state <- new_state
  }
  budget    
}

get_last <- function(x) x[length(x)] # helper function


# Simulation
walk_out_money <- rep(NA, 10000)
for(j in seq_along(walk_out_money)){
  walk_out_money[j] <- one_series(B = 200, W = 300, L = 1000, M = 100) %>% get_last
}

# Walk out money distribution
hist(walk_out_money, breaks = 100)

# Estimated probability of walking out with extra cash
mean(walk_out_money > 200)

# Estimated earnings
mean(walk_out_money - 200)
```
