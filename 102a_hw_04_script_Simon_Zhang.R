library(R6)

# gameboard and decks -----------------------------------------------------
# Do not change this code
gameboard <- data.frame(
  space = 1:40, 
  title = c(
    "Go", "Mediterranean Avenue", "Community Chest", "Baltic Avenue",
    "Income Tax", "Reading Railroad", "Oriental Avenue", "Chance",
    "Vermont Avenue", "Connecticut Avenue", "Jail", "St. Charles Place",
    "Electric Company", "States Avenue", "Virginia Avenue",
    "Pennsylvania Railroad", "St. James Place", "Community Chest",
    "Tennessee Avenue", "New York Avenue", "Free Parking",
    "Kentucky Avenue", "Chance", "Indiana Avenue", "Illinois Avenue",
    "B & O Railroad", "Atlantic Avenue", "Ventnor Avenue", "Water Works",
    "Marvin Gardens", "Go to jail", "Pacific Avenue",
    "North Carolina Avenue", "Community Chest", "Pennsylvania Avenue",
    "Short Line Railroad", "Chance", "Park Place", "Luxury Tax",
    "Boardwalk"), stringsAsFactors = FALSE)
chancedeck <- data.frame(
  index = 1:15, 
  card = c( # first 9 is moving
    "Advance to Go", "Advance to Illinois Ave.",
    "Advance to St. Charles Place", "Advance token to nearest Utility",
    "Advance token to the nearest Railroad",
    "Take a ride on the Reading Railroad",
    "Take a walk on the Boardwalk", "Go to Jail", "Go Back 3 Spaces",
    "Bank pays you dividend of $50", "Get out of Jail Free",
    "Make general repairs on all your property", "Pay poor tax of $15",
    "You have been elected Chairman of the Board", 
    "Your building loan matures"), stringsAsFactors = FALSE)
communitydeck <- data.frame(
  index = 1:16, 
  card = c( # first 2 is moving, Advance/Go to ...
    "Advance to Go", "Go to Jail",
    "Bank error in your favor. Collect $200", "Doctor's fees Pay $50",
    "From sale of stock you get $45", "Get Out of Jail Free",
    "Grand Opera Night Opening", "Xmas Fund matures", "Income tax refund",
    "Life insurance matures. Collect $100", "Pay hospital fees of $100",
    "Pay school tax of $150", "Receive for services $25",
    "You are assessed for street repairs",
    "You have won second prize in a beauty contest",
    "You inherit $100"), stringsAsFactors = FALSE)

# RandomDice class --------------------------------------------------------
# Do not change this code

RandomDice <- R6Class(
  classname = "RandomDice",
  public = list(
    verbose = NA,
    initialize = function(verbose = FALSE){
      stopifnot(is.logical(verbose))
      self$verbose = verbose
    },
    roll = function() {
      outcome <- sample(1:6, size = 2, replace = TRUE)
      if(self$verbose){
        cat("Dice Rolled:", outcome[1], outcome[2], "\n")
      }
      outcome
    }
  )
)

# randonm_dice <- RandomDice$new()
# randonm_dice$roll()

# Preset Dice -------------------------------------------------------------
# Do not change this code

PresetDice <- R6Class(
  classname = "PresetDice",
  public = list(
    verbose = NA,
    preset_rolls = double(0),
    position = 1,
    initialize = function(rolls, verbose = FALSE){
      stopifnot(is.logical(verbose))
      stopifnot(is.numeric(rolls))
      self$preset_rolls = rolls
      self$verbose = verbose
    },
    roll = function(){
      if(self$position > length(self$preset_rolls)){
        stop("You have run out of predetermined dice outcomes.")
      }
      outcome <- c(self$preset_rolls[self$position], 
                   self$preset_rolls[self$position + 1])
      self$position <- self$position + 2
      if(self$verbose){
        cat("Dice Rolled:", outcome[1], outcome[2], "\n")
      }
      outcome
    }
  )
)


# present_dice <- PresetDice$new(c(3, 3, 2, 1))
# present_dice$preset_rolls
# present_dice$position
# present_dice$roll()
# present_dice$roll()
# present_dice$roll()
# present_dice$position



# Chance and Community Decks ----------------------------------------------
# Do not change this code

# This R6 class object shuffles the card deck when initialized.
# It has one method $draw(), which will draw a card from the deck.
# If all the cards have been drawn (position = deck length), then it will
# shuffle the cards again.
# The verbose option cats the card that is drawn on to the screen.
CardDeck <- R6Class(
  classname = "CardDeck",
  public = list(
    verbose = NA,
    deck_order = double(0), 
    deck = data.frame(),
    position = 1,
    initialize = function(deck, verbose = FALSE){
      stopifnot(is.data.frame(deck),
                is.numeric(deck[[1]]),
                is.character(deck[[2]]))
      self$deck_order <- sample(length(deck[[1]]))
      self$verbose <- verbose
      self$deck <- deck
    },
    draw = function(){
      if(self$position > length(self$deck_order)){
        # if we run out of cards, shuffle deck
        # and reset the position to 1
        if(self$verbose){
          cat("Shuffling deck.\n")
        }
        self$deck_order <- sample(length(self$deck[[1]]))
        self$position <- 1
      }
      outcome <- c(self$deck_order[self$position]) # outcome is the value at position
      self$position <- self$position + 1 # advance the position by 1
      if(self$verbose){
        cat("Card:", self$deck[outcome, 2], "\n")
      }
      outcome # return the outcome, which is index 
    }
  )
)

# chance <- CardDeck$new(chancedeck)
# chance$deck[[1]]
# chance$deck[[2]]
# chance$draw()
# chance$deck_order
# chance$deck_order[1]
# 
# community <- CardDeck$new(communitydeck)
# community$deck
# community$deck_order
# community$draw()

# R6 Class SpaceTracker ---------------------------------------------------
# Do not change this code

SpaceTracker <- R6Class(
  classname = "SpaceTracker",
  public = list(
    counts = rep(0, 40),
    verbose = TRUE,
    tally = function(x){
      self$counts[x] <- self$counts[x] + 1
      if(self$verbose){
        cat("Added tally to ", x, ": ", gameboard$title[x], ".\n", sep = "")
      }
    },
    initialize = function(verbose){
      self$verbose <- verbose
    }
  )
)

# space_tracker <- SpaceTracker$new(TRUE)
# space_tracker$counts
# space_tracker$tally(40)
# space_tracker$counts


# R6 Class Player ---------------------------------------------------------
## You'll need to expand on this

Player <- R6Class(
  classname = "Player",
  public = list(
    pos = 1,
    verbose = TRUE,
    jail = 0, # check player in jail or not
    
    move_fwd = function(n){
      self$pos <- self$pos + n
      if(self$pos > 40){
        self$pos <- self$pos - 40
      }
      if(self$verbose){
        cat("Player is now at ", self$pos, ": ", gameboard$title[self$pos], ".\n", sep = "")
      }
    },
    
    initialize = function(verbose = FALSE, pos = 1) {
      self$verbose <- verbose
      self$pos <- pos
    }
  )
)

# player1 <- Player$new(T, 10)
# player1$pos
# player1$move_fwd(5)


# VERY BASIC turn taking example ------------------------------------------
# You will need to expand this
# You can write helper function if you want

take_turn <- function(player, spacetracker){
  roll <- dice$roll()
  doubles_count <- 0
  is_double <- roll[1] == roll[2]
  doubles_count <- doubles_count + 1 * is_double
  if (is_double && player$verbose == T && spacetracker$verbose == T && player$jail == 0) {
    cat("Doubles count is now ", doubles_count, ".\n", sep = "")
  }

  # player from not in jail
  if (player$jail == 0) {
    move_forward(player, spacetracker, roll)
    
    # if roll double, roll again
    while (is_double && doubles_count < 3 && player$jail == 0) {
      if (player$verbose == T && spacetracker$verbose == T) {
        cat("\nPlayer rolled doubles, so they take another turn.\n")
      }
      roll <- dice$roll()
      is_double <- roll[1] == roll[2]
      doubles_count <- doubles_count + 1 * is_double
      if (is_double && (player$jail == 0) && player$verbose == T && spacetracker$verbose == T) {
        cat("Doubles count is now ", doubles_count, ".\n", sep = "")
      }
      
      # 3 doubles in row, sent to jail
      if (doubles_count == 3) {
        player$pos <- 11
        player$jail <- 1
        if (player$verbose == T && spacetracker$verbose == T) {
          cat("Player goes to jail.\n")
        }
        spacetracker$tally(11)
      }
      
      else {
        # move forward
        move_forward(player, spacetracker, roll)
      }
    }
  } # end of not in jail
  
  
  # player in jail
  else {
    # check player is in jail for the third turn
    if (player$jail == 3) {
      if (player$verbose == T && spacetracker$verbose == T) {
        cat("Player's third turn in jail. Player must exit jail.\n")
        cat("Player exits jail.\n")
      }
      move_forward(player, spacetracker, roll)
      player$jail <- 0
    }
    
    # in jail fewer than 3 turns
    else {
      if(is_double) {
        # move forward
        if (player$verbose == T && spacetracker$verbose == T) {
          cat("In jail but rolled doubles.\n")
          cat("Player extis jail.\n")
        }
        move_forward(player, spacetracker, roll)
        player$jail <- 0
      }
      else {
        if (player$verbose == T && spacetracker$verbose == T) {
          cat("Player stays in jail.\n")
        }
        player$jail <- player$jail + 1
        spacetracker$tally(11)
      }
    }
  }
  
}

# roll <- dice$roll() -> c(3, 3)
move_forward <- function(player, spacetracker, roll) {
  # given roll outcome, then how player move forward
  
  if (player$verbose == T && spacetracker$verbose == T) {
    cat("Player starts at ", player$pos, ": ", gameboard$title[player$pos], ".\n", sep = "")
    cat("Player moves forward ", sum(roll), ".\n", sep = "")
  }
  
  player$move_fwd(sum(roll))
  
  # go to jail
  if (player$pos == 31) {
    player$pos <- 11
    player$jail <- 1
    if (player$verbose == T && spacetracker$verbose == T) {
      cat("Player goes to jail.\n")
    }
    spacetracker$tally(11)
    return()
  }
  
  # chance
  if (player$pos %in% c(8, 23, 37)) {
    spacetracker$tally(player$pos)
    if (player$verbose == T && spacetracker$verbose == T) {
      cat("Draw a Chance card.\n")
    }
    card <- chance$draw()
    if (card == 1) {
      if (player$verbose == T && spacetracker$verbose == T) {
        cat("Player moves to: ", card, ": ", gameboard$title[card], ".\n", sep = "")
      }
      player$pos <- 1
      spacetracker$tally(1)
    }
    if (card == 2) {
      if (player$verbose == T && spacetracker$verbose == T) {
        cat("Player moves to: ", card, ": ", gameboard$title[card], ".\n", sep = "")
      }
      player$pos <- 25
      spacetracker$tally(25)
    }
    if (card == 3) {
      if (player$verbose == T && spacetracker$verbose == T) {
        cat("Player moves to: ", card, ": ", gameboard$title[card], ".\n", sep = "")
      }
      player$pos <- 12
      spacetracker$tally(12)
    }
    # depends on which chance player is on, moving forward to one of two utility
    if (card == 4) {
      if (player$pos == 8) {
        player$pos <- 13
        if (player$verbose == T && spacetracker$verbose == T) {
          cat("Player moves to: ", 13, ": ", gameboard$title[13], ".\n", sep = "")
        }
        spacetracker$tally(13)
      }
      if (player$pos == 23) {
        player$pos <- 29
        if (player$verbose == T && spacetracker$verbose == T) {
          cat("Player moves to: ", 29, ": ", gameboard$title[29], ".\n", sep = "")
        }
        spacetracker$tally(29)
      }
      if (player$pos == 37) {
        player$pos <- 13
        if (player$verbose == T && spacetracker$verbose == T) {
          cat("Player moves to: ", 13, ": ", gameboard$title[13], ".\n", sep = "")
        }
        spacetracker$tally(13)
      }
      return()
    }
    
    # nearest railroad
    if (card == 5) {
      if (player$pos == 8) {
        player$pos <- 16
        if (player$verbose == T && spacetracker$verbose == T) {
          cat("Player moves to: ", 16, ": ", gameboard$title[16], ".\n", sep = "")
        }
        spacetracker$tally(16)
      }
      if (player$pos == 23) {
        player$pos <- 26
        if (player$verbose == T && spacetracker$verbose == T) {
          cat("Player moves to: ", 26, ": ", gameboard$title[26], ".\n", sep = "")
        }
        spacetracker$tally(26)
      }
      if (player$pos == 37) {
        player$pos <- 6
        if (player$verbose == T && spacetracker$verbose == T) {
          cat("Player moves to: ", 6, ": ", gameboard$title[6], ".\n", sep = "")
        }
        spacetracker$tally(6)
      }
      return()
    }
    
    if (card == 6) {
      player$pos <- 6
      if (player$verbose == T && spacetracker$verbose == T) {
        cat("Player moves to: ", card, ": ", gameboard$title[card], ".\n", sep = "")
      }
      spacetracker$tally(6)
    }
    if (card == 7) {
      player$pos <- 40
      if (player$verbose == T && spacetracker$verbose == T) {
        cat("Player moves to: ", card, ": ", gameboard$title[card], ".\n", sep = "")
      }
      spacetracker$tally(40)
    }
    
    # go to jail
    if (card == 8) {
      player$pos <- 11
      player$jail <- 1
      if (player$verbose == T && spacetracker$verbose == T) {
        cat("Player goes to jail.\n")
      }
      spacetracker$tally(11)
    }
    if (card == 9) {
      player$pos <- player$pos - 3
      if (player$verbose == T && spacetracker$verbose == T) {
        cat("Player moves to: ", card, ": ", gameboard$title[card], ".\n", sep = "")
      }
      spacetracker$tally(player$pos)
    }
    return()
  } # end of chance
  
  # community chest
  if (player$pos %in% c(3, 18, 34)) {
    spacetracker$tally(player$pos)
    if (player$verbose == T && spacetracker$verbose == T) {
      cat("Draw a Community Chest card.\n")
    }
    card <- community$draw()

    if (card == 1) {
      player$pos <- 1
      spacetracker$tally(1)
    }
    
    # card # 2: Go to Jail
    if (card == 2) {
      player$pos <- 11
      player$jail <- 1
      if (player$verbose == T && spacetracker$verbose == T) {
        cat("Player goes to jail.\n")
      }
      spacetracker$tally(11)
    }
    return()
  } # end of community
  
  
  # simple move the exact space
  spacetracker$tally(player$pos)
}

