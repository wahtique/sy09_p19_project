# Require : tidyverse

#' Parse pokemon data and combats into an advantage table
#' which will show the numerical advantage of the first pokemon
#' over the second one.
#'
#' @param combats should contains A and B, ids of pokemons
#' @param pokemons pokemons characteristics
#' @return 
#' A tibble containing the columns : 
#' hp, atk, def, spatk, spdef, speed, typeA1, typeA2, typeB1, typeB2, legA, legB
#' @export
#'
#' @examples
#' advantages <- advantage_tibble()
combat_advantage_tibble <- function(
  combats,
  pokemons = read.csv("data/pokemon.csv")
  ){
  nrows = dim(combats)[1]
  pb = txtProgressBar(min = 0, max = nrows, initial = 0, style = 3)
  res = list()
  # for each row
  for (i in 1:nrows) {
    combat_row <- combats[i,]
    A <- pokemon[combat_row$A,]
    B <- pokemon[combat_row$B,]    
    new_row <- NULL
    new_row <- tibble(
      hp = A$HP - B$HP,
      atk = A$Attack - B$Attack,
      def = A$Defense - B$Defense,
      spatk = A$`Sp. Atk` - B$`Sp. Atk`,
      spdef = A$`Sp. Def` - B$`Sp. Def`,
      speed = A$Speed - B$Speed,
      typeA1 = A$`Type 1`,
      typeA2 = A$`Type 2`,
      typeB1 = B$`Type 1`,
      typeB2 = B$`Type 2`
    )
    # add the new tibble to the list of tibbles
    res[[i]] <- new_row
    setTxtProgressBar(pb, i)
  }
  res <- bind_rows(res)
  res$typeA1 <- as.factor(res$typeA1)
  res$typeB1 <- as.factor(res$typeB1)
  res$typeA2 <- as.factor(res$typeA2)
  res$typeB2 <- as.factor(res$typeB2)
  close(pb)
  return(res)
}

#' Compute a tibble giving the numerical advantage of pokemon A over B
#'
#' @param combats combats data : first pokemon, second pokemon, optionally winner.
#'                Default : read combats data in data folder
#' @param pokemon pokemon data. Default : read in data folder
#' @param has_winners boolean indicating whether combats also show winner's ID
#'
#' @return
#' a tibble as specified in combat_advantage_tibble with optionally a winer column
#' winner is 0 if A won (first pokemon to fight win the fight) and 1 otherwise
#' @export
#'
#' @examples
#' t <- AB_advantage_tibble()
AB_advantage_tibble <- function(
  combats = read_csv("data/combats.csv"),
  pokemon = read_csv("data/pokemon.csv"),
  has_winners = TRUE
){
  tmp <- tibble(
    A = combats$First_pokemon,
    B = combats$Second_pokemon
  )
  tmp <- combat_advantage_tibble(tmp, pokemon)
  if(has_winners){
    wins <- ifelse(combats$Winner==combats$First_pokemon,1,2)
    wins <- tibble(winner = wins)
    tmp <- bind_cols(tmp, wins)
    tmp$winner <- as.factor(tmp$winner)
  }
  return(tmp)
}

#' Compute a tibble giving the advantage of the winner over the loser
#'
#' @param combats combats data : first pokemon, second pokemon, optionally winner.
#'                Default : read combats data in data folder
#' @param pokemon pokemon data. Default : read in data folder
#'
#' @return
#' #' a tibble as specified in combat_advantage_tibble with optionally
#' @export
#'
#' @examples
#' t <- winner_advantage_tibble()
winner_advantage_tibble <- function(
  combats = read_csv("data/combats.csv"),
  pokemon = read_csv("data/pokemon.csv")
){
  losers <- ifelse(
    combats$Winner == combats$First_pokemon, 
    combats$Second_pokemon,
    combats$First_pokemon
    )
  tmp <- tibble(
    A = combats$Winner,
    B = losers
  )
  tmp <- combat_advantage_tibble(tmp, pokemon)
  return(tmp)
}
