find_value <- function(player,low,high,start){
  found_value <- NULL
  if(start == low){
    last_val_in = NULL
    while(is.null(found_value) == TRUE){
      # If the start == low then we know that the player is not in the current solution.
      # So we set the first guess to be the midpoint of low and high.
      guess = ceiling((low+high)/2)
      # Now is the player in the current solution?
      # Update the constraints.
      # The salary constraint is c1_fit
      c1_fit[which(full_data2$player==input$candidate)] <- guess
      # Now solve the new MIP problem. 
      constraints_auction_block <- reactive({
        matrix(rbind(c1_fit,c2,c3,c4,c5,c6()),nrow=6)
      })
      solve_auction_block_model <- lp("max",
                        objective_model,
                        constraints_auction_block(),
                        direction,
                        rhs(),
                        all.bin=TRUE,
                        num.bin.solns = 1)$solution
      # Is the candidate in the model?
      if(solve_auction_block_model[which(full_data2$player==input$candidate)] == 1){
        # This means the player is now in the model at this value, but we want to find the highest value for which
        # he is in the model. 
        
        # We'll exit when the guess is equal to last_val_in
        if(guess == last_val_in){
          return(last_val_in)
        }
        # Otherwise we have to keep looking, so update the guess bounds and set a last_val_in variable
        else{
          last_val_in = guess
          low = guess
        }
      }
      else if(solve_auction_block_model[which(full_data2$player==input$candidate)] != 1){
        # This means the player is not in the model at this value of guess.
        # If we havent' yet found a valid value of guess to include the player we need to lower the guess
        if(is.null(last_val_in) == TRUE){
          high = guess
        }
        # If we have found a valid value to include the player our new limits need to be what we just missed on
        # and the last_val_in
        else if(is.null(last_val_in) == FALSE){
          low = guess
          high = last_val_in
        }
      }
    }
  }
  else if(start == high){
    last_val_in = NULL
    while(is.null(found_value) == TRUE){
      # If the start == high then we know that the player is in the current solution.
      # We want to find the maximum possible value that keeps him in the solution
      # So we set the first guess to be the midpoint of low and high.
      guess = ceiling((low+high)/2)
      # Now is the player in the current solution?
      # Update the constraints.
      # The salary constraint is c1_fit
      c1_fit[which(full_data2$player==input$candidate)] <- guess
      # Now solve the new MIP problem. 
      constraints_auction_block <- reactive({
        matrix(rbind(c1_fit,c2,c3,c4,c5,c6()),nrow=6)
      })
      solve_auction_block_model <- lp("max",
                                      objective_model,
                                      constraints_auction_block(),
                                      direction,
                                      rhs(),
                                      all.bin=TRUE,
                                      num.bin.solns = 1)$solution
      # Is the candidate in the model?
      if(solve_auction_block_model[which(full_data2$player==input$candidate)] == 1){
        # This means the player is now in the model at this value, but we want to find the highest value for which
        # he is in the model. 
        # We'll exit when the guess is equal to last_val_in
        if(guess == last_val_in){
          return(last_val_in)
        }
        # Otherwise we have to keep looking, so update the guess bounds and set a last_val_in variable
        else{
          last_val_in = guess
          low = guess
        }
      }
      else if(solve_auction_block_model[which(full_data2$player==input$candidate)] != 1){
        # This means the player is not in the model at this value of guess.
        # If we havent' yet found a valid value of guess to include the player we need to lower the guess
        if(is.null(last_val_in) == TRUE){
          high = guess
        }
        # If we have found a valid value to include the player our new limits need to be what we just missed on
        # and the last_val_in
        else if(is.null(last_val_in) == FALSE){
          low = guess
          high = last_val_in
        }
      }
    }
  }
  return(found_value)
}
