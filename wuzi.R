wuzi = function(){
  
  board_dim = c(19,19)
  
  init_board = function(){
    board <<- matrix(0, nrow = board_dim[1], ncol = board_dim[2])
  }
  
  play_black = function(x,y){
    if(board[x,y] != -1){
      board[x,y] <<- 1
    }
  }
  
  play_white = function(x,y){
    if(board[x,y] != 1){
      board[x,y] <<- -1
    }
  }
  
  black_win = function(){
    
    win_line = function(board_line){
      for(i in 1:(length(board_line)-5)){
        temp = board_line[i:(i + 5)]
        if(sum(temp) == 5){
          return(5)
        }else if(sum(temp) == -5){
          return(-5)
        }else{
          return(0)
        }
      }
    }
    
    #Check all the vertical and horizontal lines
    for(i in 1:board_dim[1]){
      if( any(win_line(board[i,]) == 5,
              win_line(board[,i]) == 5) ){
        return(TRUE)
      }
    }
    
    #Check all the diagonal lines
    d = row(board) - col(board)
    dig = split(board,d)
    for(i in 1:length(dig)){
      temp = unlist(dig[[i]])
      if(length(temp) >= 5){
        if(win_line(temp) == 5){
          return(TRUE)
        }
      }
    }
    return(FALSE)
  }
  
  white_win = function(){
    
    win_line = function(board_line){
      for(i in 1:(length(board_line)-5)){
        temp = board_line[i:(i + 5)]
        if(sum(temp) == 5){
          return(5)
        }else if(sum(temp) == -5){
          return(-5)
        }else{
          return(0)
        }
      }
    }
    
    #Check all the vertical and horizontal lines
    for(i in 1:board_dim[1]){
      if( any(win_line(board[i,]) == -5,
              win_line(board[,i]) == -5) ){
        return(TRUE)
      }
    }
    
    #Check all the diagonal lines
    d = row(board) - col(board)
    dig = split(board,d)
    for(i in 1:length(dig)){
      temp = unlist(dig[[i]])
      if(length(temp) >= 5){
        if(win_line(temp) == -5){
          return(TRUE)
        }
      }
    }
    return(FALSE) 
  }
  
  draw = function(){
    return(prod(board) != 0)
  }
  
  end_game = function(){
    return(any(black_win(),
               white_win(),
               draw()))
  }
  
  init_graphics = function(){
    cf = -0.5
    plot(0, 0, cex = 0,
         xlim = c(1-cf, board_dim[1]+cf),
         ylim = c(1-cf, board_dim[2]+cf),
         xaxt = 'n',
         yaxt = 'n',
         xlab = "",
         ylab = "")
    for(i in 1:board_dim[1]){
      abline(h = i)
    }
    for(i in 1:board_dim[2]){
      abline(v = i)
    }
  }
  
  update_board = function(){
    
    draw_black = function(x,y){
      points(x,y, cex = 2, pch = 16)
    }
    
    draw_white = function(x,y){
      points(x,y, cex = 2, pch = 1)
    }
    
    for(i in 1:board_dim[1]){
      for(j in 1:board_dim[2]){
        if(board[i,j] == 1){
          draw_black(i,j)
        }else if(board[i,j] == -1){
          draw_white(i,j)
        }
      }
    }
    
  }
  
  again = TRUE
  
  while(again){
    init_board()
    init_graphics()
    while(!end_game()){
      cat("Where to put black? \n")
      pos = readline(prompt = "In the format of x,y \n")
      play_black(as.numeric(unlist(strsplit(pos, ",")))[1],
                 as.numeric(unlist(strsplit(pos, ",")))[2])
      update_board()
      if(end_game()){
        break
      }
      cat("Where to put white? \n")
      pos = readline(prompt = "In the format of x,y \n")
      play_white(as.numeric(unlist(strsplit(pos, ",")))[1],
                 as.numeric(unlist(strsplit(pos, ",")))[2])
      update_board()
    }
    
    if(black_win()){
      cat("Black wins!\n")
    }else if(white_win()){
      cat("white wins! \n")
    }else{
      cat("This is a draw! \n")
    }
    
    again = readline("Do you want to play again? y/n")
    again = (again == "y") 
  }
  
}

wuzi()



