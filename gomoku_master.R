# rm(list = ls())

rotate = function(x) t(apply(x, 2, rev))

same = function(a,b){
  return(ifelse(all(dim(a) == dim(b)), all(a[which(a != b)] == 2), FALSE))
}

pat_recog = function(board, pattern){
  n_pat = 0
  for(it in 1:3){
    board = rotate(board)
    n_board = dim(board)
    n_pattern = dim(pattern)
    for(i in 1:(n_board[1]-n_pattern[1])){
      for(j in 1:(n_board[2]-n_pattern[2])){
        sub_board = board[i:(i+n_pattern[1]-1), j:(j+n_pattern[2]-1)]
        if(same(pattern, sub_board)){
          n_pat = n_pat + 1
        }
      }
    }
  }
  return(n_pat)
}

#Black winning pattern
win_b1 = matrix(rep(1,5),1,5)
win_b2 = matrix(c(1,2,2,2,2,
                  2,1,2,2,2,
                  2,2,1,2,2,
                  2,2,2,1,2,
                  2,2,2,2,1),5,5)

# White winning pattern
win_w1 = matrix(rep(-1,5),1,5)
win_w2 = matrix(c(-1,2,2,2,2,
                  2,-1,2,2,2,
                  2,2,-1,2,2,
                  2,2,2,-1,2,
                  2,2,2,2,-1),5,5)

black_win = function(board){
  return(any(pat_recog(board, win_b1) >= 1,
             pat_recog(board, win_b2) >= 1))
}

white_win = function(board){
  return(any(pat_recog(board, win_w1) >= 1,
             pat_recog(board, win_w2) >= 1))
}

init_chessboard = function(n = 19){
  chessboard <<- matrix(rep(0, n^2),n,n)
  par(mar = rep(0, 4))
  plot(1:n, type = "n", xlim = c(1, n), axes = FALSE, xlab = "", 
       ylab = "", bty = "o", lab = c(n, n, 1))
  segments(1, 1:n, n, 1:n)
  segments(1:n, 1, 1:n, n)
  points(rep(c(4, 10, 16), 3), rep(c(4, 10, 16), each = 3), 
         pch = 19, cex = 1.2)
  box()
}

play_black = function(x,y){
  if(chessboard[x,y] == 0){
    chessboard[x,y] <<- 1
    points(x,y, cex = 3, pch = 19, bg = "black")
    return(TRUE)
  }else{
    return(FALSE)
  }
}

play_white = function(x,y){
  if(chessboard[x,y] == 0){
    chessboard[x,y] <<- -1
    points(x,y, cex = 3, pch = 21, bg = "white")
    return(TRUE)
  }else{
    return(FALSE)
  }
}

index_to_cord = function(index){
  n = 1
  l = vector()
  for(i in 1:n_board){
    for(j in 1:n_board){
      a = c(n, i, j)
      l = rbind(l,a)
      n = n + 1
    }
  }
  x = l[which(l[,1] == index),][2]
  y = l[which(l[,1] == index),][3]
  return(c(x,y))
}

black_random = function(){
  possible_moves = which(chessboard == 0)
  choice_index = sample(possible_moves,1)
  x = index_to_cord(choice_index)[1]
  y = index_to_cord(choice_index)[2]
  play_black(x,y)
  cat(paste0("Black plays ", x, ", ", y, "\n"))
  Sys.sleep(1)
}

white_random = function(){
  possible_moves = which(chessboard == 0)
  choice_index = sample(possible_moves,1)
  x = index_to_cord(choice_index)[1]
  y = index_to_cord(choice_index)[2]
  play_white(x,y)
  cat(paste0("White plays ", x, ", ", y, "\n"))
  Sys.sleep(1)
}

black_manual = function(){
  l = locator(1)
  x = min(n_board, max(1, round(l$x)))
  y = min(n_board, max(1, round(l$y)))
  while(chessboard[x,y] != 0){
    l = locator(1)
    x = min(n_board, max(1, round(l$x)))
    y = min(n_board, max(1, round(l$y)))
  }
  play_black(x,y)
  cat(paste0("Black plays ", x, ", ", y, "\n"))
}

white_manual = function(){
  l = locator(1)
  x = min(n_board, max(1, round(l$x)))
  y = min(n_board, max(1, round(l$y)))
  while(chessboard[x,y] != 0){
    l = locator(1)
    x = min(n_board, max(1, round(l$x)))
    y = min(n_board, max(1, round(l$y)))
  }
  play_white(x,y)
  cat(paste0("White plays ", x, ", ", y, "\n"))
}

play_game = function(black_player = "manual",
                     white_player = "manual"){
  game_over = FALSE
  n_board <<- 9
  init_chessboard(n = n_board)
  n = 1
  while(!game_over){
    if((-1)^n<0){
      cat("It's Black's turn:  \n")
      if(black_player == "manual"){
        black_manual()
      }else if(black_player == "random"){
        black_random()
      }else if(black_player == "smart"){
        black_smart()
      }
      if(black_win(board = chessboard)){
        game_over = TRUE
      }
    }else{
      cat("It's White's turn:  \n")
      if(white_player == "manual"){
        white_manual()
      }else if(white_player == "random"){
        white_random()
      }else if(white_player == "smart"){
        white_smart()
      }
      if(white_win(board = chessboard)){
        game_over = TRUE
      }
    }
    n = n + 1
    if(length(which(chessboard == 0)) == 0){
      game_over = TRUE
    }
  }
  cat("\nGame over! \n")
  if(black_win(chessboard)){
    cat("Black wins! \n")
  }else if(white_win(chessboard)){
    cat("White wins! \n")
  }else{
    cat("Draw! \n")
  }
}

play_game(black_player = "smart", white_player = "smart")