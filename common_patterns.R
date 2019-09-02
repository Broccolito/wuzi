five_1 = matrix(c(1,1,1,1,1),1,5)
five_2 = matrix(c(1,2,2,2,2,
                  2,1,2,2,2,
                  2,2,1,2,2,
                  2,2,2,1,2,
                  2,2,2,2,1),5,5)

open4_1 = matrix(c(0,1,1,1,1,0),1,6)
open4_2 = matrix(c(0,2,2,2,2,2,
                   2,1,2,2,2,2,
                   2,2,1,2,2,2,
                   2,2,2,1,2,2,
                   2,2,2,2,1,2,
                   2,2,2,2,2,0),6,6)
open4_3 = matrix(c(1,0,1,1,1,0,1),1,7)
open4_4 = matrix(c(1,2,2,2,2,2,2,
                   2,0,2,2,2,2,2,
                   2,2,1,2,2,2,2,
                   2,2,2,1,2,2,2,
                   2,2,2,2,1,2,2,
                   2,2,2,2,2,0,2,
                   2,2,2,2,2,2,1),7,7)

hemi_obs4_1 = matrix(c(-1,1,1,1,1,0),1,6)
hemi_obs4_2 = matrix(c(-1,2,2,2,2,2,
                       2,1,2,2,2,2,
                       2,2,1,2,2,2,
                       2,2,2,1,2,2, 
                       2,2,2,2,2,0),6,6)

open3_1 = matrix(c(0,1,1,1,0),1,5)
open3_2 = matrix(c(1,1,0,1),1,4)
open3_3 = matrix(c(0,2,2,2,2,
                   2,1,2,2,2,
                   2,2,1,2,2,
                   2,2,2,1,2,
                   2,2,2,2,0),5,5)
open3_4 = matrix(c(1,2,2,2,
                   2,1,2,2,
                   2,2,0,2,
                   2,2,2,1),4,4)

hemi_obs3_1 = matrix(c(-1,1,1,1,0),1,5)
hemi_obs3_2 = matrix(c(-1,1,1,0,1),1,5)
hemi_obs3_3 = matrix(c(1,1,0,1,-1),1,5)
hemi_obs3_4 = matrix(c(-1,2,2,2,2,
                       2,1,2,2,2,
                       2,2,1,2,2,
                       2,2,2,1,2,
                       2,2,2,2,0),5,5)
hemi_obs3_5 = matrix(c(1,2,2,2,2,
                       2,1,2,2,2,
                       2,2,0,2,2,
                       2,2,2,1,2,
                       2,2,2,2,-1),5,5)
hemi_obs3_6 = matrix(c(-1,2,2,2,2,
                       2,1,2,2,2,
                       2,2,1,2,2,
                       2,2,2,0,2,
                       2,2,2,2,1),5,5)

open2_1 = matrix(c(0,1,1,0),1,4)
open2_2 = matrix(c(0,2,2,2,
                   2,1,2,2,
                   2,2,1,2,
                   2,2,2,0),4,4)
open2_3 = matrix(c(0,1,0,1,0),1,5)
open2_4 = matrix(c(0,2,2,2,2,
                   2,1,2,2,2,
                   2,2,0,2,2,
                   2,2,2,1,2,
                   2,2,2,2,0),5,5)

hemi_obs2_1 = matrix(c(-1,1,1,0),1,4)
hemi_obs2_2 = matrix(c(-1,2,2,2,
                       2,1,2,2,
                       2,2,1,2,
                       2,2,2,0),4,4)
hemi_obs2_3 = matrix(c(-1,1,0,1,0),1,5)
hemi_obs2_4 = matrix(c(-1,2,2,2,2,
                       2,1,2,2,2,
                       2,2,0,2,2,
                       2,2,2,1,2,
                       2,2,2,2,0),5,5)

flip_color = function(mat){
  b = mat == 1
  w = mat == -1
  mat[b] = rep(-1, length(which(b)))
  mat[w] = rep(1, length(which(w)))
  return(mat)
}

scrutinize = function(board, color = "black"){
  
  wt_five = 100
  wt_open4 = 70
  wt_hemi_obs4 = 60
  wt_open3 = 50
  wt_hemi_obs3 = 40
  wt_open2 = 30
  wt_hemi_obs2 = 20
  
  if(color == "white"){
    board = flip_color(board)
  }
  
  advan = pat_recog(board, five_1) * wt_five + 
    pat_recog(board, five_2) * wt_five + 
    
    pat_recog(board, open4_1) * wt_open4 + 
    pat_recog(board, open4_2) * wt_open4 + 
    pat_recog(board, open4_3) * wt_open4 + 
    pat_recog(board, open4_4) * wt_open4 + 
    
    pat_recog(board, open3_1) * wt_open3 + 
    pat_recog(board, open3_2) * wt_open3 + 
    pat_recog(board, open3_3) * wt_open3 + 
    pat_recog(board, open3_4) * wt_open3 + 
    
    pat_recog(board, open2_1) * wt_open2 + 
    pat_recog(board, open2_2) * wt_open2 + 
    pat_recog(board, open2_3) * wt_open2 + 
    pat_recog(board, open2_4) * wt_open2 + 
    
    pat_recog(board, hemi_obs4_1) * wt_hemi_obs4 + 
    pat_recog(board, hemi_obs4_1) * wt_hemi_obs4 + 
    
    pat_recog(board, hemi_obs3_1) * wt_hemi_obs3 + 
    pat_recog(board, hemi_obs3_2) * wt_hemi_obs3 + 
    pat_recog(board, hemi_obs3_3) * wt_hemi_obs3 + 
    pat_recog(board, hemi_obs3_4) * wt_hemi_obs3 + 
    pat_recog(board, hemi_obs3_5) * wt_hemi_obs3 + 
    pat_recog(board, hemi_obs3_6) * wt_hemi_obs3 + 
    
    pat_recog(board, hemi_obs2_1) * wt_hemi_obs2 + 
    pat_recog(board, hemi_obs2_2) * wt_hemi_obs2 + 
    pat_recog(board, hemi_obs2_3) * wt_hemi_obs2 + 
    pat_recog(board, hemi_obs2_4) * wt_hemi_obs2
  
  disadvan =  -1 * (pat_recog(board, flip_color(five_1)) * wt_five + 
    pat_recog(board, flip_color(five_2)) * wt_five + 
    
    pat_recog(board, flip_color(open4_1)) * wt_open4 + 
    pat_recog(board, flip_color(open4_2)) * wt_open4 + 
    pat_recog(board, flip_color(open4_3)) * wt_open4 + 
    pat_recog(board, flip_color(open4_4)) * wt_open4 + 
    
    pat_recog(board, flip_color(open3_1)) * wt_open3 + 
    pat_recog(board, flip_color(open3_2)) * wt_open3 + 
    pat_recog(board, flip_color(open3_3)) * wt_open3 + 
    pat_recog(board, flip_color(open3_4)) * wt_open3 + 
    
    pat_recog(board, flip_color(open2_1)) * wt_open2 + 
    pat_recog(board, flip_color(open2_2)) * wt_open2 + 
    pat_recog(board, flip_color(open2_3)) * wt_open2 + 
    pat_recog(board, flip_color(open2_4)) * wt_open2 + 
    
    pat_recog(board, flip_color(hemi_obs4_1)) * wt_hemi_obs4 + 
    pat_recog(board, flip_color(hemi_obs4_1)) * wt_hemi_obs4 + 
    
    pat_recog(board, flip_color(hemi_obs3_1)) * wt_hemi_obs3 + 
    pat_recog(board, flip_color(hemi_obs3_2)) * wt_hemi_obs3 + 
    pat_recog(board, flip_color(hemi_obs3_3)) * wt_hemi_obs3 + 
    pat_recog(board, flip_color(hemi_obs3_4)) * wt_hemi_obs3 + 
    pat_recog(board, flip_color(hemi_obs3_5)) * wt_hemi_obs3 + 
    pat_recog(board, flip_color(hemi_obs3_6)) * wt_hemi_obs3 + 
    
    pat_recog(board, flip_color(hemi_obs2_1)) * wt_hemi_obs2 + 
    pat_recog(board, flip_color(hemi_obs2_2)) * wt_hemi_obs2 + 
    pat_recog(board, flip_color(hemi_obs2_3)) * wt_hemi_obs2 + 
    pat_recog(board, flip_color(hemi_obs2_4)) * wt_hemi_obs2)
  
  overall_score = advan^1.2 + disadvan^0.8
  
  return(overall_score)
}

black_smart = function(){
  if(all(chessboard != 1)){
    x = round(n_board/2)
    y = round(n_board/2)
    play_black(x, y)
    cat(paste0("Black plays ", x, ", ", y, "\n"))
    return(NULL)
  }
  possible_moves = which(chessboard == 0)
  move_score_list = vector()
  for(move in possible_moves){
    temp_board = chessboard
    temp_board[move] = 1
    move_score = scrutinize(board = temp_board, color = "black") - scrutinize(board = chessboard, color = "black")
    move_score_list = c(move_score_list, move_score)
  }
  move_score_list[is.nan(move_score_list)] = -10000
  xy = index_to_cord(possible_moves[sample(which(move_score_list == max(move_score_list)),1)])
  x = xy[1]
  y = xy[2]
  play_black(x, y)
  cat(paste0("Black plays ", x, ", ", y, "\n"))
}

white_smart = function(){
  if(all(chessboard != -1)){
    x = round(n_board/2)
    y = round(n_board/2)
    if(chessboard[x,y] == 1){
      x = x + 1
      y = y + 1
      play_white(x, y)
      cat(paste0("White plays ", x, ", ", y, "\n"))
      return(NULL)
    }else{
      play_white(x, y)
      cat(paste0("White plays ", x, ", ", y, "\n"))
      return(NULL)
    }
  }
  possible_moves = which(chessboard == 0)
  move_score_list = vector()
  for(move in possible_moves){
    temp_board = chessboard
    temp_board[move] = -1
    move_score = scrutinize(board = temp_board, color = "white") - scrutinize(board = chessboard, color = "white")
    move_score_list = c(move_score_list, move_score)
  }
  move_score_list[is.nan(move_score_list)] = -10000
  xy = index_to_cord(possible_moves[sample(which(move_score_list == max(move_score_list)),1)])
  x = xy[1]
  y = xy[2]
  play_white(x, y)
  cat(paste0("White plays ", x, ", ", y, "\n"))
}



