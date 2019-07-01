board = matrix(rep(0, 361), 19, 19)

detact_shape = function(mat, current_board){
  
  it_x = 18 - dim(mat)[1]
  it_y = 18 - dim(mat)[2]
  
  unequal = vector()
  for(x in 1:it_x){
    for(y in 1:it_y){
      sub_board = board[x : x + dim(mat)[1], y : y + dim(mat)[2]]
      for(i in 1:dim(mat)[1]){
        for(j in 1:dim(mat)[2]){
          if(mat[i,j] != sub_board[i,j]){
            unequal = c(unequal, mat[i,j])
          }
        }
      }
    }
  }
  
  
  
  

  return(all(unequal == -1))
}