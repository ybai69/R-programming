# Name: YUE BAI
# Email: bai42@wisc.edu

rm(list = ls())

# Implement Connect Four in the same manner that we
# implemented Tic-tac-toe in class. Start by implementing
# the helper functions, below, and testing them by running
#   source("hw3test.R")
# Then write code for the game itself.
#
# We'll test your code by running
#   source("hw3.R")
# We might also play Connect Four, read your code, and do other tests.

# Returns TRUE if vector v (of character strings) contains
# (at least) four in a row of player (character string). e.g.
#   four.in.a.row("X", c("O","X","X","X","X","O"))
# is TRUE, while
#   four.in.a.row("O", c("O","X","X","X","X","O"))
# is FALSE.
four.in.a.row = function(player, v, debug=FALSE) {
  if (debug) {
    cat(sep="", "four.in.a.row(player=", player, ", v=", v, ")\n")
  }
  #if(sum(v==player)>=4){
  #  return(TRUE)
  #}else{
  #  return(FALSE) 
  #}
  l = length(v)
  if (l < 4) {
  	return(FALSE)
  }
  for (i in 1:(l - 3)) {
    if (v[i] == player) {
      win = TRUE
      for (j in (i+1):(i+3)) {
        if (v[j] != player) {
          win = FALSE
          break
        }
      }
      if (win) {
        return(TRUE)
      }
    }
  }
  return(FALSE)
}

# Returns TRUE if (matrix) board (of character strings)
# contains at least four in a row of (string) player, who
# just played in position (r, c). (Here "r" means "row" and
# "c" means "column").
#
# Hint: this function should call four.in.a.row() four times.
won = function(player, board, r, c, debug=FALSE) {
  if (debug) {
    cat(sep="", "won(player=", player, ", board=\n")
    print(board)
    cat(sep="", ", r=", r, ", c=", c, ")\n")
  }
  if((four.in.a.row(player=player, v=board[,c]))|
       (four.in.a.row(player=player, v=board[r,]))|
       (four.in.a.row(player=player, v=board[row(board) - col(board) == r - c]))|
       (four.in.a.row(player=player, v=board[row(board) + col(board) == r + c]))){
    return(TRUE)
  }else{
    return(FALSE) 
  }  
}

# Returns largest index of an empty position in column col
# of (matrix) board. If there is no such empty position in
# board, return value is NULL.
largest.empty.row = function(board, col, debug=FALSE) {
  if (debug) {
    cat(sep="", "largest.empty.row(board=\n")
    print(board)
    cat(sep="", ", col=", col, ")\n")
  }
  if((sum(board[,col]==""))>0){
    return(sum(board[,col]==""))
  }else{
    return(NULL)
  }
}

minimax = function(board, player, lastr, lastc, alpha = -100000, beta = 100000, depth = 7, max_depth = 7) {
  r = nrow(board)
  c = ncol(board)
  
  # terminating conditions
  if (won("X", board, lastr, lastc)) {
    return(-1.0 / (max_depth - depth + 1))
  }
  if (won("O", board, lastr, lastc)) {
    return(1.0 / (max_depth - depth + 1))
  }
  if (depth == 0) {
    return(0.0)
  }
  
  
  if (player == "X") { # minimizing
    
    for(col in 1:c) {
      row = largest.empty.row(board, col)
      if (!is.null(row)) {
        board[row, col] = "X"
        beta = min(beta, minimax(board, "O", row, col, alpha, beta, depth - 1, max_depth))
        board[row, col] = ""
        
        if (beta <= alpha) {
          break
        }
      }
    }
    return(beta)
    
  } else if (player == "O") { # maximazing
    
    for(col in 1:c) {
      row = largest.empty.row(board, col)
      if (!is.null(row)) {
        board[row, col] = "O"
        alpha = max(alpha, minimax(board, "X", row, col, alpha, beta, depth - 1, max_depth))
        board[row, col] = ""
        
        if (beta <= alpha) {
          break
        }
      }
    }
    return(alpha)
    
  }
  
}

# ... your code to implement Connect Four using the
# functions above ...
par(pty="s",xaxs="i",yaxs="i")
r = 6
c = 7
x = rep(1:c, each = r)
y = rep(1:r, times = c)
symbols(x, y, rectangles=cbind(rep(1, times=r*c),rep(1,times=r*c)),
        inches=FALSE, # match squares to axes
        xlim=c(0,c+1),
        ylim=c(r+1,0), # flip y axis to match matrix format
        axes=FALSE,
        xlab="",
        ylab=""
)
box()
board = matrix(data=rep("", times=r*c), nrow=r, ncol=c)

player = "O"
for (i in 1:(r*c)) {
  if (player == "X") { # human player
    repeat { # require play in empty square
      index = identify(x,y, n=1, plot=FALSE)
      col = x[index] 
      if (!is.null(largest.empty.row(board,col))) {
        row = largest.empty.row(board,col)
        break
      } else {
        text(x=r/2, y=0.3, labels="Click on an column with empty row.")
      }
    }
  } else { # computer player
    #repeat { 
    #  index = sample(x=which(c(board == "")), size=1)
    #  col = x[index] 
    #  if (!is.null(largest.empty.row(board,col))) {
    #    row = largest.empty.row(board,col)
    #    break
    #  } 
    #}
    index = 0
    max_value = -200000
    col = 1
    
    for (cc in 1:c) {
      rr = largest.empty.row(board, cc)
      if (!is.null(rr)) {
        board[rr, cc] = "O"
        val = minimax(board, "X", rr, cc)
        board[rr, cc] = ""
        if (val > max_value) {
          max_value = val
          col = cc
          if (val == 1.0) {
            break
          }
        }
      }
    }
    row = largest.empty.row(board, col)
  }
  text(x=col, y=row, labels=player)
  board[row, col] = player
  cat(sep="", "i=", i, ", player=", player, ", index=", index, "\n") # debugging
  print(board) # debugging
  if (won(player, board,row,col)) {
    text(x=r/2+1, y=c-0.2, labels=paste(sep="", player, " won!"), col="red")
    break
  }
  player = ifelse(test=(player == "X"), yes="O", no="X")
}

# Hint: this program is modeled on the tic-tac-toe program we did in
# class, so studying the latter program is worthwhile.

# Note that a user click in a column indicates that a checker should
# go to that column's lowest empty row (unless the column is full).

# Note that you should implement a computer player. At the least, it
# should choose randomly from among the non-full columns. (Feel free
# to do much more! If your computer player beats me on its first try,
# you will earn a package of M&Ms. This is a hard task. Feel free to
# ask for tips.)