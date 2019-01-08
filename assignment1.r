# Andrej Hafner (63160122)

printMaze <- function(maze, rows, cols) {
  for (x in seq(1, rows)) {
    print(maze[((x-1)*cols +1) : (x*cols)])
  }
}

moveUp <- function(position, rows, cols) {
  newPosition <- position - cols
  if (newPosition < 1) {
    return (c(position,OUT_OF_BOUNDS_PENALTY))
  } else {
    return (c(newPosition,0))
  }
}

moveDown <- function(position, rows, cols) {
  newPosition <- position + cols
  if (newPosition > rows*cols) {
    return (c(position,OUT_OF_BOUNDS_PENALTY))
  } else { 
    return (c(position + cols,0))
  }
}

moveLeft <- function(position, rows, cols) {
  newPosition <- position - 1
  if ((position - 1) %/% cols != (newPosition - 1) %/% cols) {
    return (c(position,OUT_OF_BOUNDS_PENALTY))
  } else {
    return (c(position - 1,0))
  }
}

moveRight <- function(position, rows, cols) {
  newPosition <- position + 1
  if ((position - 1) %/% cols != (newPosition - 1) %/% cols) {
    return (c(position,OUT_OF_BOUNDS_PENALTY))
  } else { 
    return (c(position + 1,0))
  }
}

# Get x and y from the position in the maze
getCoordinates <- function(position,rows,cols) {
  y <- ceiling(position / cols)
  x <- position  - (y-1) * cols
  pos <- c(x,y)
  pos
  
}

# Test the getCoordinates function
testCoordinates <- function(rows,cols) {
  for(i in 1:(rows * cols)) {
    print(getCoordinates(i,rows,cols))
  }
}


# Show the path that was optimized by the GA
showSolution <- function(maze,rows,cols,solution) {
  
  steps <- 0
  endPosition <- grep('e',maze)
  currentPosition <- grep('s', maze)
  
  for (move in solution) {
    oldPosition <- currentPosition
    steps <- steps + 1
    if (move == 'U') {
      pos_penalty <- moveUp(currentPosition, rows, cols)
      currentPosition <- pos_penalty[1]
    } else if (move == 'D') {
      pos_penalty <- moveDown(currentPosition, rows, cols)
      currentPosition <- pos_penalty[1]
    } else if (move == 'L') {
      pos_penalty <- moveLeft(currentPosition, rows, cols)
      currentPosition <- pos_penalty[1]
    } else if (move == 'R') {
      pos_penalty <- moveRight(currentPosition, rows, cols)
      currentPosition <- pos_penalty[1]
    } else {
      print('Error: Incorrect solution format')
      return(10000)
    }
    
    if (maze[currentPosition] == '#') {
      currentPosition <- oldPosition
    } else if (maze[currentPosition] == 'e') {
      break()
    } else  {
      maze[currentPosition] <- '+'
    }
  }
  
  print(all(currentPosition == endPosition))
  printMaze(maze,rows,cols)
  print(sprintf("Steps by my solution: %d",steps))
}

# Initialize the whole population in a matrix -> a rows is a single specimen
initPopulation <- function() {
  population <- matrix(0,nrow = POPULATION_SIZE, ncol = MOVES_MAX)
  
  for(i in 1:POPULATION_SIZE) {
    population[i,] <- initCandidate()
  }
  population
}

# Initializes a single candidate of the population
initCandidate <- function() {
  candidate <- character(MOVES_MAX)
  
  for(j in 1:MOVES_MAX) {
    if(j == 1) {
      candidate[j] <- sample(MOVES,1)
    } else {
      candidate[j] <- sampleWithoutBacking(candidate[j-1])
    }
  }
  candidate
}

# Return a random move, that doesn't send the candidate back to previous position
sampleWithoutBacking <- function(previous) {
  if(previous == 'D') {
    ret <- sample(c('D','L','R'),1)
  } else if(previous == 'U') {
    ret <- sample(c('U','L','R'),1)
  } else if(previous == 'L') {
    ret <- sample(c('D','L','U'),1)
  } else {
    ret <- sample(c('D','R','U'),1)
  }
  ret
}

# Penalize a candidate if it performs a bad move (moves to a position and then back to the previous one -> no sense)
penalizeBadMove <- function(previous,now) {
  ret <- 0
  if((previous == 'D' && now == 'U') || (previous == 'U' && now == 'D') || (previous == 'L' && now == 'R') || (previous == 'R' && now == 'L')) {
    ret <- BAD_MOVES_PENALTY
  }
  ret
}

# Performs crossover
crossover <- function(solutions) {
  size <- dim(solutions)
  rows <- size[1]
  cols <- size[2]
  
  # Add a random candidate to increase gene variability (doesn't work well this way)
  #solutions[rows,] <- initCandidate()
  
  newPopulation <- matrix(0,nrow = POPULATION_SIZE,ncol = MOVES_MAX)
  
  # Elitism -> always keep the fittest candidate among the parents  
  start <- 2
  newPopulation[1,] <- solutions[1,]
  
  # 2-point crossover
  for(i in start:POPULATION_SIZE) {
    splitPosition1 <- sample(ceiling(MOVES_MAX/3):MOVES_MAX-1,size = 1)
    splitPosition2 <- sample(2:(splitPosition1-1),size = 1)
    firstParent <- solutions[sample(1:rows,size = 1),]
    secondParent <- solutions[sample(1:rows,size = 1),]
    
    first <- firstParent[1:splitPosition2]
    second <- secondParent[(splitPosition2 + 1):splitPosition1]
    third <- firstParent[(splitPosition1 + 1):MOVES_MAX]
    newGene <- c(first,second,third)
    newPopulation[i,] <- mutation(newGene)
  }
  
  newPopulation
}

# Performs mutation -> samples a new move in such a way, that it doesn't go to the previous position
mutation <- function(solutions) {
  
  for(i in 2:MOVES_MAX) {
    if(runif(1) <= MUTATION_PROB) {
      solutions[i] <- sampleWithoutBacking(solutions[i-1])
    }
  }
  solutions
}

# Fitness function
simulateSolution <- function(maze, solution, rows, cols) {
  # Update this function to serve as a fitness funcition
  # The simplest example is shown here: return 1 if the solution found the exit and 0 if it did not
  
  
  fitness = 0
  # Penalties are given if it moves into a wall 
  penalties = 0
  visited = c(0)
  
  endPosition <- grep('e',maze)
  currentPosition <- grep('s', maze)
  visited = c(currentPosition)
  previousMove <- ' '
  
  for (move in solution) {
    oldPosition <- currentPosition
    
    if (move == 'U') {
      pos_penalty <- moveUp(currentPosition, rows, cols)
      currentPosition <- pos_penalty[1]
      penalties <- penalties + pos_penalty[2] + MOVE_PENALTY
      
    } else if (move == 'D') {
      pos_penalty <- moveDown(currentPosition, rows, cols)
      currentPosition <- pos_penalty[1]
      penalties <- penalties + pos_penalty[2] + MOVE_PENALTY
      
    } else if (move == 'L') {
      pos_penalty <- moveLeft(currentPosition, rows, cols)
      currentPosition <- pos_penalty[1]
      penalties <- penalties + pos_penalty[2] + MOVE_PENALTY
        
    } else if (move == 'R') {
      pos_penalty <- moveRight(currentPosition, rows, cols)
      currentPosition <- pos_penalty[1]
      penalties <- penalties + pos_penalty[2] + MOVE_PENALTY
      
    } else {
      print('Error: Incorrect solution format')
      return(10000)
    }
    if (maze[currentPosition] == '#') {
      currentPosition <- oldPosition
      # Penalize if it moves into a wall
      penalties <- penalties + WALL_PENALTY
    }
    if (maze[currentPosition] == 'e') {
      endPos <- getCoordinates(endPosition,rows,cols)
      currPos <- getCoordinates(currentPosition,rows,cols)
      
      fitness <- (abs(endPos[1] - currPos[1]) + abs(endPos[2] - currPos[2]))*DISTANCE_FROM_FINISH_MULTIPLIER + penalties
      if(fitness == 0) { 
        print(endPos)
        print(currPos)
        print(endPosition)
        print(currentPosition)
      }
      return(fitness)
    }
    
    # Check if there was a bad move
    penalties <- penalties + penalizeBadMove(previousMove,move)
    
    # Penalize if candidate went to previously visited position
    if(currentPosition %in% visited) {
      penalties <- penalties + BACKTRACKING_PENALTY
    }
    
    visited <- c(visited,currentPosition)
    previousMove <- move
  }
  endPos <- getCoordinates(endPosition,rows,cols)
  currPos <- getCoordinates(currentPosition,rows,cols)
  
  fitness <- (abs(endPos[1] - currPos[1]) + abs(endPos[2] - currPos[2]))*DISTANCE_FROM_FINISH_MULTIPLIER + penalties
  if(fitness == 0) { 
    print(endPos)
    print(currPos)
    print(endPosition)
    print(currentPosition)
  }
  return(fitness)
}


geneticAlgorithm <- function(maze, rows, cols,solution) {
  # Implement the genetic algorithm in this function
  # You should add additional parameters to the function as needed
  
  # Initialize the population
  population <- initPopulation()
  
  # Iterate through generations
  for(i in 1:GENERATIONS) {
    
    # Get the fitness of the population
    fitness <- rep(10000,POPULATION_SIZE)
    
    for(j in 1:POPULATION_SIZE) {
      fitness[j] <- simulateSolution(maze,population[j,],rows,cols)
    }
    
    ord <- order(fitness)
    
    # Get the top perfoming candidates
    bestFitness <- fitness[ord[1: KEEP_BEST]]
    topPerformers <- population[ord[1:KEEP_BEST],]
    
    
    if(all(topPerformers[1,1:length(solution)] == solution) || bestFitness[1] == 0) {
      print("We have found an optimal solution")
      print(topPerformers[1,])
      print(sprintf("Best fitness:%d",bestFitness[1]))
      break()
    }
    
    print(sprintf("Iteration %d ",i))
    print(bestFitness[1:5])
    
    if(i %% 50 == 0)
    {
      MUTATION_PROB <- MUTATION_PROB + 0.005
    }    
    
    population <- crossover(topPerformers)
  
  }
  
  showSolution(maze,rows,cols,topPerformers[1,])
  print(sprintf("Steps by optimal solution: %d",length(solution)))
  print(topPerformers[1,])
  
}

MOVE_PENALTY = 1
BAD_MOVES_PENALTY = 1.3
BACKTRACKING_PENALTY = 1.3
DISTANCE_FROM_FINISH_MULTIPLIER = 10
WALL_PENALTY = 1.5
OUT_OF_BOUNDS_PENALTY = WALL_PENALTY
# MOVES_MAX = ceiling(rows2 * cols2/4)
MOVES_MAX = ceiling(length(solution2) * 1.5)
POPULATION_SIZE = 200
MUTATION_PROB = 0.1
KEEP_BEST = 20
GENERATIONS = 300
CROSSOVER_PROB = 0.9
ELITISM = TRUE
MOVES = c('U','D','L','R')

# Works good!
# BAD_MOVES_PENALTY = 1.3
# BACKTRACKING_PENALTY = 1.3
# DISTANCE_FROM_FINISH_MULTIPLIER = 10
# WALL_PENALTY = 1.5
# OUT_OF_BOUNDS_PENALTY = WALL_PENALTY
# # MOVES_MAX = ceiling(rows2 * cols2/4)
# MOVES_MAX = ceiling(length(solution2) * 1.5)
# POPULATION_SIZE = 200
# MUTATION_PROB = 0.1
# KEEP_BEST = 20
# GENERATIONS = 500
# CROSSOVER_PROB = 0.9
# ELITISM = TRUE
# MOVES = c('U','D','L','R')

# Good - almost finds it
# BAD_MOVES_PENALTY = 1
# BACKTRACKING_PENALTY = 1
# DISTANCE_FROM_FINISH_MULTIPLIER = 1
# WALL_PENALTY = 2
# OUT_OF_BOUNDS_PENALTY = WALL_PENALTY
# MOVES_MAX = ceiling(rows2 * cols2/4)
# POPULATION_SIZE = 200
# MUTATION_PROB = 0.1
# KEEP_BEST = 15
# GENERATIONS = 300
# CROSSOVER_PROB = 0.9
# ELITISM = TRUE
# MOVES = c('U','D','L','R')

# OK?
# BAD_MOVES_PENALTY = 2
# BACKTRACKING_PENALTY = 2
# DISTANCE_FROM_FINISH_MULTIPLIER = 1
# WALL_PENALTY = 2
# OUT_OF_BOUNDS_PENALTY = WALL_PENALTY
# MOVES_MAX = ceiling(rows2 * cols2 / 3)
# POPULATION_SIZE = 130
# MUTATION_PROB = 0.1
# KEEP_BEST = 10
# GENERATIONS = 250
# CROSSOVER_PROB = 0.9
# ELITISM = TRUE
# MOVES = c('U','D','L','R')

# semi ok2
# BAD_MOVES_PENALTY = 2
# BACKTRACKING_PENALTY = 1
# DISTANCE_FROM_FINISH_MULTIPLIER = 1
# WALL_PENALTY = 1
# OUT_OF_BOUNDS_PENALTY = WALL_PENALTY
# MOVES_MAX = ceiling(rows2 * cols2 / 3)
# POPULATION_SIZE = 150
# MUTATION_PROB = 0.1
# KEEP_BEST = 10
# GENERATIONS = 300
# CROSSOVER_PROB = 0.9
# ELITISM = TRUE
# MOVES = c('U','D','L','R')

# Semi OK for maze2
# BAD_MOVES_PENALTY = 1
# BACKTRACKING_PENALTY = 1
# DISTANCE_FROM_FINISH_MULTIPLIER = 1
# WALL_PENALTY = 1
# OUT_OF_BOUNDS_PENALTY = WALL_PENALTY
# MOVES_MAX = ceiling(rows2 * cols2 / 3)
# POPULATION_SIZE = 150
# MUTATION_PROB = 0.1
# KEEP_BEST = 10
# GENERATIONS = 300
# CROSSOVER_PROB = 0.9
# ELITISM = TRUE
# MOVES = c('U','D','L','R')

# MOVES_PENALTY = 1
# DISTANCE_FROM_FINISH_MULTIPLIER = 1
# WALL_PENALTY = 1
# OUT_OF_BOUNDS_PENALTY = WALL_PENALTY
# MOVES_MAX = rows2*cols2/2
# POPULATION_SIZE = 130
# MUTATION_PROB = 0.07
# KEEP_BEST = 7
# GENERATIONS = 1000
# CROSSOVER_PROB = 0.9
# ELITISM = TRUE
# MOVES = c('U','D','L','R')

#Good for maze 1
# MOVES_PENALTY = 1
# OUT_OF_BOUNDS_PENALTY = 2
# WALL_PENALTY = 2
# MOVES_MAX = length(solution1)*2
# POPULATION_SIZE = 500
#  MUTATION_PROB = 0.1
#  KEEP_BEST = 5
# GENERATIONS = 500
#  CROSSOVER_PROB = 0.9
#  ELITISM = TRUE
# MOVES = c('U','D','L','R')
# 
#geneticAlgorithm(maze1,rows1,cols1,solution1)
#geneticAlgorithm(maze2,rows2,cols2,solution2)
geneticAlgorithm(maze3,rows2,cols2,solution3)



maze1 <- c(' ', ' ', ' ', ' ', 'e',
           ' ', '#', '#', '#', '#',
           ' ', ' ', 's', ' ', ' ',
           '#', '#', '#', '#', ' ',
           ' ', ' ', ' ', ' ', ' ')
rows1 <- 5
cols1 <- 5
solution1 <- c('L', 'L','U', 'U', 'R', 'R', 'R', 'R', 'R') 

maze2 <- c('#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#',
           '#', '#', ' ', '#', ' ', '#', ' ', ' ', ' ', ' ', '#', ' ', ' ', '#', ' ', '#', '#',
           '#', '#', 'e', '#', '#', '#', '#', '#', '#', '#', '#', '#', ' ', ' ', ' ', '#', '#',
           '#', '#', ' ', '#', ' ', '#', ' ', '#', ' ', '#', '#', '#', ' ', ' ', ' ', '#', '#',
           '#', '#', ' ', ' ', ' ', '#', ' ', '#', ' ', '#', '#', '#', '#', '#', ' ', ' ', ' ',
           '#', '#', ' ', '#', ' ', '#', ' ', '#', ' ', ' ', ' ', ' ', '#', '#', ' ', '#', ' ',
           '#', ' ', ' ', '#', ' ', ' ', ' ', '#', '#', '#', '#', ' ', '#', '#', ' ', '#', ' ',
           '#', '#', ' ', '#', ' ', '#', ' ', '#', '#', '#', '#', ' ', '#', '#', ' ', '#', ' ',
           '#', '#', ' ', '#', ' ', ' ', ' ', ' ', ' ', '#', '#', ' ', '#', '#', ' ', ' ', ' ',
           '#', ' ', ' ', ' ', '#', ' ', '#', ' ', '#', ' ', ' ', ' ', ' ', ' ', ' ', '#', ' ',
           '#', ' ', '#', ' ', '#', ' ', '#', ' ', '#', ' ', '#', '#', '#', '#', ' ', '#', ' ',
           '#', ' ', '#', ' ', '#', ' ', '#', ' ', '#', ' ', '#', '#', '#', '#', ' ', '#', ' ',
           '#', ' ', '#', ' ', '#', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', '#', ' ', '#', ' ',
           '#', ' ', ' ', ' ', '#', ' ', '#', ' ', '#', '#', '#', '#', ' ', '#', ' ', '#', ' ',
           '#', '#', ' ', '#', '#', '#', '#', ' ', '#', '#', ' ', ' ', ' ', ' ', ' ', '#', 's',
           '#', '#', ' ', ' ', '#', ' ', ' ', ' ', '#', '#', ' ', '#', '#', '#', ' ', '#', ' ',
           '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', ' ', '#', '#',
           '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#')

maze3 <- c('#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#',
           '#', '#', ' ', '#', ' ', '#', ' ', ' ', ' ', ' ', '#', ' ', ' ', '#', ' ', '#', '#',
           '#', '#', ' ', '#', '#', '#', '#', '#', '#', '#', '#', '#', ' ', ' ', ' ', '#', '#',
           '#', '#', ' ', '#', ' ', '#', ' ', '#', ' ', '#', '#', '#', ' ', ' ', ' ', '#', '#',
           '#', '#', ' ', ' ', ' ', '#', ' ', '#', ' ', '#', '#', '#', '#', '#', ' ', ' ', ' ',
           '#', '#', ' ', '#', ' ', '#', ' ', '#', ' ', ' ', ' ', ' ', '#', '#', ' ', '#', ' ',
           '#', ' ', ' ', '#', ' ', ' ', ' ', '#', '#', '#', '#', ' ', '#', '#', ' ', '#', ' ',
           '#', '#', ' ', '#', ' ', '#', ' ', '#', '#', '#', '#', ' ', '#', '#', ' ', '#', ' ',
           '#', '#', ' ', '#', ' ', ' ', ' ', ' ', ' ', '#', '#', ' ', '#', '#', ' ', ' ', ' ',
           '#', ' ', ' ', ' ', '#', ' ', '#', ' ', '#', ' ', ' ', ' ', ' ', ' ', ' ', '#', ' ',
           '#', ' ', '#', ' ', '#', ' ', '#', ' ', '#', ' ', '#', '#', '#', '#', ' ', '#', ' ',
           '#', ' ', '#', ' ', '#', ' ', '#', ' ', '#', ' ', '#', '#', '#', '#', ' ', '#', ' ',
           '#', ' ', '#', ' ', '#', ' ', ' ', ' ', 'e', ' ', ' ', ' ', ' ', '#', ' ', '#', ' ',
           '#', ' ', ' ', ' ', '#', ' ', '#', ' ', '#', '#', '#', '#', ' ', '#', ' ', '#', ' ',
           '#', '#', ' ', '#', '#', '#', '#', ' ', '#', '#', ' ', ' ', ' ', ' ', ' ', '#', 's',
           '#', '#', ' ', ' ', '#', ' ', ' ', ' ', '#', '#', ' ', '#', '#', '#', ' ', '#', ' ',
           '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', ' ', '#', '#',
           '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#')

solution2 <- c('U', 'U', 'U', 'U', 'U', 'U', 'L', 'L', 'D', 'L', 'L', 'L', 'L', 'L', 'D', 'D', 'D', 'L', 'L', 'L', 'L', 'U', 'U', 'U', 'U', 'L', 'U', 'U', 'U', 'U', 'L', 'L', 'U', 'U')
solution3 <- solution2[1:18]
cols2 <- 17
rows2 <- 18
