rm(list = ls()) # Clear variables from global environment before proceeding.
setwd("C:/Temp/Simulator")

############################################
## R script to create room files for the agent based simulator used in King et 
## al. 2022 https://doi.org/10.1080/23249935.2021.2017510.  

## Currently, environments are created manually by the user.

## Each function presented here creates a given environmental layout.  

## The environments used in King et al. 2022 are created using the 'empty_room' 
## and 'err_stud_room2' functions.
############################################

## Supplementary function used in assigning floor field values:

get.nns <- function(x, y, ni, nj, obstacles) {
  
  ### Finds the nearest (direct) neighbours to position (x,y) in grid that 
  ### aren't obstacles or haven't already been assigned a floor field value.
  
  ### Requires:
  ### 1. x-coordinate of grid cell to find nearest neighbours.
  ### 2. y-coordinate of grid cell to find nearest neighbours.
  ### 3. The x-dimension of the grid.
  ### 4. The y-dimension of the grid.
  ### 5. The obstacle positions in the grid.
  
  ### Returns a matrix of the x- and y- coordinates of the nearest neighbour
  ### cells of (x,y) that aren't obstacles or already assigned a floor field.

  # Coordinates of nearest neighbours of position (x,y).
  xp <- c(-1, 0, 1, 0) + x
  yp <- c(0, 1, 0, -1) + y
  del <- vector() # This will remove coordinates that are invalid.
  # For each element in the xp and yp vectors...
  for (i in 1:length(xp)) {
    #...if any nearest neighbour lies outside of the grid...
    if (xp[i] > ni || xp[i] < 1 || yp[i] > nj || yp[i] < 1) {
      # ...append index to del vector for removal.
      del <- c(del, i)
    } else { # Otherwise, if the nearest neighbours lie within the grid...
      #...if the floor field at the nearest neighbours is an obstacle or already
      # assigned...
      if (obstacles[xp[i], yp[i]] >= 0) {
        # ...append index to del vector for removal.
        del <- c(del, i)
      }
    }
  }
  # If there are any nearest neighbours that need to be removed...
  if (length(del) > 0) {
    # ...remove them!
    xp <- xp[-del]
    yp <- yp[-del]
  }
  return(rbind(xp, yp))
}

## Supplementary function used in assigning floor field values:

get.diag <- function(x, y, ni, nj, obstacles) {
  
  ### Finds the distance for unassigned diagonal nearest neighbours to 
  ### position (x,y) in grid, assigns a temporary distance value.  This is then 
  ### compared any current distance values assigned to the cell from this and/or 
  ### previous iterations.
  
  ### Takes the following inputs:
  ### 1. x-coordinate of grid cell to find nearest neighbours.
  ### 2. y-coordinate of grid cell to find nearest neighbours.
  ### 3. The x-dimension of the grid.
  ### 4. The y-dimension of the grid.
  ### 5. The obstacle positions in the grid.
  
  ### Outputs the minimum distances of the diagonal nearest neighbour cells of (x,y)
  ### that aren't obstacles or lie outside of the grid.
  
  # Coordinates of diagonal nearest neighbours of position (x,y).
  xp <- c(-1, 1, 1, -1) + x
  yp <- c(-1, 1, -1, 1) + y
  del <- vector() # This will remove coordinates that are invalid.
  # For each element in the xp and yp vectors...
  for (i in 1:length(xp)) {
    # If any nearest neighbour lies outside of the grid...
    if (xp[i] > ni || xp[i] < 1 || yp[i] > nj || yp[i] < 1) {
      # ...append index to del vector for removal.
      del <- c(del, i)
    } else { # Otherwise, if the nearest neighbour lies within the grid...
      #...if floor field at nearest neighbour has not already been assigned...
      if (obstacles[xp[i], yp[i]] <= 0) {
        # ...append index to del vector for removal.
        del <- c(del, i)
      }
    }
  }
  # If any of the diagonals has not been assigned a value...
  if (length(del) < 4) { # There can only be up to four diagonal nearest neighbours.
    if (length(del) > 0) {
      #...remove them before assigning the minimum floor field in the 
      # following loop.
      xp <- xp[-del]
      yp <- yp[-del]
    }
    # Create a vector of distance values for the remaining assigned neighbours.
    out <- rep(0, length(xp))
    # For each assigned neighbour...
    for (i in 1:length(xp)) {
      #...find their current distance value.
      out[i] <- obstacles[xp[i], yp[i]]
    }
    # Find the smallest floor field value.
    temp <- min(out)
  }
  # Otherwise, if all the diagonal neighbours have had distance values assigned 
  # from previous iterations...
  else {
    #...return a ludicrously large value for create.static.
    temp <- ni * nj
  }
  return(temp)
}

av_field_calc <- function(field, zone = 3) {
  ### This function calculates the average distance value of a cell over its 
  ### nearest neighbourhood of a given size.  Used to find the average distance
  ### so that obstacles have lower floor fields in their vicinity, making 
  ### slightly more realistic movement of agents around obstacles.  
  
  ### Requires the following inputs:
   
  ### 1. A matrix of values where each element will be averaged.
  ### 2. An integer specifying the size of the neighbourhood to perform averaging.
   
  ### Returns a matrix with the same dimensions as the input matrix with averaged 
  ### values.
  
  # Set the sizes of the grid to be averaged.
  ni <- nrow(field)
  nj <- ncol(field)
  # Set the values of all neighbouring cells, including the central one.
  abuts <- seq(-zone, zone)
  # Initialise the matrix of averaged values.
  out2 <- matrix(nrow = ni, ncol = nj)
  # Initialise the vector of x-coordinates for all abutting cells.
  xp <- numeric()
  
  ## Now set the (x, y) coordinates for all neighbouring cells.
  
  # For each neighbour...
  for (e in abuts) {
    #...set the x-values.
    z <- rep(e, times = length(abuts))
    xp <- c(xp, z)
  }
  # Set the y-coordinates of all abuts.
  yp <- rep(abuts, times = length(abuts))
  
  ## Now fill in the values of the averaged matrix.
  
  # The first and last (neighbourhood size) rows and columns are set to 0, due 
  # to their being bounding walls around the environment of a given thickness 
  # which have zero floor field.
  out2[1:zone,] <- field[1:zone,]
  out2[, 1:zone] <- field[, 1:zone]
  out2[, (nj - (zone - 1)):nj] <- field[, (nj - (zone - 1)):nj]
  out2[(ni - (zone - 1)):ni,] <- field[(ni - (zone - 1)):ni,]
  
  # For each non-zero row...
  for (k in (zone + 1):(ni - zone)) {
    #...for each non-zero column...
    for (l in (zone + 1):(nj - zone)) {
      #...collect up the values of every neighbour...
      vals <- field[k + xp, l + yp]
      #...and average them.
      out2[k, l] <- mean(vals, na.rm = T)
    }
  }
  return(out2)
}

max_field_calc <- function(field, zone = 3) {
  ### This function find the maximum floor field value of a cell over its nearest
  ### neighbourhood of a given size.  Used to produce slightly more realistic 
  ### movement of agents.  Requires the following inputs:
  
  ### 1. A matrix of values to find the local maximum floor field.
  ### 2. An integer specifying the size of the neighbourhood to search for the 
  ### maximum.
  
  ### Returns a matrix with the same dimensions as the input matrix with local 
  ### maximum values.
  
  # Set the sizes of the grid to be searched.
  ni <- nrow(field)
  nj <- ncol(field)
  # Set the values of all neighbouring cells, including the central one.
  abuts <- seq(-zone, zone)
  # Initialise the matrix of maximal values.
  out2 <- matrix(nrow = ni, ncol = nj)
  # Initialise the vector of x-coordinates for all abutting cells.
  xp <- numeric()
  
  ## Now set the (x, y) coordinates for all neighbouring cells.
  
  # For each neighbour...
  for (e in abuts) {
    #...set the x-values.
    z <- rep(e, times = length(abuts))
    xp <- c(xp, z)
  }
  # Set the y-coordinates of all abuts.
  yp <- rep(abuts, times = length(abuts))
  
  ## Now fill in the values of the maximal matrix.
  
  # The first and last (neighbourhood size) rows and columns are set to 0, due 
  # to their being bounding walls around the environment of a given thickness 
  # which have zero floor field.
  out2[1:zone,] <- field[1:zone,]
  out2[, 1:zone] <- field[, 1:zone]
  out2[, (nj - (zone - 1)):nj] <- field[, (nj - (zone - 1)):nj]
  out2[(ni - (zone - 1)):ni,] <- field[(ni - (zone - 1)):ni,]
  
  # For each non-zero row...
  for (k in (zone + 1):(ni - zone)) {
    #...for each non-zero column...
    for (l in (zone + 1):(nj - zone)) {
      #...collect up the values of every neighbour...
      vals <- field[k + xp, l + yp]
      #...and find the local maximum.
      out2[k, l] <- max(vals, na.rm = T)
    }
  }
  return(out2)
}

### Function to impose a static floor field onto an environment with obstacles,
### starting from a defined point.

create.static <- function(obstacles, target = matrix(c(1, 1), ncol = 1, nrow = 2)) {

  ### Generates the floor field and distance values for the discretised 
  ### space for the microscopic, individual-level pedestrian simulator.
  
  ### Requires:
  ### 1. A matrix representing the simulated space, with 0 in elements 
  ###    corresponding to obstacles and 1's otherwise.
  ### 2. Coordinates for the source of the floor field/destination.  Each column 
  ###    is the coordinates of one cell in the grid.  The source could span more 
  ###    than one cell.
  
  ### Returns a list with the following matrices:
  ### 1. The floor field values for each cell of the spatial grid.
  ### 2. The distance of each grid cell from the source/destination.
  
  # Set the floor field grid.
  ni <- dim(obstacles)[1]
  nj <- dim(obstacles)[2]
  # Make floor field at obstacles = 0 and -1 otherwise.
  dists <- matrix(rep(-1, ni * nj), ncol = nj, nrow = ni) * obstacles

  # For each cell of the source (usually just one cell, but could be any number 
  # of cells in any configuration)...
  for (i in 1:dim(target)[2]) {
    #...set the field at the destination = 1.
    dists[target[1, i], target[2, i]] <- 1
  }

  ## Now find the shortest distances of each cell in the grid from the source.
  ## Starts at the destination cell, finds its nearest neighbours and records 
  ## their grid coordinates.  Then assigns them their shortest distances.
  ## These nearest neighbours then become the cells to find the nearest 
  ## neighbours of in the next iteration, not counting the cells that have 
  ## already been assigned a distance from a previous iteration.
  
  # While there is a cell which has unevaluated neighbours or until every cell 
  # is assigned a shortest distance...
  while (is.matrix(target) && length(as.vector(target)) > 0) {
    #...initialise the matrix of the coordinates of nearest neighbour to each 
    # of the cells from the previous iteration.
    # For each cell whose distance was assigned in the previous iteration...
    for (i in 1:dim(target)[2]) {
      #...find the coordinates of its nearest neighbours.
      ass_cells_this_it <- get.nns(x = target[1, i], y = target[2, i], ni = ni,
                                   nj = nj, obstacles = dists)
      # If these cells are neighbours to the original source cell...
      if (i == 1) {
        #...add their coordinates of these cells to generate a considered cells 
        # matrix.
        assigned_cells <- cbind(target, ass_cells_this_it)
      }
      # Otherwise, if these cells are not neighbours to the original source cell...
      else {
        #...add the coordinates of these cells to the considered cells matrix.
        assigned_cells <- cbind(assigned_cells, ass_cells_this_it)
      }

      # If there are any cells with unassigned distances in the grid...
      if (dim(ass_cells_this_it)[2] > 0) {
        #...then for each of these cells...
        for (j in 1:dim(ass_cells_this_it)[2]) {
          #...calculate the distance from the source cell.  If this cell is 
          # directly adjacent to the source cell, then the minimum distance is
          # the distance of the selected cell + 1, if the cell is a diagonal 
          # neighbour to the selected cell, then the distance will be the 
          # distance of the selected cell + sqrt(2). 
          dists[ass_cells_this_it[1, j], ass_cells_this_it[2, j]] <- 
            min(dists[target[1, i], target[2, i]] + 1,(sqrt(2)) + get.diag(
              x = ass_cells_this_it[1, j], y = ass_cells_this_it[2, j], ni = ni, 
              nj = nj, obstacles = dists
            )
          )
        }
      }
      assigned_cells <- assigned_cells[, -1] # Remove the original destination cell.
    }
    
    # The cells that were selected in this iteration will be used in the next one.
    target <- as.matrix(assigned_cells)
  }
  
  # This scales the values of the non-zero floor field to values between 0.1 and 1.
  out <- dists
  # Sort floor field values over entire space from highest to lowest.
  lookup <- sort(unique(as.vector(dists)), decreasing = T)
  lookup <- lookup[-which(lookup == 0)] # Remove the 0 value.
  # Set interval values between 0.1 to 1 to map floor field values.
  val <- seq(from = 0.1, to = 1, length = length(lookup))
  # For each cell in the grid...
  for (i in 1:ni) {
    for (j in 1:nj) {
      #...if the cell is not an obstacle...
      if (dists[i, j] > 0) {
        #...then assign it a non-zero floor field value.
        out[i, j] <- val[which(lookup == dists[i, j])]
      }
    }
  }
  
  # This assignment blurs out the boundaries of the walls, so they must be put back.
  out2 <- out * obstacles
  # Returns the floor field and distance values for the given destination.
  return(list(out2, dists))
}

oneexit.newnew <- function(size = 100, wt = 3, exitwidth = 10, plot = T, 
                           writefile = F, name = "oneexitnew.txt") {
  
  ### Function to create a square room with one exit and a floor field towards this exit.
  ### updated version of oneexit.new.
  ### Takes the following inputs:
  ### 1. Size of the square room.
  ### 2. The thickness of the walls of the room, in number of matrix elements.
  ### 3. The width of the exit in matrix elements.
  ### 4. Whether the colour map of the floor field should be plotted.
  ### 5. Whether to write the floor field for the room to a file.
  ### 6. The name of the file to write said floor field, if needed.
  
  # If the exit width is an even number...
  if (exitwidth %% 2 == 0) {
    # ...increase size of room for later.
    size <- size + 1
  }
  # The total size of the space is 1.5 times the size of the room.
  allsize <- round(1.5 * size)
  # If the exit width is odd and the total size of the space is even...
  if (exitwidth %% 2 != 0 && allsize %% 2 != 1) {
    # ...make the size of the space odd.
    allsize <- allsize + 1
  }
  # Initialise the matrix of the space.
  grid <- matrix(rep(1, allsize * allsize), ncol = allsize, nrow = allsize)
  
  ## Create walls and door:
  
  # For each row/column of wall thickness...
  for (i in 1:wt) {
    grid[i, ] <- rep(0, allsize) # Top wall.
    grid[allsize + 1 - i, ] <- rep(0, allsize) # Bottom wall.
    grid[, i] <- rep(0, allsize) # Left wall.
    grid[, allsize + 1 - i] <- rep(0, allsize) # Right wall.
    grid[, allsize - size + i] <- rep(0, allsize) # Wall at middle of room.
  }
  
  # Find the size of the actual room in number of matrix elements.
  realroom <- length(which(grid[round(allsize / 2), (allsize - size + 1):(allsize)] > 0))
  upto <- (allsize - realroom) / 2
  
  # Set every element above and below the room = 0.
  for (i in 1:upto) {
    grid[i, ] <- rep(0, allsize)
    grid[allsize + 1 - i, ] <- rep(0, allsize)
  }

  ## Build door:
  
  # For an even-width door...
  if (exitwidth %% 2 == 0) {
    #...set the width of the door.
    width <- round(exitwidth / 2)
    from <- round(allsize / 2 - width + 1)
    upto <- round(allsize / 2 + width)
    # If the exit width is wrong...
    if (length(from:upto) != exitwidth) {
      #...let the user know.
      print("you don't get the right exitwidth")
      print(length(from:upto))
    }
    # Place door halfway along the middle wall.
    for (i in 1:wt) {
      grid[from:upto, allsize - size + i] <- rep(1, length(from:upto))
    }
  } else { # If the exit width is odd...
    #...set the width of the door.
    width <- round((exitwidth - 1) / 2)
    from <- round(allsize / 2) - width
    upto <- round(allsize / 2) + width
    # If the exit width is wrong...
    if (length(from:upto) != exitwidth) {
      #...let the user know.
      print("you don't get the right exitwidth")
    }
    # Place door halfway along the middle wall.
    for (i in 1:wt) {
      grid[from:upto, allsize - size + i] <- rep(1, length(from:upto))
    }
  }

  ## Assign static floor field:
  
  # Set the source of floor field to be at wall behind exit (bottom of plot).
  tt <- rbind((allsize / 2):(allsize / 2 + 1), rep(wt + 1, 2))
  # Create a static floor field with maximum at destination and the distance 
  # values of each cell from the destination.
  test <- create.static(obstacles = grid, target = tt)
  # Overlay the floor field onto the environment layout.
  grid <- grid * test
  # Plot the floor field if you want.
  if (plot) {
    image(grid)
  }
  #exitx <- rep(allsize - size, 4)
  # Write the coordinates of the exit.
  exitx <- rep(10, 4)
  exity <- rep(round(allsize / 2), 4)
  # If you want to write the output of this to a file...
  if (writefile) {
    #...collate all data into a table, including the size of the grid, the 
    # coordinates of the destination, and the values of the floor field.
    out <- t(t(c("oneexit", allsize, exitx, exity, as.vector(grid))))
    write.table(out, file = name, row.names = F, col.names = F, quote = F)
  }
  invisible(grid)
}

observational.routes1 <- function(plot = T, writefile = F, name = "routes1.txt", 
                                  shift1 = 0, shift2 = 0) {
  
  ### Functions to create a corridor with obstacles.
  ### Takes the following inputs:
  ### 1. Whether or not to plot the floor field.
  ### 2. Whether or not to write the floor field to a file.
  ### 3. The name of the file to write the floor field to, if applicable.
  ### 4. The amount to shift the x-position of the first destination cell.
  ### 5. The amount to shift the x-position of the second destination cell.
  
  # Fix the thickness of the walls and the total size of the space.
  wt <- 3
  allsize <- 150
  # Initialise the matrix of the space.
  grid <- matrix(rep(1, allsize * allsize), ncol = allsize, nrow = allsize)
  
  ## Create walls:
  for (i in 1:wt) {
    grid[i, ] <- rep(0, allsize) # Top wall
    grid[allsize + 1 - i, ] <- rep(0, allsize) # Bottom wall
    grid[, i] <- rep(0, allsize) # Left wall
    grid[, allsize + 1 - i] <- rep(0, allsize) # Right wall
  }
  
  # The top and bottom 58 rows are blocked out.
  for (i in 1:58) {
    grid[i, ] <- rep(0, allsize)
    if (i < 58) {
      grid[allsize + 1 - i, ] <- rep(0, allsize)
    }
  }

  ## Build obstacle.
  for (i in 1:4) {
    grid[(59 + 9 + 1):(93 - 9 - 1), allsize - 130 + i] <- 
      rep(0, length((58 + 9 + 1):(93 - 9 - 2)))
  }

  ## Assign static floor field:

  # Set destination as two cells at middle of space behind corridor exit.
  tt <- rbind((allsize / 2):(allsize / 2 + 1), rep(wt + 1, 2))
  tt[1, ] <- tt[1, ] + c(shift1, shift2)
  # Create a static floor field with maximum at destination and the distance 
  # values of each cell from the destination.
  test <- create.static(obstacles = grid, target = tt)
  # Overlay the floor field onto the environment layout.
  grid <- grid * test
  # Plot the floor field if you want.
  if (plot) {
    image(grid)
  }
  # Write the coordinates of the exit.
  exitx <- rep(allsize - 140, 4)
  exity <- rep(allsize / 2, 4)
  # If you want to write the output of this to a file...
  if (writefile) {
    #...collate all data into a table, including the size of the grid, the 
    # coordinates of the destination, and the values of the floor field.
    out <- t(t(c("oneexit", allsize, exitx, exity, as.vector(grid))))
    write.table(out, file = name, row.names = F, col.names = F, quote = F)
  }
  invisible(grid)
}

observational.routes2 <- function(gridsize, doorwidth, 
                                  wt = round(0.02 * gridsize), plot = T, 
                                  writefile = F, 
                                  name = paste0("Rooms/Jun et al_room_", gridsize, ".txt")) {

  ### Function to create a corridor with obstacles 
  ### Takes the following inputs:
  ### 1. The size of the spatial grid (currently in 10s of centimetres).
  ### 2. The width of the door.
  ### 3. The wall thickness in number of grid cells.
  ### 4. Whether the colour map of the floor field should be plotted.
  ### 5. Whether to write the floor field for the room to a file.
  ### 6. The name of the file to write said floor field, if needed.
  
  # Initialise the matrix of the space.
  grid <- matrix(rep(1, gridsize * gridsize), ncol = gridsize, nrow = gridsize)
  
  ## Create walls and door:
  for (i in 1:wt) {
    grid[i, ] <- rep(0, gridsize)
    grid[gridsize + 1 - i, ] <- rep(0, gridsize)
    grid[, i] <- rep(0, gridsize)
    grid[, gridsize + 1 - i] <- rep(0, gridsize)
  }

  ## Build wall and door:
  for (i in 1:wt) {
    doorwall <- c((wt):((gridsize / 2) - (doorwidth / 2)), 
                  ((gridsize / 2) + (doorwidth / 2)):(gridsize - i)) # wall
    grid[doorwall, (gridsize - round((13/15)*gridsize) + i)] <- 
      rep(0, length(doorwall)) # door
  }
  
  # Set the length of the obstacles.
  obst1len <- 30
  obst2len <- 50
  
  ## Build obstacles:
  for (i in 1:4) {
    obst1 <- c(round(3/8 * gridsize) - round(obst1len / 2)):(round(3/8 * gridsize) + 
                                                               round(obst1len / 2))
    grid[round(3/8 * gridsize) + i, obst1] <- rep(0, length(obst1))
    grid[round(5/8 * gridsize) + i, obst1] <- rep(0, length(obst1))
    obst2 <- (round(1/2 * gridsize) - (obst2len / 2)):(round(1/2 * gridsize) + (obst2len / 2))
    grid[obst2, (3/4 * gridsize) + i] <- rep(0, length(obst2))
  }

  ## Assign static floor field:
 
  # Set the coordinates of the destinations.
  unordered_targets <- matrix(c(round(gridsize / 2), round(gridsize / 15), round(gridsize / 2), 
                      round(11/12 * gridsize), (round(3/8 * gridsize) - 5), round(3/8 * gridsize), 
                      (round(5/8 * gridsize) + wt + 8), round(3/8 * gridsize)), nrow = 2, 
                    ncol = 4)
  
  #############################################################################
  # This next section of code orders a matrix of destination coordinates.
  # Sorts the destination coordinates in increasing x-coordinate first.
  # Then it sorts, for each unique x-coordinate, the y-coordinates in 
  # increasing order. So we get a matrix with the first row ordered in
  # increasing size and the second row is ordered in increasing order for each 
  # value in the first row.
  # E.g. [1, 3, 2, 5, 3]
  #      [2, 3, 4, 5, 1]
  # becomes: 
  #      [1, 2, 3, 3, 5]
  #      [2, 4, 1, 3, 5]
  # This is done because of a limitation in the subsequent GuiOutput.java when
  # painting each destination a different colour.
  # Initial unordered matrix.
  new <-t(apply(unordered_targets,1,function(x) x[order(unordered_targets[1,])]))
  dest_xs <- new[1,] # Collect up the freshly-ordered x-coordinates.
  uniq_x <- unique(dest_xs) # Find each unique x-coordinate and...
  #...iterate over them.
  for (i in 1:length(uniq_x)) {
    # Collect the columns of the destination matrix containing the x-coordinate.
    temp <- new[, which(dest_xs == uniq_x[i])]
    # If this x-coordinate is repeated...
    if (dim(as.matrix(temp))[2] > 1) {
      #...order the second row of this reduced matrix in increasing order.
      temp <-t(apply(temp,1,function(x) x[order(temp[2,])]))
    }
    # If we've just started, then the sorted matrix is initialised.
    if (i == 1) {targets <- temp}
    # Otherwise, the sub-matrix is appended to the current matrix.
    else {targets <- cbind(targets, temp)}
  }
  
  # If we are writing this room to a file...
  if (writefile) {
    #...write the name of the room first.
    write("Jun et al", file = name)
    # Then the grid dimensions.
    write(gridsize, file = name, append = T)
    # Followed by the number of destinations...
    write(dim(targets)[2], file = name, append = T)
    #...and the sorted destination coordinates.
    write.table(t(targets), file = name, append = T, sep = ",", row.names = F, 
                col.names = F)
  }
  
  # For each destination...
  for (i in 1:dim(targets)[2]) {
    tt <- as.matrix(targets[,i])
    #...create a static floor field with maximum at destination and the distance 
    # values of each cell from the destination.
    test <- create.static(obstacles = grid, target = tt)
    # Multiply by grid of obstacles to get 0 at obstacles.
    field <- test[[1]] * grid
    dists <- test[[2]] * grid
  
    # Plot the floor field if you want.
    if (plot) {
      filled.contour(x = seq(10, (nrow(field) * 10), length.out = nrow(field)), 
                     y = seq(10, (ncol(field) * 10), length.out = ncol(field)), 
                     z = t(field), 
                     color.palette = function(n) hcl.colors(n, "Oslo", rev = TRUE),
                     xlab = "x", 
                     ylab = "y", 
                     main = paste0("Jun et. al's setup target ", i, " floor field"))
    }
  
    # If you want to write the output of this to a file...
    if (writefile) {
      #...write the floor field of the destination...
      write.table(field, file = name, row.names = F, col.names = F, quote = F, 
                  sep = ",", append = T)
      #...and the distances of each cell from the destination.
      write.table(dists, file = name, row.names = F, col.names = F, quote = F, 
                  sep = ",", append = T)
    }
  }
  invisible(grid)
}

circle <- function(size = 100, wt = 3, plot = T, writefile = F, name = "circletest.txt") {
  ### A circular room:
  ### Takes the following inputs:
  ### 1. Size of the space/dimensions of the spatial grid.
  ### 2. Thickness of the wall.
  ### 3. Whether or not to plot the colour map of the floor field.
  ### 4. Whether or not to write the floor field values to a file.
  ### 5. The name of the file to write the output to, if applicable.
  
  # Initialise spatial grid. Initial cell values = 0.5.
  grid <- matrix(rep(0.5, size * size), ncol = size, nrow = size)
  # Set the centre of the circle.
  centrex <- size / 2
  centrey <- size / 2
  rad <- round(size / 2) - wt # circle radius.
  # Assign position of each grid cell in metres. Top left cell at (0.5, 0.5).
  pos <- seq(from = 0.5, to = size - 0.5, length = size)
  
  ## Create walls:
  
  # For each row...
  for (i in 1:size) {
    #..for each column...
    for (j in 1:size) {
      #...find distance of cell from centre.
      ddd <- sqrt((pos[i] - centrex)^2 + (pos[j] - centrey)^2)
      # If cell outside room radius...
      if (ddd >= rad) {
        #...then it is a wall.
        grid[i, j] <- 0
      }
    }
  }

  # Plot the floor field if you want.
  if (plot) {
    image(grid)
  }
  # Write the coordinates of the destination, for later use in simulator.
  exitx <- rep(1, 4)
  exity <- rep(1, 4)
  # If you want to write the output of this to a file...
  if (writefile) {
    #...collate all data into a table, including the size of the grid, the 
    # coordinates of the destination, and the values of the floor field.
    out <- t(t(c("circle", size, exitx, exity, as.vector(grid))))
    write.table(out, file = name, row.names = F, col.names = F, quote = F)
  }
  invisible(grid)
}

classroom.new <- function(gridsize = 100, wt = round(0.02 * gridsize), 
                          benchthick = wt, 
                          exitwidth = round(0.15 * gridsize), plot = T, 
                          writefile = F, 
                          name = paste0("Rooms/classroom_", gridsize, ".txt")) {
  ### Function to create a square room with one destination and some bench-like
  ### obstacles (inspired by classroom setup).
  ### Takes the following inputs:
  ### 1. Size of room (not the environment created).
  ### 2. Thickness of the walls in spatial cells.
  ### 3. Thickness of benches.
  ### 4. Width of exit door in spatial cells.
  ### 5. Whether or not to plot the floor field.
  ### 6. Whether or not to write floor field values to a file.
  ### 7. Name of output file, if applicable.
  
  # Initialise spatial grid.  Default cell value = 1.
  grid <- matrix(rep(1, gridsize * gridsize), ncol = gridsize, nrow = gridsize)
  # Size of room.
  size <- round(1/3 * gridsize)
  
  ## Create walls:
  for (i in 1:wt) {
    grid[i, ] <- rep(0, gridsize) # Top wall.
    grid[gridsize + 1 - i, ] <- rep(0, gridsize) # Bottom wall.
    grid[, i] <- rep(0, gridsize) # Left-most wall.
    grid[, gridsize + 1 - i] <- rep(0, gridsize) # Right-most wall.
    grid[, gridsize - size + i] <- rep(0, gridsize) # Dividing wall.
  }
  
  # Calculate the width of the room, starting from the dividing wall
  # and ending at the right-most wall.
  lect_width <- length(which(grid[round(gridsize / 2), (gridsize - size + 1):(gridsize)] > 0))

  ## Build door:
  width <- round(exitwidth / 2)
  # Where the door starts.
  from <- round((gridsize / 2) - width + 1)
  # Where the door ends.
  upto <- round((gridsize / 2) + width)
  # Create door.
  for (i in 1:wt) {
    grid[from:upto, gridsize - size + i] <- rep(1, length(from:upto))
  }

  ## Build benches:
  # Calculate the total number of benches.
  bno <- floor(((gridsize - size) - 2 * wt) / (benchthick)) - 3
  # Assign bench widths so that there is enough central space for agents to reach door.
  upper_bench_width <- gridsize - wt - upto
  lower_bench_width <- from - wt
  # Index for keeping track of the destination.
  dest_ind <- 1
  # Y-coordinates for the destinations.  Destinations are between the first and 
  # second benches.
  bench_dest_y <- numeric()
  # For each bench...
  for (i in 1:bno) {
    # If this is the first bench...
    if (i %% 4 == 1) {
      #...block out the row from the start of the first bench to the end of the last.
      bench <- (wt + (i + 2) * benchthick + 1):
        (wt + (i + 3) * benchthick)
      grid[, bench] <- rep(0, gridsize)
      # Set the empty space between benches.
      grid[from:upto, bench] <- rep(1, length(from:upto))
      # Set the destination y-coordinate between the benches.
      bench_dest_y[dest_ind] <- wt + (i * benchthick)
      dest_ind <- dest_ind + 1
    }
  }
  
  # Set destination x-coordinates as midway along the benches.
  bench_dest_upper_x <- round((gridsize - wt) - (upper_bench_width / 4))
  bench_dest_x_lower <- round(lower_bench_width / 4)
  # Create vector of destination coordinates with length = number of destinations / 2.
  dests <- c(round(gridsize / 2), round(gridsize - (size / 2)), 
             bench_dest_upper_x, bench_dest_y[1], bench_dest_upper_x, 
             bench_dest_y[2], bench_dest_upper_x, bench_dest_y[3], 
             bench_dest_upper_x, bench_dest_y[4], bench_dest_upper_x, 
             bench_dest_y[5], bench_dest_upper_x, bench_dest_y[6], 
             bench_dest_upper_x, bench_dest_y[7], bench_dest_x_lower, 
             bench_dest_y[1], bench_dest_x_lower, bench_dest_y[2], 
             bench_dest_x_lower, bench_dest_y[3], bench_dest_x_lower, 
             bench_dest_y[4], bench_dest_x_lower, bench_dest_y[5], 
             bench_dest_x_lower, bench_dest_y[6], bench_dest_x_lower, 
             bench_dest_y[7])
  
  #############################################################################
  # This next section of code orders a matrix of destination coordinates.
  # Sorts the destination coordinates in increasing x-coordinate first.
  # Then it sorts, for each unique x-coordinate, the y-coordinates in 
  # increasing order. So we get a matrix with the first row ordered in
  # increasing size and the second row is ordered in increasing order for each 
  # value in the first row.
  # E.g. [1, 3, 2, 5, 3]
  #      [2, 3, 4, 5, 1]
  # becomes: 
  #      [1, 2, 3, 3, 5]
  #      [2, 4, 1, 3, 5]
  # This is done because of a limitation in the subsequent GuiOutput.java when
  # painting each destination a different colour.
  # Convert to matrix such that row 1 is x-coordinates and second row is y.
  unordered_targets <- matrix(dests, nrow = 2, 
                              ncol = length(dests)/2)
  # Sort by first row, maintaining corresponding values in row 2.
  new <-t(apply(unordered_targets,1,function(x) x[order(unordered_targets[1,])]))
  dest_xs <- new[1,] # Collect up the freshly-ordered x-coordinates.
  uniq_x <- unique(dest_xs) # Find each unique x-coordinate and...
  # ...iterate over them.
  for (i in 1:length(uniq_x)) {
    # Collect the columns of the destination matrix containing the x-coordinate.
    temp <- new[, which(dest_xs == uniq_x[i])]
    # If this x-coordinate is repeated...
    if (dim(as.matrix(temp))[2] > 1) {
      #...order the second row of this reduced matrix in increasing order.
      temp <-t(apply(temp,1,function(x) x[order(temp[2,])]))
    }
    # If we've just started, then the sorted matrix is initialised.
    if (i == 1) {targets <- temp}
    # Otherwise, the sub-matrix is appended to the current matrix.
    else {targets <- cbind(targets, temp)}
  }

  # If we are writing this room to a file...
  if (writefile) {
    #...write the name of the room first.
    write("classroom", file = name)
    # Then the grid dimensions.
    write(gridsize, file = name, append = T)
    # Followed by the number of destinations...
    write(dim(targets)[2], file = name, append = T)
    #...and the sorted destination coordinates.
    write.table(t(targets), file = name, append = T, sep = ",", row.names = F, 
                col.names = F)
  }
  
  # For each destination...
  for (i in 1:dim(targets)[2]) {
    tt <- as.matrix(targets[,i])
    #...create a static floor field with maximum at destination and the distance 
    # values of each cell from the destination.
    test <- create.static(obstacles = grid, target = tt)
    # multiply by grid of obstacles to get 0 at obstacles.
    field <- test[[1]] * grid
    dists <- test[[2]] * grid
    # If you want to see what it looks like...
    if (plot) {
      filled.contour(x = seq(10, (nrow(field) * 10), length.out = nrow(field)), 
                     y = seq(10, (ncol(field) * 10), length.out = ncol(field)), 
                     z = t(field), 
                     color.palette = function(n) hcl.colors(n, "Oslo", rev = TRUE),
                     xlab = "x", 
                     ylab = "y", 
                     main = paste0("Classroom setup destination ", i, " floor field"))
    }
    # If you want to write the output of this to a file...
    if (writefile) {
      #...write the floor field of the destination...
      write.table(field, file = name, row.names = F, col.names = F, quote = F, 
                  sep = ",", append = T)
      #...and the distances of each cell from the destination.
      write.table(dists, file = name, row.names = F, col.names = F, quote = F, 
                  sep = ",", append = T)
    }
  }
  invisible(grid)
}

officeblock.new <- function(size = 100, wt = 3, exitwidth = 10, plot = T, 
                            writefile = F, name = "officeblocknew.txt") {
  ### Function to create a simple office block with up to five offices along a 
  ### central corridor with a large room at the bottom.  One destination is set 
  ### within this bottom room.
  ### Takes the following input:
  ### 1. Size of the room (not the total environment) in units of 10s of centimetres.
  ### 2. Thickness of the walls in spatial cells.
  ### 3. Width of exit door in spatial cells.
  ### 4. Whether or not to plot the floor field.
  ### 5. Whether or not to write floor field values to file.
  ### 6. The name of the file to write said floor field, if needed.
  
  # Environment is 1.5x room size.
  allsize <- round(1.5 * size)
  # A quick check to see if input exit width = output exit width.  Not crucial 
  # to function success.
  width <- round(exitwidth / 2)
  from <- round(allsize / 2 - width + 1)
  upto <- round(allsize / 2 + width)
  # If the length of the exit door is not correct...
  if (length(from:upto) != exitwidth) {
    #...let the user know.
    print("you don't get the right exitwidth")
  }
  # Initialise spatial grid.  Default cell value = 1.
  grid <- matrix(rep(1, allsize * allsize), ncol = allsize, nrow = allsize)
  
  ## Create walls:
  for (i in 1:wt) {
    grid[i, ] <- rep(0, allsize) # Top wall.
    grid[allsize + 1 - i, ] <- rep(0, allsize) # Bottom wall.
    grid[, i] <- rep(0, allsize) # Left-most wall.
    grid[, allsize + 1 - i] <- rep(0, allsize) # Right-most wall.
    grid[, allsize - size + i] <- rep(0, allsize) # Dividing wall.
    grid[(from - 1 - i), (allsize - size + 1):allsize] <- 
      rep(0, length((allsize - size + 1):allsize)) # Bottom left wall
    grid[(upto + 1 + i), (allsize - size + 1):allsize] <- 
      rep(0, length((allsize - size + 1):allsize)) # Bottom right wall
  }
  
  ## Build central door:
  for (i in 1:wt) {
    grid[(from):(upto), allsize - size + i] <- rep(1, length((from):(upto)))
  }
  
  ## Build offices:
  # The number of offices on each side of the corridor depend on the size of 
  # the environment such that only a set number of offices of equal width that 
  # can fit into the remaining environment space are included.
  offno <- floor((size - 2 * wt) / 30)
  # Maximum of five offices in the space.
  if (offno > 5) {
    offno <- 5
  }
  # Set the width of each office so that they are large enough for agents to 
  # move around.
  offwidth <- floor((size - 2 * wt) / offno)
  
  # For each office...
  for (i in 1:offno) {
    #...if i is less than the office number...
    if (i < offno) {
      #...then set the section of each row of the grid that is an office space.
      iis <- c(1:(from - 2), (upto + 2):allsize)
      # Set the section of each column of the total space that is an office space.
      jjs <- (allsize - (i) * offwidth - wt + 1):(allsize - (i) * offwidth)
      # First, block out all the space.
      grid[iis, jjs] <- matrix(rep(0, length(iis) * length(jjs)),
        ncol = wt, nrow = length(iis))
    }
    # Now build the office space.
    jjs <- (allsize - (i - 1) * offwidth - wt + 1 - 0 - exitwidth):
      (allsize - (i - 1) * offwidth - wt + 1 - 1)
    iis <- c((from - 1 - wt):(from - 2), (upto + 2):(upto + 1 + wt))
    grid[iis, jjs] <- matrix(rep(1, length(iis) * length(jjs)), 
                             ncol = length(jjs), nrow = length(iis))
  }

  ## Assign static floor field:

  # Assign destination as two cells in middle x-position next to bottom wall.
  tt <- rbind((allsize / 2):(allsize / 2 + 1), rep(wt + 1, 2))
  # Create a static floor field with maximum at destination and the distance 
  # values of each cell from the destination.
  test <- create.static(obstacles = grid, target = tt)
  grid <- test * grid
  # Plot the floor field if you want.
  if (plot) {
    image(grid)
  }
  # Write the coordinates of the destination, for later use in simulator.
  exitx <- rep(allsize - size - round(allsize / 6), 4)
  exity <- rep(allsize / 2, 4)
  # If you want to write the output of this to a file...
  if (writefile) {
    #...collate all data into a table, including the size of the grid, the 
    # coordinates of the destination, and the values of the floor field.
    out <- t(t(c("officeblock", allsize, exitx, exity, as.vector(grid))))
    write.table(out, file = name, row.names = F, col.names = F, quote = F)
  }
  invisible(grid)
}

vehicle.new <- function(size = 200, wt = 3, width = 26, exitwidth = 10, 
                        plot = T, writefile = F, name = "vehiclenew.txt") {
  ### Function to create a public transport container, with a floor field towards
  ### two exits at either end of the container and some bench-like obstacles:
  ### Takes the following inputs:
  ### 1. Size of environment.
  ### 2. Thickness of walls.
  ### 3. Width of container.
  ### 4. Width of the exits.
  ### 4. Whether or not to plot floor field.
  ### 5. Whether or not to write floor field values to file.
  ### 6. Name of output file, if applicable.
  ### Container positioned in middle of environment.
  
  # Set the start and end points for the container wall.
  from <- round(size / 2) - width / 2 + 1
  upto <- round(size / 2) + width / 2
  # Initialise spatial grid.  Default cell value = 1.
  grid <- matrix(rep(1, size * size), ncol = size, nrow = size)
  
  ## Create walls:
  for (i in 1:wt) {
    grid[i, ] <- rep(0, size) # Top wall.
    grid[size + 1 - i, ] <- rep(0, size) # Bottom wall.
    grid[, i] <- rep(0, size) # Left-most wall.
    grid[, size + 1 - i] <- rep(0, size) # Right-most wall.
    grid[from - i, ] <- rep(0, size) # Top container wall.
    grid[upto + i, ] <- rep(0, size) # Bottom container wall.
  }
  
  ## Build doors:
  for (i in 1:wt) {
    grid[from - i, (wt + 6):(wt + exitwidth + 5)] <- rep(1, exitwidth) # Left door
    grid[from - i, (size - wt - exitwidth - 5):(size - wt - 6)] <- 
      rep(1, exitwidth) # Right door
  }

  ## Build some benches:
  # Upper and lower bench widths.
  on <- wt + exitwidth + 12
  off <- size - wt - exitwidth - 11
  # Positions along the container to place benches.  Currently benches have a 
  # hard-coded separation of 12 cells.
  iis <- c(from:(from + 6), (upto - 6):(upto))
  # Build benches from the middle of the container and work outwards until the 
  # boundaries are met.
  while ((off - on) >= 14) {
    grid[iis, c(on, off)] <- matrix(rep(0, 2 * length(iis)), ncol = 2, nrow = length(iis))
    on <- on + 12
    off <- off - 12
  }

  # Add wall to the end of the container.
  grid[(upto + wt):size, ] <- matrix(rep(0, size * length((upto + wt):size)))
  grid <- t(grid)

  ## Shift the container down so that it sits in the middle of the environment.
  gridold <- grid
  # Amount to shift container by.
  add <- (round(size / 2) - round(width / 2)) - round(size / 3)
  # For each row of the spatial grid...
  for (i in 1:size) {
    #...for each cell in the row...
    for (j in 1:size) {
      # If the cell is in the upper half of the grid...
      if (j > (size - add)) {
        #..do nothing.
      } 
      # Otherwise, if the cell is in the lower half...
      else {
        #...shift them downwards.
        grid[i, j] <- gridold[i, j + add]
      }
    }
  }
  # Now fill in the freshly-made space from the shift with wall.
  for (i in 1:wt) {
    grid[, i] <- rep(0, size)
  }

  ## Assign static floor field:

  # Write the coordinates of the exit, for later use in simulator.
  exitx <- c(from - add - 15, from - add - 15, upto - add + 15, upto - add + 15)
  exity <- c(
    wt + exitwidth / 2 + 5, (size - wt - exitwidth / 2 - 5),
    wt + exitwidth / 2 + 5, (size - wt - exitwidth / 2 - 5)
  )
  # Assign destinations as the two exits of the container.
  tt <- rbind(exity, exitx)
  # Create a static floor field with maximum at destination and the distance 
  # values of each cell from the destination.
  test <- create.static(obstacles = grid, target = tt)
  grid <- test[[1]] * grid
  # Plot the floor field if you want.
  if (plot) {
    image(grid)
  }
  # If you want to write the output of this to a file...
  if (writefile) {
    #...collate all data into a table, including the size of the grid, the 
    # coordinates of the destination, and the values of the floor field.
    out <- t(t(c("vehicle", size, exitx, exity, as.vector(grid))))
    write.table(out, file = name, row.names = F, col.names = F, quote = F)
  }
  invisible(grid)
}

twocorridor.new <- function(size = 100, wt = 3, exitwidth = 10, 
                            plot = T, writefile = F, name = "twocorridornew.txt") {
  ### Function to build room with two corridors leading towards it:
  ### Takes the following input:
  ### 1. Size of the room (not the total environment) in units of 10s of centimetres.
  ### 2. Thickness of the walls in spatial cells.
  ### 3. Width of exit door in spatial cells.
  ### 4. Whether or not to plot the floor field.
  ### 5. Whether or not to write floor field values to file.
  ### 6. The name of the file to write the output to, if applicable.
  
  # Environment is 1.5x room size.
  allsize <- round(1.5 * size)
  # Initialise spatial grid.  Default cell value = 1.
  grid <- matrix(rep(1, allsize * allsize), ncol = allsize, nrow = allsize)

  ## Create walls:
  for (i in 1:wt) {
    grid[i, ] <- rep(0, allsize) # Top wall.
    grid[allsize + 1 - i, ] <- rep(0, allsize) # Bottom wall.
    grid[, i] <- rep(0, allsize) # Left-most wall.
    grid[, allsize + 1 - i] <- rep(0, allsize) # Right-most wall.
    # Horizontal wall.
    grid[1:allsize, allsize - 1.2 * size + i] <- rep(0, allsize)
    # Lower vertical walls.
    grid[size / 4 + i, (0.3 * size + wt):(allsize - wt - exitwidth - 5)] <-
      rep(0, length((0.3 * size + wt):(allsize - wt - exitwidth - 5)))
    grid[allsize - size / 4 + 1 - i, (0.3 * size + wt):(allsize - wt - exitwidth - 5)] <-
      rep(0, length((0.3 * size + wt):(allsize - wt - exitwidth - 5)))
    # Upper vertical walls.
    grid[size / 4 + i, (allsize - wt - 5):allsize] <-
      rep(0, length((allsize - wt - 5):allsize))
    grid[allsize - size / 4 + 1 - i, (allsize - wt - 5):allsize] <-
      rep(0, length((allsize - wt - 5):allsize))
  }

  ## Build doors to corridors:
  for (i in 1:wt) {
    grid[c((wt + 5):(wt + 15), (allsize - wt - 5):(allsize - wt - 15)), 
         allsize - 1.2 * size + i] <-
      rep(1, length(c((wt + 5):(wt + 15), (allsize - wt - 5):(allsize - wt - 15))))
  }
  
  ## Assign static floor field:

  # Assign destination as two cells in middle x-position at bottom.
  tt <- rbind((allsize / 2):(allsize / 2 + 1), rep(wt + 1, 2))
  # Create a static floor field with maximum at destination and the distance 
  # values of each cell from the destination.
  test <- create.static(obstacles = grid, target = tt)
  grid <- test * grid
  # Plot the floor field if you want.
  if (plot) {
    image(grid)
  }
  
  # Write the coordinates of the destination, for later use in simulator.
  exitx <- rep(allsize - 1.2 * size - round(0.3 * size / 2), 4)
  exity <- (allsize / 2 - 1):(allsize / 2 + 2)

  # If you want to write the output of this to a file...
  if (writefile) {
    #...collate all data into a table, including the size of the grid, the 
    # coordinates of the destination, and the values of the floor field.
    out <- t(t(c("twocorridor", allsize, exitx, exity, as.vector(grid))))
    write.table(out, file = name, row.names = F, col.names = F, quote = F)
  }
  invisible(grid)
}

stadiumstand.new <- function(size = 100, wt = 3, exitwidth = 20, 
                             plot = T, writefile = F, name = "stadiumstand.txt") {
  ### Function to build a mock setup for a stand in a stadium
  ### Takes the following input:
  ### 1. Size of the environment in units of 10s of centimetres.
  ### 2. Thickness of the walls in spatial cells.
  ### 3. Width of exit door in spatial cells.
  ### 4. Whether or not to plot the floor field.
  ### 5. Whether or not to write floor field values to file.
  ### 6. The name of the output file, if applicable.
  
  # Initialise spatial grid.  Default cell value = 1.
  grid <- matrix(rep(1, size * size), ncol = size, nrow = size)
  # Set the start and end points for the stand wall.
  from <- round(size / 2 - exitwidth / 2) - wt
  upto <- round(size / 2 + exitwidth / 2) + wt
  sep <- round(size / 3)

  ## Create walls:
  for (i in 1:wt) {
    grid[i, ] <- rep(0, size) # Top wall.
    grid[size + 1 - i, ] <- rep(0, size) # Bottom wall.
    grid[, i] <- rep(0, size) # Left-most wall.
    grid[, size + 1 - i] <- rep(0, size) # Right-most wall.
    # Horizontal wall of exit.
    grid[from:upto, sep + 10 + exitwidth + i] <- rep(0, length(from:upto))
    # Left wall of exit.
    grid[from - 1 + i, (sep + 10):(sep + 10 + exitwidth)] <- 
      rep(0, length((sep + 10):(sep + 10 + exitwidth)))
    # Right wall of exit.
    grid[upto + 1 - i, (sep + 10):(sep + 10 + exitwidth)] <- 
      rep(0, length((sep + 10):(sep + 10 + exitwidth)))
  }

  ## Build seating rows in lower half of the environment:
  
  # Set the width of the corridor between benches.
  s <- round(size / 2 - 6)
  e <- round(size / 2 + 6)
  ind <- c(1:s, e:size)
  tmp <- sep
  # Create parallel benches in the lower half of the environment with a 
  # hard-coded spacing of 9 cells.
  while (tmp > 0) {
    grid[ind, tmp] <- rep(0, length(ind))
    tmp <- tmp - 9
  }
  
  # Add little vertical walls on the edge of the uppermost lower benches.
  grid[c(rep(s, 1), rep(e, 1)), (sep + 1):(sep + 1)] <- rep(0, 2)
  
  # Now build benches on the upper half of the environment.  These are separated 
  # by two corridors on either side of the main exit.
  s <- round(from - 11)
  e <- round(upto + 11)
  ind <- c(1:s, e:size)
  tmp <- sep + 9
  # Build the upper benches.
  while (tmp < size) {
    # If the row number of the grid is above the exit walls...
    if (tmp >= sep + 10 + exitwidth + 3) {
      #...then there are additional benches in the middle.
      ind <- c(ind, from:upto)
    }
    grid[ind, tmp] <- rep(0, length(ind))
    tmp <- tmp + 9
  }

  ## Assign static floor field:
  
  tt <- rbind((size / 2 - 1):(size / 2 + 2), rep(sep + 10 + exitwidth - 1, 4))
  # Create a static floor field with maximum at destination and the distance 
  # values of each cell from the destination.
  test <- create.static(obstacles = grid, target = tt)
  grid <- test * grid
  # Plot the floor field if you want.
  if (plot) {
    image(grid)
  }
  # Write the coordinates of the exit, for later use in simulator.
  exitx <- rep(sep + 5 + exitwidth - 1, 4)
  exity <- (size / 2 - 1):(size / 2 + 2)
  # If you want to write the output of this to a file...
  if (writefile) {
    #...collate all data into a table, including the size of the grid, the 
    # coordinates of the destination, and the values of the floor field.
    out <- t(t(c("stadiumstand", size, exitx, exity, as.vector(grid))))
    write.table(out, file = name, row.names = F, col.names = F, quote = F)
  }
  invisible(grid)
}

gamenew.new <- function(size = 100, wt = 3, exitwidth = 10, plot = T, writefile = F,
                        name = "test.txt") {
  ### function to build a more complex environmental layout:
  ### Takes the following input:
  ### 1. Size of the main room (not the total environment) in 10s of centimetres.
  ### 2. Thickness of the walls in spatial cells.
  ### 3. Width of exit door in spatial cells.
  ### 4. Whether or not to plot the floor field.
  ### 5. Whether or not to write floor field values to file.
  ### 6. The name of the output file, if applicable.
  
  # Environment is 1.5x room size.
  allsize <- round(1.5 * size)
  # Initialise spatial grid.  Default cell value = 1.
  grid <- matrix(rep(1, allsize * allsize), ncol = allsize, nrow = allsize)

  ## Create walls:
  for (i in 1:wt) {
    grid[i, ] <- rep(0, allsize) # Top wall.
    grid[allsize + 1 - i, ] <- rep(0, allsize) # Bottom wall.
    grid[, i] <- rep(0, allsize) # Left-most wall.
    grid[, allsize + 1 - i] <- rep(0, allsize) # Right-most wall.
    # Middle horizontal wall.
    grid[1:allsize, allsize - 1.2 * size + i] <- rep(0, allsize)
    # Bottom horizontal wall.
    grid[(wt + exitwidth + 1):(allsize - exitwidth - wt - 1),
      allsize - 1.35 * size + i] <- rep(0, length((wt + exitwidth + 1):
    (allsize - exitwidth - wt - 1)))
    # Top horizontal wall.
    grid[(size / 4 + 1):(allsize - size / 4), (allsize - 2 * wt - exitwidth - 10) + i] <-
      rep(0, length((size / 4 + 1):(allsize - size / 4)))
    # Vertical walls
    grid[size / 4 + i, (0.3 * size + wt):(0.3 * size + wt + 5)] <-
      rep(0, length((0.3 * size + wt):(0.3 * size + wt + 5)))
    grid[allsize - size / 4 + 1 - i, (0.3 * size + wt):(0.3 * size + wt + 5)] <-
      rep(0, length((0.3 * size + wt):(0.3 * size + wt + 5)))
    grid[size / 4 + i, (0.3 * size + wt + 7 + exitwidth):
    (allsize - 2 * wt - exitwidth - 10)] <-
      rep(0, length((0.3 * size + wt + 7 + exitwidth):
      (allsize - 2 * wt - exitwidth - 10)))
    grid[allsize - size / 4 + 1 - i, (0.3 * size + wt + 7 + exitwidth):
    (allsize - 2 * wt - exitwidth - 10)] <-
      rep(0, length((0.3 * size + wt + 7 + exitwidth):
      (allsize - 2 * wt - exitwidth - 10)))
  }

  ## Build doors to corridors:
  for (i in 1:wt) {
    grid[c((allsize / 2 - 5):(allsize / 2 + 5)), allsize - 1.2 * size + i] <-
      rep(1, length(c((allsize / 2 - 5):(allsize / 2 + 5))))
  }
  
  ## Assign static floor field:

  # Destination at top off-centre right.
  tt <- rbind((size):(size + 1), rep(allsize - wt, 2))
  # Destination at top off-centre left.
  tt <- rbind((allsize - size):(allsize - size - 1), rep(allsize - wt, 2))
  # Destination in top middle of space.
  tt <- rbind((allsize / 2):(allsize / 2 + 1), rep(allsize - wt, 2))
  # Destination in upper left corner
  tt <- rbind(c(wt + 5, wt + 6), rep(allsize - wt, 2))
  # Destination in upper right corner.
  tt <- rbind(c(allsize - wt - 5, allsize - wt - 6), rep(allsize - wt, 2))
  # Destination at bottom of space in the middle.
  tt <- rbind((allsize / 2):(allsize / 2 + 1), rep(wt + 2, 2))
  # Create a static floor field with maximum at destination and the distance 
  # values of each cell from the destination.
  test <- create.static(obstacles = grid, target = tt)
  grid <- test * grid

  ## Add some padding to the inside of the horizontal walls separating the main 
  ## room from the corridors:
  for (i in 1:2) {
    grid[c((allsize / 2 - 6 - i):(allsize / 2 - 46)), allsize - 1.2 * size + wt + i] <-
      rep(0.5, length(c((allsize / 2 - 6 - i):(allsize / 2 - 46))))
    grid[c((allsize / 2 + 6 + i):(allsize / 2 + 46)), allsize - 1.2 * size + wt + i] <-
      rep(0.5, length(c((allsize / 2 + 6 + i):(allsize / 2 + 46))))
  }
  
  # Plot the floor field if you want.
  if (plot) {
    image(grid)
  }
  # Write the coordinates of the exit, for later use in simulator.
  exitx <- rep(allsize - wt - 5 - exitwidth / 2, 4)
  exity <- rep(tt[1, ], 2)
  
  # If you want to write the output of this to a file...
  if (writefile) {
    #...collate all data into a table, including the size of the grid, the 
    # coordinates of the destination, and the values of the floor field.
    out <- t(t(c("twocorridor", allsize, exitx, exity, as.vector(grid))))
    write.table(out, file = name, row.names = F, col.names = F, quote = F)
  }
  invisible(grid)
}

### Need to assign the floor field in this function.
ulrichgame.new <- function(size = 100, wt = 3, exitwidth = 10, plot = T, 
                           writefile = F, name = "ulrichgame.txt") {
  ### Function to build another more complicated environmental layout:
  ### Takes the following input:
  ### 1. size of the room (not the total environment).
  ### 2. Thickness of the walls in spatial cells.
  ### 3. Width of exit door in spatial cells.
  ### 4. Whether or not to plot the floor field.
  ### 5. Whether or not to write floor field values to file.
  ### 6. The name of the output file, if applicable.
  
  # Environment is 1.5x room size.
  allsize <- round(1.5 * size)
  # Initialise spatial grid.  Default cell value = 1.
  grid <- matrix(rep(1, allsize * allsize), ncol = allsize, nrow = allsize)

  ## Create walls:
  for (i in 1:wt) {
    grid[i, ] <- rep(0, allsize) # Top wall.
    grid[allsize + 1 - i, ] <- rep(0, allsize) # Bottom wall.
    grid[, i] <- rep(0, allsize) # Left-most wall.
    grid[, allsize + 1 - i] <- rep(0, allsize) # Right-most wall.
    # Bottom dividing wall.
    grid[1:allsize, allsize - (150 - 15) / 150 * 1.5 * size + i] <- rep(0, allsize)
    # Vertical walls.
    grid[size / 4 + i, (0.4 * size + wt):(allsize - wt - exitwidth - 10)] <-
      rep(0, length((0.4 * size + wt):(allsize - wt - exitwidth - 10)))
    grid[allsize - size / 4 + 1 - i, (0.4 * size + wt):(allsize - wt - exitwidth - 10)] <-
      rep(0, length((0.4 * size + wt):(allsize - wt - exitwidth - 10)))
    grid[size / 4 + i, (allsize - wt - 10):allsize] <-
      rep(0, length((allsize - wt - 10):allsize))
    grid[allsize - size / 4 + 1 - i, (allsize - wt - 5):allsize] <-
      rep(0, length((allsize - wt - 5):allsize))
    # Horizontal wall.
    grid[(size / 4 + 1):(allsize - size / 4 + 1 - 1), allsize - 1.1 * size + i] <-
      rep(0, length((size / 4 + 1):(allsize - size / 4 + 1 - 1)))
  }

  ## Build doors to corridors:
  for (i in 1:wt) {
    grid[c((wt + 5):(wt + 15)), allsize - (150 - 15) / 150 * 1.5 * size + i] <-
      rep(1, length(c((wt + 5):(wt + 15))))
  }
  
  ## Assign static floor field here:

  # Plot the floor field if you want.
  if (plot) {
    image(grid)
  }
  # Write the coordinates of the exit, for later use in simulator.
  exitx <- rep(wt + 1, 4)
  exity <- (wt + 8):(wt + 11)

  # If you want to write the output of this to a file...
  if (writefile) {
    #...collate all data into a table, including the size of the grid, the 
    # coordinates of the destination, and the values of the floor field.
    out <- t(t(c("twocorridor", allsize, exitx, exity, as.vector(grid))))
    write.table(out, file = name, row.names = F, col.names = F, quote = F)
  }
  invisible(grid)
}

bigbangroom.v1 <- function(size = 50, wt = 3, rows = 5, benchthick = 3, 
                           exitwidth = 8, plot = T, writefile = F, 
                           name = "bigbangroom1.txt") {
  ### Function to create a room with one exit, a floor field towards this exit and 
  ### some bench-like obstacles (inspired by classroom setup).
  ### Takes the following input:
  ### 1. Size of the room (not the total environment) in 10s of centimetres.
  ### 2. Thickness of the walls in spatial cells.
  ### 3. The number of rows of benches to include in the room.
  ### 4. Set how thick the benches are in spatial cells.
  ### 5. Width of exit door in spatial cells.
  ### 6. Whether or not to plot the floor field.
  ### 7. Whether or not to write floor field values to file.
  ### 8. The name of the output file, if applicable.
  
  # Environment is 1.5x room size.
  allsize <- round(1.5 * size)
  # If the size of the environment is an odd number of cells...
  if (allsize %% 2 != 0) {
    #...make it even.
    allsize <- allsize + 1
  }
  # Initialise spatial grid.  Default cell value = 1.
  grid <- matrix(rep(1, allsize * allsize), ncol = allsize, nrow = allsize)
  
  ## Create walls:
  for (i in 1:wt) {
    grid[i, ] <- rep(0, allsize) # Top wall.
    grid[allsize + 1 - i, ] <- rep(0, allsize) # Bottom wall.
    grid[, i] <- rep(0, allsize) # Left-most wall.
    grid[, allsize + 1 - i] <- rep(0, allsize) # Right-most wall.
    grid[, allsize - size - wt + 1 - i] <- rep(0, allsize) # Horizontal divider.
  }

  # Calculate the width of the room, starting from the dividing wall
  # and ending at the top wall.
  realroom <- length(which(grid[round(allsize / 2), 
                                (allsize - size - wt + 1 - 1):(allsize)] > 0))
  # Reduce the width of the room.
  upto <- (allsize - realroom) / 2
  for (i in 1:upto) {
    grid[i, ] <- rep(0, allsize)
    grid[allsize + 1 - i, ] <- rep(0, allsize)
  }

  ## Build door:
  # A quick check to see if input exit width = output exit width.  Not crucial 
  # to function success.
  from <- upto + 1
  upto <- from + exitwidth - 1
  # If the length of the exit door is not correct...
  if (length(from:upto) != exitwidth) {
    #...let the user know.
    print("you don't get the right exitwidth")
  }
  # Make the door.
  for (i in 1:wt) {
    grid[from:upto, allsize - size - wt + 1 - i] <- rep(1, length(from:upto))
  }

  ## Build benches:
  width <- floor((realroom - (rows - 1) * benchthick) / rows)
  # For each row of benches (except the last one)...
  for (i in 1:(rows - 1)) {
    #...draw in the benches across the room...
    grid[, (allsize - size - wt + i * width + (i - 1) * benchthick + 1):
           (allsize - size - wt + i * width + (i) * benchthick)] <- rep(0, allsize)
    #...and add a space on the left for agents to move through.
    grid[(from):(upto), (allsize - size - wt + i * width + (i - 1) * benchthick + 1):
           (allsize - size - wt + i * width + (i) * benchthick)] <- 
      rep(1, length((from):(upto)))
    # If this is the last row of benches...
    if (i == (rows - 1)) {
      #...then this adds thickness to the top wall.
      grid[, (allsize - size - wt + (i + 1) * width + (i) * benchthick + 1):
             (allsize - size - wt + (i + 1) * width + (i + 1) * benchthick)] <- 
        rep(0, allsize)
    }
  }

  ## Assign static floor field:
  
  # Destination placed at bottom left corner of the room.
  tt <- rbind(c(from + exitwidth / 2 - 1, upto - exitwidth / 2), rep(wt + 1, 2))
  # Create a static floor field with maximum at destination and the distance 
  # values of each cell from the destination.
  test <- create.static(obstacles = grid, target = tt)
  grid <- test * grid
  # Plot the floor field if you want.
  if (plot) {
    image(grid)
  }
  # Write the coordinates of the exit, for later use in simulator.
  exitx <- rep(1 * wt, 4)
  exity <- seq(from = from, by = 2, length = 4)
  # If you want to write the output of this to a file...
  if (writefile) {
    #...collate all data into a table, including the size of the grid, the 
    # coordinates of the destination, and the values of the floor field.
    out <- t(t(c("classroom", allsize, exitx, exity, as.vector(grid))))
    write.table(out, file = name, row.names = F, col.names = F, quote = F)
  }
  invisible(grid)
}

jordan.game <- function(size = 100, wt = 3, exitwidth = 10, plot = T, 
                        writefile = F, name = "jordangame.txt") {
  ### Function to create a mirrored environment with two areas separated by a 
  ### dividing wall.
  ### Takes the following input:
  ### 1. Size of the room (not the total environment) in 10s of centimetres.
  ### 2. Thickness of the walls in spatial cells.
  ### 3. Width of exit door in spatial cells.
  ### 4. Whether or not to plot the floor field.
  ### 5. Whether or not to write floor field values to file.
  ### 6. The name of the output file, if applicable.
  
  # Environment is 1.5x room size.
  allsize <- round(1.5 * size)
  # If the size of the environment is an odd number of cells...
  if (allsize %% 2 != 0) {
    #...make it even.
    allsize <- allsize + 1
  }
  # Initialise spatial grid.  Default cell value = 1.
  grid <- matrix(rep(1, allsize * allsize), ncol = allsize, nrow = allsize)
  
  ## Create walls:
  for (i in 1:wt) {
    grid[i, ] <- rep(0, allsize) # Top wall.
    grid[allsize + 1 - i, ] <- rep(0, allsize) # Bottom wall.
    grid[, i] <- rep(0, allsize) # Left-most wall.
    grid[, allsize + 1 - i] <- rep(0, allsize) # Right-most wall.
    # Middle dividing wall.
    grid[, allsize / 2 + 2 - i] <- rep(0, allsize)
    grid[, allsize / 2 - 1 + i] <- rep(0, allsize)
    # Make the boundary walls thicker.
    grid[, i + 3] <- rep(0, allsize)
    grid[, i + 6] <- rep(0, allsize)
    grid[, allsize - 2 - i] <- rep(0, allsize)
    grid[, allsize - 5 - i] <- rep(0, allsize)
    # Horizontal wall section in each half of the space.
    grid[40:allsize, allsize / 2 - 39 - i] <- rep(0, length(40:allsize))
    grid[40:allsize, allsize / 2 + 1 + 39 + i] <- rep(0, length(40:allsize))
    # Vertical wall section in each half of the space.
    grid[40 - 1 + i, (allsize / 2 - 2 - 11):(allsize / 2 - 1 - 39)] <- 
      rep(0, length((allsize / 2 - 2 - 11):(allsize / 2 - 1 - 39)))
    grid[40 - 1 + i, (allsize / 2 + 2 + 11):(allsize / 2 + 1 + 39)] <- 
      rep(0, length((allsize / 2 + 2 + 11):(allsize / 2 + 1 + 39)))
  }

  ## Build doors:
  from <- 4
  upto <- from + exitwidth - 1
  # A quick check to see if input exit width = output exit width.  Not crucial 
  # to function success.
  # # If the length of the exit door is not correct...
  if (length(from:upto) != exitwidth) {
    #...let the user know.
    print("you don't get the right exitwidth")
  }
  # Make doors.
  for (i in 1:wt) {
    grid[from:upto, i + 3] <- rep(1, length(from:upto))
    grid[from:upto, i + 6] <- rep(1, length(from:upto))
    grid[from:upto, allsize - 2 - i] <- rep(1, length(from:upto))
    grid[from:upto, allsize - 5 - i] <- rep(1, length(from:upto))
  }

  ## Assign static floor field:
  from <- 4
  upto <- 13
  # Place destinations at upper and lower doors of the space.
  tt <- rbind(c(from + exitwidth / 2 - 1, upto - exitwidth / 2), rep(wt + 1, 2))
  # Create a static floor field with maximum at destination and the distance 
  # values of each cell from the destination.
  test <- create.static(obstacles = grid, target = tt)
  grid <- test[[1]] * grid

  # For the top half of the environment...
  for (i in (allsize / 2):allsize) {
    #...the free space cells are given negative floor field, since the 
    # destination is currently in the lower half...
    ind <- which(grid[, i] < 0)
    #...and convert them back to free space.
    grid[ind, i] <- 1
  }
  # Plot the floor field if you want.
  if (plot) {
    image(grid)
  }
  # Write the coordinates of the exit, for later use in simulator.
  exitx <- rep(1 * wt, 4)
  exity <- rep(from + exitwidth / 2 - 1, 4)
  # If you want to write the output of this to a file...
  if (writefile) {
    #...collate all data into a table, including the size of the grid, the 
    # coordinates of the destination, and the values of the floor field.
    out <- t(t(c("jordangame", allsize, exitx, exity, as.vector(grid))))
    write.table(out, file = name, row.names = F, col.names = F, quote = F)
  }
  invisible(grid)
}

ryan.game <- function(size = 100, wt = 3, exitwidth = 10, plot = T, 
                      writefile = F, name = "ryangame.txt") {
  ### Function to create to separate rooms - one that is empty and the other with 
  ### snaking corridors.
  ### Takes the following input:
  ### 1. Size of the room (not the total environment) in 10s of centimetres.
  ### 2. Thickness of the walls in spatial cells.
  ### 3. Width of exit door in spatial cells.
  ### 4. Whether or not to plot the floor field.
  ### 5. Whether or not to write floor field values to file.
  ### 6. The name of the output file, if applicable.
  
  # Environment is 1.5x room size.
  allsize <- round(1.5 * size)
  # If the size of the environment is an odd number of cells...
  if (allsize %% 2 != 0) {
    #...make it even.
    allsize <- allsize + 1
  }
  # Initialise spatial grid.  Default cell value = 1.
  grid <- matrix(rep(1, allsize * allsize), ncol = allsize, nrow = allsize)
  
  ## Create walls:
  for (i in 1:wt) {
    grid[i, ] <- rep(0, allsize) # Top wall.
    grid[allsize + 1 - i, ] <- rep(0, allsize) # Bottom wall.
    grid[, i] <- rep(0, allsize) # Left-most wall.
    grid[, allsize + 1 - i] <- rep(0, allsize) # Right-most wall.
    grid[, allsize / 2 + 2 - i] <- rep(0, allsize) # Dividing wall.
    grid[, allsize / 2 - 1 + i] <- rep(0, allsize)
    # First (left-most) stalagmite.
    grid[allsize / 2 + i, 1:(allsize / 2 - 21)] <- rep(0, length(1:(allsize / 2 - 21)))
    # First stalagtite wall.
    grid[allsize / 2 + 24 + i, (20):(allsize / 2)] <- rep(0, length((20):(allsize / 2)))
    # Second (right-most) stalagmite.
    grid[allsize / 2 + 49 + i, 1:(allsize / 2 - 21)] <- rep(0, length(1:(allsize / 2 - 21)))
  }

  ## Assign static floor field:

  # Set destination to be bottom right corner at the end of the snake path.
  tt <- rbind(c(138, 139), rep(wt + 1, 2))
  # Create a static floor field with maximum at destination and the distance 
  # values of each cell from the destination.
  test <- create.static(obstacles = grid, target = tt)
  grid <- test[[1]] * grid
  
  # For the top half of the environment...
  for (i in (allsize / 2):allsize) {
    #...the free space cells are given negative floor field, since the 
    # destination is currently in the lower half...
    ind <- which(grid[, i] < 0)
    #...and convert them back to free space.
    grid[ind, i] <- 1
  }

  ## Smoothing around corners in an attempt to avoid agents hugging the corners:
  for (u in 1:2) {
    grid2 <- grid
    for (j in 2:(allsize / 2 - 1)) {
      for (i in 2:(allsize - 2)) {
        grid2[i, j] <- mean(c(grid[i, j], grid[i - 1, j], grid[i + 1, j], grid[i, j - 1],
          grid[i, j + 1], 1 / sqrt(2) * grid[i - 1, j - 1], 1 / sqrt(2) *
            grid[i + 1, j + 1], 1 / sqrt(2) * grid[i - 1, j + 1], 1 / sqrt(2) *
            grid[i + 1, j - 1]))
      }
    }
    grid <- grid2
  }
  
  # For the top half of the environment...
  for (i in (allsize / 2):allsize) {
    #...the free space cells are given negative floor field, since the 
    # destination is currently in the lower half...
    ind <- which(grid[, i] < 0)
    #...and convert them back to free space.
    grid[ind, i] <- 1
  }
  
  # Plot the floor field if you want.
  if (plot) {
    image(grid)
  }
  
  # Write the coordinates of the exit, for later use in simulator.
  exitx <- rep(1 * wt + 3, 4)
  exity <- rep(139, 4) ## seq(from=from,by=2,length=4)
  # If you want to write the output of this to a file...
  if (writefile) {
    #...collate all data into a table, including the size of the grid, the 
    # coordinates of the destination, and the values of the floor field.
    out <- t(t(c("ryangame", allsize, exitx, exity, as.vector(grid))))
    write.table(out, file = name, row.names = F, col.names = F, quote = F)
  }
  invisible(grid)
}

corridor.new <- function(size = 100, wt = 30, plot = T, writefile = T, name = "corridor.txt") {
  ### Function to create a single corridor without floor field:
  ### Takes the following input:
  ### 1. size of the room (not the total environment).
  ### 2. Thickness of the walls in spatial cells.
  ### 3. Width of exit door in spatial cells.
  ### 4. Whether or not to plot the floor field.
  ### 5. Whether or not to write floor field values to file.
  ### 6. The name of the output file, if applicable.
  
  # Initialise spatial grid.  Default cell value = 1.
  grid <- matrix(rep(1, size * size), ncol = size, nrow = size)
  
  ## Create walls:
  for (i in 1:wt) {
    grid[i, ] <- rep(0, size) # Top wall.
    grid[size + 1 - i, ] <- rep(0, size) # Bottom wall.
  }
  
  # If you want to write the output of this to a file...
  if (writefile) {
    #...collate all data into a table, including the size of the grid, the 
    # coordinates of the destination, and the values of the floor field.
    out <- t(t(c("corridor", size, rep(0, 8), as.vector(grid))))
    write.table(out, file = name, row.names = F, col.names = F, quote = F)
  }
  invisible(grid)
}

intersection.new <- function(size = 100, width = 40, plot = T, writefile = F, 
                             name = "intersection.txt", T1 = T) {
  ### Function to create a four-way intersection.
  ### Takes the following input:
  ### 1. size of the environment in units of 10s of centimetres.
  ### 2. Whether or not to plot the floor field.
  ### 3. Whether or not to write floor field values to file.
  ### 4. The name of the output file, if applicable.
  ### 5. Whether or not destination is exit of one corridor or the other.
  ### 6. Whether the destination is in the top or horizontal corridor.
  
  # Initialise spatial grid.  Default cell value = 1.
  grid <- matrix(rep(1, size * size), ncol = size, nrow = size)
  
  ## create walls:
  # Wall thickness is half the remaining space between the corridors and outer walls.
  wt <- (size - width) / 2
  for (i in 1:wt) {
    for (j in 1:wt) {
      grid[i, 1:wt] <- rep(0, wt) # Top left box.
      grid[1:wt, size - i + 1] <- rep(0, wt) # Top right box.
      grid[size:(size - wt + 1), size - i + 1] <- rep(0, wt) # Bottom right box.
      grid[size - i + 1, 1:wt] <- rep(0, wt) # Bottom left box.
    }
  }

  ## Assign static floor field:
  # If the destination is in the vertical corridor...
  if (T1) {
    #...set the destination at top of vertical corridor.
    tt <- rbind((wt + 1):(wt + width), rep(size, width))
  } 
  # Otherwise, if the destination is in the horizontal corridor...
  else {
    #...set the destination at end of horizontal corridor.
    tt <- rbind(rep(size, width), (wt + 1):(wt + width))
  }
  # Create a static floor field with maximum at destination and the distance 
  # values of each cell from the destination.
  test <- create.static(obstacles = grid, target = tt)
  grid <- test[[1]] * grid
  
  # Plot the floor field if you want.
  if (plot) {
    image(grid)
  }
  
  # If you want to write the output of this to a file...
  if (writefile) {
    #...collate all data into a table, including the size of the grid, the 
    # coordinates of the destination, and the values of the floor field.
    out <- t(t(c("intersection", size, rep(0, 8), as.vector(grid))))
    write.table(out, file = name, row.names = F, col.names = F, quote = F)
  }
  invisible(grid)
}

cross_room <- function(size = 100, wt = 3, plot = T, writefile = F,
                        name = "Rooms/cross_room.txt") {
  ### Function to build an empty room with a '+' set of walls in the centre.
  ### Requires the following inputs:
  ### 1. Size of the environment in units of 10s of centimetres.
  ### 2. Whether or not to plot the floor field.
  ### 3. Whether or not to write floor field values to file.
  ### 4. The name of the output file, if applicable.
  ### 5. Whether or not destination is exit of one corridor or the other.
  ### 6. Whether the destination is in the top or horizontal corridor.
  
  # Initialise the matrix of the space.
  grid <- matrix(rep(1, size * size), ncol = size, nrow = size)
  
  ## Create walls:
  for (i in 1:wt) {
    grid[i, ] <- rep(0, size) # Top wall.
    grid[size + 1 - i, ] <- rep(0, size) # Bottom wall.
    grid[, i] <- rep(0, size) # Left wall.
    grid[, size + 1 - i] <- rep(0, size) # Right wall.
    # Vertical cross piece. 40 cells high.
    grid[size/2-round(wt/2)+i, seq(size/2 - 20, size/2 + 20, by = 1)] <- 
      rep(0, length(seq(size/2 - 20, size/2 + 20, by = 1)))
    # Horizontal cross piece. 40 cells wide.
    grid[seq(size/2 - 20, size/2 + 20, by = 1), size/2-round(wt/2)+i] <- 
      rep(0, length(seq(size/2 - 20, size/2 + 20, by = 1)))
  }
  
  #############################################################################
  # This next section of code orders a matrix of destination coordinates.
  # Sorts the destination coordinates in increasing x-coordinate first.
  # Then it sorts, for each unique x-coordinate, the y-coordinates in 
  # increasing order. So we get a matrix with the first row ordered in
  # increasing size and the second row is ordered in increasing order for each 
  # value in the first row.
  # E.g. [1, 3, 2, 5, 3]
  #      [2, 3, 4, 5, 1]
  # becomes: 
  #      [1, 2, 3, 3, 5]
  #      [2, 4, 1, 3, 5]
  # This is done because of a limitation in the subsequent GuiOutput.java when
  # painting each destination a different colour.
  # Initial unordered matrix.
  unordered_targets <- matrix(c(round(size/4), round(size/4), round(3*size/4), 
                                round(3*size/4), round(size/4), round(3*size/4), 
                                round(3*size/4), round(size/4)), nrow = 2, 
                              ncol = 4)
  # Sort by first row, maintaining corresponding values in row 2.
  new <-t(apply(unordered_targets,1,function(x) x[order(unordered_targets[1,])]))
  dest_xs <- new[1,] # Collect up the freshly-ordered x-coordinates.
  uniq_x <- unique(dest_xs) # Find each unique x-coordinate and...
  #...iterate over them.
  for (i in 1:length(uniq_x)) {
    # Collect the columns of the destination matrix containing the x-coordinate.
    temp <- new[, which(dest_xs == uniq_x[i])]
    # If this x-coordinate is repeated...
    if (dim(as.matrix(temp))[2] > 1) {
      #...order the second row of this reduced matrix in increasing order.
      temp <-t(apply(temp,1,function(x) x[order(temp[2,])]))
    }
    # If we've just started, then the sorted matrix is initialised.
    if (i == 1) {targets <- temp}
    # Otherwise, the sub-matrix is appended to the current matrix.
    else {targets <- cbind(targets, temp)}
  }
  
  # If we are writing output to a file...
  if (writefile) {
    # 'Type' of room.
    write("cross", file = name)
    # Grid dimensions.
    write(size, file = name, append = T)
    write(dim(targets)[2], file = name, append = T)
    # Sorted destination(s) coordinates.
    write.table(t(targets), file = name, append = T, sep = ",", row.names = F, 
                col.names = F)
  }
  
  # For each destination...
  for (i in 1:dim(targets)[2]) {
    tt <- as.matrix(targets[,i])
    #...create a static floor field with maximum at destination and the distance 
    # values of each cell from the destination.
    test <- create.static(obstacles = grid, target = tt)
    # Multiply by grid of obstacles to get 0 at obstacles.
    field <- test[[1]] * grid
    dists <- test[[2]] * grid
    # If you want to see what it looks like...
    if (plot) {
      image(field)
    }
    # If you want to write the output of this to a file...
    if (writefile) {
      #...write the floor field of the destination...
      write.table(field, file = name, row.names = F, col.names = F, quote = F, 
                  sep = ",", append = T)
      #...and the distances of each cell from the destination.
      write.table(dists, file = name, row.names = F, col.names = F, quote = F, 
                  sep = ",", append = T)
    }
  }
  invisible(field)
}

empty_room <- function(size = 100, wt = ceiling(0.03*size), dest_pos = 0.1,
                       plot = T, writefile = F, name = "Rooms/empty_room.txt") {
  ### Function to create a completely empty room with four destinations placed 
  ### at the corners of a square centred on the centre of the environment with 
  ### a given side length.
  ### Requires the following inputs:
  ### 1. Size of the environment in units of 10s of centimetres.
  ### 2. Thickness of the walls in spatial cells.
  ### 3. The relative position of the destinations from the boundaries as a 
  ### proportion of the environment width.
  ### 4. Whether or not to plot the floor field.
  ### 5. Whether or not to write floor field values to file.
  ### 6. The name of the output file, if applicable.
  ### The 'empty' environment described in the Appendix of King et al. 2022.
  ### https://doi.org/10.1080/23249935.2021.2017510.

  # Initialise the matrix of the space.
  grid <- matrix(rep(1, size * size), ncol = size, nrow = size)
  
  ## Create walls:
  for (i in 1:round(wt)) {
    grid[i, ] <- rep(0, size) # Top wall.
    grid[size + 1 - i, ] <- rep(0, size) # Bottom wall.
    grid[, i] <- rep(0, size) # Left wall.
    grid[, size + 1 - i] <- rep(0, size) # Right wall.
  }
  
  #############################################################################
  # This next section of code orders a matrix of destination coordinates.
  # Sorts the destination coordinates in increasing x-coordinate first.
  # Then it sorts, for each unique x-coordinate, the y-coordinates in 
  # increasing order. So we get a matrix with the first row ordered in
  # increasing size and the second row is ordered in increasing order for each 
  # value in the first row.
  # E.g. [1, 3, 2, 5, 3]
  #      [2, 3, 4, 5, 1]
  # becomes: 
  #      [1, 2, 3, 3, 5]
  #      [2, 4, 1, 3, 5]
  # This is done because of a limitation in the subsequent GuiOutput.java when
  # painting each destination a different colour.
  # Initial unordered matrix.  Each destination is placed at a distance from the 
  # boundaries proportional to the width of the environment.
  unordered_targets <- matrix(c(round(dest_pos*size), round(dest_pos*size), 
                                round((1-dest_pos)*size), round((1-dest_pos)*size), 
                                round(dest_pos*size), round((1-dest_pos)*size), 
                                round((1-dest_pos)*size), round(dest_pos*size)), 
                              nrow = 2, ncol = 4)
  # Sort by first row, maintaining corresponding values in row 2.
  new <-t(apply(unordered_targets,1,function(x) x[order(unordered_targets[1,])]))
  dest_xs <- new[1,] # Collect up the freshly-ordered x-coordinates.
  uniq_x <- unique(dest_xs) # Find each unique x-coordinate and...
  #...iterate over them.
  for (i in 1:length(uniq_x)) {
    # Collect the columns of the destination matrix containing the x-coordinate.
    temp <- new[, which(dest_xs == uniq_x[i])]
    # If this x-coordinate is repeated...
    if (dim(as.matrix(temp))[2] > 1) {
      #...order the second row of this reduced matrix in increasing order.
      temp <-t(apply(temp,1,function(x) x[order(temp[2,])]))
    }
    # If we've just started, then the sorted matrix is initialised.
    if (i == 1) {targets <- temp}
    # Otherwise, the sub-matrix is appended to the current matrix.
    else {targets <- cbind(targets, temp)}
  }
  # If we are writing output to a file...
  if (writefile) {
    # 'Type' of room.
    write("empty", file = name)
    # Grid dimensions.
    write(size, file = name, append = T)
    # Number of destinations.
    write(dim(targets)[2], file = name, append = T)
    # Sorted destination(s) coordinates.
    write.table(t(targets), file = name, append = T, sep = ",", row.names = F, 
                col.names = F)
  }
  # For each destination...
  for (i in 1:dim(targets)[2]) {
    tt <- as.matrix(targets[,i])
    #...create a static floor field with maximum at destination and the distance 
    # values of each cell from the destination.
    test <- create.static(obstacles = grid, target = tt)
    # Multiply by grid of obstacles to get 0 at obstacles.
    field <- test[[1]] * grid
    dists <- test[[2]] * grid
    
    # If you want to see what it looks like...
    if (plot) {
      filled.contour(field, col = 
                       heat.colors(20, alpha = 1), main = "Floor Field", 
                     xlab = "x", ylab = "y")
    }
    
    # If you want to write the output of this to a file...
    if (writefile) {
      #...write the floor field of the destination...
      write.table(field, file = name, row.names = F, col.names = F, quote = F, 
                  sep = ",", append = T)
      #...and the distances of each cell from the destination.
      write.table(dists, file = name, row.names = F, col.names = F, quote = F, 
                  sep = ",", append = T)
    }
  }
  return(list(field, dists))
}

four_room_room <- function(size = 100, wt = 3, doorthick = 8, plot = T, writefile = F,
                       name = "Rooms/four_room_room.txt") {
  ### Function which creates an environment split into four rooms each connected 
  ### to their neighbours by a doorway.
  ### Requires the following inputs:
  ### 1. Size of the environment in units of 10s of centimetres.
  ### 2. Thickness of the walls in spatial cells.
  ### 3. Width of the doors in spatial cells.
  ### 4. Whether or not to plot the floor field.
  ### 5. Whether or not to write floor field values to file.
  ### 6. The name of the output file, if applicable.
  
  # Initialise the matrix of the space.
  grid <- matrix(rep(1, size * size), ncol = size, nrow = size)
  
  # Define the widths and positions of the doors  They should be centred along 
  # each wall dividing the rooms.
  from_lower <- round(size/4) + round(doorthick/2) - doorthick
  upto_lower <- round(size/4) + round(doorthick/2)
  from_upper <- round(3*size/4) + round(doorthick/2) - doorthick
  upto_upper <- round(3*size/4) + round(doorthick/2)
  
  ## create walls and doors:
  for (i in 1:wt) {
    grid[i, ] <- rep(0, size) # Top wall.
    grid[size + 1 - i, ] <- rep(0, size) # Bottom wall.
    grid[, i] <- rep(0, size) # Left wall.
    grid[, size + 1 - i] <- rep(0, size) # Right wall.
    grid[, (size/2) + 2 - i] <- rep(0, size) # Middle vertical wall.
    grid[(size/2) + 2 - i,] <- rep(0, size) # Middle horizontal wall.
    # Left-hand door.
    grid[(size/2) + 2 - i, seq(from_lower, upto_lower)] <- rep(1, (doorthick + 1))
    # Right-hand door.
    grid[(size/2) + 2 - i, seq(from_upper, upto_upper)] <- rep(1, (doorthick + 1))
    # Lower door.
    grid[seq(from_lower, upto_lower), (size/2) + 2 - i] <- rep(1, (doorthick + 1))
    # Upper door.
    grid[seq(from_upper, upto_upper), (size/2) + 2 - i] <- rep(1, (doorthick + 1))
  }
  
  #############################################################################
  # This next section of code orders a matrix of destination coordinates.
  # Sorts the destination coordinates in increasing x-coordinate first.
  # Then it sorts, for each unique x-coordinate, the y-coordinates in 
  # increasing order. So we get a matrix with the first row ordered in
  # increasing size and the second row is ordered in increasing order for each 
  # value in the first row.
  # E.g. [1, 3, 2, 5, 3]
  #      [2, 3, 4, 5, 1]
  # becomes: 
  #      [1, 2, 3, 3, 5]
  #      [2, 4, 1, 3, 5]
  # This is done because of a limitation in the subsequent GuiOutput.java when
  # painting each destination a different colour.
  # Initial unordered matrix.
  unordered_targets <- matrix(c(round(size/4), round(size/4), round(3*size/4), 
                               round(size/4), round(3*size/4), 
                              round(3*size/4), round(size/4), round(3*size/4)), nrow = 2, 
                           ncol = 4)
  # Sort by first row, maintaining corresponding values in row 2.
  new <-t(apply(unordered_targets,1,function(x) x[order(unordered_targets[1,])]))
  dest_xs <- new[1,] # Collect up the freshly-ordered x-coordinates.
  uniq_x <- unique(dest_xs) # Find each unique x-coordinate and...
  #...iterate over them.
  for (i in 1:length(uniq_x)) {
    # Collect the columns of the destination matrix containing the x-coordinate.
    temp <- new[, which(dest_xs == uniq_x[i])]
    # If this x-coordinate is repeated...
    if (dim(as.matrix(temp))[2] > 1) {
      #...order the second row of this reduced matrix in increasing order.
      temp <-t(apply(temp,1,function(x) x[order(temp[2,])]))
    }
    # If we've just started, then the sorted matrix is initialised.
    if (i == 1) {targets <- temp}
    # Otherwise, the sub-matrix is appended to the current matrix.
    else {targets <- cbind(targets, temp)}
  }
  
  # If we are writing output to a file...
  if (writefile) {
    # 'Type' of room.
    write("cross", file = name)
    # Grid dimensions.
    write(size, file = name, append = T)
    # Number of destinations.
    write(dim(targets)[2], file = name, append = T)
    # Sorted destination(s) coordinates.
    write.table(t(targets), file = name, append = T, sep = ",", row.names = F, 
                col.names = F)
  }
  # For each destination...
  for (i in 1:dim(targets)[2]) {
    tt <- as.matrix(targets[,i])
    #...create a static floor field with maximum at destination and the distance 
    # values of each cell from the destination.
    test <- create.static(obstacles = grid, target = tt)
    # multiply by grid of obstacles to get 0 at obstacles.
    field <- test[[1]] * grid
    
    # If you want to see what it looks like...
    if (plot) {
      image(field)
    }
    
    # If you want to write the output of this to a file...
    if (writefile) {
      #...write the floor field of the destination.
      write.table(field, file = name, row.names = F, col.names = F, quote = F, 
                  sep = ",", append = T)
    }
  }
  invisible(field)
}

err_stud_room2 <- function(size = 100, wt = 3, plot = T, writefile = F,
                           name = "sym_horseshoe.txt", sym = F, shorten = F) {
  ### Function to create a 'horseshoe'-shaped environment, with a central area 
  ### connecting to rooms spread around the left, top, and right boundaries.  
  ### Could represent part of an indoor shopping centre with shops around a 
  ### central plaza.
  ### Requires the following input:
  ### 1. Size of the environment in units of 10s of centimetres.
  ### 2. Thickness of the walls in spatial cells.
  ### 3. Whether or not to plot the floor field.
  ### 4. Whether or not to write floor field values to file.
  ### 5. The name of the output file, if applicable.
  ### 6. Should the environment be centrally symmetric? If true, then all room 
  ### walls are the same length and same y-positions.  False for the paper.
  ### 7. Should the walls be artificially shortened? False for the paper.
  ### The 'asymmetric horseshoe' environment used in the King et al. 2022.
  ### https://doi.org/10.1080/23249935.2021.2017510.
  
  # The number of repetitions with which to locally-average floor field values 
  # so that there is a smoother floor field values close to the boundaries.
  av_reps <- 2
  # Initialise spatial grid. Initial cell values = 1.
  grid <- matrix(rep(1, size * size), ncol = size, nrow = size)
  # Set the centre of the circular part at the top of the environment.
  centrex <- size / 2
  centrey <- size / 2
  rad <- round(size / 2) - wt # circle radius is half the width of the environment.
  # Assign position of each grid cell in 0.1*metres.
  pos <- seq(from = 0.5, to = size - 0.5, length = size)
  # Fixed aspect of the wall lengths.
  free_depth <- round(0.4 * size)
  # The number of nearest neighbours over which to calculate locally-average 
  # floor field values.
  abut_size <- 1
  # The proportion of wall lengths by which walls are artificially reduced.
  wall_red <- 0.1
  
  ## Create walls:
  for (i in 1:size) {
    for (j in 1:(size/2)) {
      # Find distance of cell from centre.
      ddd <- sqrt((pos[i] - centrex)^2 + (pos[j] - centrey)^2)
      # If cell outside room radius...
      if (ddd >= rad) {
        #...then it is a wall.
        grid[i, j] <- 0
      }
    }
  }

  ## Create walls:
  for (i in 1:wt) {
    grid[i, ] <- rep(0, size) # Top wall (left wall seen in plot).
    grid[size + 1 - i, ] <- rep(0, size) # Bottom wall (right wall seen in plot).
    grid[, i] <- rep(0, size) # Left wall (bottom wall seen in plot).
    grid[, size + 1 - i] <- rep(0, size) # Right wall (top wall seen in plot).
  }
  
  
  d1 <- length(which(grid[(wt + 1), ] == 1))
  d2 <- length(which(grid[, size - (wt + 1)] == 1))
  lrw <- round((d1 - 2*wt)/3)
  rrw <- round((d1 - 2*wt)/2) # /2 when asymmetric, /3 when symmetric
  vrw <- round((d2 - 2*wt)/3) # set the initial spacing of bottom walls
  wxl <- seq((lrw + wt), by = (lrw + wt), length.out = 3) 
  wxr <- seq((rrw + wt), by = (rrw + wt), length.out = 2)
  wy <- seq((vrw + wt), by = (vrw + wt), length.out = 2)
  rd <- round((d2 - free_depth) / 2) # Length of all walls
  # If you want the vertically-symmetric version...
  if (sym) {
    #...the left, right, and vertical wall depths are all equal...
    lrd <- rd
    rrd <- rd
    vrd <- rd
    #...so too are the wall lengths.
    lwl <- length(wt:(wt + lrd))
    rwl <- length(wt:(wt + rrd))
    vwl <- length(wt:(wt + vrd))
    # Over the thickness of the walls...
    for (i in 1:wt) {
      #...create the left, right, and vertical walls.
      # Left walls (in the image)
      grid[wt:(wt + lrd), size - wxl + i] <- rep(0, length(wxl)*lwl) 
      # Right walls (in the image)
      grid[(size - wt):(size - wt - rrd), size - wxr + i] <- rep(0, length(wxr)*rwl)
      # Bottom walls (in the image)
      grid[wy + i, wt:(wt + vrd)] <- rep(0, length(wy)*vwl) 
    }
  }
  # If we are creating the asymmetric variant...
  else {
    #...specify the lengths of each wall as permutations of the symmetric wall length.
    # Change these constant coefficients to adjust wall lengths.
    lrd <- c(0, round(0.03*size), -round(0.04*size)) + rd 
    rrd <- c(-round(0.06*size), round(0.02*size)) + rd
    vrd <- c(0, -round(0.03*size)) + rd 
    # Calculate the lengths of each wall.
    lwl <- sapply(lrd, function(x, t) {return(length(t:(t + x)))}, t = wt)
    rwl <- sapply(rrd, function(x, t) {return(length(t:(t + x)))}, t = wt)
    vwl <- sapply(vrd, function(x, t) {return(length(t:(t + x)))}, t = wt)
    tlwl <- lwl - round(wall_red*lwl)
    trwl <- rwl - round(wall_red*rwl)
    tvwl <- vwl - round(wall_red*vwl)
    # Specify the positions of each wall.
    # Change these constant coefficients to adjust wall positions.
    pert_v_wall <- c(round(0.08*size), round(0.05*size))
    pert_l_wall <- c(-round(0.05*size), round(0.07*size), round(0.1*size))
    pert_r_wall <- c(-round(0.08*size), -round(0.05*size))
    wxl <- wxl + pert_l_wall
    wxr <- wxr + pert_r_wall
    wy <- wy + pert_v_wall
    # Over the thickness of the walls...
    for (i in 1:wt) {
      #...for each left wall...
      for (l in 1:length(wxl)) {
        #...build the wall.
        grid[wt:(wt + lrd[l]), size - wxl[l] + i] <- rep(0, lwl[l]) 
      }
      #...for each right wall...
      for (r in 1:length(wxr)) {
        #...build the wall.
        grid[(size - wt):(size - wt - rrd[r]), size - wxr[r] + i] <- rep(0, rwl[r]) 
      }
      #...for each vertical wall...
      for (v in 1:length(wy)) {
        #...build the wall.
        grid[wy[v] + i, wt:(wt + vrd[v])] <- rep(0, vwl[v])
      }
    }
  }
  
  # If you want to see what the environment looks like...
  if (plot) {
    #...create a static floor field with maximum at destination and the distance 
    # values of each cell from the destination.
    test <- create.static(obstacles = grid)
    # Multiply by grid of obstacles to get 0 at obstacles.
    field <- test[[1]] * grid
    # Create 2D plot of space. Note the object is transposed.
    image(field)
  }
  
  #############################################################################
  # This next section of code orders a matrix of destination coordinates.
  # Sorts the destination coordinates in increasing x-coordinate first.
  # Then it sorts, for each unique x-coordinate, the y-coordinates in 
  # increasing order. So we get a matrix with the first row ordered in
  # increasing size and the second row is ordered in increasing order for each 
  # value in the first row.
  # E.g. [1, 3, 2, 5, 3]
  #      [2, 3, 4, 5, 1]
  # becomes: 
  #      [1, 2, 3, 3, 5]
  #      [2, 4, 1, 3, 5]
  # This is done because of a limitation in the subsequent GuiOutput.java when
  # painting each destination a different colour.
  # Initial unordered matrix. Goes c(x1, y1, x2, y2,...).  Place a destination 
  # in each room.
  unordered_targets <- matrix(c(round(0.12*size), round(0.95*size), round(0.12*size), 
                                round(3*size/4), round(0.15*size), round(0.45*size), 
                                round(0.2*size), round(0.2*size), round(0.55*size), 
                                round(0.1*size), round(0.85*size), round(0.25*size), 
                              round(0.9*size), round(0.7*size), round(0.9*size), 
                              round(0.95*size)), nrow = 2, ncol = 8, byrow = F)
  # Sort by first row, maintaining corresponding values in row 2.
  new <-t(apply(unordered_targets,1,function(x) x[order(unordered_targets[1,])]))
  dest_xs <- new[1,] # Collect up the freshly-ordered x-coordinates.
  uniq_x <- unique(dest_xs) # Find each unique x-coordinate and...
  #...iterate over them.
  for (i in 1:length(uniq_x)) {
    # Collect the columns of the destination matrix containing the x-coordinate.
    temp <- new[, which(dest_xs == uniq_x[i])]
    # If this x-coordinate is repeated...
    if (dim(as.matrix(temp))[2] > 1) {
      #...order the second row of this reduced matrix in increasing order.
      temp <-t(apply(temp,1,function(x) x[order(temp[2,])]))
    }
    # If we've just started, then the sorted matrix is initialised.
    if (i == 1) {targets <- temp}
    # Otherwise, the sub-matrix is appended to the current matrix.
    else {targets <- cbind(targets, temp)}
  }
  
  # If we are writing output to a file...
  if (writefile) {
    # 'Type' of room.
    write("asym-horseshoe", file = name)
    # Grid dimensions.
    write(size, file = name, append = T)
    write(dim(targets)[2], file = name, append = T)
    # Sorted destination(s) coordinates.
    write.table(t(targets), file = name, append = T, sep = ",", row.names = F, 
                col.names = F)
  }
  
  # For each destination...
  for (i in 1:dim(targets)[2]) {
    tt <- as.matrix(targets[,i])
    #...create a static floor field with maximum at destination and the distance 
    # values of each cell from the destination.
    test <- create.static(obstacles = grid, target = tt)
    # Multiply by grid of obstacles to get 0 at obstacles.
    field <- test[[1]] * grid
    dists <- test[[2]] * grid
    
    ## If you want to try artificially shortening the walls after routes have 
    ## been calculated, these next for loops are required.
    if (shorten) {
      # For each left wall...
      for (l in 1:length(wxl)) {
        #...for each unit thickness of the wall...
        for (t in 1:wt) {
          #...set the field and distances at the end to be NA...
          field[(wt + tlwl[l]):(wt + lrd[l]), size - wxl[l] + t] <- 
            rep(NA, round(wall_red*lwl[l]))
          dists[(wt + tlwl[l]):(wt + lrd[l]), size - wxl[l] + t] <- 
            rep(NA, round(wall_red*lwl[l]))
          grid[(wt + tlwl[l]):(wt + lrd[l]), size - wxl[l] + t] <- 
            rep(1, round(wall_red*lwl[l]))
        }
        for (h in 1:av_reps) {
          #...these will then be filled in with locally-averaged values.
          # We need a sub-matrix of the area that needs calculating with additional 
          # rows and columns around it to pad the averaging process.
          h <- field[(wt + tlwl[l] - abut_size):(wt + lrd[l] + abut_size), 
                     (size - wxl[l] + 1 - abut_size):(size - wxl[l] + 3 + abut_size)]
          field[(wt + tlwl[l] - abut_size):(wt + lrd[l] + abut_size), 
                (size - wxl[l] + 1 - abut_size):(size - wxl[l] + 3 + abut_size)] <- 
            max_field_calc(field = h, zone = abut_size)
          # We need a sub-matrix of the area that needs calculating with additional 
          # rows and columns around it to pad the averaging process.
          p <- dists[(wt + tlwl[l] - abut_size):(wt + lrd[l] + abut_size), 
                     (size - wxl[l] + 1 - abut_size):(size - wxl[l] + 3 + abut_size)]
          # Calculate the locally-averaged distances for the NA cells.
          dists[(wt + tlwl[l] - abut_size):(wt + lrd[l] + abut_size), 
                (size - wxl[l] + 1 - abut_size):(size - wxl[l] + 3 + abut_size)] <- 
            av_field_calc(field = p, zone = abut_size)
        }
      }
      # For each right wall...
      for (r in 1:length(wxr)) {
        #...for each unit thickness of the wall...
        for (t in 1:wt) {
          ##...set the field and distances at the end to be NA...
          field[(size - wt - trwl[r]):(size - wt - rrd[r]), size - wxr[r] + t] <- 
            rep(NA, round(wall_red*rwl[r]))
          dists[(size - wt - trwl[r]):(size - wt - rrd[r]), size - wxr[r] + t] <- 
            rep(NA, round(wall_red*rwl[r]))
          grid[(size - wt - trwl[r]):(size - wt - rrd[r]), size - wxr[r] + t] <- 
            rep(1, round(wall_red*rwl[r]))
        }
        for (h in 1:av_reps) {
          #...these will then be filled in with locally-averaged values.
          # We need a sub-matrix of the area that needs calculating with additional 
          # rows and columns around it to pad the averaging process.
          h <- field[(size - wt - rrd[r] - abut_size):(size - wt - trwl[r] + abut_size), 
                     (size - wxr[r] + 1 - abut_size):(size - wxr[r] + 3 + abut_size)]
          # Calculate the locally-averaged floor field for the NA cells.
          field[(size - wt - rrd[r] - abut_size):(size - wt - trwl[r] + abut_size), 
                (size - wxr[r] + 1 - abut_size):(size - wxr[r] + 3 + abut_size)] <- 
            max_field_calc(field = h, zone = abut_size)
          # We need a sub-matrix of the area that needs calculating with additional 
          # rows and columns around it to pad the averaging process.
          p <- dists[(size - wt - rrd[r] - abut_size):(size - wt - trwl[r] + abut_size), 
                     (size - wxr[r] + 1 - abut_size):(size - wxr[r] + 3 + abut_size)]
          # Calculate the locally-averaged distances for the NA cells.
          dists[(size - wt - rrd[r] - abut_size):(size - wt - trwl[r] + abut_size), 
                (size - wxr[r] + 1 - abut_size):(size - wxr[r] + 3 + abut_size)] <- 
            av_field_calc(field = p, zone = abut_size)
        }
      }
        
      # For each vertical wall...
      for (v in 1:length(wy)) {
        #...for each unit thickness of the wall...
        for (t in 1:wt) {
          #...set the field and distances at the end to be NA...
          field[wy[v] + t, (wt + tvwl[v]):(wt + vrd[v])] <- 
            rep(NA, round(wall_red*vwl[v]))
          dists[wy[v] + t, (wt + tvwl[v]):(wt + vrd[v])] <- 
            rep(NA, round(wall_red*vwl[v]))
          grid[wy[v] + t, (wt + tvwl[v]):(wt + vrd[v])] <- 
            rep(1, round(wall_red*vwl[v]))
        }
        for (h in 1:av_reps) {
          #...these will then be filled in with locally-averaged values.
          # We need a sub-matrix of the area that needs calculating with additional 
          # rows and columns around it to pad the averaging process.
          h <- field[(wy[v] + 1 - abut_size):(wy[v] + 3 + abut_size), 
                     (wt + tvwl[v] - abut_size):(wt + vrd[v] + abut_size)]
          # Calculate the locally-averaged floor field for the NA cells.
          field[(wy[v] + 1 - abut_size):(wy[v] + 3 + abut_size), 
                (wt + tvwl[v] - abut_size):(wt + vrd[v] + abut_size)] <- 
            max_field_calc(field = h, zone = abut_size)
          # We need a sub-matrix of the area that needs calculating with additional 
          # rows and columns around it to pad the averaging process.
          p <- dists[(wy[v] + 1 - abut_size):(wy[v] + 3 + abut_size), 
                     (wt + tvwl[v] - abut_size):(wt + vrd[v] + abut_size)]
          # Calculate the locally-averaged distances for the NA cells.
          dists[(wy[v] + 1 - abut_size):(wy[v] + 3 + abut_size), 
                (wt + tvwl[v] - abut_size):(wt + vrd[v] + abut_size)] <- 
            av_field_calc(field = p, zone = abut_size)
        }
      }
      field <- field * grid
    }
    for (h in 1:av_reps) {
      # Average the floor field in each cell over its neighbours to get a repulsive 
      # effect from the walls.
      field <- av_field_calc(field = field, zone = 1)
    }
    
    # If you want to see what it looks like...
    if (plot) {
      #...plot the environment and floor field for this destination.
      filled.contour(field, col = heat.colors(20, alpha = 1), 
                     main = "Floor Field", plot.axes = {})
      # Save a PDF of the floor field for this destination.
      dev.copy2pdf(file = paste0("mall_horseshoe_dest_", i, ".pdf"))
    }
    
    # If you want to write the output of this to a file...
    if (writefile) {
      #...write the floor field of the destination...
      write.table(field, file = name, row.names = F, col.names = F, quote = F, 
                  sep = ",", append = T)
      #...and the distances of each cell from the destination.
      write.table(dists, file = name, row.names = F, col.names = F, quote = F, 
                  sep = ",", append = T)
    }
  }
  invisible(field)
}