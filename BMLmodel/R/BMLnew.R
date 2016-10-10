CreateGrid = function (r, c, ncars = c(red, blue)) 
{   
  if(r <= 0| c <= 0) stop("Dimensions of the grid should be positive")
  if(any(ncars < 0)) stop('Number of cars can not be negative')
  if(sum(ncars) > r*c) stop("Total number of cars exceeds the maximum")
  
  grids = matrix(1, nrow = r, ncol = c)
  set.seed(1) 
  pos = sample(1:(r*c), sum(ncars))
  grids [pos] = rep(c(2, 3), c(ncars[1], ncars[2]))
  class(grids) = c("BMLgrid", class(grids))
  return(grids)
}   


location_car = function (grids)
{
  w = (grids !=1)
  i = row(grids)[w]
  j = col(grids)[w]
  pos = cbind(i,j)
  structure(pos, dimnames = list(grids[pos], c("i", "j")))
}


MoveCars = function(grid, color=2) 
{
  cars = location_car(grid)
  index = which(rownames(cars) == color) 
  rows = cars [index,1]
  cols = cars [index,2]
  
  if(color == 2){
    nextrows = rows
    nextcols = cols + 1L
    nextcols[nextcols > ncol(grid)] = 1L
  } else {
    nextrows = rows + 1L    
    nextrows[nextrows > nrow(grid)] = 1L
    nextcols = cols
  }
  
  nextlocation = cbind (nextrows, nextcols) 
  emptycheck = grid[nextlocation] == 1  
  grid[nextlocation[emptycheck, , drop = FALSE]] = color
  grid[cbind(rows, cols)[emptycheck, , drop = FALSE]] = 1
  
  unblocked_car = sum(emptycheck)
  tot_car = nrow(nextlocation) 
  v = unblocked_car/tot_car 
  
  return (list(grid=grid,velocity = v))
}


RunBMLgrid = function (grid, NumSteps)
{ 
  colors = rep(c(3, 2), ceiling(NumSteps/2)) 
  for (i in 1: NumSteps){
    grid = MoveCars (grid, color = colors[i])$grid
    velocity = MoveCars (grid, color = colors[i])$velocity
  }
  list(grid = grid, velocity = velocity, NumSteps = NumSteps)
}


plot.BMLgrid = function(grid) 
{
  z = matrix(match(grid, c(1, 2, 3)), nrow(grid), ncol(grid)) 
  image(t(z), col = c("white", "red", "blue"),
        axes = FALSE)
  box()
}


summary.BMLgrid = function(x) {
  dim_grid = dim(x$grid)
  Numsteps = x$NumSteps
  velocity = x$velocity
  blue_cars = sum(x$grid==3)
  red_cars = sum(x$grid==2)
  density = (blue_cars + red_cars)/prod(dim_grid)
  moving_car = ifelse(x$NumSteps%% 2 == 0, 2, 3)
  out = list(Grid_Dimension = dim_grid, Current_Steps = Numsteps, Current_Velocity = velocity, Tot.Blue_Cars = blue_cars, Tot.Red_Cars = red_cars, Current.Moving_car = moving_car, Density = density)
  return(out)
}


crunBMLGrid = function(grids, NumSteps)
{
  browser()
  origin.class = class(grids)
  n.row = nrow(grids)
  n.col = ncol(grids)
  velocity = rep(0, NumSteps)
  pos = location_car(grids)
  red = pos[rownames(pos) == "2",]
  blue = pos[rownames(pos) == "3",]
  dyn.load("HMoveCars.dll")
  ans = .C("Movecars", grids = as.integer(grids), newgrid = as.integer(grids), row = as.integer(n.row), col = as.integer(n.col),
           PosRed = as.integer(red),  PosBlue = as.integer(blue), NumRed = as.integer(nrow(red)), NumBlue = as.integer(nrow(blue)),
           NumStep = as.integer(NumSteps), velocity = as.double(velocity))
  ans$NumSteps = NumSteps
  ans$grids = matrix(ans$grids, nrow = n.row, ncol = n.col)
  ans = ans[c("grids", "velocity", "NumSteps")]
  class(ans$grids) = origin.class
  ans
}