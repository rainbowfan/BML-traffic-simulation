CreateGrid = function (r, c, ncars = c(red, blue)) 
{   
  if (sum(ncars) >= r*c) stop ("overload")
  
  grid = matrix("", nrow = r, ncol = c)
  set.seed(1) 
  pos = sample(1:(r*c), sum(ncars))
  grid [pos] = rep(c("red", "blue"), c(ncars[1], ncars[2]))
  class(grid) = c("BMLgrid", class(grid))
  return(grid)
}   


location_car = function (grid)
{
  i = row(grid)[grid != ""]
  j = col(grid)[grid != ""]
  pos = cbind(i,j)
  data.frame(i=i, j=j, colors = grid[pos])
}


MoveCars = function(grid, color="red") 
{
  cars = location_car(grid)
  index = which(cars$color == color) 
  rows = cars [index,1]
  cols = cars [index,2]
  
  if(color == "red"){
    nextrows = rows
    nextcols = cols + 1L
    nextcols[nextcols > ncol(grid)] = 1L
  } else {
    nextrows = rows + 1L    
    nextrows[nextrows > nrow(grid)] = 1L
    nextcols = cols
  }
  
  nextlocation = cbind (nextrows, nextcols) 
  emptycheck = grid[nextlocation] == ""  
  grid[nextlocation[emptycheck, , drop = FALSE]] = color
  grid[cbind(rows, cols)[emptycheck, , drop = FALSE]] = ""
  
  unblocked_car = sum(emptycheck)
  tot_car = nrow(nextlocation) 
  v = unblocked_car/tot_car 
  
  returnlist = list(grid,v)
}


RunBMLgrid = function (r, c, ncars = c(red, blue), NumSteps)
{ 
  grid = CreateGrid(r, c, ncars)
  colors = rep(c("blue", "red"), ceiling(NumSteps/2)) 
  locations = MoveCars (grid, color = colors[1])[[1]]
  velocity = MoveCars (grid, color = colors[1])[[2]]
  for (i in 2: NumSteps){
    locations = MoveCars (locations, color = colors[i])[[1]]
    velocity = MoveCars (locations, color = colors[i])[[2]]
  }
  list(locations, velocity, NumSteps)
}


plot.BMLgrid = function(grid) 
{
  z = matrix(match(grid, c("", "red", "blue")), nrow(grid), ncol(grid)) 
  image(t(z), col = c("white", "red", "blue"),
        axes = FALSE)
  box()
}


summary.BMLgrid = function(x) {
  dim_grid = dim(x[[1]])
  Numsteps = x[[3]]
  velocity = x[[2]]
  blue_cars = sum(x[[1]]=="blue")
  red_cars = sum(x[[1]]=="red")
  density = (blue_cars + red_cars)/prod(dim_grid)
  moving_car = ifelse(x[[3]]%% 2 == 0,"red", "blue")
  out = list(Grid_Dimension = dim_grid, Current_Steps = Numsteps, Current_Velocity = velocity, Tot.Blue_Cars = blue_cars, Tot.Red_Cars = red_cars, Current.Moving_car = moving_car, Density = density)
  return(out)
}