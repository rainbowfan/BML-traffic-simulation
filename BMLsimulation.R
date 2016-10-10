CreateGrid = function (r, c, ncars = c(red, blue)) #ncars is a vector allowing callers to specify the number of blue cars and red cars.
{
  if (sum(ncars) >= r*c) stop ("overload")
  grid = matrix("", nrow = r, ncol = c)
  set.seed(1)
  pos = sample(1:(r*c), sum(ncars))
  grid [pos] = rep(c("red", "blue"), c(ncars[1], ncars[2])) 
  class(grid) = c("BMLgrid", class(grid))
  return(grid) 
}

location_car = function (grid) {
  i = row(grid)[grid != ""]
  j = col(grid)[grid != ""]
  pos = cbind(i,j)
  data.frame(i=i, j=j, colors = grid[pos])
}

##The first part of MoveCars function is quoted from professor Duncan Temple Lang's class notes in class.
MoveCars = function(grid, color="red") #Movecars returns a new grid and velocity
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
    nextrows = rows + 1L #position in image() is different from matrix 
    nextrows[nextrows > nrow(grid)] = 1L
    nextcols = cols
  }
  ￼nextlocation = cbind (nextrows, nextcols) #n*2 matrix
  emptycheck = grid[nextlocation] == "" ##check if next location is empty 
  grid[nextlocation[emptycheck, , drop = FALSE]] = color 
  grid[cbind(rows, cols)[emptycheck, , drop = FALSE]] = ""
  unblocked_car = sum(emptycheck)
  tot_car = nrow(nextlocation) #same color cars as unblocked
  v = unblocked_car/tot_car #velocity of cars at time t. velocity is defined as ratio of unblocked cars and total number of cars at time t.
  
  returnlist = list(grid,v) 
}


RunBMLgrid = function (r, c, ncars = c(red, blue), NumSteps) {
  grid = CreateGrid(r, c, ncars)
  colors = rep(c("blue", "red"), ceiling(NumSteps/2)) #t = 1,3,5...blue cars run; t =2,4,6...red cars run.
  locations = MoveCars(grid, color = colors[1])[[1]] 
  velocity = MoveCars(grid, color = colors[1])[[2]] 
  for (i in 2: NumSteps){
     locations = MoveCars(locations, color = colors[i])[[1]]
     velocity = MoveCars(locations, color = colors[i])[[2]] 
  }
  list(locations, velocity, NumSteps) 
}

###Slower version (old version) of RunBMLgrid (this is replaced by a faster function after profiling)
RunBMLgrid = function (r, c, ncars = c(red, blue), NumSteps)
{
  grid = CreateGrid(r, c, ncars)
  colors = rep(c("blue", "red"), NumSteps)
  locations = list()
  velocity = list()
  locations[[1]] = MoveCars(grid, color = colors[1])[[1]] 
  velocity[[1]] = MoveCars(grid, color = colors[1])[[2]] 
  for (i in 2: NumSteps){
    locations[[i]] = MoveCars(locations[[i-1]], color = colors[i])[[1]]
    velocity[[i]] = MoveCars(locations[[i-1]], color = colors[i])[[2]] 
  }
  list(locations, velocity) 
}

BMLgridPlot = function(grid) {
  z = matrix(match(grid, c("", "red", "blue")), nrow(loca), ncol(loca)) 
  image(t(z), col = c("white", "red", "blue"), axes = FALSE) 
  box()
}

￼##summary of RunBMLgrid
summary = function(x) {
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

###Density changes from 0.2 to 0.7 (we force the number of red cars and number of blue cars to be equal and fix grid to be 200*200)
p = seq(0.2, 0.7, by =0.05)
r = c =200

quant_cars = matrix(rep(p*r*c/2,2), nrow = 2, byrow = TRUE)

change.density = apply(quant_cars, 2, function(ncars) RunBMLgrid(200, 200, ncars, 5000))

rep.times = length(p) 
velocity = rep(0, rep.times) 
for (i in 1:rep.times) {
  velocity[i] = change.density[[i]][[2]] 
}

plot1 = plot(p, velocity, main = "Velocity vs Density (Square Lattice with L=200)", xlab = "density")

grid = list() #Getting grids at different densities 
for (i in 1:rep.times){
  grid[[i]] = change.density[[i]][[1]] 
}

#plot grids at p = 0.35 and p = 0.4 and compare with grid plots at p = 0.2 and 0.7 
par(mfrow = c(2,2))
image_p0.20 = BMLgridPlot(grid[[1]])
image_p0.35 = BMLgridPlot(grid[[4]])
image_p0.40 = BMLgridPlot(grid[[5]]) 
image_p0.70 = BMLgridPlot(grid[[11]])

###Grid size changes: L =50, 100, 150, 200, 250, 300 and fixed density p = 0.40 and steps = 5000 
grid_l50 = RunBMLgrid(50, 50, ncars=c(500,500), 5000)
grid_l100 = RunBMLgrid(100, 100, ncars=c(2000,2000), 5000)
grid_l150 = RunBMLgrid(150, 150, ncars=c(4500, 4500), 5000)
grid_l250 = RunBMLgrid(250, 250, ncars=c(12500, 12500), 5000) 
grid_l300 = RunBMLgrid(300, 300, ncars=c(18000,18000), 5000)

velocity_l = c(grid_l50[[2]], grid_l100[[2]], grid_l150[[2]], velocity[5], grid_l250[[2]], grid_l300[[2]])
L = seq(50, 300, by = 50)
plot(L, velocity_l, main = "Velocity vs Grid Size", xlab = "Grid Size")

par(mfrow = c(3,2))
image_l50 = BMLgridPlot(grid_l50[[1]]) 
image_l100 = BMLgridPlot(grid_l100[[1]]) 
image_l150 = BMLgridPlot(grid_l150[[1]]) 
image_l200 = BMLgridPlot(grid[[5]]) 
image_l250 = BMLgridPlot(grid_l250[[1]]) 
image_l300 = BMLgridPlot(grid_l300[[1]])

###Profiling (choose r = c = 1000, ncars = c(800, 800), NumSteps = 5000) 
#CreateGrid function
Rprof("CreateGrid.out")
y = CreateGrid(1000, 1000, ncars = c(800,800)) # Call the function to be profiled 
Rprof(NULL)
summaryRprof("CreateGrid.out")

#location_car function
Rprof("location_car.out")
y = location_car(CreateGrid(1000, 1000, ncars = c(800,800))) 
Rprof(NULL)
summaryRprof("location_car.out")

#MoveCars function
Rprof("MoveCars.out")
y = MoveCars(CreateGrid(1000, 1000, ncars = c(800,800)), color = "red") 
Rprof(NULL)
summaryRprof("MoveCars.out")

#RunBMLgrid function
Rprof("RunBMLgrid.out")
y = RunBMLgrid(1000, 1000, ncars = c(800,800), 5000) 
Rprof(NULL)
summaryRprof("RunBMLgrid.out")