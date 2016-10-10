##Adding C Code to improve performance
##Comparisons between BML Model with C code and without C code

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


MoveCars = function(grids, color=2) 
{
  cars = location_car(grids)
  index = which(rownames(cars) == color) 
  rows = cars [index,1]
  cols = cars [index,2]
  
  if(color == 2){
    nextrows = rows
    nextcols = cols + 1L
    nextcols[nextcols > ncol(grids)] = 1L
  } else {
    nextrows = rows + 1L    
    nextrows[nextrows > nrow(grids)] = 1L
    nextcols = cols
  }
  
  nextlocation = cbind (nextrows, nextcols) 
  emptycheck = grids[nextlocation] == 1  
  grids[nextlocation[emptycheck, , drop = FALSE]] = color
  grids[cbind(rows, cols)[emptycheck, , drop = FALSE]] = 1
  
  unblocked_car = sum(emptycheck)
  tot_car = nrow(nextlocation) 
  v = unblocked_car/tot_car 
  
  return (list(grids=grids,velocity = v))
}


RunBMLgrid = function (grids, NumSteps)
{ 
  colors = rep(c(3, 2), ceiling(NumSteps/2)) 
  for (i in 1: NumSteps){
    grids = MoveCars (grids, color = colors[i])$grids
    velocity = MoveCars (grids, color = colors[i])$velocity
  }
  list(grids = grids, velocity = velocity, NumSteps = NumSteps)
}


plot.BMLgrid = function(grids) 
{
  z = matrix(match(grids, c(1, 2, 3)), nrow(grids), ncol(grids)) 
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
  origin.class = class(grids)
  n.row = nrow(grids)
  n.col = ncol(grids)
  velocity = rep(0, NumSteps)
  pos = location_car(grids)
  red = pos[rownames(pos) == "2",]
  blue = pos[rownames(pos) == "3",]
  ans = .C("Movecars", grids = as.integer(grids), newgrid = as.integer(grids), row = as.integer(n.row), col = as.integer(n.col),
           PosRed = as.integer(red),  PosBlue = as.integer(blue), NumRed = as.integer(nrow(red)), NumBlue = as.integer(nrow(blue)),
           NumStep = as.integer(NumSteps), velocity = as.double(velocity))
  ans$NumSteps = NumSteps
  ans$grids = matrix(ans$newgrid, nrow = n.row, ncol = n.col)
  ans = ans[c("grids", "velocity", "NumSteps")]
  class(ans$grids) = origin.class
  ans
}


#Plot 
grids = CreateGrid(4,4,c(2,2))
par(mfrow=c(2,2))
g1 = lapply(c(5,6), function(x) crunBMLGrid(grids, x)$grids)
#Comparison between continuous steps
lapply(g1, function(x) plot.BMLgrid(x)) 
#comparison between crunBMLGrid() and RunBMLgrid
grid1 = CreateGrid(50,50, c(500,500))
gr = RunBMLgrid(grid1, 5000)$grids
gc = crunBMLGrid(grid1, 5000)$grids
plot.BMLgrid(gr)
plot.BMLgrid(gc)
grid1 = CreateGrid(6,6, c(6,2))
gr = RunBMLgrid(grid1, 1)$grids
gc = crunBMLGrid(grid1, 1)$grids
plot.BMLgrid(gr)
plot.BMLgrid(gc)

#####Performance change as grid size changes and density changes.
g1 = lapply(seq(10, 100, 10), function(x) CreateGrid(x, x, c(x*x*0.4, x*x*0.4))) #density is 0.8
t = sapply(g1, function(x) system.time(RunBMLgrid(x, 10000)))[3,]
tc = sapply(g1, function(x) system.time(crunBMLGrid(x, 10000)))[3,]

par(mfrow = c(1,2))
plot(t ~ seq(10, 100, 10), type = 'l', xlab = 'Grid Size', ylab = 'Time (seconds)', main = 'Time vs Grid Size (Steps = 10000, density =0.8)')
lines(seq(10, 100, 10) ,tc,  col = "red")
legend('topleft', legend = c('runBMLGrid()', 'crunBMLGrid()'), col = c("black","red"), lty = 1,cex = 0.7)

g2 = lapply(seq(0.1, 0.9, 0.1), function(x) CreateGrid(50, 50, c(1250*x, 1250*x)))
t2 = sapply(g2, function(x) system.time(RunBMLgrid(x, 10000)))[3,]
t2c = sapply(g2, function(x) system.time(crunBMLGrid(x, 10000)))[3,]
plot(t2 ~ seq(0.1, 0.9, 0.1), ylim = c(0,100), type = 'l', xlab = 'Density', ylab = 'Time (seconds)', main = 'Time vs Density (Steps = 10000)')
lines(seq(0.1, 0.9, 0.1) ,t2c,  col = "red")
legend('topleft', legend = c('runBMLGrid()', 'crunBMLGrid()'), col = c("black","red"), lty = 1,cex = 0.7)