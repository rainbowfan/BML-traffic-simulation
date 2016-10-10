# BML-traffic-simulation
##About this project
Bihman-Middleton-Levin Traffic Model Simulation is one of my projects in 2015 Spring STA 242 Statistical Programming course at UC Davis. It involves a simulation of a simple traffic flow model that exhibits a phase transition.

##About Biham-Middleton-Levine Traffic Model
This model involves two types of cars: “red” and “blue”. Initially, the cars are placed in random on the r * c dimensional grid without occupying the same cell. In order to simplify the process, we assume the number of red cars and blue cars are equal, but callers can adjust the quantities of each type of cars by themselves.  In this model, the blue cars move vertically upward at time periods t = 1, 3, 5, ..., while red cars move horizontally rightwards at time periods t = 2, 4, 6, .... When a blue car gets to the top row, it will go to the bottom row of the same column when it moves next time. Similarly, when a red car gets to the right edge of the lattice will move to the first column of the lattice next time. But one cell on the grid cannot be occupied by two cars simultaneously. In this model, cars that advance are treated as having velocity v = 1, while cars are blocked when v = 0, indicating severe traffic congestions.

##R package creation and performance improvement through profiling and adding C code
BMLmodel folder contains all the files needed to create BML R package.
crunBMLGrid.c is added later to improve algorithm performance. 
Comparisons between algorithm with and without c code can be found in BML_Ccomparisons.pdf.
Findings from simulations are covered in BMLsimulation.pdf.


