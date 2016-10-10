library(BMLmodel)
context("UnitTest")

## set original grid as

#     "blue" "blue" ""   
#     ""     ""     ""   
#     "red"  ""     "red"

set.seed(100)
p = createBMLGrid(r = 3, c = 3, ncar=c(2,2))

## after 5 steps the grid should be

#     "" "blue" ""   
#     ""     ""     ""   
#     "blue"  "red"     "red"

p1.out = runBMLGrid(p,5)
p2.out = crunBMLGrid(p,5)
p.outcheck=matrix("",3,3)
p.outcheck[1, 2] = p.outcheck[3, 1] = "blue"
p.outcheck[3, 2] = p.outcheck[3, 3] = "red"
expect_true(all(p1.out$grid == p.outcheck))
expect_true(all(p2.out$grid == p.outcheck))
