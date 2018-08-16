## This is a short program to generate a picture illustrating
## that non-orthoganility is 
## reobatianed when iteratively applying projections
##
## A 2D point is projected onto two 1d linear subspaces which
## in this case are lines through the origin
##
## Note the formula for projecting a 2D point (y1, y2) onto a subspace
## (z, b * z) for z in R^1 
##
## Gives horizontal axis point z = p * Y2 / b + (1-p) * y1
## where p = b^2 / (b^2 + 1)
##
## Vertical axis point z * b
##
## The residual vector is then of course the substraction of these two
##
## Notation
## p[]sj means project [] onto subspace j so, for example
## pys1 = project point y onto subspace 1
## ppys2s2 = project the projection of point y onto subspace 1 onto subspace 2
## r is the same thing for residual

## The 2D response is (y1, y2)
y = c(.4, .8)

## The slopes of the linear subspace 
## Subspace1 is the set {z * slope1 | z in R}
## Subspace2 is the set {z * slope2 | z in R}
slope1 = .75
slope2 = .05

## The projection function of a point (y1, y2)
## onto a the subspace defined by a 
## line through the origin with slope b
project = function(y, b) {
  p = b ^ 2 / (b ^ 2 + 1)
  
  point1 = p * y[2] / b +  (1 - p) * y[1]
  point2 = b * point1
  
  projection = c(point1, point2)
  residual = y - projection
  list(projection = projection,
       residual = residual)
}

## Make the arrows function like the lines function so that
## I can just switch back and forth between lines and arrow
arrow = function(x, y, ...)
  arrows(x[1], y[1], x[2], y[2], ...)

## The projection and residual of the observed
## data point onto the first subspace
temp = project(y, slope1)
pys1 = temp$projection
rys1 = temp$residual
rm(temp)

## The projection and residual onto the second subspace 
## of the residual of the projection from the first subspace 
temp = project(rys1, slope2)
prys1s2 = temp$project
rrys1s2 = temp$residual
rm(temp)

## The inner product of this residual and the first residual
## Note you'd want this to be orthongoal since the first residual
## is guaranteed to be orthogonal to any point in S1
bleedover = sum(rys1, rrys1s2)

## define a plotrange, just cover all of the points
plotrange = range(c(y, pys1, prys1s2, rys1, rrys1s2))

## Create a blank plot
plot(plotrange, plotrange, type = "n", xlab = "", ylab = "", 
     frame = FALSE, 
     axes = FALSE, asp = 1)
## Uncomment this if you want the title to display the inner
## product of the second residual with the first
#title(paste0("The inner product is: ", round(bleedover, 3)))

## The two linear subspaces
abline(0, slope1)
abline(0, slope2)

## The response as a point
points(y[1], y[2])

## The projection of the response onto the first line
points(pys1[1], pys1[2])

## The line from the data to the projected point on the first subspace
arrow(c(y[1], pys1[1]), c(y[2], pys1[2]), col = "blue", lty = 3)

## A dashed line connecting the projection of the 
## original data point to the residual from projecting onto the 
## first subspace
arrow(c(pys1[1], rys1[1]), c(pys1[2], rys1[2]), lty = 3, col = "blue")

## The residual from the first projection
points(rys1[1], rys1[2])

## The line to the origin from that residual
lines(c(0, rys1[1]), c(0, rys1[2]), col = "blue", lwd = 2)

## The residual projected onto the second space
points(prys1s2[1], prys1s2[2])

## The line from the residual to its projection on the second space
arrow(c(rys1[1], prys1s2[1]), c(rys1[2], prys1s2[2]), 
      col = "red", lty = 3)

## A dashed line from the second projection to its residual
arrow(c(prys1s2[1], rrys1s2[1]), c(prys1s2[2], rrys1s2[2]),
      col = "red", lty = 3)

## the second residual shown at the origin
points(rrys1s2[1], rrys1s2[2])
lines(c(0, rrys1s2[1]), c(0, rrys1s2[2]), col = "red", lwd = 2)



