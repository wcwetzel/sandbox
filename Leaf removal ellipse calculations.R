# Calculating what proportion of an ellipse (leaf) to cut along the major
# axis to remove a certain proportion of total ellipse area

# Apr 2024

# Formula for area of ellipse
area.ellipse = function(A, B) {
  A/2 * B/2 * pi
}

# Formula for area of segment of ellipse, cut perpendicular to major axis (A)
area.seg = function(A, B, H) {
  (A*B/4) *
    ( acos(1-2*H/A) -
        (1 - 2*H/A) * sqrt(4*H/A - 4*H^2 /A^2) )
}
# See https://www.had2know.org/academics/ellipse-segment-tank-volume-calculator.html




# Now convert to proportions ####

# X = prop of leaf desired
# A = major axis length (leaf length)
# B = minor axis length (leaf width)

prop.area = function(A, B, H) {

  ( acos(1-2*H/A) -
      (1 - 2*H/A) * sqrt(4*H/A - 4*H^2 /A^2) ) / pi

}


cut.proportions = seq(0,1, length=1000)

prop.areas = prop.area(10, 1, cut.proportions*10)

plot(cut.proportions ~ prop.areas, type='l', col=2)
abline(a=0,b=1)



