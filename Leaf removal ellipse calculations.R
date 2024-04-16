# Calculating what proportion of an ellipse (leaf) to cut along the major
# axis to remove a certain proportion of total ellipse area

# Apr 2024


area.ellipse = function(A, B) {
  A/2 * B/2 * pi
}

area.seg = function(A, B, H) {
  (A*B/4) *
    ( acos(1-2*H/A) -
        (1 - 2*H/A) * sqrt(4*H/A - 4*H^2 /A^2) )
}


# X = prop of leaf desired

prop.major = function(X, A, B) {

}
