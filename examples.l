; s = function to print sums
[let [s [l [x y]
          [print "x: " @x ", y: " @y ", x + y = " [add @x @y] "\n"]]]
  [do [@s #1 #2]
      [@s #13 #27]
      [@s #0 #0]]]

; f = Factorial
[let [f0 [l [x p]
          [if [equal @x #0] 
              @p
              [mul @x
                   [@f0 [sub @x 1] @p]]]]
      f [l [x] [@f0 @x 1]]]
  [@f 5]]
