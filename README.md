# asciichartr

![](http://cranlogs.r-pkg.org/badges/grand-total/asciichartr)
[![Build Status](https://travis-ci.org/blmayer/asciichartr.svg?branch=master)](https://travis-ci.org/blmayer/asciichartr)

> Print charts directly on your R terminal. This is a port for
> the R programming language.


# Installing the package

As this package is on CRAN one can run `install.packages("asciichartr")`.


# Using the package

Execute `asciiPlot` with a numeric vector as input and print the result,
for example:

```
ts <- c(1,1,2,3,6,3,10,5,1,7,9,9,10,6,9,7,10,8)
cat(asciiPlot(ts))
#   10.00 ┤     ╭╮    ╭╮  ╭╮ 
#    9.00 ┤     ││  ╭─╯│╭╮││ 
#    8.00 ┤     ││  │  ││││╰ 
#    7.00 ┤     ││ ╭╯  ││╰╯  
#    6.00 ┤   ╭╮││ │   ╰╯    
#    5.00 ┤   │││╰╮│         
#    4.00 ┤   │││ ││         
#    3.00 ┤  ╭╯╰╯ ││         
#    2.00 ┤ ╭╯    ││         
#    1.00 ┼─╯     ╰╯         
```

Also check the documentation for configuration options: `?asciiPlot`.


# Credits

Inspired by [asciichart](https://github.com/kroitor/asciichart).

