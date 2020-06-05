# asciichartr

> Print charts directly on your R terminal. This is a port for
> the R programming language.


# Using the package

Execute `asciiPlot` with a numeric vector as input and print the result,
for example:

```
ts <- c(2,1,4,6,7,3,5,5,3)
cat(asciiPlot(ts))
#    7.00 ┤   ╭╮    
#    6.00 ┤  ╭╯│    
#    5.00 ┤  │ │╭─╮ 
#    4.00 ┤ ╭╯ ││ │ 
#    3.00 ┤ │  ╰╯ ╰ 
#    2.00 ┼╮│       
#    1.00 ┤╰╯       
```

Also check the documentation for configuration options: `?asciiPlot`.


# Credits

Inspired by [asciichart](https://github.com/kroitor/asciichart)

