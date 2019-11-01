# Leon

Leon is a reimagining of [Forge](https://www.github.com/zesterer/forge), an interpreted language I wrote almost a year ago.
It has the same broad ambitions as Forge, but with a greater emphasis on optimisation and code quality.

## Example

```
let square = |x| x * x;

var squares = [];
for i in 1..=100 {
    squares += square(i);
}

for square in squares {
    println(square);
}
```
