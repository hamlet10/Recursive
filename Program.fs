// Learn more about F# at http://fsharp.org

open System
//
let rec GCD x y =
    if y = 0 then x
    else GCD y (x % y)

let rec sum(n:int):bigint =
//Sum
//Simple-Recursive Sum
    match n with
    | 0 -> bigint(0)
    | _ -> bigint(n) + sum(n - 1)

//Iterative Sum
let iterSum n: int =
    let mutable resultado = 0
    for i in 1..n do
        resultado <- i + resultado
    resultado

//Tail-Recursion sum
let tSum(n: int): bigint =
    let rec tailSum n ac =
        if n <= bigint(1) then
            ac
        else
            tailSum(n - bigint(1))(ac + n)
    tailSum(bigint(n))(bigint(1))


//factorial

//Simple-Recusion Factorial
let rec factorial(n: int) : bigint =
    match n with
    | 0 -> bigint(1)
    | _ -> bigint(n) * factorial (n - 1)

//iterative factorial
let iterFactorial n: int =
    let mutable resultado = 1
    for i in 1..n do
        resultado <- i + resultado
    resultado

//Tail-Recusion Factorial
let tFactorial (n:int) : bigint =
    let rec tailFactorial n ac =
        if n <= bigint(1) then
            ac
        else
            tailFactorial(n - bigint(1))(ac * n)
    tailFactorial(bigint(n))(bigint(1))

// printfn "recursive factorial: %O" (factorial(10000))
// printfn "iterative factorial: %O" (iterFactorial(10000))
// printfn "tailRecusive factorial: %O" (tFactorial(10000))
// printfn "Simple-Recursion sumariar: %O" (sum(10000))
// printfn "Iteravie sumariar: %O" (iterSum(10000))
// printfn "Tail-Recursion sumariar: %O" (tSum(10000))
printfn "Tail-Recursion GCD: %O" (GCD(25)(5))


