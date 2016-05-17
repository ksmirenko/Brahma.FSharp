module ArrayGPU.TimeTests

let compare1 arr arrFunc parFunc myFunc n (name: string) = //functions that take 1 array. n=3 if parallel fun exist, n=2 otherwise
    let t = ArrayGPU.init arr
    let time f =
        let beg = System.DateTime.Now
        for i in 1..10 do f() |> ignore 
        (((System.DateTime.Now - beg).TotalMilliseconds))/10.00
    
    System.Console.WriteLine (name)
    System.Console.WriteLine ("Array: {0}", time (fun() -> arrFunc arr))
    if n = 3 then System.Console.WriteLine ("ArrayParallel: {0}", time (fun() -> parFunc arr))
    System.Console.WriteLine ("ArrayGPU: {0}", time (fun() -> myFunc arr t))

let compare2 arr1 arr2 arrFunc parFunc myFunc n (name: string) = //functions that take 2 arrays - map2 only
    let t = ArrayGPU.init arr1
    let time f =
        let beg = System.DateTime.Now
        for i in 1..10 do f() |> ignore 
        ((System.DateTime.Now - beg).TotalMilliseconds)/10.00
    
    System.Console.WriteLine (name)
    System.Console.WriteLine ("Array: {0}", time (fun() -> arrFunc arr1 arr2 ))
    if n = 3 then System.Console.WriteLine ("ArrayParallel: {0}", time (fun() -> parFunc arr1 arr2 ))
    System.Console.WriteLine ("ArrayGPU: {0}", time (fun() -> myFunc arr1 arr2 t))

let arr1 = [|for i in 1..10000000 -> i|]
let arr2 = [|for i in 1..10000000 -> i|]

compare1 arr1 (Array.map (fun x -> x + 2)) (Array.Parallel.map (fun x -> x + 2)) (ArrayGPU.Map (<@fun x -> x + 2 @>) ) 3 "MAP"
compare1 arr1 (Array.mapi (fun i x -> x + i)) (Array.Parallel.mapi (fun i x -> x + i)) (ArrayGPU.Mapi (<@ fun i x-> x + i @>) ) 3 "\nMAPI"
compare2 arr1 arr2 (Array.map2 (fun i x -> x + i)) (Array.map2 (fun i x -> x + i)) (ArrayGPU.Map2 (<@ fun i x-> x + i @>) ) 2 "\nMAP2"
compare1 arr1 (Array.rev ) (Array.rev) (ArrayGPU.Reverse) 2 "\nREVERSE"

let n = System.Console.ReadLine()