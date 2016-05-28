namespace Common

open System
open FSharp.Collections

module RVD =

    let M = float Int32.MaxValue

    //БСВ с начальным элементом b
    let multiCongurSeq b = 
        let rec multiCongurSeq_astar (prev : float) = seq { 
            let cur = (prev * b) % M
            yield cur
            yield! multiCongurSeq_astar cur
        }
        (multiCongurSeq_astar b) |> Seq.map (fun x -> x / M)

    // Равномерно распределенная на интервале [a,b] случайная последовательность    
    let uniformDistSeq (a:float, b:float) s = (multiCongurSeq s) |> Seq.map (fun x -> a + (b-a) * x)

    // Дискретная случайная последовательность (prob, val)
    let discreteDistSeq (prob : (float * float) list) s =
        let unfolder (sum : float, l: (float * float) list) = 
            match l with
            | [] -> None
            | (prob, value)::t -> Some((sum + prob, value), (sum + prob, l.Tail))            

        let q = Seq.unfold unfolder (0., prob)

        let mapper a =
             let (_, value) = q |> Seq.find (fun (qi, _) -> a <= qi)
             value

        (multiCongurSeq s) |> Seq.map mapper