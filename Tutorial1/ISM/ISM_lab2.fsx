#load @"..\Scripts\load-references-debug.fsx"
#load @"Common.fs"

open MathNet.Numerics.Statistics
open Common

let S P ik = P * (1. + (List.sum ik) / float ik.Length)
let l3prob = [(0.1, 0.086); (0.3, 0.087); (0.5, 0.088); (0.1, 0.092) ]
let l4prob = [(0.1, 0.087); (0.6, 0.089); (0.3, 0.093) ]
let P = 1230.
let S1 = 1335.
let S2 = 1340.

let i1seq = RVD.uniformDistSeq (0.082, 0.09) 1000.
let i2seq = RVD.uniformDistSeq (0.084, 0.091) 2000.
let i3seq = RVD.discreteDistSeq l3prob 3000.
let i4seq = RVD.discreteDistSeq l4prob 4000.

let Sseq = 
    let tail = Seq.skip 1
    let rec SSeq1 i1 i2 i3 i4 = seq {
        yield S P [Seq.head i1; Seq.head i2; Seq.head i3; Seq.head i4;]
        yield! SSeq1 (tail i1) (tail i2) (tail i3) (tail i4)
    }
    SSeq1 i1seq i2seq i3seq i4seq |> Seq.take 1000

let (mean, variance) = Statistics.MeanVariance Sseq
let cdf = Statistics.empiricalCDFFunc Sseq

printfn "S max = %f" (Seq.max Sseq)
printfn "S min = %f" (Seq.min Sseq)
printfn "E[S] = %f" mean
printfn "D[S] = %f" variance
printfn "P[1335 <= S <= 1340] = %f" (cdf S2 - cdf S1)