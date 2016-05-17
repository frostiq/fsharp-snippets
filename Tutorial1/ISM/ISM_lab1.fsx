#load @"..\Scripts\load-references-debug.fsx"

open System
open FSharp.Charting
open MathNet.Numerics.Statistics
open MathNet.Numerics.Distributions
open FSharp.Collections.ParallelSeq

let M = double Int32.MaxValue
let b = 4099.
let lambda = [| 0.5; 1.; 2.; 4.; 6.; 10. |]
let m = 1000
let sqr x = x * x

let rec multiCongurSeq_astar (prev : double) = 
    seq { 
        let cur = (prev * b) % M
        yield cur
        yield! multiCongurSeq_astar cur
    }

let multiCongurSeq = (multiCongurSeq_astar b) |> Seq.map (fun x -> x / M)
let samples = multiCongurSeq |> Seq.chunkBySize m
let doubleSeq seq = seq |> Seq.map double

let puassonSeq l = 
    samples |> Seq.map (fun s -> 
                   s
                   |> Seq.filter (fun x -> x < (l / double m))
                   |> Seq.length)

let chiSqrPuasson (k : int, n : int) l = 
    let samples = puassonSeq l |> Seq.take n
    let upperBound = (samples |> Seq.max) + 1
    let h = double upperBound / double k
    
    let data = 
        samples
        |> Seq.countBy (fun x -> int (float x / h))
        |> Seq.sortBy (fun (i, _) -> i)
    
    let p i = Poisson.CDF(l, float (i + 1) * h) - Poisson.CDF(l, float i * h)
    let sum ni pi = (sqr (float ni - float n * pi)) / float pi / float n
    let value = (data |> Seq.sumBy (fun (i, ni) -> sum ni (p i)))
    (value, data, h, upperBound)

let kolmogorovD n sequence = 
    sequence
    |> Seq.take n
    |> Seq.sort
    |> Seq.mapi (fun i xi -> abs (xi - double i / double n))
    |> Seq.max

let checkKolmogorov n sequence = 
    let c = sqrt (-0.5 * log (0.05 / 2.0))
    float (kolmogorovD n sequence) * sqrt (double n) < c

let sample precision count dist = 
    let p = System.Math.Pow(10., precision)
    dist
    |> Seq.take count
    |> Seq.countBy (fun v -> floor (v * p) / p)
    |> Seq.sortBy (fun (x, _) -> x)

let distrib n (sequence : seq<double>) = 
    let cdf = Statistics.empiricalCDFFunc (Seq.take n sequence)
    [ for tau in 0.0..0.05..20.0 -> (tau, cdf tau) ]

let puassonSamples l = sample 1. 10000 ((puassonSeq l) |> doubleSeq)
let puassonDistrib l = distrib 10000 ((puassonSeq l) |> doubleSeq)

let charts (points : seq<float * int>) = 
    [| for l in lambda -> Chart.Line(points, l.ToString()) |]

let probCharts = 
    [| for l in lambda -> Chart.Line(puassonSamples l, l.ToString()) |]

let ditrCharts = 
    [| for l in lambda -> Chart.Line(puassonDistrib l, l.ToString()) |]

let combine charts = Chart.Combine(charts).WithXAxis(Min = 0.).WithLegend(Enabled = true, InsideArea = false)
let show (chart : ChartTypes.GenericChart) = chart.ShowChart()
//show (combine ditrCharts)
let (value, data, h, max) = chiSqrPuasson (7, 100) 10.

Chart.Column data |> show

let delta = ChiSquared.InvCDF(6., 0.95)
