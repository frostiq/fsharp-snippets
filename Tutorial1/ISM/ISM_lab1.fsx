#load @"..\Scripts\load-references-debug.fsx"
#load @"Common.fs"

open System
open FSharp.Charting
open MathNet.Numerics.Statistics
open MathNet.Numerics.Distributions
open Common

// константы
let lambda = [| 0.5; 1.; 2.; 4.; 6.; 10. |]
let m = 1000
let b = 4099.

let sqr x = x * x
let samples = (RVD.multiCongurSeq b) |> Seq.chunkBySize m

// Функции для манипуляции с последовательностями
let sample precision count dist = 
    let p = System.Math.Pow(10., precision)
    dist
    |> Seq.take count
    |> Seq.countBy (fun v -> floor (v * p) / p)
    |> Seq.map (fun (x, n) -> (x, float n))
    |> Seq.sortBy (fun (x, _) -> x)

let distrib n (sequence : seq<float>) = 
    let cdf = Statistics.empiricalCDFFunc (Seq.take n sequence)
    seq { 
        for tau in 0.0..0.05..20.0 -> (tau, cdf tau)
    }

let floatSeq seq = seq |> Seq.map float

//Пуассон
let puassonSeq l = 
    samples |> Seq.map (fun s -> 
                   s
                   |> Seq.filter (fun x -> x < (l / float m))
                   |> Seq.length)

let puassonSeq2 l = 
    let e = Math.Exp(-l)
    let rec impl baseseq = seq {        
        let rec prod sampl k curr = 
            if curr <= e then (sampl, k - 1, curr)
            else prod (Seq.tail sampl) (k + 1) (curr * Seq.head sampl)
        
        let (rest, res, _) = prod baseseq 0 1.
        yield res
        yield! impl rest
    }

    impl (RVD.multiCongurSeq b)

let p l (a, b) = Poisson.CDF(l, a) - Poisson.CDF(l, b)

// Хи квадрат
let chiSqrPuasson samples (k : int) (p : float * float -> float) = 
    let n = Seq.length samples
    let upperBound = (samples |> Seq.max) + 1
    let h = float upperBound / float k
    
    let data = 
        samples
        |> Seq.countBy (fun x -> int (float x / h))
        |> Seq.sortBy (fun (i, _) -> i)
    
    let sum ni pi = (sqr (float ni - float n * pi)) / float pi / float n
    let _p i = p ((float (i + 1) * h), (float i * h))
    let value = (data |> Seq.sumBy (fun (i, ni) -> sum ni (_p i)))
    (value, data, h, upperBound)

let puassonSamples l = sample 1. 10000 ((puassonSeq l) |> floatSeq)
let puassonDistrib l = distrib 10000 ((puassonSeq l) |> floatSeq)

// функции для работы с графиками
let charts (points : float -> seq<float * float>) = 
    [| for l in lambda -> Chart.Line(points l, l.ToString()) |]

let combine charts = Chart.Combine(charts).WithXAxis(Min = 0.).WithLegend(Enabled = true, InsideArea = false)
let show (chart : ChartTypes.GenericChart) = chart.ShowChart()

let probCharts = combine (charts puassonSamples) |> Chart.WithTitle "PMF"
let ditrCharts = combine (charts puassonDistrib) |> Chart.WithTitle "Distribution"

// вывод результатов
//show probCharts
//show ditrCharts
//
//let (value, data, h, max) = chiSqrPuasson (Seq.take 100 (puassonSeq 10.)) 7 (p 10.)
//Chart.Column data |> show
//let delta = ChiSquared.InvCDF(6., 0.95)