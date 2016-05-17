#load @"..\Scripts\load-references-debug.fsx"

open System
open MathNet.Numerics.Distributions

let (a, b) = (0., Math.PI / 2.)
let f x = Math.Cos x
let dist = new ContinuousUniform(a, b)

let samples n = dist.Samples() |> Seq.map f |> Seq.take n

let value n = (b - a) / float n * (samples n |> Seq.sum)

let accuracy n = 3. * Math.Sqrt ( dist.Variance / float n)