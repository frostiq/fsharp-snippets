#load @"..\Scripts\load-references-debug.fsx"

open System
open System.IO
open FSharp.Data
open Newtonsoft.Json

[<Literal>]
let path = "C:\\temp\\trainingSet.json"

type Obj = byte []
type Class = seq<Obj>
type TrainingSet = seq<Class>

type TrainingDataParser = JsonProvider<path>

let b1 t (X : Class) = float (X |> Seq.sumBy (fun x -> x.[t])) / float (Seq.length X)
let b2 t (X0 : TrainingSet) = (X0 |> Seq.sumBy (b1 t)) / float (Seq.length X0)

let a (X0 : TrainingSet) = 
    let a (X : Class) = 
        let a t = Math.Abs(b1 t X - b2 t X0)
        [| for i in 0..(Array.length (Seq.head X))-1 -> a i |]
    [| for i in X0 -> a i |]

let data = 
    let data = TrainingDataParser.Load(File.OpenText path)
    data
    |> Seq.groupBy (fun x -> x.ClassIndex)
    |> Seq.sortBy (fun (i, _) -> i)
    |> Seq.map (fun (_, objs) -> objs |> Seq.map (fun o -> o.Attributes |> Array.map Convert.ToByte))

let res = a data
File.WriteAllText("C:\\temp\\output.json", JsonConvert.SerializeObject res)

b1 0 (Seq.head data)