#r @"..\..\packages\FSharp.Data.2.2.5\lib\net40\FSharp.Data.dll"
#r @"..\..\packages\FSharp.Text.RegexProvider.0.0.7\lib\net40\FSharp.Text.RegexProvider.dll"
#r @"..\..\packages\Newtonsoft.Json.8.0.3\lib\net45\Newtonsoft.Json.dll"

open FSharp.Data
open System.IO
open System
open System.Text

[<Literal>]
let sample2 = """{"took":56042,"timed_out":false,"_shards":{"total":1,"successful":1,"failed":0},"hits":{"total":5612408,"max_score":0.0,"hits":[]},"aggregations":{"group_by_qs":{"doc_count_error_upper_bound":0,"sum_other_doc_count":387179,"buckets":[{"key":"adx","doc_count":5612408}]}}}"""
type Parser2 = JsonProvider<sample2>

let data = File.ReadAllText(@"C:\temp\sample.txt") |> Parser2.Parse
let records = data.Aggregations.GroupByQs.Buckets

let tryParse s =
    try Convert.FromBase64String s 
    with :? FormatException -> null

let groupped (input : seq<Parser2.Bucket>) =
    input |> 
    Seq.map (fun x -> x.Key) |>
    Seq.map tryParse |>
    Seq.filter (fun x -> x <> null) |>
    Seq.map Encoding.UTF8.GetString |>
    Seq.collect (fun x -> x.Split '&') |>
    Seq.map (fun x -> x.Split '=') |>
    Seq.filter (fun x -> x.Length = 2) |>
    id

for e in  groupped (records |> Seq.take 10) do printfn "%s=%s" e.[0] e.[1]

records |> 
Seq.skip 3 |>
Seq.take 10 |>
Seq.map (fun x -> x.Key) |>
Seq.map tryParse |>
Seq.filter (fun x -> x <> null) |>
Seq.map Encoding.UTF8.GetString |>
id