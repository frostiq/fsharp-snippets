#r @"..\..\packages\FSharp.Data.2.2.5\lib\net40\FSharp.Data.dll"
#r @"..\..\packages\FSharp.Text.RegexProvider.0.0.7\lib\net40\FSharp.Text.RegexProvider.dll"
#r @"..\..\packages\Newtonsoft.Json.8.0.3\lib\net45\Newtonsoft.Json.dll"

open FSharp.Data
open System.IO
open System

let makeBody = sprintf """{
    "from": 0,
    "size": 0,
    "query": {
        "query_string": {
            "query": "expandable",
            "fields": [
                "ExceptionDetails"
            ],
            "phrase_slop": 1
        }
    },
    "aggregations": {
        "group_by_qs": {
            "terms": {
                "field": "QueryString",
                "from": %d,
                "size": %d,
            }
        }
    }
}"""

[<Literal>]
let sample1 = """{
   "took": 5200,
   "timed_out": false,
   "_shards": {
      "total": 1,
      "successful": 1,
      "failed": 0
   },
   "hits": {
      "total": 5612408,
      "max_score": 0,
      "hits": []
   },
   "aggregations": {
      "group_by_qs": {
         "doc_count_error_upper_bound": 0,
         "sum_other_doc_count": 110546302,
         "buckets": [
            {
               "key": "adx",
               "doc_count": 5612408
            }
         ]
      }
   }
}"""
type Parser1 = JsonProvider<sample1>

let request (skip, take) = 
    let body = makeBody skip take
    Http.RequestString(@"http://10.2.14.63:9200/adx_errors-2016.05.01/_search", httpMethod = "POST", body = TextRequest body)

let rec requests (skip, take) = 
    seq { 
            let req =  request (skip, take) |> Parser1.Parse
            let limit = req.Hits.Total
            printfn "%d, %f" skip (float skip / float limit) 
            yield req
            if skip + take < limit then
                yield! requests ((skip + take), take)
        }


let rec messages batchSize = 

    requests (0, batchSize) |> 
    Seq.collect (fun x -> x.Aggregations.GroupByQs.Buckets) |>
    Seq.map (fun x -> x.Key) |>
    id

printfn "total: %d" ((requests (0,1) |> Seq.head).Hits.Total)
let outFile = new StreamWriter(@"C:\temp\dump.json", true)
let mes = messages 500
for m in mes do outFile.WriteLine(m)
outFile.Close()