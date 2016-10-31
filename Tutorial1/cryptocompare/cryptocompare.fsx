#r @"../../packages/FSharp.Data/lib/net40/FSharp.Data.dll"

open FSharp.Data

let url = sprintf "https://www.cryptocompare.com/api/data/price?fsym=%s&tsyms=%s"
type CryptoCompare = JsonProvider<"https://www.cryptocompare.com/api/data/price?fsym=BTC&tsyms=USD">
let loadRate fromCur toCur = if fromCur <> toCur then CryptoCompare.Load(url fromCur toCur).Data.[0].Price else 1.0M
let getPaths fromCur throughCurrs toCur = throughCurrs |> Seq.map (fun t -> (loadRate fromCur t) * (loadRate t toCur))

getPaths "ZEC" ["ETH"; "BTC"; "ZEC"] "USD"