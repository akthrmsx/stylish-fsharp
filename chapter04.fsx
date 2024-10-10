open System

type House = { Address: string; Price: decimal }

let houses: House array = [|
    {
        Address = "1 Acacia Avenue"
        Price = 250_000m
    }
    {
        Address = "2 Bradley Street"
        Price = 380_000m
    }
    {
        Address = "1 Carlton Road"
        Price = 98_000m
    }
|]

let cheapHouses: House array =
    houses |> Array.filter (fun house -> house.Price < 100_000m)

module House =
    let private random = Random(1)

    let getRandom (count: int) : House array =
        Array.init
            count
            (fun i -> {
                Address = sprintf "%i Stochastic Street" (i + 1)
                Price = random.Next(50_000, 500_000) |> decimal
            })

    let getRandomSeq (count: int) : House seq =
        Seq.init
            count
            (fun i -> {
                Address = sprintf "%i Stochastic Street" (i + 1)
                Price = random.Next(50_000, 500_000) |> decimal
            })

module Distance =
    let private random = Random(1)

    let tryToSchool (house: House) : double option =
        let distance = random.Next(10) |> double
        if distance < 5. then distance |> Some else None

type PriceBand =
    | Cheap
    | Medium
    | Expensive

module PriceBand =
    let fromPrice (price: decimal) : PriceBand =
        if price < 100_000m then Cheap
        elif price < 200_000m then Medium
        else Expensive

    let order: PriceBand -> int =
        function
        | Cheap -> 0
        | Medium -> 1
        | Expensive -> 2

let housePrices: string array =
    House.getRandom 20
    |> Array.map (fun house -> sprintf "Address: %s - Price: %f" house.Address house.Price)

let averagePrice: decimal = House.getRandom 20 |> Array.averageBy _.Price

let expensiveHouses: House array =
    House.getRandom 20 |> Array.filter (fun house -> house.Price > 250_000m)

let housesWithDistance: (House * double) array =
    House.getRandom 20
    |> Array.choose (fun house ->
        match house |> Distance.tryToSchool with
        | Some(distance) -> Some(house, distance)
        | None -> None
    )

House.getRandom 20
|> Array.filter (fun house -> house.Price > 100_000m)
|> Array.sortByDescending _.Price
|> Array.iter (fun house -> printfn "Address: %s Price: %f" house.Address house.Price)

let expensiveHousesAveragePrice: decimal =
    House.getRandom 20
    |> Array.filter (fun house -> house.Price > 200_000m)
    |> Array.averageBy _.Price

let firstHouseWithDistance: House * double =
    House.getRandom 20
    |> Array.filter (fun house -> house.Price < 100_000m)
    |> Array.pick (fun house ->
        match house |> Distance.tryToSchool with
        | Some(distance) -> Some(house, distance)
        | None -> None
    )

let housesGroupedByPriceBand: (PriceBand * House array) array =
    House.getRandom 20
    |> Array.groupBy (fun house -> house.Price |> PriceBand.fromPrice)
    |> Array.map (fun (band, houses) -> band, houses |> Array.sortBy _.Price)

module Array =
    let inline averageOrZero (values: 'T array) : 'T =
        if values.Length = 0 then
            LanguagePrimitives.GenericZero<'T>
        else
            values |> Array.average

    let inline averageOr (defaultValue: 'T) (values: 'T array) : 'T =
        if values.Length = 0 then
            defaultValue
        else
            values |> Array.average

    let inline tryAverage (values: 'T array) : 'T option =
        if values.Length = 0 then
            None
        else
            values |> Array.average |> Some

    let inline tryAverageBy (f: 'T -> 'U) (values: 'T array) : 'U option =
        if values.Length = 0 then
            None
        else
            values |> Array.averageBy f |> Some

let expensiveHousesAveragePriceOption: decimal option =
    House.getRandom 20
    |> Array.filter (fun house -> house.Price > 200_000m)
    |> Array.tryAverageBy _.Price

let firstHouseWithDistanceOption: (House * double) option =
    House.getRandom 20
    |> Array.filter (fun house -> house.Price > 200_000m)
    |> Array.tryPick (fun house ->
        match house |> Distance.tryToSchool with
        | Some(distance) -> Some(house, distance)
        | None -> None
    )

let novelWords: Set<string> = Set [ "The"; "the"; "quick"; "brown"; "Fox"; "fox" ]
let lowerWords: Set<string> = novelWords |> Set.map _.ToLowerInvariant()

let averageDistanceToSchool: double option =
    House.getRandom 20 |> Array.choose Distance.tryToSchool |> Array.tryAverage

let housesWithPriceBand: (House * PriceBand) array =
    House.getRandom 20
    |> Array.map (fun house -> house, house.Price |> PriceBand.fromPrice)

House.getRandom 20
|> Seq.groupBy (fun house -> house.Price |> PriceBand.fromPrice)
|> Seq.sortBy (fun (band, _) -> band |> PriceBand.order)
|> Seq.iter (fun (band, houses) ->
    printfn "---- %A ----" band
    houses |> Seq.iter (fun house -> printfn "%s - %f" house.Address house.Price)
)

House.getRandom 20
|> Seq.groupBy (fun house -> house.Price |> PriceBand.fromPrice)
|> Seq.map (fun (band, houses) -> {| PriceBand = band; Houses = houses |})
|> Seq.sortBy (fun group -> group.PriceBand |> PriceBand.order)
|> Seq.iter (fun group ->
    printfn "---- %A ----" group.PriceBand

    group.Houses
    |> Seq.iter (fun house -> printfn "%s - %f" house.Address house.Price)
)
