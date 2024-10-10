open System

type Shape<'T> =
    | Square of 'T
    | Rectangle of 'T * 'T
    | Circle of 'T

let describe (shape: Shape<float>) : string =
    match shape with
    | Square(h) -> sprintf "Square of height %f" h
    | Rectangle(h, w) -> sprintf "Rectangle %f x %f" h w
    | Circle(r) -> sprintf "Circle of radius %f" r

let displayName (name: string option) : string =
    match name with
    | Some(name) -> name
    | None -> ""

type Delivery =
    | AsBilling
    | Physical of string
    | Download
    | ClickAndCollect of int

type BillingDetails = {
    Name: string
    Billing: string
    Delivery: Delivery
}

let tryDeliveryLabel (details: BillingDetails) : string option =
    match details.Delivery with
    | AsBilling -> details.Billing |> Some
    | Physical(address) -> address |> Some
    | Download -> None
    | ClickAndCollect(_) -> None
    |> Option.map (fun address -> sprintf "%s\n%s" details.Name address)

let deliveryLabels (orders: BillingDetails seq) : string seq = orders |> Seq.choose tryDeliveryLabel

let collectionsFor (storeId: int) (orders: BillingDetails seq) : BillingDetails seq =
    orders
    |> Seq.choose (fun detail ->
        match detail.Delivery with
        | ClickAndCollect(id) when id = storeId -> detail |> Some
        | _ -> None
    )

let countNonNullBillingAddresses (orders: BillingDetails seq) : int =
    orders |> Seq.map _.Billing |> Seq.map Option.ofObj |> Seq.sumBy Option.count

let printParam (param: string) =
    param
    |> Option.ofObj
    |> Option.map _.ToUpper()
    |> Option.defaultValue "(none)"
    |> printfn "%s"

let showHeartRate (rate: Nullable<int>) : string =
    rate
    |> Option.ofNullable
    |> Option.map _.ToString()
    |> Option.defaultValue "N/A"

let random = new Random()

let tryLocationDescription (locationId: int) (description: string byref) : bool =
    let r = random.Next(1, 100)

    if r < 50 then
        description <- sprintf "Location number: %i" r
        true
    else
        description <- null
        false

let getHeartRateInternal () : int option =
    let rate = random.Next(0, 200)
    if rate = 0 then None else rate |> Some

let tryGetHeartRate () : Nullable<int> =
    getHeartRateInternal () |> Option.toNullable

let valueOptionString (v: int voption) : string =
    match v with
    | ValueSome(v) -> sprintf "Value: %i" v
    | ValueNone -> sprintf "No value"
