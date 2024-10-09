open System

type MilesYards = MilesYards of int * int

module MilesYards =
    let private (~~) = float

    let fromMilesPointYards (milesPointYards: float) : MilesYards =
        if milesPointYards < 0. then
            raise <| ArgumentOutOfRangeException((nameof milesPointYards), "Must be >= 0.0")

        let wholeMiles = ~~milesPointYards |> int
        let fraction = milesPointYards - ~~wholeMiles

        if fraction > 0.1759 then
            raise
            <| ArgumentOutOfRangeException((nameof milesPointYards), "Fractional part must be <= 0.1759")

        let yards = fraction * 10_000. |> round |> int
        MilesYards(wholeMiles, yards)

    let toDecimalMiles (MilesYards(wholeMiles, yard)) : float = ~~wholeMiles + (~~yard / 1760.)

type MilesChains = MilesChains of int * int

module MilesChains =
    let private (~~) = float

    let fromMilesChains (wholeMiles: int) (chains: int) : MilesChains =
        if wholeMiles < 0 then
            raise <| ArgumentOutOfRangeException((nameof wholeMiles), "Must be >= 0")

        if chains < 0 || chains >= 80 then
            raise <| ArgumentOutOfRangeException((nameof chains), "Must be >= 0 and < 80")

        MilesChains(wholeMiles, chains)

    let toDecimalMiles (MilesChains(wholeMiles, chains)) : float = ~~wholeMiles + (~~chains / 80.)
