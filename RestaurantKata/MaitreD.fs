module BitsForThought.MaitreD

open System

type Reservation = { GuestCount: int; Date: DateTime }

type Table = Separate of int | Group of int list

type SeatingDetails = { Tables: Table list; Duration: TimeSpan }

let reserve seatingDetails reservations r =
    let overlappingReservations =
        let timesOverlap candidateTime existingReservation =
            existingReservation.Date + seatingDetails.Duration > candidateTime &&
            existingReservation.Date < candidateTime + seatingDetails.Duration

        reservations |> List.filter (timesOverlap r.Date)

    let tryMatchTable r (ts, matched) t =
        if matched then (t::ts, true) // We already found one; don't use this table.
        else
            match t with
            | Separate capacity ->
                if r.GuestCount <= capacity then (ts, true)
                else (t::ts, false)
            | Group tables ->
                match List.sumUntil r.GuestCount tables with
                | ([], 0) -> (ts, true)
                | (remainingTables, 0) -> ((Group remainingTables)::ts, true)
                | (_, _) -> (t::ts, false)

    let reserveTable (tables, _) r =
        let (remainingTables, matched) = tables |> List.fold (tryMatchTable r) ([], false)
        (List.rev remainingTables, matched)

    let canMatchAllTables tables (reservations : Reservation list) =
        let (_, matched) = reservations |> List.fold reserveTable (tables, false)
        matched

    let tablesBySizeAsc =
        let compareTables t1 t2 =
            match (t1, t2) with
            | (Separate x, Separate y) -> x - y
            | (Separate _, Group _) -> 1
            | (Group _, Separate _) -> -1
            | (Group l1, Group l2) -> List.min l1 - List.min l2

        let sortTable = function
            | Separate x -> Separate x
            // Match bigger tables first as they are more constrained (can't be split).
            | Group l -> List.sortDescending l |> Group

        seatingDetails.Tables |> List.map sortTable |> List.sortWith compareTables
    
    // Put existing reservations first, since they should have already been allocated.
    // We can't say anything definitive about the order of the existing reservations, since
    // they weren't necessarily placed in the order of their requested seating times.
    overlappingReservations @ [r] |> canMatchAllTables tablesBySizeAsc