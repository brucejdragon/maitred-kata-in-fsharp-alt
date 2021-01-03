module BitsForThought.MaitreDTests

open System
open Xunit
open Swensen.Unquote
open BitsForThought.MaitreD

let private days = TimeSpan.FromDays
let private hours = TimeSpan.FromHours
let private now = DateTime.Now

let private d1 = DateTime(2023, 9, 14)
let private d2 = DateTime(2024, 6, 7)
let private d3 = DateTime(2023, 10, 22)
let private d4 = d3 + hours 20.0 

let makeReservation (guestCount, date) = { GuestCount = guestCount; Date = date }

type BoutiqueTestCases () as this =
    inherit TheoryData<int, (int * DateTime) list, int * DateTime, bool> ()
    do
        this.Add (12, [], (1, now), true)
        this.Add (12, [], (13, now), false)
        this.Add (12, [], (12, now), true)
        this.Add (4, [(2, d1)], (3, d1), false)
        this.Add (10, [(2, d1)], (3, d1), true)
        this.Add (10, [(3, d1); (2, d1); (3, d1)], (3, d1), false)
        this.Add (4, [(2, d1 + days 1.0)], (3, d1), true)

[<Theory; ClassData(typeof<BoutiqueTestCases>)>]
let ``Boutique restaurant`` (capacity, reservationData, r, expected) =
    let restaurant = { Tables = [List.replicate capacity 1 |> Group]; Duration = days 1.0 }
    let reservations = reservationData |> List.map makeReservation

    let actual = reserve restaurant reservations (makeReservation r)

    expected =! actual

type HauteCuisineTestCases () as this =
    inherit TheoryData<int list, (int * DateTime) list, int * DateTime, bool> ()
    do
        this.Add ([2; 2; 4; 4], [], (4, d2), true)
        this.Add ([2; 2; 4; 4], [], (5, d2), false)
        this.Add ([2; 2; 4], [(2, d2)], (4, d2), true)
        this.Add ([2; 2; 4], [(3, d2)], (4, d2), false)

[<Theory; ClassData(typeof<HauteCuisineTestCases>)>]
let ``Haute cuisine`` (tables, reservationData, r, expected) =
    let restaurant = { Tables = tables |> List.map Separate; Duration = days 1.0 }
    let reservations = reservationData |> List.map makeReservation

    let actual = reserve restaurant reservations (makeReservation r)
    
    expected =! actual

type SecondSeatingTestCases () as this =
    inherit TheoryData<int list * TimeSpan, (int * DateTime) list, int * DateTime, bool> ()
    do
        this.Add (([2; 2; 4], hours 2.0), [(4, d3 + hours 18.0)], (3, d3 + hours 20.0), true)
        this.Add (([2; 4; 4], hours 2.5), [(2, d3 + hours 18.0); (1, d3 + hours 18.25); (2, d3 + hours 17.75)], (3, d3 + hours 20.0), false)
        this.Add (([2; 4; 4], hours 2.5), [(2, d3 + hours 18.0); (2, d3 + hours 17.75)], (3, d3 + hours 20.0), true)
        this.Add (([2; 4; 4], hours 2.5), [(2, d3 + hours 18.0); (1, d3 + hours 18.25); (2, d3 + hours 17.75)], (3, d3 + hours 20.25), true)

[<Theory; ClassData(typeof<SecondSeatingTestCases>)>]
let ``Restaurant with second seating`` ((tables, duration), reservationData, r, expected) =
    let restaurant = { Tables = tables |> List.map Separate; Duration = duration }
    let reservations = reservationData |> List.map makeReservation

    let actual = reserve restaurant reservations (makeReservation r)

    expected =! actual

type AlternateSeatingTestCases () as this =
    inherit TheoryData<Table list, (int * DateTime) list, int * DateTime, bool> ()
    do
        this.Add ([Separate 4; Separate 1; Separate 2; Group [2; 2; 2]], [], (6, d4), true)
        this.Add ([Separate 4; Separate 1; Separate 2; Group [2; 2; 2]], [(4, d4)], (4, d4), true)
        this.Add ([Separate 4; Separate 1; Separate 2; Group [2; 2; 2]], [], (8, d4), false)
        this.Add ([Separate 4; Separate 1; Separate 2; Group [2; 2; 2]], [(4, d4); (2, d4); (2, d4); (1, d4)], (4, d4), true)
        this.Add ([Separate 4; Separate 1; Separate 2; Group [2; 2; 2]], [(4, d4); (2, d4); (2, d4); (1, d4)], (5, d4), false)
        this.Add ([Separate 4; Separate 1; Separate 2; Group [2; 2; 2]], [(4, d4); (2, d4); (2, d4); (1, d4)], (5, d4 - hours 2.0), true)
        this.Add ([Separate 4; Separate 1; Separate 2; Group [2; 4]], [], (6, d4), true)
        this.Add ([Separate 4; Separate 1; Separate 2; Group [2; 4]], [(4, d4)], (4, d4), true)
        this.Add ([Separate 4; Separate 1; Separate 2; Group [2; 4]], [], (8, d4), false)
        this.Add ([Separate 4; Separate 1; Separate 2; Group [2; 4]], [(4, d4); (2, d4); (2, d4); (1, d4)], (4, d4), true)
        this.Add ([Separate 4; Separate 1; Separate 2; Group [2; 4]], [(4, d4); (2, d4); (2, d4); (1, d4)], (5, d4), false)
        this.Add ([Separate 4; Separate 1; Separate 2; Group [2; 4]], [(4, d4); (2, d4); (2, d4); (1, d4)], (5, d4 - hours 2.0), true)
        this.Add ([Separate 4; Group [2; 2; 2]; Group [1; 2]], [], (6, d4), true)
        this.Add ([Separate 4; Group [2; 2; 2]; Group [1; 2]], [(4, d4)], (4, d4), true)
        this.Add ([Separate 4; Group [2; 2; 2]; Group [1; 2]], [], (8, d4), false)
        this.Add ([Separate 4; Group [2; 2; 2]; Group [1; 2]], [(4, d4); (2, d4); (2, d4); (1, d4)], (4, d4), true)
        this.Add ([Separate 4; Group [2; 2; 2]; Group [1; 2]], [(4, d4); (2, d4); (2, d4); (1, d4)], (5, d4), false)
        this.Add ([Separate 4; Group [2; 2; 2]; Group [1; 2]], [(4, d4); (2, d4); (2, d4); (1, d4)], (5, d4 - hours 2.0), true)
        this.Add ([Separate 4; Group [2; 2; 2]; Group [1; 2]], [(4, d4); (4, d4)], (3, d4), true)
        this.Add ([Separate 4; Separate 1; Separate 2; Group [2; 2; 2]], [(3, d4); (1, d4); (2, d4)], (2, d4), true)
        this.Add ([Separate 4; Separate 1; Separate 2; Group [2; 2; 2]], [(3, d4); (1, d4); (2, d4)], (7, d4), false)
        this.Add ([Separate 4; Separate 1; Separate 2; Group [2; 2; 2]], [(3, d4); (1, d4); (2, d4); (1, d4)], (4, d4), true)
        this.Add ([Separate 4; Separate 1; Separate 2; Group [2; 2; 2]], [(3, d4); (1, d4); (2, d4); (1, d4); (4, d4)], (3, d4), false)

[<Theory; ClassData(typeof<AlternateSeatingTestCases>)>]
let ``Restaurant with alternate table arrangements`` (tables, reservationData, r, expected) =
    let restaurant = { Tables = tables; Duration = hours 2.0 }
    let reservations = reservationData |> List.map makeReservation

    let actual = reserve restaurant reservations (makeReservation r)

    expected =! actual
