#r "bin/Debug/ScalaTrainFSharp.exe"
#r "../packages/FSharp.Data.2.2.5/lib/net40/FSharp.Data.dll"

module OO =
  open Train.OO

  let train = new Train("", 0, seq { yield new Station(""); yield new Station("") })

  let time = Time.FromMinutes 90

module Functional =
  open Train.Functional

  let train = 
    let firstStop = Stop.create (Time.fromHours 1u) (Station.create "One")
    let secondStop = Stop.create (Time.fromHours 2u) (Station.create "Two")
    Schedule.create firstStop secondStop List.empty
    |> Train.create (Train.InterCityExpress (1u, false))

  let planner = JourneyPlanner.create <| Set.singleton train

  let time = Time.fromMinutes 90u

  let b1 = JourneyPlanner.isShortTrip (Station.create "One") (Station.create "Two") planner
  let b2 = JourneyPlanner.isShortTrip (Station.create "One") (Station.create "Three") planner

  let t = Time.fromJson """{"hours": 1, "minutes": 30}"""
  let t2 = Time.fromJson """{"hours": 1, "minutes": "bla"}"""
