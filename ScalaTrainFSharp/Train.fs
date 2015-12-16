namespace Train

module OO =

  [<Sealed>]
  type Station(name: string) =
    member this.Name = name
    
  [<Sealed>]
  type Train(kind: string, number: int, schedule: Station seq) =
    do if Seq.length schedule < 2 then invalidArg "schedule" "must contain at least two stations"

    member this.Kind = kind
    member this.Number = number
    member this.Schedule = schedule
    
  [<Sealed>]
  type Time(?hours: int, ?minutes: int) =
    member this.Hours = defaultArg hours 0
    member this.Minutes = defaultArg minutes 0
    member this.AsMinutes = this.Hours * 60 + this.Minutes
    member this.Minus(that: Time) = this.AsMinutes - that.AsMinutes
    static member FromMinutes(minutes) = 
      let hours = minutes / 60
      let normalizedMinutes = minutes % 60
      new Time(hours, normalizedMinutes)
    
  let (-) (t1: Time) (t2: Time) = t1.Minus t2

module Functional =

  module Time =
    open FSharp.Data
    open FSharp.Data.JsonExtensions

    type T = {
      Hours: uint32
      Minutes: uint32
    }

    let asMinutes t = t.Hours * 60u + t.Minutes

    let defaultTime = { Hours = 0u; Minutes = 0u }

    let fromMinutes minutes = 
      let hours = minutes / 60u
      let normalizedMinutes = minutes % 60u
      { Hours = hours; Minutes = normalizedMinutes }

    let fromHours hours = { defaultTime with Hours = hours }
  
    let (-) t1 t2 = fromMinutes <| asMinutes t1 - asMinutes t2
    let (+) t1 t2 = fromMinutes <| asMinutes t1 + asMinutes t2

    let fromHoursAndMinutes hours minutes = fromHours hours + fromMinutes minutes

    let fromJson json = 
      let parsed = JsonValue.Parse(json)
      let hours = defaultArg (parsed.TryGetProperty "hours" |> Option.map (fun v -> try Some (v.AsInteger()) with | _ -> None)) None
      let minutes = defaultArg (parsed.TryGetProperty "minutes" |> Option.map (fun v -> try Some (v.AsInteger()) with | _ -> None)) None
      Option.map (fun h -> { Hours = uint32 h; Minutes = uint32 (defaultArg minutes 0) }) hours

  module Station =
    type T = {
      Name: string
    }

    let create name = { Name = name }

  module Stop =
    type T = {
     Time: Time.T
     Station: Station.T
    }
    
    let create time station = { Time = time; Station = station }
    let getTime stop = stop.Time
    let getStation stop = stop.Station

  module Schedule =

    type T = {
      First: Stop.T
      Second: Stop.T
      Rest: Stop.T list
    }

    let create first second rest = { First = first; Second = second; Rest = rest }

    let asList s = s.First :: s.Second :: s.Rest

    let validate schedule =
      let times = asList schedule |> List.map Stop.getTime
      Seq.tail times 
      |> Seq.zip (Seq.skip 1 times)
      |> Seq.forall (fun t -> fst t < snd t)

  module Train =
    type TrainKind =
    | InterCityExpress of number: uint32 * hasWifi: bool
    | RegionalExpress of number: uint32
    | BavarianRegional of number: uint32

    type T = {
      Kind: TrainKind
      Schedule: Schedule.T
    }

    let create kind schedule = { Kind = kind; Schedule = schedule }
    let getSchedule t = t.Schedule

    let getStations = 
      getSchedule 
      >> Schedule.asList 
      >> Seq.map Stop.getStation

    let getTimeAt station = 
      getSchedule 
      >> Schedule.asList 
      >> Seq.tryFind (Stop.getStation >> (=) station) 
      >> Option.map Stop.getTime

  module JourneyPlanner =
    type T = {
      Trains: Train.T Set
    }

    let getTrains p = p.Trains

    let create trains = { Trains = trains }

    let getStations = 
      getTrains
      >> Set.map (Train.getStations >> Set.ofSeq) 
      >> Set.unionMany

    let getTrainsAt station = 
      getTrains
      >> Set.filter (Train.getStations >> Seq.contains station)

    let getStopsAt station planner = 
      query {
        for t in getTrainsAt station planner do
        for s in Schedule.asList t.Schedule do
        where (s.Station = station)
        select (s.Time, t)
      } |> Set.ofSeq

    let isShortTrip from toS = 
      let getStationsStartingWithFrom = 
        Train.getStations 
        >> Seq.skipWhile (not << (=) from) 
        >> List.ofSeq

      getTrainsAt from 
      >> Seq.map getStationsStartingWithFrom
      >> Seq.exists (function 
                     | f :: _ :: t :: xs 
                     | f :: t :: xs when f = from && t = toS -> true
                     | _ -> false)