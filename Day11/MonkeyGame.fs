namespace AoC2022.Day11

/// Represents a monkey game (piggy in the middle.)
type MonkeyGame =
   {
       Monkeys: Monkey array
   }

/// Monkey game module for day 11.
module MonkeyGame =

   /// Take a single turn.
   let takeTurn (limitWorry: bool) t (game: MonkeyGame) =       
       let monkey = game.Monkeys.[t]
       let limiter =
           game.Monkeys
           |> Array.map (fun m -> m.Test.Divisibility)
           |> Array.reduce (*)
       let rec processItems limiter monkey =
           match monkey.StartingItems with
           | [] -> ()
           | _ ->
               let monkey', item', throwTo =
                   monkey
                   |> Monkey.inspectNextItem limitWorry limiter
               match item', throwTo with
               | Some item, Some throwTo ->
                   game.Monkeys.[t]       <- monkey'
                   game.Monkeys.[throwTo] <- game.Monkeys.[throwTo] |> Monkey.receiveItem item
               | _ -> ()
               monkey' |> processItems limiter
       monkey |> processItems limiter      
       game

   /// Plays a single round.
   let playRound (limitWorry: bool) (game: MonkeyGame) =       
       (game, [0..game.Monkeys.Length-1]) ||> List.fold (fun g t -> g |> takeTurn limitWorry t)
      
   /// Plays a given number of rounds.
   let playRounds (limitWorry: bool) n (game: MonkeyGame) =
       (game, [1..n]) ||> List.fold (fun g _ -> g |> playRound limitWorry)
      
   /// Get total monkey business.
   let getMonkeyBusiness (game: MonkeyGame) =
       game.Monkeys
       |> Array.sortByDescending (fun m -> m.NoInspections)
       |> Array.take 2
       |> Array.map (fun m -> m.NoInspections |> int64)
       |> Array.reduce (*)
