namespace AoC2022.Day20

open AoC2022.Utilities

type EncryptionFile =
   {
       InitialNumbers: (int64*int) List
       CurrentNumbers: ResizeArray<int64*int>
       Modulo: int
   }

/// Module for AoC day 20 encryption file.
module EncryptionFile =

   /// Create a new encryption file with the given numbers and decryption key.
   let create decryptionKey numbers =
       let currentNumbers = ResizeArray<int64*int>()
       numbers
       |> Seq.mapi (fun i l -> int64(l) * decryptionKey, i)
       |> Seq.iter (fun item -> currentNumbers.Add(item))       
       let initialNumbers = currentNumbers |> Seq.map (fun item -> item) |> List.ofSeq
       {InitialNumbers = initialNumbers; CurrentNumbers = currentNumbers; Modulo = numbers |> Seq.length}

   /// Get list of final grove coordinates after mixing.
   let getGroveCoordinates encryptionFile =          
       let numbersList =
           encryptionFile.CurrentNumbers |> Seq.map(fun (n,_) -> n)
           |> List.ofSeq       
       let zeroPosition = numbersList |> List.findIndex(fun x -> x = 0L)            
       [numbersList.[(zeroPosition + 1000) % encryptionFile.Modulo];
        numbersList.[(zeroPosition + 2000) % encryptionFile.Modulo];
        numbersList.[(zeroPosition + 3000) % encryptionFile.Modulo]]

   /// Mix an encryption file for a given number of rounds.
   let mix noRounds (encryptionFile: EncryptionFile) =                
       let swap (numbers: ResizeArray<int64*int>) index item =   
           let number, _ = item               
           let newIndex =
               let index' = int ((int64 index + number) % int64(encryptionFile.Modulo - 1))
               if index' < 0 then encryptionFile.Modulo + index' - 1 else index'       
           numbers.RemoveAt(index)
           numbers.Insert(newIndex, item)
           numbers
       for _ in 1..noRounds do
           (encryptionFile.CurrentNumbers, encryptionFile.InitialNumbers)
           ||> Seq.fold (fun cn inum ->
               let i = cn |> Seq.findIndex(fun n -> n = inum)
               swap cn i inum) |> ignore  
       encryptionFile
