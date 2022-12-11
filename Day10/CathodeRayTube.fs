namespace AoC2022.Day10 

open AoC2022.Utilities 

/// Represents a Cathode Ray Tube CPU instruction. 
type Instruction = 
   | Addx of int 
   | Noop 
   static member parse (instruction: string) =        
       let opCode = instruction.[0..3] 
       match opCode with 
       | "addx" ->  
           let operand = (instruction.Split ' ').[1] |> int 
           Addx(operand) 
       | "noop" -> Noop 
       | _ -> failwith "Unsupported instruction." 

/// Represents a Cathode Ray Tube CPU. 
type CPU = 
   { 
       X:                  int 
       CycleCount:         int 
       InstructionPointer: int 
   } 
   static member create () =  
       { 
           X = 1 
           CycleCount = 0 
           InstructionPointer = 0 
       }         

/// Represents Cathode Ray Tube's screen. 
type Screen = 
   { 
       Grid: char array             
   } 
   static member create () = 
       { 
           Grid = Array.init 240 (fun _ -> '.') 
       } 

module Screen = 

   /// Display Cathode Ray Tube's screen. 
   let display (screen: Screen) = 
       screen.Grid 
       |> Array.chunkBySize 40 
       |> Array.map (fun a -> a |> Array.map (string) |> String.concat "") 
       |> Array.iter (fun row -> printfn "%s" row) 

   /// Activate given pixel on the screen. 
   let activatePixel n (screen: Screen) = 
       screen.Grid.[n] <- '#' 
       screen 

/// Represents a Cathode Ray Tube. 
type CathodeRayTube = 
   { 
       CPU:    CPU 
       Memory: Instruction array 
       Screen: Screen 
   } 
   static member create program = 
       { 
           CPU    = CPU.create () 
           Memory =  
               program  
               |> Seq.ofFileLines 
               |> Seq.map (fun inst -> inst |> Instruction.parse) 
               |> Array.ofSeq             
           Screen =  
               Screen.create () 
       } 

/// Advent of Code 2022 day 10 Cathode Ray Tube module. 
module CathodeRayTube = 

   /// Reset the Cathode Ray tube anc lear the screen. 
   let reset (crt: CathodeRayTube) = 
       {crt with CPU = CPU.create (); Screen = Screen.create ()} 

   /// Step through and execute next instruction in program. 
   let step (crt: CathodeRayTube) = 
       let instructionPointer = crt.CPU.InstructionPointer 
       let instruction        = crt.Memory.[instructionPointer]                         
       let updateScreen x cycleCount = 
           match x-1, x, x+1 with 
           | p1, _, _ when p1 = cycleCount % 40 -> crt.Screen |> Screen.activatePixel ((cycleCount / 40) * 40 + p1) |> ignore 
           | _, p2, _ when p2 = cycleCount % 40 -> crt.Screen |> Screen.activatePixel ((cycleCount / 40) * 40 + p2) |> ignore                     
           | _, _, p3 when p3 = cycleCount % 40 -> crt.Screen |> Screen.activatePixel ((cycleCount / 40) * 40 + p3) |> ignore 
           | _ -> ()             
       let x, cycleCount = 
           match instruction with 
           | Addx(v) ->  
               updateScreen (crt.CPU.X) (crt.CPU.CycleCount+1) 
               let x', cycleCount' = crt.CPU.X+v, crt.CPU.CycleCount+2 
               updateScreen x' cycleCount' 
               x', cycleCount' 
           | Noop    ->  
               let x', cycleCount' = crt.CPU.X, crt.CPU.CycleCount+1 
               updateScreen x' cycleCount' 
               x', cycleCount' 
       {crt with CPU = {X = x; CycleCount = cycleCount; InstructionPointer = instructionPointer+1}} 

   /// Run the computer to the required cycle. 
   let rec runToCycle cycle (crt: CathodeRayTube) = 
       let crt' = crt |> step 
       let cycleCount = crt'.CPU.CycleCount 
       if cycleCount = cycle-1 || cycleCount = cycle-2 then                             
           crt' 
       else  
           crt' |> runToCycle cycle 

   /// Run the entire program. 
   let run (crt: CathodeRayTube) = 
       (crt, [1..crt.Memory.Length]) 
       ||> List.fold (fun crt _ -> crt |> step) 

   /// Get value of X register in CPU. 
   let getX (crt: CathodeRayTube) = 
       crt.CPU.X 

   /// Display screen of cathode ray tube. 
   let displayScreen (crt: CathodeRayTube) = 
       crt.Screen  
       |> Screen.activatePixel 0 // Will always be set. 
       |> Screen.display