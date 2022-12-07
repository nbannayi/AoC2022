namespace AoC2022.Utilities

module Stack =

    type Stack<'T> = StackContents of 'T list

    let empty = StackContents []
   
    /// Push a value on the stack.
    let push item (StackContents contents) =
        StackContents (item::contents)

    /// Pop a value from the stack and return it and the new stack as a tuple.
    let pop (StackContents contents) = 
        match contents with 
        | top::rest -> 
            let newStack = StackContents rest
            (top,newStack)
        | [] -> (None, StackContents [])
        
    /// Return the top value.
    let peek (StackContents contents) =
        match contents with 
        | top::_ -> top
        | [] -> None

    /// Return the value before the top value.
    let peekPrevious (StackContents contents) =
        match contents with 
        | _::rest ->
            match rest with
            | prev::_ -> prev
            | [] -> None
        | [] -> None

    /// Check if the stack is empty.
    let isEmpty (StackContents contents) =
        contents.Length = 0

    /// Get size of stack.
    let size (StackContents contents) =
        contents.Length

    /// Convert stack to a sequence.
    let toSeq (StackContents contents) =
        contents |> Seq.ofList

    /// Convert stack to an array.
    let toArray (StackContents contents) =
        contents |> Array.ofList

    /// Reverse order of elements in stack.
    let reverse (StackContents contents) =
        StackContents (contents |> List.rev)