﻿module Processor

open System.Collections.Concurrent
open System.Collections.Generic
open TTA.ASM
open Cell

exception ParallelException of int*int
type Processor<'a> (functions: array<'a -> 'a -> 'a>) =
    do
        if functions.Length = 0
        then raise(System.ArgumentNullException())
                 
    let grid = Array.init functions.Length (fun i -> ConcurrentDictionary<int, Cell<'a>>())
    let addCell row column = grid.[column].TryAdd(row, Cell(functions.[column]))

    let execute command = 
        match command with
        |Set ((row1, col1), arg) ->
            let row, col = int row1, int col1
            if not(grid.[col].ContainsKey row)
            then
                addCell row col
                grid.[col].[row].Value <- arg
            else
                grid.[col].[row].Value <- arg
        |Mov((row2, col2), (row1, col1)) ->
            let toRow, toCol, fromRow, fromCol = int row2, int col2, int row1, int col1
            if not(grid.[toCol].ContainsKey toRow)
            then
                addCell toRow toCol
                ()
            if not(grid.[fromCol].ContainsKey fromRow)
            then
                addCell fromRow fromCol
                ()
            grid.[toCol].[toRow].Execute grid.[fromCol].[fromRow].Value
        |Mvc((row1, col1), arg) ->
            let row, col = int row1, int col1
            if not(grid.[col].ContainsKey row)
            then
                addCell row col
                grid.[col].[row].Execute arg
            else
                grid.[col].[row].Execute arg
        |Eps -> ()

    member this.ValueAt row col =
        if not(grid.[col].ContainsKey row)
        then
            addCell row col
            grid.[col].[row].Value
        else grid.[col].[row].Value
        
    member this.Check(arr : Asm<'a>[]) =
        let CellsSet = new HashSet<(int*int)>()
        for operation in arr do
            match operation with
            |Set((row, col), arg)
            |Mvc((row, col), arg) ->
                if CellsSet.Add (int row, int col) = false
                then raise(ParallelException (int row, int col))
            |Mov((row2, col2), (row1, col1)) ->
                if CellsSet.Add (int row1, int col1) = false
                then raise(ParallelException (int row1, int col1))
                if CellsSet.Add (int row2, int col2) = false
                then raise(ParallelException (int row2, int col2))
    
    member this.countCells =
        let mutable count = 0
        for i in grid do
            for j in i do
            count <- count + 1
        count

    member this.executeArray(arr : Program<'a>) =
        for i in arr do
            this.Check i
            Array.Parallel.iter execute i