namespace Battleship.Core

module Grid =

    type Dims = int * int

    type Coord = int * int

    type 'a Grid = Empty | Row of 'a list * 'a Grid

    //Function to get a list of all sectors in a grid and apply a function to each sector
    let getAllSector grid f =
        //Recursive function to iterate through the grid. Store all coords in a list
        let rec checkGrid grid rowIndex =
            match grid with
            //If empty, return empty list
            | Empty -> []
            //If row, check individual sector with getAllActiveSectorRow (returns a list of coords) and concatenate with recursive call on checkGrid
            | Row (sectorList, restGrid) -> (f sectorList rowIndex)@(checkGrid restGrid (rowIndex + 1))
        //Call recursive function
        checkGrid grid 0