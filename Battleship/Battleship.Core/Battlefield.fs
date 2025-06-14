namespace Battleship.Core

module Battlefield =
    open Grid
    open Ship
    open Navigation

    type Data = { Dims: Dims; Ships: Ship list }


    //Go to specific coord and do something
    let getSector (grid: Sector Grid) (coord: Coord) f =
        //Get x and y of coord
        let (x, y) = coord

        //Check if sector is Active, return name if active or none if clear
        let checkSectorStatus sector =
            //Call passed fun
            f sector

        //Go to the right sector using recursion and index
        let rec checkSectorList sectorList sectorIndex =
            match sectorList with
            //Return none if reached the end
            | [] -> None
            //If reached the index, checkSectorStatus else recursively advance through sectorList
            | sector::restSectorList -> if sectorIndex = y then (checkSectorStatus sector) else (checkSectorList restSectorList (sectorIndex + 1))

        //Go to the right row using recursion and index
        let rec checkGrid grid' rowIndex =
            match grid' with
            //Return none if reached the end
            | Empty -> None
            //If reached the index, checkSectorList else recursively advance through grid
            | Row (sectorList, restGrid) -> if rowIndex = x then (checkSectorList sectorList 0) else (checkGrid restGrid (rowIndex + 1))
        
        //Call the recursive fun
        checkGrid grid 0

    let setSector (grid: Sector Grid) (coord: Coord) (newSector: Sector): Sector Grid =
       //Get x and y of coord
        let (x, y) = coord
       
       //Goes to y coord with sectorIndex and updates the data with newSector on the passed row
        let updateSector (sectorIndex: int) (newSector: Sector) (row: Sector List): Sector List =
            //Go through list. If i = sector index then return newSector, else return oldSector. Creates new list with the returns
            List.mapi (fun i oldSector -> if i = sectorIndex then newSector else oldSector) row

        //Recursively go through the grid
        let rec updateRow rowIndex grid =
            match grid with
            | Empty -> Empty
            | Row (sectorList, restGrid) ->
                //If we have reached the x coord
                if rowIndex = x then
                    //Update row with new sector
                    let updatedRow = (updateSector y newSector sectorList)
                    //Return the new row with updated sector list and the rest of the grid
                    Row(updatedRow, restGrid)
                else
                    //If we haven't reached the x coord, return a new return with the current sector list, and recursively go to the next row
                    Row(sectorList, updateRow(rowIndex + 1) restGrid)

        //Call the recursive fun
        updateRow 0 grid

    //Iterate through the grid and apply f to each element
    let iterGrid (grid: Sector Grid) f: Sector Grid =
        //Apply f to each sector
        let updateSector sector x y =
            f sector x y

        //Iterate through the sector list
        let rec iterSectorList sectorList rowIndex sectorIndex: Sector List =
            match sectorList with
            | [] -> []
            | sector::restSectorList -> (updateSector sector rowIndex sectorIndex)::(iterSectorList restSectorList rowIndex (sectorIndex + 1))

        //Iterate through the grid
        let rec iterRows grid' rowIndex = 
            match grid' with
            | Empty -> Empty
            | Row (sectorList, restGrid) -> 
                let updatedRow = (iterSectorList sectorList rowIndex 0)
                Row(updatedRow, (iterRows restGrid (rowIndex + 1)))
        
        //Call the recursive function
        iterRows grid 0

    let removeShip (shipName: Name) (grid: Sector Grid) =
        //Function passed to iterGrid
        let extractDataFromGrid sector x y =
            match sector with
            //If clear, stay clear
            | Clear -> Clear
            //If active and name = shipName, clear
            | Active (name, _) when name = shipName -> Clear
            //If active and name != shipName, copy sector
            | Active (name, pos) -> Active (name, pos) 
        //Call recursive function
        iterGrid grid extractDataFromGrid

    //Initiate a grid based on the passed Dims. All the sectors are set to Clear
    let initClearGrid (dims: Dims) : Sector Grid =
        //Get the dimensions of the grid
        let (nbRows,nbColumns) = dims
        //Recursively generate Rows
        let rec initRows nbRows' =
            //If nbRows reaches 0, stop generating new rows
            if nbRows' <= 0 then
                Empty
            else
                //Initialize a new Row with a list of sector that are set to Clear, and the length of the list is nbColumns
                let newRow = List.init nbColumns (fun _ -> Clear)
                //Create the row, then recursive call
                Row(newRow, initRows(nbRows' - 1))
        //Call the recursive fun
        initRows nbRows

    //Add a ship to the grid
    let addShip (ship: Ship) (grid: Sector Grid) : Sector Grid =
        //Recursively go through all the coords of the ship and update the sector based on the coords
        //posBlock is the # of the block of the ship (front of the ship is 0 and last is size of the ship -1)
        let rec toggleSectorsOfShipCoords grid' shipCoords posBlock = 
            //Create new sector with Active type containing ship name and block #
            let newSector = Active(ship.Name, posBlock - 1)
            match shipCoords with
            //If we reach the end of the ship's coord list, return the grid
            | [] -> grid'
            //Update the sectors, then go to the next coord recursively
            | coord::restCoord -> toggleSectorsOfShipCoords (setSector grid' coord newSector) restCoord (posBlock - 1)
        
        //Determine size of the ship based on its name
        let shipSize = 
            match ship.Name with
            | Spy -> 2
            | PatrolBoat -> 2
            | Destroyer -> 3
            | Submarine -> 3
            | Cruiser -> 4
            | AircraftCarrier -> 5
        
        //Call the recursive fun
        toggleSectorsOfShipCoords grid ship.Coords shipSize

    let replaceShip (ship: Ship) (grid: Sector Grid) : Sector Grid =
        //New grid with removed ship
        let updatedGrid = (removeShip ship.Name grid)
        //Add ship to updated grid
        (addShip ship updatedGrid)

    let getSelectedName (coord: Coord) (grid: Sector Grid) : Name option =
        //Function to retrieve name of ship on a sector
        let retrieveName sector =
            match sector with
            //If sector is clear return none
            | Clear -> None
            //If sector is active return ship name
            | Active (name, _) -> Some name

        getSector grid coord retrieveName

    let extractData (grid: Sector Grid) : Data =
        (* ------- À COMPLÉTER ------- *)
        (* ----- Implémentation ------ *)
  
        //let extractDataFromGrid sector x y =
        //    match sector with
        //    | Clear -> Clear
        //    | Active (name, blockPos) -> 
        //        match name with
        //        | Spy ->
        //            let spyCoords: Coord List = (x, y)::spyCoords
        //            let spyShip = {
        //                Coords = spyCoords
        //                Center = (0,0)
        //                Facing = North
        //                Name = Spy
        //            }
        //            Clear
        //        | 
        //        let test = name::test
        //        Active(Spy,1)
                

        //iterGrid grid extractDataFromGrid
        { Dims = (0, 0); Ships = [] }

    let loadData (data: Data) : Sector Grid =
        (* ------- À COMPLÉTER ------- *)
        (* ----- Implémentation ------ *)
        Empty