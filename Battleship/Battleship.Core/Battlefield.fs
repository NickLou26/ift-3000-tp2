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

    //Function to find a ship's direction based on the last coordinate
    let findDirection existingX x pos =
        //If the new x coord is the same as the existing one, direction is vertical (North or South)
        if existingX = x then
            if pos = 2 then
                North
            else
                South
        //If not vertical, then direction is horizontal (East or West)
        else
            if pos = 2 then
                West
            else
                East

     //Function to get a list of all Active sectors in a row
    let getAllSectorRow (sectorList: Sector List) (rowIndex: int) =
        //Recursive function to iterate through the row. Store all coords in a list
        let rec checkRow sectorList sectorIndex =
            match sectorList with
            //If reached the end of the list, return empty list
            | [] -> []
            //If there is a sector, match
            | sector::restSectors ->
                match sector with
                //If the sector is clear, check the rest of the row
                | Clear -> (checkRow restSectors (sectorIndex + 1))
                //If the sector is active, add the current coord to the list and check the rest of the row recursively
                | Active(name,pos) -> (name, pos, rowIndex, sectorIndex)::checkRow restSectors (sectorIndex + 1)
        //Call recursive function
        checkRow sectorList 0
    

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
            let newSector = Active(ship.Name, posBlock)
            match shipCoords with
            //If we reach the end of the ship's coord list, return the grid
            | [] -> grid'
            //Update the sectors, then go to the next coord recursively
            | coord::restCoord -> toggleSectorsOfShipCoords (setSector grid' coord newSector) restCoord (posBlock + 1)
        
        //Call the recursive fun
        toggleSectorsOfShipCoords grid ship.Coords 0

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
        //Calculate the dimensions of the grid
        let dimsGrid = Navigation.getDimsGrid grid

        //Organize the list of all active sectors : Sorts all the sectors of the same ships in order of the block#
        let rec orgranizeData list =
            match list with
            | [] -> []
            | (newShipName, _, _, _)::_ -> 
                let rest = List.filter (fun (name, _, _, _) -> not (name = newShipName)) list
                let newShipInfo = List.filter (fun (name, _, _, _) -> name = newShipName) list
                let orderedShipInfo = List.sortBy (fun (_, pos, _, _) -> pos) newShipInfo
                orderedShipInfo @ orgranizeData rest

        //Creates a list of ships with the organized data, there will be duplicates and all center and facing will be default
        let rec createShipListFromOrganizedData list =
            match list with
            | [] -> []
            | (shipName, pos, x, y)::rest ->
                {Name = shipName; Coords = [(x, y)]; Center = (0,0); Facing = North}::(createShipListFromOrganizedData rest)

        //Iterate through the list and combines the coords of the ships with the same name
        let rec removeDups list =
            match list with
            | [] -> []
            | [ship] -> [ship]
            | ship1::ship2::rest ->
                if ship1.Name = ship2.Name then
                    let combinedShip = {ship1 with Coords = ship1.Coords@ship2.Coords}
                    removeDups(combinedShip::rest)
                else
                    ship1::removeDups(ship2::rest)
        //Determine the center of each ship
        let calculateCenters list =    
            List.map (fun ship ->
                {
                    ship with Center = Navigation.getNewCenter ship.Coords
                }
            ) list

        let calculateDirection list =       
            List.map (fun ship ->
                let (x1, y1) = List.head ship.Coords
                let (x2, y2) = List.last ship.Coords
                let newDirection = 
                    if x1 = x2 then         //If horizontal
                        if y1 < y2 then
                            West
                        else                
                            East
                    else                    //If vertical
                        if x1 < x2 then
                            North
                        else
                            South
                {
                    ship with Facing = newDirection
                }
            ) list

        let listOfShips = 
            Grid.getAllSector grid getAllSectorRow  //Get a list of all active sectors as (name, pos, rowIndex, sectorIndex)
            |> orgranizeData                        //Organize data in order by block# and ship name
            |> createShipListFromOrganizedData      //Create a list of ships from the previously organized data
            |> removeDups                           //Combies coord of ships with same name, keeping the order
            |> calculateCenters                     //Set the center to each ship
            |> calculateDirection                   //Set the direction to each ship

        { Dims = dimsGrid; Ships = listOfShips }

    let loadData (data: Data) : Sector Grid =
        //Create a new grid full of clear sectors
        let newGrid = initClearGrid data.Dims
      
        //Update the sectors with the ships
        let rec loadShips listOfShips grid =
            match listOfShips with
            | [] -> grid
            | ship::rest -> 
                loadShips rest (addShip ship grid) 
        
        loadShips data.Ships newGrid
     

        