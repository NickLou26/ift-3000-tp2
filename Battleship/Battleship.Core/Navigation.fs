namespace Battleship.Core

module Navigation =
    open Grid
    open Ship

    type Sector = Clear | Active of Name * int * bool | Torpedo

    type Rotation =
        | Clockwise
        | Counterclockwise

    let getDegrees (direction: Direction) : int =
        match direction with
        | South -> 0
        | West -> 90
        | North -> 180
        | East -> 270

    //Function to get dimensions of the grid
    let getDimsGrid (grid: Sector Grid) : Dims =

        //Recursively go to the last row while incrementing rowIndex
        let rec countRows grid' rowIndex = 
            match grid' with
            | Empty -> rowIndex
            | Row (sectorList, restGrid) -> (countRows restGrid (rowIndex + 1))
        let nbRows = countRows grid 0

        //Get the length of the first sector list
        let countColumns grid' =
            match grid' with
            | Empty -> 0
            | Row (sectorList, _) -> List.length sectorList
        let nbColumns = countColumns grid
        (nbRows, nbColumns)
    
    //Function to get a list of all Active sectors in a row
    let getAllActiveSectors (sectorList: Sector List) (rowIndex: int): Coord List =
        //Recursive function to iterate through the row. Store all coords in a list
        let rec checkRow sectorList sectorIndex =
            match sectorList with
            //If reached the end of the list, return empty list
            | [] -> []
            //If there is a sector, match
            | sector::restSectors ->
                match sector with
                //If the sector is clear, check the rest of the row
                | Clear | Torpedo -> (checkRow restSectors (sectorIndex + 1))
                //If the sector is active, add the current coord to the list and check the rest of the row recursively
                | Active(_,_,_) -> (rowIndex, sectorIndex)::(checkRow restSectors (sectorIndex + 1))
        //Call recursive function
        checkRow sectorList 0

    //Function to verify if the new coords can move
    let canPlaceCoords (coords: Coord List) (grid: Sector Grid) (coordsToExclude: Coord List) (checkPerimeter: bool) (isDroneCheck: bool): bool =
        //Get dimensions of the grid
        let (dimx, dimy) = getDimsGrid grid

        //Get a list of coords of all active sectors
        let allActiveCoords = Grid.getAllSector grid getAllActiveSectors

        //Remove coords from coordsToExclude list from the allActiveCoords list. Useful for canMove (exclude old ship coords)
        let allActiveCoords = List.filter (fun coord -> not (List.contains coord coordsToExclude)) allActiveCoords

        //Verifies if newShipCoords has coords that are out of bounds
        let outOfBounds = List.exists (fun (x, y) -> x < 0 || x > (dimx - 1) || y < 0 || y > (dimy - 1)) coords
        //Verifies if newShipCoords has coords that are on an active sector
        let onActiveSector = 
            if isDroneCheck then
                false
            else
                List.exists (fun elem -> List.contains elem allActiveCoords) coords

        //Verifies if newShipCoords has coords that are on a perimeter
        let onPerimeter = 
            if checkPerimeter then
                List.exists (fun elem -> List.contains elem (Ship.getPerimeterCoords allActiveCoords (dimx, dimy))) coords
            else
                false
        
        //All verifications
        not outOfBounds && not onActiveSector && not onPerimeter

    //Get a list of the moved coord based on direction
    let getMoveCoords (coords: Coord List) (direction: Direction): Coord List =
        //Mapped list of ship coords. Translate coord according to direction.
        let movedShipCoords = (List.map (fun (x, y) -> 
            match direction with
            | North -> (x - 1, y)
            | South -> (x + 1, y)
            | East -> (x, y + 1)
            | West -> (x, y - 1)
        ) coords)

        movedShipCoords

    //Find center by size
    let getCenterBlockPos (size: int) = 
        if size % 2 = 0 then
            (size / 2) - 1
        else
            size / 2

    //Get a new center of a moved ship
    let getNewCenter (movedShipCoords: Coord List) : Coord =
        
        //Get the pos of the center block of the ship
        let centerBlockPos = getCenterBlockPos (List.length movedShipCoords)

        //Get the center block coords based on the moved ship coords and the center block position
        let rec getCenterBlockCoords shipCoords' pos = 
            match shipCoords' with
            //If we reach the end of the list, return (0,0) (shouldn't happen)
            | [] -> (0, 0)
            //If we reach the center block, return the coord
            | coord::_ when pos = centerBlockPos -> coord
            //If we haven't reached the center block, continue to the next coord
            | coord::restCoord -> getCenterBlockCoords restCoord (pos + 1)
        
        //Get the center block coords based on the moved ship coords and the center block position
        getCenterBlockCoords movedShipCoords 0

    let getRotateCoords (coords: Coord List) (oldDirection: Direction) (newDirection: Direction): Coord List = 
        let centerBlockPos = getCenterBlockPos (List.length coords)

        let rotatedCoords = (List.mapi (fun currentPos (x, y) -> 
            let distanceFromCenter = centerBlockPos - currentPos
            match oldDirection, newDirection with
            | North, East ->    (x + distanceFromCenter, y + distanceFromCenter)    //From north to east
            | North, West ->    (x + distanceFromCenter, y - distanceFromCenter)    //From north to west
            | North, South ->   (x + (distanceFromCenter * 2), y)                   //From north to south

            | South, East ->    (x - distanceFromCenter, y + distanceFromCenter)    //From south to east
            | South, West ->    (x - distanceFromCenter, y - distanceFromCenter)    //From south to west
            | South, North ->   (x - (distanceFromCenter * 2), y)                   //From south to north

            | East, North ->    (x - distanceFromCenter, y - distanceFromCenter)    //From east to north      
            | East, South ->    (x + distanceFromCenter, y - distanceFromCenter)    //From east to south
            | East, West ->     (x, y  - (distanceFromCenter * 2))                  //From east to west

            | West, North ->    (x - distanceFromCenter, y + distanceFromCenter)    //From west to north
            | West, South ->    (x + distanceFromCenter, y + distanceFromCenter)    //From west to south
            | West, East ->     (x, y  + (distanceFromCenter * 2))                  //From west to east
            | _, _ -> (0,0)

        ) coords)

        rotatedCoords

    let canPlace (center: Coord) (direction: Direction) (name: Name) (grid: Sector Grid) : bool =
        //Get a list of coords of the ship to be place
        let newShipCoords = (Ship.calculateCoordsByShip center name direction)

        //Verifies if coords can be placed with coords
        canPlaceCoords newShipCoords grid [] true false
       

    let canMove (ship: Ship) (direction: Direction) (grid: Sector Grid) : bool =
        //Coords of moved ship
        let movedShipCoords = getMoveCoords ship.Coords direction

        //Verifies if coords can be placed with new coords. Send current ship coords to exclude from checking
        canPlaceCoords movedShipCoords grid ship.Coords true false

    let move (ship: Ship) (direction: Direction) : Ship =
        //Coords of moved ship
        let movedShipCoords = getMoveCoords ship.Coords direction

        //Get the center block coords based on the moved ship coords and the center block position
        let centerBlockCoords = getNewCenter movedShipCoords

        //Ship with new coords and center
        let movedShip = {
            ship with
                Coords = movedShipCoords
                Center = centerBlockCoords
        }

        //Return the moved ship
        movedShip

    let canRotate (ship: Ship) (direction: Direction) (grid: Sector Grid) : bool =
        //Coords of moved ship
        let rotatedShipCoords = getRotateCoords ship.Coords ship.Facing direction

        //Verifies if coords can be placed with new coords. Send current ship coords to exclude from checking
        canPlaceCoords rotatedShipCoords grid ship.Coords true false

    let rotate (ship: Ship) (direction: Direction) : Ship =
        //Coords of moved ship
        let rotatedShipCoords = getRotateCoords ship.Coords ship.Facing direction
        
        //Ship with new coords and direction
        let rotatedShip = {
            ship with
                Coords = rotatedShipCoords
                Facing = direction
        }

        rotatedShip

    let canMoveForward (ship: Ship) (grid: Sector Grid) : bool =
        //Coords of moved ship
        let movedShipCoords = getMoveCoords ship.Coords ship.Facing

        //Verifies if coords can be placed with new coords. Send current ship coords to exclude from checking. Send false to remove perimeters from checks
        canPlaceCoords movedShipCoords grid ship.Coords false false

    let moveForward (ship: Ship) : Ship =
        move ship ship.Facing

    let getNextDirection (current: Direction) (rotation: Rotation) : Direction =
        match current, rotation with
        | North, Clockwise -> East
        | North, Counterclockwise -> West
        | East, Clockwise -> South
        | East, Counterclockwise -> North
        | South, Clockwise -> West
        | South, Counterclockwise -> East
        | West, Clockwise -> North
        | West, Counterclockwise -> South

    let canRotateForward (ship: Ship) (rotation: Rotation) (grid: Sector Grid) : bool =
        //Coords of rotated ship
        let rotatedCoords = getRotateCoords ship.Coords ship.Facing (getNextDirection ship.Facing rotation)
        //Coords of rotated and moved ship
        let movedAndRotatedCoords = getMoveCoords rotatedCoords (getNextDirection ship.Facing rotation)

        //Verifies if coords can be placed with new coords. Send current ship coords to exclude from checking. Send false to remove perimeters from checks
        canPlaceCoords movedAndRotatedCoords grid ship.Coords false false

    let rotateForward (ship: Ship) (rotation: Rotation) : Ship =
        //Ship with rotated coords
        let rotatedShip = rotate ship (getNextDirection ship.Facing rotation)
        //Ship with rotated and moved ship
        move rotatedShip (getNextDirection ship.Facing rotation)

    let canMoveDrone (drone: Coord) (direction: Direction) (grid: Sector Grid) : bool =
        //Coords of moved drone
        let movedDroneCoords = getMoveCoords [drone] direction

        //Verifies if coords can be placed with new coords. Send current ship coords to exclude from checking
        canPlaceCoords movedDroneCoords grid [] false true

    let moveDrone (drone: Coord) (direction: Direction) : Coord =
        List.head (getMoveCoords [drone] direction)