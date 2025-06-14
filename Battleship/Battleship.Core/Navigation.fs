namespace Battleship.Core

module Navigation =
    open Grid
    open Ship

    type Sector = Clear | Active of Name * int

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
    
    let getAllActiveSectorRow (sectorList: Sector List) (rowIndex: int): Coord List =
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
                 | Active(_,_) -> (rowIndex, sectorIndex)::(checkRow restSectors (sectorIndex + 1))
        //Call recursive function
        checkRow sectorList 0

    let getAllActiveSector (grid: Sector Grid) : Coord List =
        //Recursive function to iterate through the grid. Store all coords in a list
        let rec checkGrid grid rowIndex =
            match grid with
            //If empty, return empty list
            | Empty -> []
            //If row, check individual sector with getAllActiveSectorRow (returns a list of coords) and concatenate with recursive call on checkGrid
            | Row (sectorList, restGrid) -> (getAllActiveSectorRow sectorList rowIndex)@(checkGrid restGrid (rowIndex + 1))
        //Call recursive function
        checkGrid grid 0

    let canPlace (center: Coord) (direction: Direction) (name: Name) (grid: Sector Grid) : bool =
        //Get dimensions of the grid
        let (dimx, dimy) = getDimsGrid grid
        //Get a list of coords of the ship to be place
        let newShipCoords = (Ship.calculateCoordsByShip center name direction)
        //Get a list of coords of all active sectors
        let allActiveCoords = getAllActiveSector grid
        //Get a list of all perimeter coords
        let allPerimeter = Ship.getPerimeterCoords allActiveCoords (dimx, dimy)

        //Verifies if newShipCoords has coords that are out of bounds
        let outOfBounds = List.exists (fun (x, y) -> x < 0 || x > (dimx - 1) || y < 0 || y > (dimy - 1)) newShipCoords
        //Verifies if newShipCoords has coords that are on an active sector
        let onActiveSector = List.exists (fun elem -> List.contains elem allActiveCoords) newShipCoords
        //Verifies if newShipCoords has coords that are on a perimeter
        let onPerimeter = List.exists (fun elem -> List.contains elem allPerimeter) newShipCoords
        
        //All verifications
        not outOfBounds && not onActiveSector && not onPerimeter

    let canMove (ship: Ship) (direction: Direction) (grid: Sector Grid) : bool =
        (* ------- À COMPLÉTER ------- *)
        (* ----- Implémentation ------ *)
        false

    let move (ship: Ship) (direction: Direction) : Ship =
        (* ------- À COMPLÉTER ------- *)
        (* ----- Implémentation ------ *)
        { Coords = []; Center = (0, 0); Facing = North; Name = Spy }

    let canRotate (ship: Ship) (direction: Direction) (grid: Sector Grid) : bool =
        (* ------- À COMPLÉTER ------- *)
        (* ----- Implémentation ------ *)
        false

    let rotate (ship: Ship) (direction: Direction) : Ship =
        (* ------- À COMPLÉTER ------- *)
        (* ----- Implémentation ------ *)
        { Coords = []; Center = (0, 0); Facing = North; Name = Spy }

    let canMoveForward (ship: Ship) (grid: Sector Grid) : bool =
        (* ------- À COMPLÉTER ------- *)
        (* ----- Implémentation ------ *)
        false

    let moveForward (ship: Ship) : Ship =
        (* ------- À COMPLÉTER ------- *)
        (* ----- Implémentation ------ *)
        { Coords = []; Center = (0, 0); Facing = North; Name = Spy }

    let getNextDirection (current: Direction) (rotation: Rotation) : Direction =
        (* ------- À COMPLÉTER ------- *)
        (* ----- Implémentation ------ *)
        North

    let canRotateForward (ship: Ship) (rotation: Rotation) (grid: Sector Grid) : bool =
        (* ------- À COMPLÉTER ------- *)
        (* ----- Implémentation ------ *)
        false

    let rotateForward (ship: Ship) (rotation: Rotation) : Ship =
        (* ------- À COMPLÉTER ------- *)
        (* ----- Implémentation ------ *)
        { Coords = []; Center = (0, 0); Facing = North; Name = Spy }