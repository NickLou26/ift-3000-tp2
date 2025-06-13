namespace Battleship.Core

module Ship =
    open Grid

    type Name =
        | Spy
        | PatrolBoat
        | Destroyer
        | Submarine
        | Cruiser
        | AircraftCarrier

    type Direction =
        | North
        | South
        | East
        | West

    type Ship = {Coords: Coord list; Center: Coord; Facing: Direction; Name: Name}

    //Calculate coords from the end to the front
    let calculateCoords (facing: Direction) (sizeLeft: int) (x: int, y: int): Coord =
        match facing with
        | North -> (x + sizeLeft, y)
        | South -> (x - sizeLeft, y)
        | East -> (x, y - sizeLeft)
        | West -> (x, y + sizeLeft)

    //Function to generate the list of coords of a new ship
    let generateCoordsList (size : int) (center: Coord) (facing: Direction): Coord List =
        let (center_x, center_y) = center //Get the x and y of the center
        let (x, y) = //Determine the coords of the front of the ship
            match facing with
            | North -> (center_x - (size / 2), center_y)
            | South -> (center_x + (size / 2), center_y)
            | East -> (center_x, center_y + (size / 2))
            | West -> (center_x, center_y - (size / 2))

        let rec generateShipCoords (sizeLeft: int): Coord List = //Recursive generate the list of coords, starting from the last position
            match sizeLeft with
            | 0 -> []
            | _ -> if size % 2 = 0 then (calculateCoords facing sizeLeft (x,y))::generateShipCoords(sizeLeft - 1) 
                                   else (calculateCoords facing (sizeLeft - 1) (x,y))::generateShipCoords(sizeLeft - 1) //Adjust center for odd sizes
        in generateShipCoords size

    //Calculate coords for each different ship
    let calculateCoordsByShip (center: Coord) (name: Name) (facing: Direction): Coord List =  
        match name with
        | Spy -> (generateCoordsList 2 center facing)
        | PatrolBoat -> (generateCoordsList 2 center facing)
        | Destroyer -> (generateCoordsList 3 center facing)
        | Submarine -> (generateCoordsList 3 center facing)
        | Cruiser -> (generateCoordsList 4 center facing)
        | AircraftCarrier -> (generateCoordsList 5 center facing)

    //Generate a list of coords around the passed coord
    let surroundCoordWithPerimeter (coord: Coord) : Coord List =
        let (x, y) = coord
        [(x - 1, y);(x + 1, y);(x, y - 1);(x, y + 1);(x - 1, y - 1);(x - 1, y + 1);(x + 1, y - 1);(x + 1, y + 1)] //Adds every position around the coord

    //Create a ship
    let createShip (center: Coord) (facing: Direction) (name: Name) : Ship =
        { Coords = (calculateCoordsByShip center name facing); Center = center; Facing = facing; Name = name }

    //Determine perimeter of the ship
    let getPerimeter (ship: Ship) (dims: Dims) : Coord list =
        let shipCoords = ship.Coords
        let (dimx, dimy) = dims
        let rec generatePerimeterCoords shipCoords = //For every square of the ship, add the surrounding coords to the perimeter list
            match shipCoords with
            | [] -> []
            | (x, y)::r -> (surroundCoordWithPerimeter(x,y))@(generatePerimeterCoords r)
        let perimeterCoords = 
            generatePerimeterCoords shipCoords
            |> List.filter (fun (x,y) -> x >= 0 && x < dimx && y >= 0 && y < dimy) //Remove coords that are out of bounds
            |> List.filter (fun coord -> not (List.contains coord shipCoords)) //Remove the ship coords from the list
            |> List.fold (fun acc coord -> if (List.contains coord acc) then acc else coord::acc) [] //Remove duplicates
        perimeterCoords

