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
    let calculateCoords (facing: Direction) (blocksLeft: int) (x: int, y: int): Coord =
        //Return the correct position based on the direction of the ship and the number of blocks left to calculate
        match facing with
        | North -> (x + blocksLeft, y)
        | South -> (x - blocksLeft, y)
        | East -> (x, y - blocksLeft)
        | West -> (x, y + blocksLeft)

    //Function to generate the list of coords of a new ship
    let generateCoordsList (size : int) (center: Coord) (facing: Direction): Coord List =
        //Get the x and y of the center
        let (center_x, center_y) = center
        //Get offset based on size of the size
        let offset = 
            if size % 2 = 0 then
                (size / 2) - 1
            else
                size / 2
        //Determine the coords in front of the ship
        let (frontx, fronty) =
            match facing with
            | North -> (center_x - offset, center_y)
            | South -> (center_x + offset, center_y)
            | East -> (center_x, center_y + offset)
            | West -> (center_x, center_y - offset)

        //Recursively generate the list of coords, starting from the last position, so the front of the ship is the first elem of the list (blocks = each coord of the ship)
        let rec generateShipCoords (currentBlock: int): Coord List = 
            if currentBlock = size then
                []
            else
                (calculateCoords facing (currentBlock) (frontx,fronty))::generateShipCoords (currentBlock + 1) 
        
        generateShipCoords 0

    //Calculate coords for each different ship
    let calculateCoordsByShip (center: Coord) (name: Name) (facing: Direction): Coord List =
        //Calls generateCoordsList with the size according to the ship name
        match name with
        | Spy -> (generateCoordsList 2 center facing)
        | PatrolBoat -> (generateCoordsList 2 center facing)
        | Destroyer -> (generateCoordsList 3 center facing)
        | Submarine -> (generateCoordsList 3 center facing)
        | Cruiser -> (generateCoordsList 4 center facing)
        | AircraftCarrier -> (generateCoordsList 5 center facing)

    //Create a ship
    let createShip (center: Coord) (facing: Direction) (name: Name) : Ship =
        //Create a Ship type, with coords calculated by calculateCoordsByShip
        { Coords = (calculateCoordsByShip center name facing); Center = center; Facing = facing; Name = name }

    //Generate a list of coords around the passed coord
    let surroundCoordWithPerimeter (coord: Coord) : Coord List =
        //Get the x and y of coord
        let (x, y) = coord
        //Add every position around the coord
        [(x - 1, y);(x + 1, y);(x, y - 1);(x, y + 1);(x - 1, y - 1);(x - 1, y + 1);(x + 1, y - 1);(x + 1, y + 1)]

    let getPerimeterCoords (coordsList: Coord List) (dims: Dims): Coord List = 
        //Get the dimension of the grid
        let (dimx, dimy) = dims
        //Recursively add every coord surrounding coord to a list
        let rec generatePerimeterCoords coords =
            match coords with
            | [] -> []
            | (x, y)::r -> (surroundCoordWithPerimeter(x,y))@(generatePerimeterCoords r)
        let perimeterCoords = 
            //Call the recursive fun to generate all the coords
            generatePerimeterCoords coordsList
            //Then remove coords that are out of bounds from the list
            |> List.filter (fun (x,y) -> x >= 0 && x < dimx && y >= 0 && y < dimy) 
            //Then remove the ship coords from the list
            |> List.filter (fun coord -> not (List.contains coord coordsList)) 
            //Then remove duplicates
            |> List.fold (fun acc coord -> if (List.contains coord acc) then acc else coord::acc) []
        //Return the list
        perimeterCoords

    //Determine perimeter of the ship
    let getPerimeter (ship: Ship) (dims: Dims) : Coord list =
        getPerimeterCoords ship.Coords dims

