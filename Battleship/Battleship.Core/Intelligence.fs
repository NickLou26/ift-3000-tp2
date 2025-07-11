namespace Battleship.Core

module Intelligence =
    open Stream
    open Grid
    open Ship
    open Navigation
    open Battlefield

    (* ------- À COMPLÉTER ------- *)
    (* --- Nouvelles fonctions --- *)

    let repairOtherShip (grid: Sector Grid) : Sector Grid =
        (* ------- À COMPLÉTER ------- *)
        (* ----- Implémentation ------ *)
        grid

    let repairSpy (grid: Sector Grid) : Sector Grid =
        (* ------- À COMPLÉTER ------- *)
        (* ----- Implémentation ------ *)
        grid

    let shootAtSpy (grid: Sector Grid) : Sector Grid =
        (* ------- À COMPLÉTER ------- *)
        (* ----- Implémentation ------ *)
        grid

    let launchTorpedo (grid: Sector Grid) : Sector Grid =
        (* ------- À COMPLÉTER ------- *)
        (* ----- Implémentation ------ *)
        grid

    let advanceTorpedoes (grid: Sector Grid) : Sector Grid =
        (* ------- À COMPLÉTER ------- *)
        (* ----- Implémentation ------ *)
        grid

    let interceptNextHit (coord: Coord) (grid: Sector Grid) : Sector Grid =
        (* ------- À COMPLÉTER ------- *)
        (* ----- Implémentation ------ *)
        grid

    let getPlanePath (grid: Sector Grid) : Coord list =
        (* ------- À COMPLÉTER ------- *)
        (* ----- Implémentation ------ *)
        []

    let advancePlane (stream: Coord Stream) : Coord Stream =
        (* ------- À COMPLÉTER ------- *)
        (* ----- Implémentation ------ *)
        stream

    let shiftPath (grid: Sector Grid) (stream: Coord Stream) : Coord Stream =
        (* ------- À COMPLÉTER ------- *)
        (* ----- Implémentation ------ *)
        stream

    let revertPath (grid: Sector Grid) (stream: Coord Stream) : Coord Stream =
        (* ------- À COMPLÉTER ------- *)
        (* ----- Implémentation ------ *)
        stream