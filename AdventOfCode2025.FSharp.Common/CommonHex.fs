module CommonHex
    
    type HexCoordCube = { Q: int; R: int; S: int; }

    let hexCoords_axial2cube (x,y) = { Q=x; R=y; S= -x-y; }
    let hexCoords_cube2axial (c:HexCoordCube)  = (c.Q, c.R)

    let hexCoords_GetNeighbours (x,y) =
        (x,y)
        |> hexCoords_axial2cube
        |> fun c -> [
            { c with Q=c.Q+1; R=c.R-1; };
            { c with Q=c.Q+1; S=c.S-1; };
            { c with R=c.R+1; Q=c.Q-1; };
            { c with R=c.R+1; S=c.S-1; };
            { c with S=c.S+1; Q=c.Q-1; };
            { c with S=c.S+1; R=c.R-1; };
            ]
        |> List.map hexCoords_cube2axial

    let hexCoords_Dist (x1,y1) (x2,y2) =
        let c1 = hexCoords_axial2cube (x1,y1)
        let c2 = hexCoords_axial2cube (x2,y2)

        seq { abs(c1.Q-c2.Q) ; abs(c1.R-c2.R) ; abs(c1.S-c2.S) }
        |> Seq.max

    let hexCoords_IsNeighbour (x1,y1) (x2,y2) =
        hexCoords_Dist (x1,y1) (x2,y2)
        |> (=)1

