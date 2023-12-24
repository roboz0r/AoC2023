#r "nuget: FParsec, 1.1.1"

open System
open FParsec

[<Measure>]
type m

[<Measure>]
type ns

type Orientation =
    | Clockwise
    | CounterClockwise
    | Collinear

[<Struct>]
type Point = { X: float<m>; Y: float<m> }

module Point =
    let onSegment (p: Point) (q: Point) (r: Point) =
        (q.X <= (max p.X r.X)
         && q.X >= (min p.X r.X)
         && q.Y <= (max p.Y r.Y)
         && q.Y >= (min p.Y r.Y))

    let orientation (p: Point) (q: Point) (r: Point) =
        let v =
            (q.Y - p.Y) * (r.X - q.X)
            - (q.X - p.X) * (r.Y - q.Y)

        if v = 0.0<_> then Collinear
        elif v > 0.0<_> then Clockwise
        else CounterClockwise

    let doIntersect (p1: Point) (q1: Point) (p2: Point) (q2: Point) =
        let o1 = orientation p1 q1 p2
        let o2 = orientation p1 q1 q2
        let o3 = orientation p2 q2 p1
        let o4 = orientation p2 q2 q1

        if o1 <> o2 && o3 <> o4 then
            true
        elif o1 = Collinear && onSegment p1 p2 q1 then
            true
        elif o2 = Collinear && onSegment p1 q2 q1 then
            true
        elif o3 = Collinear && onSegment p2 p1 q2 then
            true
        elif o4 = Collinear && onSegment p2 q1 q2 then
            true
        else
            false

    // Equation for line from two points
    // y = mx + c
    // m = (y2 - y1) / (x2 - x1)
    // c = y1 - m * x1

    let line (p1: Point) (p2: Point) =
        let m = (p2.Y - p1.Y) / (p2.X - p1.X)
        let c = p1.Y - m * p1.X
        m, c
[<Struct>]
type Position =
    {
        X: float<m>
        Y: float<m>
        Z: float<m>
    }
    override this.ToString() =
        sprintf "<%g,%g,%g>" this.X this.Y this.Z
    member this.Point = { X = this.X; Y = this.Y }

[<Struct>]
type Particle =
    {
        PX: float<m>
        PY: float<m>
        PZ: float<m>
        VX: float<m / ns>
        VY: float<m / ns>
        VZ: float<m / ns>
    }
    override this.ToString() =
        sprintf "p=<%g,%g,%g>, v=<%g,%g,%g>" this.PX this.PY this.PZ this.VX this.VY this.VZ

    member this.XAt(t: float<ns>) = this.PX + this.VX * t
    member this.YAt(t: float<ns>) = this.PY + this.VY * t
    member this.ZAt(t: float<ns>) = this.PZ + this.VZ * t

type Intersection =
    | Always
    | Never
    | At of float<ns>

module Particle =
    let intersection (pA: float<m>) (vA: float<m / ns>) (pB: float<m>) (vB: float<m / ns>) =
        // pA + vA * t = pB + vB * t
        // pA - pB = (vB - vA) * t
        if vA = vB then
            if pA = pB then Always else Never
        else
            let t = (pA - pB) / (vB - vA)
            At t

    let intersectionXY (pA: Particle) (pB: Particle) =
        let x = intersection pA.PX pA.VX pB.PX pB.VX
        let y = intersection pA.PY pA.VY pB.PY pB.VY

        match x, y with
        | Always, Always -> Always
        | Never, _
        | _, Never -> Never
        | At tX, At tY -> if tX = tY then At tX else Never
        | Always, At tY -> At tY
        | At tX, Always -> At tX

    let positionAtX (p: Particle) (x: float<m>) =
        let t = (x - p.PX) / p.VX
        { X = x; Y = p.YAt t; Z = p.ZAt t }, t

    let positionAtY (p: Particle) (y: float<m>) =
        let t = (y - p.PY) / p.VY
        { X = p.XAt t; Y = y; Z = p.ZAt t }, t

type Input = Particle list

module Input =
    // 19, 13, 30 @ -2,  1, -2
    let pParticle: Parser<Particle, unit> =
        let pXYZ =
            pfloat .>> (pchar ',' .>> spaces) .>>. pfloat
            .>> (pchar ',' .>> spaces)
            .>>. pfloat

        pipe3 pXYZ (spaces .>> pchar '@' .>> spaces) pXYZ (fun ((px, py), pz) _ ((vx, vy), vz) ->
            {
                PX = px * 1.0<m>
                PY = py * 1.0<m>
                PZ = pz * 1.0<m>
                VX = vx * 1.0<m/ns>
                VY = vy * 1.0<m/ns>
                VZ = vz * 1.0<m/ns>
            })

    let pInput: Parser<Input, unit> = many1Till (pParticle .>> newline) eof

    let parse (s: string) =
        match run pInput s with
        | Success (r, _, _) -> r
        | Failure (e, _, _) -> failwith e

    let intersectionsBetween (input: Input) (minXY: float<m>) (maxXY: float<m>) =
        let rec loop (pA: Particle) (particles: Particle list) acc =
            let toAdd =
                particles
                |> List.choose (fun pB -> 
                    let ppA = (fst (Particle.positionAtX pA minXY)).Point
                    let qA = (fst (Particle.positionAtX pA maxXY)).Point
                    let ppB = (fst (Particle.positionAtX pB minXY)).Point
                    let qB = (fst (Particle.positionAtX pB maxXY)).Point
                    if Point.doIntersect ppA qA ppB qB then 
                        let mA, cA = Point.line ppA qA
                        let mB, cB = Point.line ppB qB
                        // mA * x + cA = mB * x + cB
                        let x = (cB - cA) / (mA - mB)
                        let posA, tA = Particle.positionAtX pA x
                        let y = pA.YAt tA
                        let posB, tB = Particle.positionAtX pB x
                        if tA > 0.0<_> && tB > 0.0<_> && (minXY <= x && x <= maxXY && minXY <= y && y <= maxXY ) then
                            Some (pA, pB, x, y)
                        else
                            None
                    else 
                        None)

            let acc = toAdd @ acc

            match particles with
            | [] -> acc
            | p :: ps -> loop p ps acc

        match input with
        | [] -> []
        | p :: ps -> loop p ps []


let sample =
    """19, 13, 30 @ -2,  1, -2
18, 19, 22 @ -1, -1, -2
20, 25, 34 @ -2, -2, -4
12, 31, 28 @ -1, -2, -1
20, 19, 15 @  1, -5, -3
"""

let sampleInput = Input.parse sample

let sampleIntersections =
    Input.intersectionsBetween sampleInput 7.<_> 27.<_>
    |> List.iter (fun (a, b, x, y) -> printfn "%O %O x=%g y=%g" a b x y)

open System.IO

let input = File.ReadAllText(Path.Combine(__SOURCE_DIRECTORY__,"..", "input.txt"))

let parsedInput = Input.parse input


let part1Answer = 
    Input.intersectionsBetween parsedInput (200000000000000.0<_>) (400000000000000.0<_>)
    |> List.length


// Part 2 - Need to solve a system of equations
// 9 unknowns (3x position, 3x velocity, 3x time) so first 3 hailstones is enough to create 9 equations

// 320870677764563, 335750934489987, 282502845957937 @ -40, -24, 10
// 219235623600942, 408022798608755, 245679379684914 @ 127, -45, 66
// 171834827764229, 225154401936948, 232302441670972 @ -122, -521, 95
// (rockpx + rockvx * tA) = (320870677764563 + -40 * tA)
// (rockpy + rockvy * tA) = (335750934489987 + -24 * tA)
// (rockpz + rockvz * tA) = (282502845957937 + 10 * tA)
// (rockpx + rockvx * tB) = (219235623600942 + 127 * tB)
// (rockpy + rockvy * tB) = (408022798608755 + -45 * tB)
// (rockpz + rockvz * tB) = (245679379684914 + 66 * tB)
// (rockpx + rockvx * tC) = (171834827764229 + -122 * tC)
// (rockpy + rockvy * tC) = (225154401936948 + -521 * tC)
// (rockpz + rockvz * tC) = (232302441670972 + 95 * tC)
// (rockpx + rockvx * tD) = (399408000414510 + -110 * tD)

// Substitute for single character variables and solved using https://www.geogebra.org/
// Solve({x+a t=320870677764563-40 t,y+b t=335750934489987-24 t,z+c t=282502845957937+10 t,x+a u=219235623600942+127 u,y+b u=408022798608755-45 u,z+c u=245679379684914+66 u,x+a v=171834827764229-122 v,y+b v=225154401936948-521 v,z+c v=232302441670972+95 v},{x,y,z,a,b,c,t,u,v})
// {{x = 149412455352770, y = 174964385672289, z = 233413147425100, a = 201, b = 202, c = 79, t = 711444906273, u = 943556327678, v = 69419109633}}
// rockpx = 149412455352770
// rockpy = 174964385672289
// rockpz = 233413147425100
// rockvx = 201
// rockvy = 202
// rockvz = 79
// tA = 711444906273
// tB = 943556327678
// tC = 69419109633

let part2Answer = 149412455352770L + 174964385672289L + 233413147425100L

