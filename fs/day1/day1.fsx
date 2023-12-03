// calibration data
open System
open System.IO

let sample = """1abc2
pqr3stu8vwx
a1b2c3d4e5f
treb7uchet"""

let sumCalibrationData (s: string) =
    s.Split([|'\n'; '\r'|], StringSplitOptions.RemoveEmptyEntries)
    |> Array.choose (fun s -> 
        let firstDigit = s.IndexOfAny [|'0'..'9'|]
        let lastDigit = s.LastIndexOfAny [|'0'..'9'|]
        match firstDigit, lastDigit with
        | -1, _ | _, -1 -> None
        | _ -> 
            let digits = $"{s.[firstDigit]}{s.[lastDigit]}"
            Some (int digits)
    )
    |> Array.sum

let result = sumCalibrationData sample

let input = File.ReadAllText(Path.Combine(__SOURCE_DIRECTORY__, "input.txt"))

let result2 = sumCalibrationData input

let samplePt2 = """two1nine
eightwothree
abcone2threexyz
xtwone3four
4nineeightseven2
zoneight234
7pqrstsixteen"""

let validDigit (s:string) (i, v) = 
    match i, v with
    | -1, _ -> None
    | _, 0 -> Some (i, (int (s[i]) - 48))
    | _ -> Some (i, v)
    

let firstWordOrDigit (s: string) = 
    [|
        s.IndexOfAny [|'0'..'9'|], 0
        s.IndexOf("one"), 1
        s.IndexOf("two"), 2
        s.IndexOf("three"), 3
        s.IndexOf("four"), 4
        s.IndexOf("five"), 5
        s.IndexOf("six"), 6
        s.IndexOf("seven"), 7
        s.IndexOf("eight"), 8
        s.IndexOf("nine"), 9
    |]
    |> Array.choose (validDigit s)
    |> Array.minBy (fun (i, v) -> i)

let lastWordOrDigit (s: string) = 
    [|
        s.LastIndexOfAny [|'0'..'9'|], 0
        s.LastIndexOf("one"), 1
        s.LastIndexOf("two"), 2
        s.LastIndexOf("three"), 3
        s.LastIndexOf("four"), 4
        s.LastIndexOf("five"), 5
        s.LastIndexOf("six"), 6
        s.LastIndexOf("seven"), 7
        s.LastIndexOf("eight"), 8
        s.LastIndexOf("nine"), 9
    |]
    |> Array.choose (validDigit s)
    |> Array.maxBy (fun (i, v) -> i)

let sumCalibrationDataPt2 (s: string) =
    s.Split([|'\n'; '\r'|], StringSplitOptions.RemoveEmptyEntries)
    |> Array.choose (fun s -> 
        let (firstDigit, firstVal) = firstWordOrDigit s
        let (lastDigit, lastVal) = lastWordOrDigit s
        match firstDigit, lastDigit with
        | -1, _ | _, -1 -> None
        | _ -> 
            Some (firstVal * 10 + lastVal)
    )
    |> Array.sum

let sampleResultPt2 = sumCalibrationDataPt2 samplePt2

let resultPt2 = sumCalibrationDataPt2 input
