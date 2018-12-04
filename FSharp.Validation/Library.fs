module FSharp.Validation

[<AutoOpen>]
module internal Util =
    module internal String =
        let inline private get (s: string) = s
        let contains needle haystack = (get haystack).Contains (get needle)
        let isEmpty input = get input |> String.length |> (=) 0

    module internal Integer =
        let numDigits = float >> log10 >> ceil >> int

type IErrorCode = interface end

type ErrorCode =
| EmptyString

| StringMinLength of minLength: int
| StringMaxLength of maxLength: int

| ContainsSpaces

| DoesNotContainAll of list: string list
| DoesNotContainAny of list: string list

| UnmatchedRegex of patternName: string

| InvalidInteger

| IntegerMinValue of min: int
| IntegerMaxValue of max: int

| MustBeNDigits of n: int

    interface IErrorCode

type Result<'success> =
| Ok of 'success
| Error of IErrorCode list
type Validator<'value> = 'value -> Result<'value>

let ok x = Ok x
let error x = Error [x]
let retn x = Ok x

let isOk x =
    match x with
    | Ok _ -> true
    | Error _ -> false
let isError x = (isOk >> not) x

let getOk (Ok x) = x
let getError (Error x) = x

let map f x =
    match x with
    | Ok x -> Ok (f x)
    | Error error -> Error error

let apply f x =
    match f, x with
    | Ok f, Ok x -> Ok (f x)
    | Error fErr, Error xErr -> Error (fErr @ xErr)
    | Error err, _ -> Error err
    | _, Error err -> Error err

let lift2 f x y = f <| map |> x <| apply |> y
let lift3 f x y z = f <| map |> x <| apply |> y <| apply |> z
let lift4 f x y z w = f <| map |> x <| apply |> y <| apply |> z <| apply |> w

let ( <* ) x y = lift2 (fun lhs _ -> lhs) x y
let ( *> ) x y = lift2 (fun _ rhs -> rhs) x y

let bind f x =
    match x with
    | Ok x -> f x
    | Error err -> Error err

module Operators =
    let (<*>) = apply
    let (<!>) = map

    let (>>=) x f = bind f x
    let (=<<) f x = bind f x

    let (>=>) f g = fun x -> f x >>= g
    let (<=<) g f = fun x -> f x >>= g

module Builder =
    type ResultBuilder() =
        member __.ReturnFrom x = x
        member __.Return x = Ok x
        member __.Bind (x, f) = bind f x

    let result = ResultBuilder()

module Validators =
    // TODO
    let [<Literal>] EmailRegex = ""
    let [<Literal>] IntMax = System.Int32.MaxValue
    // TODO
    let InvalidCharacters = [""]

    module String =
        open System
        open System.Text.RegularExpressions

        let notEmpty input =
            match input with
            | "" -> error EmptyString
            | _ -> ok input

        let lengthBetween min max input =
            let len = String.length input
            if len < min then
                error (StringMinLength min)
            elif len > max then
                error (StringMaxLength max)
            else
                ok input

        let minLength min = lengthBetween min IntMax
        let maxLength max = lengthBetween 0 max

        let private listChecker outFn innerFn lst input =
            if outFn (innerFn input) lst then
                ok input
            else
                error (DoesNotContainAll lst)
        let containsAll = listChecker List.forall String.contains
        let containsAny = listChecker List.exists String.contains
        let containsNone =
            let noneList f lst = List.exists f lst |> not
            listChecker noneList String.contains
        let noInvalidCharacters = containsNone InvalidCharacters

        let noSpaces input =
            let isSpace c = c = ' '
            if String.filter isSpace input |> String.isEmpty then
                ok input
            else
                error ContainsSpaces

        let regex regex patternName input =
            let m = Regex.Match(regex, input)
            if m.Success then
                ok input
            else
                error (UnmatchedRegex patternName)

        let integer (input: string) =
            match Int32.TryParse input with
            | false, _ -> error InvalidInteger
            | true, i -> ok i

    module Integer =
        let between min max input =
            if input < min then
                error (IntegerMinValue min)
            elif input > max then
                error (IntegerMaxValue max)
            else
                ok input
        let greaterThan min = between min IntMax
        let lessThan max = between 0 max

        let nDigits n input =
            if Integer.numDigits input = n then
                ok input
            else
                error (MustBeNDigits n)
