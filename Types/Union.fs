[<AutoOpen>]
module GraphQL.FSharp.Types.Union

open System
open GraphQL.Types

// TODO: Clean this up
[<AttributeUsage(AttributeTargets.Class)>]
type TestAttribute() = inherit Attribute()

module ``Enum Tests`` =
    let private mapValues f (``type``: EnumerationGraphType) =
        ``type``.Values
        |> Seq.toList
        |> List.map f
    let private valueNames = mapValues (fun value -> value.Name)
    let private values = mapValues (fun value -> value.Name, value.Value)

    [<Test>]
    module ``Enum Typed ClassicEnum`` =
        type ClassicEnumType =
        | First = 1
        | Second = 2

        let enumeration = Convert.Enum<ClassicEnumType> "MyEnum"
        assert (enumeration.Name = "MyName")
        assert (valueNames enumeration = ["First"; "Second"])
        assert (enumeration.ParseValue "first" = box ClassicEnumType.First)

    [<Test>]
    module ``Enum Typed FSharpUnionEnum`` =
        type FSharpUnionEnumType =
        | First
        | Second

        let enumeration = Convert.Enum<FSharpUnionEnumType> "MyEnum"
        assert (enumeration.Name = "MyName")
        assert (valueNames enumeration = ["First"; "Second"])
        assert (enumeration.ParseValue "first" = box First)

    [<Test>]
    module ``EnumValue`` =
        let value =
            Define.EnumValue (
                name = "MyEnumValue",
                value = 1
            )
        assert (value.Name = "MyEnumValue")
        assert (value.Value = box 1)

    [<Test>]
    module ``Enum Untyped`` =
        let enumeration =
            Define.Enum (
                name = "MyEnum",
                cases = [
                    Define.EnumValue (
                        name = "First",
                        value = "first"
                    )
                    Define.EnumValue (
                        name = "Second",
                        value = "second"
                    )
                ]
            )

        assert (enumeration.Name = "MyEnum")
        assert (values enumeration = ["First", box "first"; "Second", box "second"])
        assert (enumeration.ParseValue "first" = box "first")

type Define with
    static member Union
        (name: string,
         ?types: IObjectGraphType list,
         ?description,
         ?deprecationReason) =
        let ``type`` = UnionGraphType () |> setBasicProps name description deprecationReason
        ``type``.PossibleTypes <- optionList types
        ``type``
