module GraphQL.FSharp.Auto

let Enum<'t> = AutoEnum.Enum<'t>
let Union<'t> = AutoUnion.Union<'t>
let Interface<'t> = AutoInterface.Interface<'t>
let Object<'t> = AutoObject.Object<'t>
let InputObject<'t when 't: (new: unit -> 't)> = AutoInputObject.InputObject<'t>
