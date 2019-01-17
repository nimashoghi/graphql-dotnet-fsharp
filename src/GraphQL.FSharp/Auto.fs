module GraphQL.FSharp.Auto

let Enum<'t> = AutoImplementation.Enum.Enum<'t>
let Union<'t> = AutoImplementation.Union.Union<'t>
let Interface<'t> = AutoImplementation.Interface.Interface<'t>
let Object<'t when 't: (new: unit -> 't)> = AutoImplementation.Object.Object<'t>
let InputObject<'t when 't: (new: unit -> 't)> = AutoImplementation.InputObject.InputObject<'t>
