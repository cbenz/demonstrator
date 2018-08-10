module Types exposing (..)

import Dict exposing (Dict)
import List.Extra as List
import Maybe.Extra as Maybe


-- INDIVIDUALS


type alias Individual =
    { inputValues : Dict VariableName InputValue
    , roles : Dict EntityKey RoleKey
    }


type InputValue
    = BoolInputValue Bool
    | DateInputValue String
    | EnumInputValue String
    | FloatInputValue Float
    | IntInputValue Int
    | StringInputValue String


inputValueFromVariable : Variable -> InputValue
inputValueFromVariable variable =
    case variable of
        BoolVariable ( _, { default } ) ->
            BoolInputValue default

        DateVariable ( _, { default } ) ->
            DateInputValue default

        EnumVariable ( _, { default } ) ->
            EnumInputValue default

        FloatVariable ( _, { default } ) ->
            FloatInputValue default

        IntVariable ( _, { default } ) ->
            IntInputValue default

        StringVariable ( _, { default } ) ->
            StringInputValue default


type alias IndividualIndex =
    Int


type alias IndividualId =
    String


individualId : IndividualIndex -> IndividualId
individualId index =
    "individual_" ++ (toString (index + 1))



-- ENTITIES


type alias RoleKey =
    String


type alias Role =
    { key : RoleKey
    , label : String
    , max : Maybe Int
    , subroles : Maybe (List String)
    }


type alias EntityKey =
    String


type alias IndividualEntity =
    { key : EntityKey
    , label : String
    }


type alias GroupEntity =
    { key : EntityKey
    , label : String
    , roles : List Role
    }


type alias Entities =
    { individual : IndividualEntity
    , groups : List GroupEntity
    }


{-| The max number of individuals having a role is defined either:
- by the `.max` field
- by the length of the `.subroles` field
-}
roleMax : Role -> Maybe Int
roleMax role =
    role.max
        |> Maybe.or (role.subroles |> Maybe.map List.length)


{-| Find the next available role given an individual index, in a list of roles.

For example (roles list is simplified):
parents = {key = "parent", max = Just 2}
enfants = {key = "enfant", max = Nothing}
roles = [parents, enfants]
nextRole [] roles = parents
nextRole [x] roles = parents
nextRole [x, y] roles = enfants
nextRole [x, y, z] roles = enfants
...
-}
nextRole : List Individual -> List Role -> Maybe Role
nextRole individuals roles =
    roles
        |> List.find
            (\role ->
                let
                    nbRolesFounds =
                        individuals
                            |> List.filterMap
                                (\individual ->
                                    individual.roles
                                        |> Dict.toList
                                        |> List.find (\( _, roleKey ) -> role.key == roleKey)
                                )
                            |> List.length
                in
                    case roleMax role of
                        Nothing ->
                            True

                        Just max ->
                            nbRolesFounds < max
            )


nextRoles : List Individual -> List GroupEntity -> Dict EntityKey RoleKey
nextRoles individuals groupEntities =
    groupEntities
        |> List.filterMap
            (\entity ->
                nextRole individuals entity.roles
                    |> Maybe.map (\role -> ( entity.key, role.key ))
            )
        |> Dict.fromList



-- GROUPED INDIVIDUALS (sub-part of a test-case)


type alias GroupedIndividualIds =
    Dict EntityKey (Dict RoleKey (List IndividualId))


{-| Groups individuals by entity key then by role key.

before =
    [ Dict.fromList
        [ ( "familles", Dict.fromList [ ( "parents", [ "individual_1" ] ) ] )
        , ( "foyers_fiscaux", Dict.fromList [ ( "declarants", [ "individual_1" ] ) ] )
        , ( "menages", Dict.fromList [ ( "personne_de_reference", [ "individual_1" ] ) ] )
        ]
    , Dict.fromList
        [ ( "familles", Dict.fromList [ ( "parents", [ "individual_2" ] ) ] )
        , ( "foyers_fiscaux", Dict.fromList [ ( "declarants", [ "individual_2" ] ) ] )
        , ( "menages", Dict.fromList [ ( "conjoint", [ "individual_2" ] ) ] )
        ]
    ]

after =
    Just
        (Dict.fromList
            [ ( "familles", Dict.fromList [ ( "parents", [ "individual_1", "individual_2" ] ) ] )
            , ( "foyers_fiscaux", Dict.fromList [ ( "declarants", [ "individual_1", "individual_2" ] ) ] )
            , ( "menages"
              , Dict.fromList
                    [ ( "conjoint", [ "individual_2" ] )
                    , ( "personne_de_reference", [ "individual_1" ] )
                    ]
              )
            ]
        )

TODO Do not store a `List Individual` when max = 1 for example. Use OneOrMany perhaps, like so:
type OneOrMany a
    = One a
    | Many (List a)
type alias GroupedIndividualIds =
    Dict EntityKey (Dict RoleKey (OneOrMany IndividualId))
-}
groupByEntityAndRole : List Individual -> Maybe GroupedIndividualIds
groupByEntityAndRole individuals =
    let
        mergeTestCases : GroupedIndividualIds -> GroupedIndividualIds -> GroupedIndividualIds
        mergeTestCases testCase1 testCase2 =
            Dict.merge
                Dict.insert
                (\entityKey roles1 roles2 individualAccu ->
                    individualAccu |> Dict.insert entityKey (mergeEntities roles1 roles2)
                )
                Dict.insert
                testCase1
                testCase2
                Dict.empty

        mergeEntities : Dict RoleKey (List IndividualId) -> Dict RoleKey (List IndividualId) -> Dict RoleKey (List IndividualId)
        mergeEntities entities1 entities2 =
            Dict.merge
                Dict.insert
                (\roleKey individualIds1 individualIds2 entityAccu ->
                    entityAccu |> Dict.insert roleKey (individualIds1 ++ individualIds2)
                )
                Dict.insert
                entities1
                entities2
                Dict.empty
    in
        individuals
            |> List.indexedMap
                (\index { roles } ->
                    roles
                        |> Dict.map
                            (\_ roleKey ->
                                Dict.singleton roleKey [ individualId index ]
                            )
                )
            |> List.foldl1 mergeTestCases



-- AXES


type alias Axis =
    { count : Int
    , individualIndex : IndividualIndex
    , max : Float
    , min : Float
    , variableName : VariableName
    , selectedIndex : Int
    }


axisValue : Axis -> Float
axisValue axis =
    ((toFloat axis.selectedIndex) * axis.max) / (axis.count - 1 |> toFloat)


getValue : List Float -> List Axis -> Maybe Float
getValue values axes =
    let
        index =
            axes
                |> List.map .selectedIndex
                -- TODO Really take into account selected indexes when multiple axes
                |>
                    List.head
                |> Maybe.withDefault 0
    in
        List.getAt index values
            |> Maybe.andThen
                (\value ->
                    if value == 0 then
                        Nothing
                    else
                        Just value
                )


findAxis : IndividualIndex -> VariableName -> List Axis -> Maybe Axis
findAxis individualIndex variableName axes =
    List.find (\axis -> axis.individualIndex == individualIndex && axis.variableName == variableName) axes



-- PERIODS


type alias Period =
    Int



-- SIMULATE


type SimulateNode
    = SimulateNode SimulateNodeFields


type alias SimulateNodeFields =
    { children : List SimulateNode
    , code : String
    , color : ( Int, Int, Int )
    , name :
        -- Not really a VariableName (could be a decomposition node which does not correspond to a variable)
        String
    , shortName : String
    , values : List Float
    }



-- VARIABLES


type alias VariableName =
    String


type alias VariableCommonFields =
    { entity : EntityKey
    , label : Maybe String
    , name : VariableName
    }


type alias BoolVariableFields =
    { default : Bool }


type alias DateVariableFields =
    { default : String }


type alias EnumVariableFields =
    { default : String
    , labels : Dict String String
    }


type alias FloatVariableFields =
    { default : Float }


type alias IntVariableFields =
    { default : Int }


type alias StringVariableFields =
    { default : String }


type Variable
    = BoolVariable ( VariableCommonFields, BoolVariableFields )
    | DateVariable ( VariableCommonFields, DateVariableFields )
    | EnumVariable ( VariableCommonFields, EnumVariableFields )
    | FloatVariable ( VariableCommonFields, FloatVariableFields )
    | IntVariable ( VariableCommonFields, IntVariableFields )
    | StringVariable ( VariableCommonFields, StringVariableFields )


type alias VariablesResponse =
    { countryPackageName : String
    , countryPackageVersion : String
    , currency : String
    , variables : Dict VariableName Variable
    }


variableCommonFields : Variable -> VariableCommonFields
variableCommonFields variable =
    case variable of
        BoolVariable ( x, _ ) ->
            x

        DateVariable ( x, _ ) ->
            x

        EnumVariable ( x, _ ) ->
            x

        FloatVariable ( x, _ ) ->
            x

        IntVariable ( x, _ ) ->
            x

        StringVariable ( x, _ ) ->
            x


variableLabel : Dict VariableName Variable -> VariableName -> String
variableLabel variables variableName =
    variables
        |> Dict.get variableName
        |> Maybe.andThen (variableCommonFields >> .label)
        |> Maybe.withDefault variableName
