module Requests exposing (..)

import Dict exposing (Dict)
import Http
import Json.Decode exposing (..)
import Json.Decode.Extra exposing (..)
import Json.Encode as Encode
import List.Extra as List
import Types exposing (..)


-- CONSTANTS


nbYearsPadding : Int
nbYearsPadding =
    5



-- TYPES (specific to encoders and decoders)


type Entity
    = IndividualE IndividualEntity
    | GroupE GroupEntity



-- ENCODERS


encodeGroupedIndividualIds : GroupedIndividualIds -> List ( RoleKey, Encode.Value )
encodeGroupedIndividualIds groupedIndividualIds =
    groupedIndividualIds
        |> Dict.toList
        |> List.map
            (\( entityKey, roles ) ->
                ( entityKey
                , Encode.list
                    [ Encode.object
                        (roles
                            |> Dict.toList
                            |> List.map
                                (\( roleKey, individualIds ) ->
                                    ( roleKey, encodeIndividualIds individualIds )
                                )
                        )
                    ]
                )
            )


encodeIndividualIds : List IndividualId -> Encode.Value
encodeIndividualIds individualIds =
    case List.head individualIds of
        Nothing ->
            Encode.list []

        Just firstIndividualId ->
            if List.length individualIds == 1 then
                Encode.string firstIndividualId
            else
                individualIds
                    |> List.map Encode.string
                    |> Encode.list


encodeInputValue : InputValue -> Encode.Value
encodeInputValue inputValue =
    case inputValue of
        BoolInputValue bool ->
            Encode.bool bool

        DateInputValue string ->
            Encode.string string

        EnumInputValue string ->
            Encode.string string

        FloatInputValue float ->
            Encode.float float

        IntInputValue int ->
            Encode.int int

        StringInputValue string ->
            Encode.string string


encodeTestCase : List Individual -> Period -> List Axis -> Encode.Value
encodeTestCase individuals year axes =
    Encode.object
        (( "individus"
           -- TODO Do not hardcode "individus"
         , Encode.list
            (individuals
                |> List.indexedMap
                    (\individualIndex { inputValues } ->
                        Encode.object
                            (( "id", Encode.string (individualId individualIndex) )
                                :: (inputValues
                                        |> Dict.toList
                                        |> List.filterMap
                                            (\( variableName, inputValue ) ->
                                                case findAxis individualIndex variableName axes of
                                                    Nothing ->
                                                        Just
                                                            ( variableName
                                                            , Encode.object
                                                                ((List.range 0 (nbYearsPadding - 1))
                                                                    |> List.map
                                                                        (\n ->
                                                                            ( year - n |> toString
                                                                            , encodeInputValue inputValue
                                                                            )
                                                                        )
                                                                )
                                                            )

                                                    Just _ ->
                                                        Nothing
                                            )
                                   )
                            )
                    )
            )
         )
            :: case groupByEntityAndRole individuals of
                Nothing ->
                    []

                Just groupedIndividualIds ->
                    encodeGroupedIndividualIds groupedIndividualIds
        )



-- DECODERS


entityDecoder : Decoder Entity
entityDecoder =
    -- TODO (list roleDecoder) does not work with optionalField
    -- create an issue at https://github.com/elm-community/json-extra
    -- (optionalField "roles" (dict roleDecoder)
    --     |> map (Maybe.map Dict.values >> Maybe.withDefault [])
    -- )
    field "isPersonsEntity" bool
        |> withDefault False
        |> andThen
            (\isPersonsEntity ->
                if isPersonsEntity then
                    individualEntityDecoder |> map IndividualE
                else
                    groupEntityDecoder |> map GroupE
            )


groupEntityDecoder : Decoder GroupEntity
groupEntityDecoder =
    keyFromPluralOrSingularDecoder
        |> andThen
            (\key ->
                succeed (GroupEntity key)
                    |: (field "label" string)
                    |: (field "roles" (list roleDecoder))
            )


individualEntityDecoder : Decoder IndividualEntity
individualEntityDecoder =
    keyFromPluralOrSingularDecoder
        |> andThen
            (\key ->
                succeed (IndividualEntity key)
                    |: (field "label" string)
            )


keyFromPluralOrSingularDecoder : Decoder String
keyFromPluralOrSingularDecoder =
    map2 (,)
        (field "key" string)
        (maybe (field "plural" string))
        |> map
            (\( singular, plural ) ->
                plural |> Maybe.withDefault singular
            )


roleDecoder : Decoder Role
roleDecoder =
    -- TODO (list string) does not work with optionalField
    -- create an issue at https://github.com/elm-community/json-extra
    -- (optionalField "subroles" (dict string)
    --     |> map (Maybe.map Dict.values >> Maybe.withDefault [])
    -- )
    keyFromPluralOrSingularDecoder
        |> andThen
            (\key ->
                succeed (Role key)
                    |: (field "label" string)
                    |: (maybe (field "max" int))
                    |: (maybe (field "subroles" (list string)))
            )


simulateNodeDecoder : Decoder SimulateNode
simulateNodeDecoder =
    succeed SimulateNodeFields
        |: (field "children" (list (lazy (\_ -> simulateNodeDecoder))) |> withDefault [])
        |: (field "code" string)
        |: (field "color"
                (list int
                    |> andThen
                        (\xs ->
                            case xs of
                                [ a, b, c ] ->
                                    succeed ( a, b, c )

                                _ ->
                                    fail "color should be a list of 3 integers"
                        )
                )
           )
        |: (field "name" string)
        |: (field "short_name" string)
        |: (field "values" (list float))
        |> map SimulateNode


variableCommonFieldsDecoder : Decoder VariableCommonFields
variableCommonFieldsDecoder =
    succeed VariableCommonFields
        |: (field "entity" string)
        |: (maybe (field "label" string))
        |: (field "name" string)


variableDecoder : Decoder Variable
variableDecoder =
    map2 (,)
        variableCommonFieldsDecoder
        (field "@type" string)
        |> andThen
            (\( variableCommonFields, type_ ) ->
                case type_ of
                    "Boolean" ->
                        map BoolVariableFields
                            (field "default" bool)
                            |> map (\fields -> BoolVariable ( variableCommonFields, fields ))

                    "Date" ->
                        map DateVariableFields
                            (field "default" string)
                            |> map (\fields -> DateVariable ( variableCommonFields, fields ))

                    "Enumeration" ->
                        map2 EnumVariableFields
                            (field "default" string)
                            (field "labels" (dict string))
                            |> map (\fields -> EnumVariable ( variableCommonFields, fields ))

                    "Float" ->
                        map FloatVariableFields
                            (field "default" float)
                            |> map (\fields -> FloatVariable ( variableCommonFields, fields ))

                    "Integer" ->
                        map IntVariableFields
                            (field "default" int)
                            |> map (\fields -> IntVariable ( variableCommonFields, fields ))

                    "String" ->
                        map
                            StringVariableFields
                            (field "default" string)
                            |> map (\fields -> StringVariable ( variableCommonFields, fields ))

                    _ ->
                        fail ("Unsupported type: " ++ type_)
            )


variablesResponseDecoder : Decoder VariablesResponse
variablesResponseDecoder =
    succeed VariablesResponse
        |: (field "country_package_name" string)
        |: (field "country_package_version" string)
        |: (field "currency" string)
        |: (field "variables"
                (list variableDecoder
                    |> map
                        (List.map
                            (\variable ->
                                ( variableCommonFields variable |> .name
                                , variable
                                )
                            )
                            >> Dict.fromList
                        )
                )
           )



-- REQUESTS


entities : String -> Http.Request Entities
entities baseUrl =
    Http.get
        (baseUrl ++ "/2/entities")
        (field "entities" (dict entityDecoder)
            |> map Dict.values
            |> andThen
                (\entities ->
                    let
                        individualEntity =
                            entities
                                |> List.find
                                    (\entity ->
                                        case entity of
                                            IndividualE _ ->
                                                True

                                            GroupE _ ->
                                                False
                                    )
                                |> Maybe.andThen
                                    (\entity ->
                                        case entity of
                                            IndividualE individualEntity ->
                                                Just individualEntity

                                            GroupE _ ->
                                                Nothing
                                    )

                        groupEntities =
                            entities
                                |> List.filterMap
                                    (\entity ->
                                        case entity of
                                            IndividualE _ ->
                                                Nothing

                                            GroupE groupEntity ->
                                                Just groupEntity
                                    )
                    in
                        case individualEntity of
                            Nothing ->
                                fail "One person entity must exist."

                            Just individualEntity ->
                                case groupEntities of
                                    [] ->
                                        fail "At least one group entity must exist."

                                    _ ->
                                        succeed
                                            { individual = individualEntity
                                            , groups = groupEntities
                                            }
                )
        )


simulate : String -> List Individual -> Period -> List Axis -> Http.Request SimulateNode
simulate baseUrl individuals year axes =
    let
        body =
            Encode.object
                [ ( "scenarios"
                  , Encode.list
                        [ Encode.object
                            ([ ( "period", Encode.int year )
                             , ( "test_case", encodeTestCase individuals year axes )
                             ]
                                ++ (axes
                                        |> List.map
                                            (\axis ->
                                                ( "axes"
                                                , Encode.list
                                                    [ Encode.object
                                                        [ ( "count", Encode.int axis.count )
                                                        , ( "index", Encode.int axis.individualIndex )
                                                        , ( "max", Encode.float axis.max )
                                                        , ( "min", Encode.float axis.min )
                                                        , ( "name", Encode.string axis.variableName )

                                                        -- , ( "period"
                                                        --   , Encode.string
                                                        --         (toString (year - nbYearsPadding + 1)
                                                        --             ++ ":"
                                                        --             ++ (toString nbYearsPadding)
                                                        --         )
                                                        --   )
                                                        ]
                                                    ]
                                                )
                                            )
                                   )
                            )
                        ]
                  )
                ]
                |> Http.jsonBody
    in
        Http.post (baseUrl ++ "/1/simulate")
            body
            (field "value" simulateNodeDecoder)


variables : String -> Http.Request VariablesResponse
variables baseUrl =
    Http.get (baseUrl ++ "/1/variables") variablesResponseDecoder
