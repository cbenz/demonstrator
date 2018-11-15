module Main exposing (..)

import Debounce exposing (Debounce)
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Decode
import Json.Encode as Encode
import List.Extra as List
import Ports
import RemoteData exposing (RemoteData(..), WebData)
import Requests
import Response
import Task
import Time
import Types exposing (..)


type alias Flags =
    { apiBaseUrl : String
    , displayDisclaimer : Bool
    }


main : Program Flags Model Msg
main =
    Html.programWithFlags
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- CONSTANTS


salaireVariableName : String
salaireVariableName =
    -- TODO Do not hardcode "salaire_de_base"
    "salaire_de_base"



-- MODEL


type alias Model =
    { apiBaseUrl : String
    , axes : List Axis
    , debounce : Debounce ( Period, List Individual )
    , displayDisclaimer : Bool
    , displayRoles : Bool
    , entitiesWebData : WebData Entities
    , individuals : List Individual
    , nextVariables : Dict IndividualIndex VariableName
    , simulateWebData : WebData SimulateNode
    , year : Period
    , variablesWebData : WebData VariablesResponse
    }


initialModel : Model
initialModel =
    { apiBaseUrl = "//localhost:2000/api"
    , axes =
        []

    -- [
    --   { count = 50
    --   , individualIndex = 0
    --   , max = 100000
    --   , min = 0
    --   , selectedIndex = 0
    --   , variableName = salaireVariableName
    --   }
    --   , { count = 50
    --     , individualIndex = 1
    --     , max = 100000
    --     , min = 0
    --     , selectedIndex = 0
    --     , variableName = salaireVariableName
    --     }
    -- ]
    , debounce = Debounce.init
    , displayDisclaimer = True
    , displayRoles = False
    , entitiesWebData = NotAsked
    , individuals = []
    , nextVariables = Dict.empty
    , simulateWebData = NotAsked
    , year = 2015
    , variablesWebData = NotAsked
    }


initialIndividuals : List Individual -> Entities -> List Individual
initialIndividuals individuals entities =
    [ { inputValues =
            Dict.fromList
                [ ( salaireVariableName, FloatInputValue 0 )

                -- , ( "statut_marital", EnumInputValue "1" )
                ]
      , roles = nextRoles individuals entities.groups
      }
    ]


initialCmds : String -> List (Cmd Msg)
initialCmds apiBaseUrl =
    let
        entitiesCmd =
            Requests.entities apiBaseUrl
                |> RemoteData.sendRequest
                |> Cmd.map EntitiesResult

        variablesCmd =
            Requests.variables apiBaseUrl
                |> RemoteData.sendRequest
                |> Cmd.map VariablesResult
    in
        [ entitiesCmd, variablesCmd ]


init : Flags -> ( Model, Cmd Msg )
init { apiBaseUrl, displayDisclaimer } =
    let
        newModel =
            { initialModel
                | apiBaseUrl = apiBaseUrl
                , displayDisclaimer = displayDisclaimer
                , entitiesWebData = Loading
                , variablesWebData = Loading
            }
    in
        newModel ! initialCmds newModel.apiBaseUrl



-- UPDATE


type Msg
    = AddIndividual Entities
    | AddVariable IndividualIndex
    | AddVariableInput IndividualIndex String
    | CloseDisclaimer
    | DebounceMsg Debounce.Msg
    | EntitiesResult (WebData Entities)
    | RemoveIndividual IndividualIndex
    | ResetApplication
    | SetAxisSelectedIndex IndividualIndex VariableName Int
    | SetDisplayRoles Bool
    | SetInputValue IndividualIndex String InputValue
    | SetRole IndividualIndex String String
    | SetYear Int
    | Simulate ( Period, List Individual )
    | SimulateResult (WebData SimulateNode)
    | ToggleAxis IndividualIndex VariableName Bool
    | VariablesResult (WebData VariablesResponse)


debounceConfig : Debounce.Config Msg
debounceConfig =
    { strategy = Debounce.later (1 * Time.second)
    , transform = DebounceMsg
    }


simulate : ( Period, List Individual ) -> Cmd Msg
simulate pair =
    Task.perform Simulate (Task.succeed pair)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        renderWaterfallCmd webData =
            webData
                |> RemoteData.map
                    (\simulateNode ->
                        Ports.waterfallData model.axes simulateNode
                            |> Ports.renderWaterfall
                    )
                |> RemoteData.withDefault Cmd.none
    in
        case msg of
            AddIndividual entities ->
                let
                    newIndividuals =
                        model.individuals
                            ++ [ { inputValues = Dict.fromList [ ( salaireVariableName, FloatInputValue 0 ) ]
                                 , roles = nextRoles model.individuals entities.groups
                                 }
                               ]
                in
                    { model | individuals = newIndividuals }
                        ! [ simulate ( model.year, newIndividuals ) ]

            AddVariable individualIndex ->
                (case Dict.get individualIndex model.nextVariables of
                    Nothing ->
                        model

                    Just nextVariables ->
                        model.variablesWebData
                            |> RemoteData.map
                                (\variablesResponse ->
                                    case Dict.get nextVariables variablesResponse.variables of
                                        Nothing ->
                                            model

                                        Just variable ->
                                            let
                                                newIndividuals =
                                                    model.individuals
                                                        |> List.updateIfIndex ((==) individualIndex)
                                                            (\individual ->
                                                                { individual
                                                                    | inputValues =
                                                                        Dict.insert
                                                                            nextVariables
                                                                            (inputValueFromVariable variable)
                                                                            individual.inputValues
                                                                }
                                                            )
                                            in
                                                { model
                                                    | individuals = newIndividuals
                                                    , nextVariables =
                                                        Dict.remove
                                                            individualIndex
                                                            model.nextVariables
                                                }
                                )
                            |> RemoteData.withDefault model
                )
                    ! []

            AddVariableInput individualIndex str ->
                { model | nextVariables = Dict.insert individualIndex str model.nextVariables } ! []

            CloseDisclaimer ->
                { model | displayDisclaimer = False }
                    ! [ Ports.writeToLocalStorage
                            { key = "display-disclaimer"
                            , value = Just (Encode.bool False)
                            }
                      , renderWaterfallCmd model.simulateWebData
                      ]

            DebounceMsg msg ->
                Debounce.update debounceConfig (Debounce.takeLast simulate) msg model.debounce
                    |> Response.mapModel (\childModel -> { model | debounce = childModel })

            EntitiesResult webData ->
                let
                    newIndividuals =
                        webData
                            |> RemoteData.map (initialIndividuals model.individuals)
                            |> RemoteData.withDefault model.individuals
                in
                    { model
                        | entitiesWebData =
                            webData
                                |> RemoteData.mapError (Debug.log "model.entitiesWebData Failure")
                        , individuals = newIndividuals
                    }
                        ! [ if RemoteData.isSuccess webData then
                                simulate ( model.year, newIndividuals )
                            else
                                Cmd.none
                          ]

            RemoveIndividual individualIndex ->
                let
                    newIndividuals =
                        model.individuals |> List.removeAt individualIndex
                in
                    { model | individuals = newIndividuals }
                        ! [ simulate ( model.year, newIndividuals ) ]

            ResetApplication ->
                initialModel
                    ! (Ports.writeToLocalStorage
                        { key = "display-disclaimer"
                        , value = Nothing
                        }
                        :: initialCmds model.apiBaseUrl
                      )

            SetAxisSelectedIndex individualIndex variableName selectedIndex ->
                let
                    newAxes =
                        model.axes
                            |> List.map
                                (\axis ->
                                    if
                                        (axis.individualIndex == individualIndex)
                                            && (axis.variableName == variableName)
                                    then
                                        { axis | selectedIndex = selectedIndex }
                                    else
                                        axis
                                )
                in
                    { model | axes = newAxes }
                        ! [ renderWaterfallCmd model.simulateWebData ]

            SetDisplayRoles bool ->
                { model | displayRoles = bool } ! []

            SetInputValue individualIndex variableName inputValue ->
                let
                    newIndividuals =
                        model.individuals
                            |> List.updateIfIndex ((==) individualIndex)
                                (\individual ->
                                    let
                                        newInputValues =
                                            Dict.insert variableName inputValue individual.inputValues
                                    in
                                        { individual | inputValues = newInputValues }
                                )
                in
                    Debounce.push debounceConfig ( model.year, newIndividuals ) model.debounce
                        |> Response.mapModel (\childModel -> { model | debounce = childModel })
                        |> Response.mapModel (\model -> { model | individuals = newIndividuals })

            SetRole individualIndex entityKey roleKey ->
                let
                    newIndividuals =
                        model.individuals
                            |> List.updateIfIndex ((==) individualIndex)
                                (\individual ->
                                    let
                                        newRoles =
                                            Dict.insert entityKey roleKey individual.roles
                                    in
                                        { individual | roles = newRoles }
                                )
                in
                    Debounce.push debounceConfig ( model.year, newIndividuals ) model.debounce
                        |> Response.mapModel (\childModel -> { model | debounce = childModel })
                        |> Response.mapModel (\model -> { model | individuals = newIndividuals })

            SetYear newYear ->
                Debounce.push debounceConfig ( newYear, model.individuals ) model.debounce
                    |> Response.mapModel (\childModel -> { model | debounce = childModel })
                    |> Response.mapModel (\model -> { model | year = newYear })

            Simulate ( year, individuals ) ->
                -- Do not set to Loading to be able to display previous data.
                -- See https://github.com/krisajenkins/remotedata/issues/9
                -- { model | simulateWebData = Loading }
                model
                    ! [ Requests.simulate model.apiBaseUrl individuals year model.axes
                            |> RemoteData.sendRequest
                            |> Cmd.map SimulateResult
                      ]

            SimulateResult webData ->
                { model
                    | simulateWebData =
                        webData
                            |> RemoteData.mapError (Debug.log "model.simulateWebData Failure")
                }
                    ! [ renderWaterfallCmd webData ]

            ToggleAxis individualIndex variableName checkboxChecked ->
                Debounce.push debounceConfig ( model.year, model.individuals ) model.debounce
                    |> Response.mapModel (\childModel -> { model | debounce = childModel })
                    |> Response.mapModel
                        (\model ->
                            let
                                newAxes =
                                    model.axes
                                        |> List.filterMap
                                            (\axis ->
                                                if
                                                    (axis.individualIndex == individualIndex)
                                                        && (axis.variableName == variableName)
                                                then
                                                    Nothing
                                                else
                                                    Just axis
                                            )
                                        |> List.append
                                            (if checkboxChecked then
                                                [ { count = 50
                                                  , individualIndex = individualIndex
                                                  , max = 100000
                                                  , min = 0
                                                  , selectedIndex = 0
                                                  , variableName = variableName
                                                  }
                                                ]
                                             else
                                                []
                                            )
                            in
                                { model | axes = newAxes }
                        )

            VariablesResult webData ->
                { model
                    | variablesWebData =
                        webData
                            |> RemoteData.mapError (Debug.log "model.variablesWebData Failure")
                }
                    ! []



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ viewNavBar
        , div [ class "container-fluid" ]
            ((if model.displayDisclaimer then
                [ viewDisclaimer ]
              else
                []
             )
                ++ (case RemoteData.append model.entitiesWebData model.variablesWebData of
                        NotAsked ->
                            []

                        Loading ->
                            [ p []
                                [ text "Loading data..."

                                -- TODO i18n
                                ]
                            ]

                        Failure err ->
                            [ div [ class "alert alert-danger" ]
                                -- TODO i18n
                                [ h4 [] [ text "We are sorry" ]
                                , p [] [ text "There was an error while loading data." ]
                                , p [] [ text "If you're a technical person, you can look at your browser console to see the detailed error." ]
                                ]
                            ]

                        Success ( entities, variablesResponse ) ->
                            [ div [ class "row" ]
                                (viewIndividuals model entities variablesResponse
                                    :: [ div [ class "col-sm-8" ]
                                            (case model.simulateWebData of
                                                NotAsked ->
                                                    []

                                                Loading ->
                                                    [ p []
                                                        [ text "Calculation in progress..."

                                                        -- TODO i18n
                                                        ]
                                                    ]

                                                Failure err ->
                                                    [ div [ class "alert alert-danger" ]
                                                        -- TODO i18n
                                                        [ h4 [] [ text "We are sorry" ]
                                                        , p [] [ text "There was an error while calculating result." ]
                                                        , p [] [ text "If you're a technical person, you can look at your browser console to see the detailed error." ]
                                                        ]
                                                    ]

                                                Success simulateNode ->
                                                    [ viewDecomposition model.axes simulateNode ]
                                            )
                                       ]
                                )
                            ]
                   )
                ++ [ button
                        [ class "btn btn-default pull-right"
                        , onClick ResetApplication
                        , title "Clear all custom data and make the application look like the first time it was loaded."
                        ]
                        [ text "Reset application"
                        ]
                   ]
            )
        , viewFooter
        ]


viewDecomposition : List Axis -> SimulateNode -> Html Msg
viewDecomposition axes simulateNode =
    let
        viewSimulateNode : SimulateNode -> Html Msg
        viewSimulateNode (SimulateNode fields) =
            div []
                (case getValue fields.values axes of
                    Nothing ->
                        []

                    Just value ->
                        [ text fields.name
                        , text " "
                        , samp [] [ text (toString value) ]
                        ]
                            ++ (if List.isEmpty fields.children then
                                    []
                                else
                                    [ div [ style [ ( "margin-left", "1em" ) ] ]
                                        (List.map viewSimulateNode fields.children)
                                    ]
                               )
                )
    in
        div [ class "panel panel-default" ]
            [ div [ class "panel-heading" ]
                [ h3 [ class "panel-title" ]
                    [ text "Decomposition of household income"

                    -- TODO i18n
                    ]
                ]
            , ul [ class "list-group" ]
                [ li [ class "list-group-item" ]
                    [ div [ id "waterfall" ]
                        [-- Filled by "renderWaterfall" port
                        ]
                    ]
                , li [ class "list-group-item" ]
                    [ viewSimulateNode simulateNode ]
                ]
            ]


viewDisclaimer : Html Msg
viewDisclaimer =
    div [ class "alert alert-info" ]
        [ button
            [ attribute "aria-hidden" "true"
            , class "close"
            , onClick CloseDisclaimer
            , type_ "button"
            ]
            [ text "×" ]

        -- TODO i18n (all below)
        , h4 [] [ text "À propos de cet outil" ]
        , ul []
            [ li [] [ text "OpenFisca est un simulateur socio-fiscal en cours de développement." ]
            , li [] [ text "Les résultats des simulations peuvent comporter des erreurs, et vous pouvez commencer à contribuer au projet en les faisant remonter si vous les détectez." ]
            , li [] [ text "Cet outil est un démonstrateur pour OpenFisca qui illustre ses possibilités concernant des cas types." ]
            , li [] [ text "Les données que vous saisissez ne sont jamais stockées." ]
            ]
        , p []
            [ strong [] [ text "Les résultats affichés n'ont en aucun cas un caractère officiel." ] ]
        , button
            [ class "btn btn-link"
            , onClick CloseDisclaimer
            ]
            [ text "J'ai compris, ne plus afficher" ]
        ]


viewFooter : Html msg
viewFooter =
    footer
        [ style
            [ ( "padding-top", "50px" )
            , ( "padding-bottom", "50px" )
            , ( "margin-top", "100px" )
            , ( "color", "#99979c" )
            , ( "text-align", "left" )
            , ( "background-color", "#2a2730" )
            ]
        ]
        [ div [ class "container" ]
            [ ul
                [ style
                    [ ( "padding-left", "0" )
                    , ( "margin-bottom", "20px" )
                    ]
                ]
                [ li
                    [ style [ ( "display", "inline-block" ) ] ]
                    [ a
                        [ href "https://www.openfisca.fr/"
                        , style [ ( "color", "white" ) ]
                        , target "_blank"
                        ]
                        [ text "Home"

                        -- TODO i18n
                        ]
                    ]
                , li
                    [ style [ ( "display", "inline-block" ), ( "margin-left", "15px" ) ] ]
                    [ a
                        [ href "https://twitter.com/OpenFisca"
                        , style [ ( "color", "white" ) ]
                        , target "_blank"
                        ]
                        [ text "Twitter" ]
                    ]
                , li
                    [ style [ ( "display", "inline-block" ), ( "margin-left", "15px" ) ] ]
                    -- TODO Do not hardcode URL
                    [ a
                        [ href "https://github.com/openfisca/demonstrator"
                        , style [ ( "color", "white" ) ]
                        , target "_blank"
                        ]
                        [ text "GitHub" ]
                    ]
                ]
            , p []
                [ text "Designed and built with all the love in the world by "
                , a [ href "https://twitter.com/ChristopheBenz", target "_blank" ]
                    [ text "@ChristopheBenz" ]
                , text ". Maintained by the "
                , a [ href "https://github.com/orgs/openfisca/people" ]
                    [ text "core team" ]
                , text " with the help of "
                , a [ href "https://github.com/openfisca/openfisca-france/graphs/contributors" ]
                    [ text "our contributors" ]
                , text "."
                ]
            , p []
                [ text "Code licensed "

                -- TODO Fix link
                , a
                    [ href "https://github.com/openfisca/demonstrator/blob/master/LICENSE"
                    , target "_blank"
                    , rel "license"
                    ]
                    -- TODO Fix license name
                    [ text "GNU Affero GPL version 3+" ]
                ]
            ]
        ]


viewIndividual : Model -> Entities -> VariablesResponse -> IndividualIndex -> Individual -> Html Msg
viewIndividual model entities variablesResponse individualIndex individual =
    let
        viewIndividualRoles roles =
            div []
                (roles
                    |> Dict.toList
                    |> List.filterMap
                        (\( entityKey, roleKey ) ->
                            entities.groups
                                |> List.find (\entity -> entity.key == entityKey)
                                |> Maybe.andThen
                                    (\entity ->
                                        entity.roles
                                            |> List.find (\role -> role.key == roleKey)
                                            |> Maybe.map (viewIndividualRole entities individualIndex entityKey entity)
                                    )
                        )
                )
    in
        div [ class "panel panel-default" ]
            [ div [ class "panel-heading" ]
                [ h3 [ class "panel-title" ]
                    [ text (entities.individual.label ++ " " ++ (toString (individualIndex + 1))) ]
                ]
            , ul [ class "list-group" ]
                ([ li [ class "list-group-item" ]
                    [ div []
                        ((individual.inputValues
                            |> Dict.toList
                            |> List.map
                                (\( variableName, inputValue ) ->
                                    let
                                        label =
                                            variableLabel variablesResponse.variables variableName
                                    in
                                        viewInputValue model individualIndex variableName label inputValue
                                )
                         )
                            ++ [ div [ class "input-group" ]
                                    [ input
                                        [ class "form-control"
                                        , onInput (AddVariableInput individualIndex)
                                        , placeholder "Name of the variable to add (example: statut_marital)"

                                        -- TODO i18n
                                        , type_ "text"
                                        , value
                                            (Dict.get individualIndex model.nextVariables
                                                |> Maybe.withDefault ""
                                            )
                                        ]
                                        []
                                    , span [ class "input-group-btn" ]
                                        [ button
                                            [ class "btn btn-default"
                                            , disabled
                                                (case Dict.get individualIndex model.nextVariables of
                                                    Nothing ->
                                                        -- Disable if no value is typed for this individual.
                                                        True

                                                    Just nextVariables ->
                                                        model.variablesWebData
                                                            |> RemoteData.map
                                                                (\variablesResponse ->
                                                                    case Dict.get nextVariables variablesResponse.variables of
                                                                        Nothing ->
                                                                            -- Disable if the value typed does not correspond to a valid variable name.
                                                                            -- TODO Disable if the value typed corresponds to an already used variable.
                                                                            True

                                                                        Just _ ->
                                                                            False
                                                                )
                                                            |> RemoteData.withDefault True
                                                )
                                            , onClick (AddVariable individualIndex)
                                            ]
                                            [ text "Add variable"

                                            -- TODO i18n
                                            ]
                                        ]
                                    ]
                               ]
                        )
                    ]
                 ]
                    ++ (if model.displayRoles then
                            [ li [ class "list-group-item" ]
                                [ viewIndividualRoles individual.roles ]
                            ]
                        else
                            []
                       )
                    ++ (let
                            isLastRemainingIndividual =
                                individualIndex == 0 && List.length model.individuals == 1
                        in
                            if isLastRemainingIndividual then
                                []
                            else
                                [ li [ class "list-group-item clearfix" ]
                                    [ button
                                        [ class "btn btn-default pull-right"
                                        , onClick (RemoveIndividual individualIndex)
                                        ]
                                        [ text "Remove individual"

                                        -- TODO i18n
                                        ]
                                    ]
                                ]
                       )
                )
            ]


viewIndividualRole : Entities -> IndividualIndex -> EntityKey -> GroupEntity -> Role -> Html Msg
viewIndividualRole entities individualIndex entityKey entity role =
    div [ class "form-group" ]
        [ label [] [ text entity.label ]
        , select
            [ class "form-control"
            , on "change"
                (targetValue |> Decode.map (SetRole individualIndex entityKey))
            ]
            (entity.roles
                |> List.map
                    (\role1 ->
                        option
                            [ selected (role1.key == role.key)
                            , value (role1.key)
                            ]
                            [ text role1.label ]
                    )
            )
        ]


viewIndividuals : Model -> Entities -> VariablesResponse -> Html Msg
viewIndividuals model entities variablesResponse =
    div [ class "col-sm-4" ]
        ((model.individuals
            |> List.indexedMap (viewIndividual model entities variablesResponse)
         )
            ++ [ button
                    [ class "btn btn-default"
                    , onClick (AddIndividual entities)
                    ]
                    [ text "Add an individual"

                    -- TODO i18n
                    ]
               , div [ class "checkbox" ]
                    [ label []
                        [ input
                            [ onCheck SetDisplayRoles
                            , type_ "checkbox"
                            ]
                            []
                        , text " "
                        , text "Display roles"

                        -- TODO i18n
                        ]
                    ]
               , label [ class "form-group" ]
                    [ text "Calculate for year"

                    --TODO i18n
                    , input
                        [ class "form-control"
                        , onInput (SetYear << (String.toInt >> Result.withDefault model.year))
                        , step "1"
                        , type_ "number"
                        , value (toString model.year)
                        ]
                        []
                    ]
               ]
        )


viewInputValue : Model -> IndividualIndex -> VariableName -> String -> InputValue -> Html Msg
viewInputValue model individualIndex variableName variableLabel inputValue =
    let
        axis =
            findAxis individualIndex variableName model.axes

        controlVariationCheckbox =
            div [ class "checkbox" ]
                [ label []
                    [ input
                        [ checked (axis /= Nothing)
                        , onCheck (ToggleAxis individualIndex variableName)
                        , type_ "checkbox"
                        ]
                        []
                    , text " "
                    , text "Control variation"

                    -- TODO i18n
                    ]
                ]
    in
        div [ class "form-group" ]
            ([ let
                truncatedLabel =
                    String.left 25 variableLabel
               in
                label
                    [ title variableLabel

                    -- Let the user hover the truncated label to see the full label
                    ]
                    [ text (truncatedLabel ++ "...") ]
             ]
                ++ (case inputValue of
                        BoolInputValue bool ->
                            [ text "TODO" ]

                        DateInputValue string ->
                            [ text "TODO" ]

                        EnumInputValue string ->
                            [ text "TODO" ]

                        FloatInputValue float ->
                            (case axis of
                                Nothing ->
                                    [ input
                                        [ class "form-control"
                                        , onInput
                                            (\str ->
                                                let
                                                    newInputValue =
                                                        if String.isEmpty str then
                                                            FloatInputValue 0
                                                        else
                                                            case String.toFloat str of
                                                                Ok newFloat ->
                                                                    FloatInputValue newFloat

                                                                Err _ ->
                                                                    FloatInputValue float
                                                in
                                                    SetInputValue individualIndex variableName newInputValue
                                            )
                                        , step "any"
                                        , type_ "number"
                                        , value (toString float)
                                        ]
                                        []
                                    ]

                                Just axis ->
                                    [ input
                                        [ class "form-control"
                                        , Html.Attributes.max (axis.count - 1 |> toString)
                                        , Html.Attributes.min "0"

                                        -- TODO Use Html.Extra.Event
                                        , onInput
                                            (SetAxisSelectedIndex individualIndex variableName
                                                << (String.toInt >> Result.withDefault 0)
                                            )
                                        , type_ "range"
                                        , value (toString axis.selectedIndex)
                                        ]
                                        []
                                    , samp [] [ text (toString (axisValue axis)) ]
                                    ]
                            )
                                ++ (if individualIndex == 0 && variableName == salaireVariableName then
                                        [ controlVariationCheckbox ]
                                    else
                                        []
                                   )

                        IntInputValue int ->
                            [ input
                                [ class "form-control"
                                , onInput
                                    (\str ->
                                        let
                                            newInputValue =
                                                if String.isEmpty str then
                                                    IntInputValue 0
                                                else
                                                    case String.toInt str of
                                                        Ok newInt ->
                                                            IntInputValue newInt

                                                        Err _ ->
                                                            IntInputValue int
                                        in
                                            SetInputValue individualIndex variableName newInputValue
                                    )
                                , step "1"
                                , type_ "number"
                                , value (toString int)
                                ]
                                []
                            ]

                        StringInputValue string ->
                            [ text "TODO" ]
                   )
            )


viewNavBar : Html msg
viewNavBar =
    nav [ class "navbar navbar-inverse navbar-static-top", attribute "role" "navigation" ]
        [ div [ class "container" ]
            [ div [ class "navbar-header" ]
                [ button
                    [ class "navbar-toggle"
                    , attribute "data-target" "#topbar-collapse"
                    , attribute "data-toggle" "collapse"
                    , type_ "button"
                    ]
                    [ span [ class "sr-only" ]
                        [ text "Basculer la navigation" ]
                    , span [ class "icon-bar" ]
                        []
                    , span [ class "icon-bar" ]
                        []
                    , span [ class "icon-bar" ]
                        []
                    ]
                , a [ class "navbar-brand", href "https://ui.openfisca.fr/" ]
                    -- TODO Add to program flags
                    [ text "Démonstrateur OpenFisca" ]
                ]
            , div [ class "collapse navbar-collapse", id "topbar-collapse" ]
                [ ul [ class "nav navbar-nav" ]
                    []
                , ul [ class "nav navbar-nav navbar-right" ]
                    [ li [ class "visible-xs-block" ]
                        [ a [ href "http://stats.data.gouv.fr/index.php?idSite=4" ]
                            -- TODO Add to program flags
                            [ text "Statistiques du site" ]
                        ]
                    , li [ class "visible-xs-block" ]
                        [ a [ href "https://www.openfisca.fr/mentions-legales" ]
                            -- TODO Add to program flags
                            [ text "Mentions légales" ]
                        ]

                    -- , li [ class "visible-xs-block" ] -- TODO
                    --     [ a [ href "/privacy-policy" ]
                    --         [ text "Politique de confidentialité" ]
                    --     ]
                    -- , li [ class "dropdown" ] -- TODO
                    --     [ a [ class "dropdown-toggle", attribute "data-toggle" "dropdown", href "#" ]
                    --         [ text "Français "
                    --         , span [ class "caret" ]
                    --             []
                    --         ]
                    --     , ul [ class "dropdown-menu", attribute "role" "menu" ]
                    --         [ li [ class "dropdown-header", attribute "role" "presentation" ]
                    --             [ text "France" ]
                    --         , li []
                    --             [ a [ href "/" ]
                    --                 [ text "Français" ]
                    --             ]
                    --         , li []
                    --             [ a [ href "/en" ]
                    --                 [ text "English" ]
                    --             ]
                    --         ]
                    --     ]
                    ]
                ]
            ]
        ]
