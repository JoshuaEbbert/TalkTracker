module TalkTracker exposing (..)

import Html exposing (..)
import Html.Attributes exposing (id, class, classList, src, name, type_, title, href, style, required, value, defaultValue, alt, width, height, action, target, placeholder, required)
import Html.Events exposing (on, targetValue)
import Parser exposing (run, int)
import Json.Decode as Json
import Array exposing (initialize, fromList, toList)
import Bootstrap.Form exposing (help)
import Regex exposing (Regex)
import String.Case exposing (toCamelCaseLower)

{-- preSpeakers is a list containing the input fields that will be rendered before the speaker fields. --}
preSpeakers : List String
preSpeakers =  [ "Number of Speakers", "Total Special Musical Numbers", "Ward", "Date", "Time", "Conducting", "Organist", "Chorister", "Opening Hymn", "Invocation", "Sacrament Hymn" ]


{-- postSpeakers is a list containing the input fields that will follow the speaker fields --}
postSpeakers : List String
postSpeakers = [ "Closing Hymn", "Benediction" ]


onBlurWithTargetValue : (String -> msg) -> Attribute msg 
onBlurWithTargetValue tagger = 
    on "blur" (Json.map tagger targetValue)


view : Model -> Html Msg
view model =
    div [] 

    {-- Navbar (sit on top) --}
        [ div [ class "top" ] 
              [ div [ class "bar white wide padding card" ] 
                    [ a [ href "#home", class "bar-item button"]
                        [ b [] [ text "Talk" ], text "Tracker"
                        ]
                {-- Float links to the right. Hide them on small screens --}
                    , div [ class "right hide-small" ]
                          [ a [ href "#sacrament-meeting-program", class "bar-item button" ] [ text "Program" ]
                          , a [ href "#about", class "bar-item button" ] [ text "About" ]
                          , a [ href "#contact", class "bar-item button" ] [ text "Contact" ]
                          ]
                    ]
              ]

    {-- Header --}
        , header [ class "display-container content wide", id "home", style [ ("max-width", "1500px") ] ] 
                 [ img [ class "image", src "http://travelhdwallpapers.com/wp-content/uploads/2014/02/SaltLake-Temple-8.jpg", alt "Salt Lake City Temple", style [ ("width", "100%"), ("height", "800") ] ] []
                 , div [ class "display-middle margin-top center" ] 
                       [ h1 [ class "xxlarge text-white" ] 
                            [ span [ class "padding black opacity-min" ] [ b [] [ text "Talk" ] ]      
                            , span [ class "hide-small text-light-grey" ] [ text "Tracker" ]
                            ]
                       ]
                 ]

    {-- Page Content --}
        , div [ class "content padding", style [ ("max-width", "1564px") ] ]

        {-- Sacrament Meeting Program Section --}
              [ div [ class "container padding-32", id "sacrament-meeting-program" ] [ h3 [ class "border-bottom border-light-grey padding-16" ] [ text "Sacrament Meeting Program" ] ]
              , div [ class "row-padding" ] []
{--              , input [ class "input section border", id "numSpeakerInput", placeholder "Number of Speakers (e.g. 4)", onBlurWithTargetValue (parseInt SetNumSpeakers) ] []
              , input [ class "input section border", id "numSpcMusicInput", placeholder "Total Special Musical Numbers (e.g. 1)", onBlurWithTargetValue (parseInt SetNumMusic) ] []  --}
              , form [] ( model.numSpeakers
                            |> speakerList
                            |> (\list -> List.concat [ preSpeakers, list, ( specialMusicNumberList model.numSpcMusic ), postSpeakers ] )
                            |> List.map viewInputField )

                        
              , button [ class "button black section" ] [ i [ class "fa fa-paper-plane" ] [ text "SUBMIT" ] ]

        {-- About Section --}
              , div [ class "container padding-32", id "about" ]
                    [ h3 [ class "border-bottom border-light-grey padding-16" ] [ text "About" ] 
                    , p [] [ text "Fill me with information about TalkTracker!" ]
                    ]

        {-- Contact Section --}        
              , div [ class "container padding-32", id "contact" ] 
                    [ h3 [ class "border-bottom border-light-grey padding-16" ] [ text "Contact" ]
                    , p [] [ text "For help fill out the form below." ]
                    , form [ action "/action_page.php", target "_blank" ]
                           [ input [ class "input border", placeholder "Name", name "Name" ] []
                           , input [ class "input section border", placeholder "Email", name "Email" ] []
                           , input [ class "input section border", placeholder "Subject", name "Subject" ] []
                           , input [ class "input section border", placeholder "Comment", name "Comment" ] []
                           , button [ class "button black section" ] [ i [ class "fa fa-paper-plane" ] [ text "SEND MESSAGE" ] ]
                           ]
                    ]

    {-- End page content --}
              ]

    {-- Footer --}
        , footer [ class "center black padding-16" ] [ p [] [ text "Created by Joshua Ebbert" ] ]
        ]


{-- Functions used for input fields --}
viewInputField : String -> Html Msg
viewInputField name =
    div [] [ input [ class "input section border", placeholder name, required True, id (nameToId name), onChange (validateField name) ] [] 
        , help [] [ helpText name ]
        ]


helpText : Html msg
helpText name = 
    let
        name = toCamelCaseLower name
    in
        text model.help.name


validateField name value = 
    case name of 
        "Number of Speakers" ->
            case (run int value) of
                Ok num ->
                    SetNumSpeakers (Ok num)

                Err _ ->
                    SetNumSpeakers (Err "Please enter a whole number (e.g. 4)")

        "Total Special Musical Numbers" ->
            case (run int value) of
                Ok num ->
                    SetNumMusic (Ok num)

                Err _ ->
                    SetNumMusic (Err "Please enter a whole number (e.g. 4)")

        "Ward" ->
            case value of
                "" ->
                    SetWard (Err "Please enter a Ward")

                _ ->
                    SetWard (Ok value)

        "Date" ->
            case value of 
                Regex "\\d\\d/\\d\\d/\\d\\d" ->
                    SetDate (Ok value)

                _ -> 
                    SetDate (Err "Please enter a valid date (e.g. 04/09/18)")
        "Time" ->
            case value of
                Regex "\\d\\d:\\d\\d" ->
                    SetTime (Ok value)

                _ ->
                    SetTime (Err "Please enter a valid time (e.g. 12:00 a.m.)")

        "Conducting" ->
            case value of
                "" ->
                    SetConducting (Err "Please enter the name of the person who will be conducting")

                _ ->
                    SetConducting (Ok value)

        "Organist" ->
            case value of
                "" ->
                    SetOrganist (Err "Please enter an organist")

                _ ->
                    SetOrganist (Ok value)

        "Chorister" ->
            case value of
                "" ->
                    SetChorister (Err "Please enter a chorister")

                _ ->
                    SetChorister (Ok value)

        "Opening Hymn" ->
            case value of
                "" ->
                    SetOpeningHymn (Err "Please enter an opening hymn")

                _ ->
                    SetOpeningHymn (Ok value)

        "Invocation" ->
            case value of
                "" ->
                    SetInvocation (Err "Please enter a name for the invocation")

                _ ->
                    SetInvocation (Ok value)

        "Sacrament Hymn" ->
            case value of
                "" ->
                    SetSacramentHymn (Err "Please enter a sacrament hymn")

                _ ->
                    SetSacramentHymn (Ok value)

        "Closing Hymn" ->
            case value of
                "" ->
                    SetClosingHymn (Err "Please enter a closing hymn")

                _ ->
                    SetClosingHymn (Ok value)

        "Benediction" ->
            case value of
                "" ->
                    SetBenediction (Err "Please enter a name for the benediction")

                _ ->
                    SetBenediction (Ok value)

        {-- How will I account for the speaker and special musical number inputs? --}
        _ -> SetSpeakerAndSpcMusic


speakerList : Int -> List String 
speakerList numSpeakers = 
    Array.toList <| initialize numSpeakers (\int -> "Speaker#" ++ toString (int + 1))


specialMusicNumberList : Int -> List String
specialMusicNumberList numSpcMusic = 
    Array.toList <| initialize numSpcMusic (\int -> "Special Music Number#" ++ toString (int + 1))


nameToId : String -> String
nameToId name = 
    name
        |> String.toLower 
        |> String.split " " 
        |> String.join "-"
        |> (\string -> string ++ "-input")


parseInt : ( (Result Parser.Error Int) -> Msg ) -> String -> Msg
parseInt msg string = 
    let 
        parseResult = run int string
    in
        msg parseResult


fixSpeakersRecord : Int -> Array.Array { int : String }
fixSpeakersRecord numSpeakers = 
    initialize numSpeakers ((+) 1 ) 
        |> toList
        |> (List.map (\int -> { int = "" } ) )
        |> fromList


fixSpcMusicalNumbersRecord : Int -> Array.Array { int : String }
fixSpcMusicalNumbersRecord numSpcMusic = 
    initialize numSpcMusic ((+) 1 ) 
        |> toList
        |> (List.map (\int -> { int = "" } ) )
        |> fromList


toRecord : a -> { int : String }
toRecord int =  { int = "" }

{-- Backup
toRecord numSpeakers int = 
    if (numSpeakers == 0) then {} else
        case int of
            numSpeakers -> { numSpeakers = ""
                        , toRecord numSpeakers (int - 1)
                        }

            1 -> 1 = ""

            int -> int = "", toRecord numSpeakers (int - 1)   
--}

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model = 
    case msg of
        SetNumSpeakers (Ok numSpeakers) ->
            ( { model | numSpeakers = numSpeakers, speakers = fixSpeakersRecord numSpeakers, help.numSpeakers = "" }, Cmd.none )
        
        SetNumSpeakers (Err msg) ->
            ( { model | help.numSpeakers = msg }, Cmd.none)

        SetNumMusic (Ok numSpcMusic) ->
            ( { model | numSpcMusic = numSpcMusic, spcMusicalNumbers = fixSpcMusicalNumbersRecord numSpcMusic, help.numSpcMusic = "" }, Cmd.none )
        
        SetNumMusic (Err msg) ->
            ( { model | help.numSpcMusic = msg }, Cmd.none)

        SetWard (Ok value) ->
            ( { model | ward = value, help.ward = "" }, Cmd.none )

        SetWard (Err msg) ->
            ( { model | help.ward = msg }, Cmd.none )

        SetDate (Ok value) ->
            ( { model | date = value, help.date = "" }, Cmd.none )

        SetDate (Err msg) ->
            ( { model | help.date = msg }, Cmd.none )

        SetTime (Ok value) ->
            ( { model | time = value, help.time = "" }, Cmd.none )

        SetTime (Err msg) ->
            ( { model | help.time = msg }, Cmd.none )

        SetConducting (Ok value) ->
            ( { model | conducting = value, help.conducting = "" }, Cmd.none )

        SetConducting (Err msg) ->
            ( { model | help.conducting = msg }, Cmd.none )

        SetOrganist (Ok value) ->
            ( { model | organist = value, help.organist = "" }, Cmd.none )

        SetOrganist (Err msg) ->
            ( { model | help.organist = msg }, Cmd.none )

        SetChorister (Ok value) ->
            ( { model | chorister = value, help.chorister = "" }, Cmd.none )

        SetChorister (Err msg) ->
            ( { model | help.chorister = msg }, Cmd.none )

        SetOpeningHymn (Ok value) ->
            ( { model | openingHymn = value, help.openingHymn = "" }, Cmd.none )

        SetOpeningHymn (Err msg) ->
            ( { model | help.openingHymn = msg }, Cmd.none )

        SetInvocation (Ok value) ->
            ( { model | invocation = value, help.invocation = "" }, Cmd.none )

        SetInvocation (Err msg) ->
            ( { model | help.invocation = msg }, Cmd.none )

        SetSacramentHymn (Ok value) ->
            ( { model | sacramentHymn = value, help.sacramentHymn = "" }, Cmd.none )

        SetSacramentHymn (Err msg) ->
            ( { model | help.sacramentHymn = msg }, Cmd.none )

        SetSpeakersAndSpcMusic ->
            ( model, Cmd.none )

        SetClosingHymn (Ok value) ->
            ( { model | closingHymn = value, help.closingHymn = "" }, Cmd.none )

        SetClosingHymn (Err msg) ->
            ( { model | help.closingHymn = msg }, Cmd.none )

        SetBenediction (Ok value) ->
            ( { model | benediction = value, help.benediction = "" }, Cmd.none )

        SetBenediction (Err msg) ->
            ( { model | help.benediction = msg }, Cmd.none )

        


type Msg
    = SetNumSpeakers (Result (Err msg) (Ok value) )
    | SetNumMusic (Result (Err msg) (Ok value) )
    | SetWard (Result (Err msg) (Ok value) )
    | SetDate (Result (Err msg) (Ok value) )
    | SetTime (Result (Err msg) (Ok value) )
    | SetConducting (Result (Err msg) (Ok value) )
    | SetOrganist (Result (Err msg) (Ok value) )
    | SetChorister (Result (Err msg) (Ok value) )
    | SetOpeningHymn (Result (Err msg) (Ok value) )
    | SetInvocation (Result (Err msg) (Ok value) )
    | SetSacramentHymn (Result (Err msg) (Ok value) )
    | SetSpeakersAndSpcMusic
    | SetClosingHymn (Result (Err msg) (Ok value) )
    | SetBenediction (Result (Err msg) (Ok value) )


initialModel : Model
initialModel = { numSpeakers = 0
               , numSpcMusic = 0 
               , ward = "No Input Yet"
               , date = "No Input Yet"
               , time = "No Input Yet"
               , conducting = "No Input Yet"
               , organist = "No Input Yet"
               , chorister = "No Input Yet"
               , openingHymn = "No Input Yet"
               , invocation = "No Input Yet"
               , sacramentHymn = "No Input Yet"
               , speakers = fixSpeakersRecord 0
               , spcMusicalNumbers = fixSpcMusicalNumbersRecord 0
               , closingHymn = "No Input Yet"
               , benediction = "No Input Yet"
               , help = { numSpeakers = ""
                        , numSpcMusic = ""
                        , ward = ""
                        , date = ""
                        , time = ""
                        , conducting = ""
                        , organist = ""
                        , chorister = ""
                        , openingHymn = ""
                        , invocation = ""
                        , sacramentHymn = ""
                        , speakers = ""
                        , spcMusicalNumbers = ""
                        , closingHymn = ""
                        , benediction = ""
                        }
               }


type alias Model = { numSpeakers : Int
                   , numSpcMusic : Int 
                   , ward : String
                   , date : String
                   , time : String
                   , conducting : String
                   , organist : String
                   , chorister : String
                   , openingHymn : String
                   , invocation : String
                   , sacramentHymn : String
                   , speakers : Array.Array { int : String } 
                   , spcMusicalNumbers : Array.Array { int : String }
                   , closingHymn : String
                   , benediction : String
                   , help = { numSpeakers = String
                        , numSpcMusic = String
                        , ward = String
                        , date = String
                        , time = String
                        , conducting = String
                        , organist = String
                        , chorister = String
                        , openingHymn = String
                        , invocation = String
                        , sacramentHymn = String
                        , speakers = String
                        , spcMusicalNumbers = String
                        , closingHymn = String
                        , benediction = String
                        }
                   }


main : Program Never Model Msg
main = 
    Html.program
        { init = ( initialModel, Cmd.none )
        , view = view
        , update = update
        , subscriptions = (\_ -> Sub.none)
        }
