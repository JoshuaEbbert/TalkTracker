module TalkTracker exposing (..)

import Html exposing (..)
import Html.Attributes exposing (id, class, classList, src, name, type_, title, href, style, required, value, defaultValue, alt, width, height, action, target, placeholder, required)
import Html.Events exposing (on, targetValue)
import Parser exposing (run, int)
import Json.Decode as Json
import Array exposing (initialize, fromList, toList)

{-- preSpeakers is a list containing the input fields that will be rendered before the speaker fields. --}
preSpeakers : List String
preSpeakers =  [ "Ward", "Date", "Time", "Conducting", "Organist", "Chorister", "Opening Hymn", "Invocation", "Sacrament Hymn" ]


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
              , input [ class "input section border", id "numSpeakerInput", placeholder "Number of Speakers (e.g. 4)", onBlurWithTargetValue (parseInt SetNumSpeakers) ] []
              , input [ class "input section border", id "numSpcMusicInput", placeholder "Total Special Musical Numbers (e.g. 1)", onBlurWithTargetValue (parseInt SetNumMusic) ] []
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


{-- Functions used to render inputs --}
viewInputField : String -> Html Msg
viewInputField name =
    div [] [ input [ class "input section border", placeholder name, required True, id (nameToId name) ] [] ]


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
            ( { model | numSpeakers = numSpeakers, speakers = fixSpeakersRecord numSpeakers }, Cmd.none )
        
        SetNumSpeakers (Err _) ->
            (model, Cmd.none)

        SetNumMusic (Ok numSpcMusic) ->
            ( { model | numSpcMusic = numSpcMusic, spcMusicalNumbers = fixSpcMusicalNumbersRecord numSpcMusic }, Cmd.none )
        
        SetNumMusic (Err _) ->
            (model, Cmd.none)


type Msg
    = SetNumSpeakers (Result Parser.Error Int)
    | SetNumMusic (Result Parser.Error Int)


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
                   }


main : Program Never Model Msg
main = 
    Html.program
        { init = ( initialModel, Cmd.none )
        , view = view
        , update = update
        , subscriptions = (\_ -> Sub.none)
        }
