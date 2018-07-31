module TalkTracker exposing (..)

import Html exposing (..)
import Html.Attributes exposing (id, class, classList, src, name, type_, title, href, style, required, value, defaultValue, alt, width, height, action, target, placeholder, required)
import Html.Events exposing (on, targetValue, onInput)
import Parser exposing (run, int)
import Json.Decode as Json
import Array exposing (initialize, fromList, toList, set)
import Bootstrap.Form exposing (help)
import Regex exposing (regex, contains, find)


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
                            |> List.map (viewInputField model) )

                        
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
viewInputField : Model -> String -> Html Msg
viewInputField model name =
    div [] [ input [ class "input section border", placeholder name, required True, id (nameToId name), onInput (validateField name) ] [] 
        , help [] [ helpText model name ]
        ]


helpText : Model -> String -> Html msg
helpText model name = 
    case matchHelper name of
        Speaker ->
            model.help.speakers

        SpecialMusicalNumber ->
            model.help.spcMusicalNumbers

        Other ->
            case name of
                "Number of Speakers" ->
                    model.help.numSpeakers
            
                "Total Special Musical Numbers" ->
                    model.help.numSpcMusic

                "Ward" ->
                    model.help.ward

                "Date" ->
                    model.help.date

                "Time" ->
                    model.help.time

                "Conducting" ->
                    model.help.conducting

                "Organist" ->
                    model.help.organist

                "Chorister" ->
                    model.help.chorister

                "Opening Hymn" ->
                    model.help.openingHymn

                "Invocation" ->
                    model.help.invocation

                "Sacrament Hymn" ->
                    model.help.sacramentHymn

                "Closing Hymn" ->
                    model.help.closingHymn

                "Benediction" ->
                    model.help.benediction

                _ ->
                    "No help message for this field"


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
                "12/12/18" ->                       {-- I will work with the RegEx later --}
                    SetDate (Ok value)

                _ -> 
                    SetDate (Err "Please enter a valid date (e.g. 04/09/18)")
        "Time" ->
            case value of
                "12:00" ->
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

        _ -> 
            case matchHelper name of
                Speaker ->
                    if (value == "") then 
                        SetSpeaker (Err "Please enter a name")
                    else if (findInt name == Nothing) then 
                        SetSpeaker (Err "There has been an error with the form. Please reload the page")
                    else 
                        case run int (findInt name) of
                            Ok num ->
                                SetSpeaker ( Ok (num, value) )

                            Err _ ->
                                SetSpeaker ( Err "There has been an error with the input. Please reload the page")

                SpecialMusicalNumber ->
                    if (value == "") then SetSpecialMusicalNumber (Err "Please enter a name")
                    else if (findInt name == Nothing) then SetSpecialMusicalNumber (Err "There has been an error with the form. Please reload the page")
                    else SetSpecialMusicalNumber ( Ok ( findInt name, value ) )

                Other ->
                    MsgError


matchHelper string = 
    if contains speakerRegEx string then Speaker 
    else if contains spcMusicNumRegEx string then SpecialMusicalNumber
    else Other


findInt string = 
    let 
        maybeMatch = 
            string  
                |> find Regex.All (regex "\\d+")
                |> List.head
    in  
        case maybeMatch of
            Just record ->
                record.match
            
            Nothing ->
                Nothing


speakerRegEx = regex "^(Speaker#\\d+)$"

spcMusicNumRegEx = regex "^(Special\\sMusical\\sNumber#\\d+)$"

{--dateRegEx = regex "(^(((0[1-9]|1[0-9]|2[0-8])[\/](0[1-9]|1[012]))|((29|30|31)[\/](0[13578]|1[02]))|((29|30)[\/](0[4,6,9]|11)))[\/](19|[2-9][0-9])\\d\\d$)|(^29[\/]02[\/](19|[2-9][0-9])(00|04|08|12|16|20|24|28|32|36|40|44|48|52|56|60|64|68|72|76|80|84|88|92|96)$)" --}

speakerList : Int -> List String 
speakerList numSpeakers = 
    Array.toList <| initialize numSpeakers (\int -> "Speaker#" ++ toString (int + 1))


specialMusicNumberList : Int -> List String
specialMusicNumberList numSpcMusic = 
    Array.toList <| initialize numSpcMusic (\int -> "Special Musical Number#" ++ toString (int + 1))


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

{-- help set functions --}

setHelpNumSpeakers help value = 
    { help | numSpeakers = value }

setHelpNumSpcMusic help value = 
    { help | numSpcMusic = value }

setHelpWard help value = 
    { help | ward = value }

setHelpDate help value = 
    { help | date = value } 

setHelpTime help value = 
    { help | time = value }

setHelpConducting help value = 
    { help | conducting = value } 

setHelpOrganist help value = 
    { help | organist = value }

setHelpChorister help value = 
    { help | chorister = value }

setHelpOpeningHymn help value = 
    { help | openingHymn = value }

setHelpInvocation help value = 
    { help | invocation = value }

setHelpSacramentHymn help value = 
    { help | sacramentHymn = value }

setHelpClosingHymn help value = 
    { help | closingHymn = value }

setHelpBenediction help value = 
    { help | benediction = value }

setHelpSpeakers help value = 
    { help | speakers = value }

setHelpSpcMusicalNumbers help value = 
    { help | speakers = value }

-- End set help functions


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model = 
    case msg of
        SetNumSpeakers (Ok numSpeakersInput) ->
            ( { model | numSpeakers = numSpeakersInput, speakers = fixSpeakersRecord numSpeakersInput, help = setHelpNumSpeakers model.help "" }, Cmd.none )
        
        SetNumSpeakers (Err helpMsg) ->
            ( { model | help = setHelpNumSpeakers model.help helpMsg }, Cmd.none)

        SetNumMusic (Ok numSpcMusicInput) ->
            ( { model | numSpcMusic = numSpcMusicInput, spcMusicalNumbers = fixSpcMusicalNumbersRecord numSpcMusicInput, help = setHelpNumSpcMusic model.help "" }, Cmd.none )
        
        SetNumMusic (Err helpMsg) ->
            ( { model | help = setHelpNumSpcMusic model.help helpMsg }, Cmd.none)

        SetWard (Ok value) ->
            ( { model | ward = value, help = setHelpWard model.help "" }, Cmd.none )

        SetWard (Err helpMsg) ->
            ( { model | help = setHelpWard model.help helpMsg }, Cmd.none )

        SetDate (Ok value) ->
            ( { model | date = value, help = setHelpDate model.help "" }, Cmd.none )

        SetDate (Err helpMsg) ->
            ( { model | help = setHelpDate model.help helpMsg }, Cmd.none )

        SetTime (Ok value) ->
            ( { model | time = value, help = setHelpTime model.help "" }, Cmd.none )

        SetTime (Err helpMsg) ->
            ( { model | help = setHelpTime model.help helpMsg }, Cmd.none )

        SetConducting (Ok value) ->
            ( { model | conducting = value, help = setHelpConducting model.help "" }, Cmd.none )

        SetConducting (Err helpMsg) ->
            ( { model | help = setHelpConducting model.help helpMsg }, Cmd.none )

        SetOrganist (Ok value) ->
            ( { model | organist = value, help = setHelpOrganist model.help "" }, Cmd.none )

        SetOrganist (Err helpMsg) ->
            ( { model | help = setHelpOrganist model.help helpMsg }, Cmd.none )

        SetChorister (Ok value) ->
            ( { model | chorister = value, help = setHelpChorister model.help "" }, Cmd.none )

        SetChorister (Err helpMsg) ->
            ( { model | help = setHelpChorister model.help helpMsg }, Cmd.none )

        SetOpeningHymn (Ok value) ->
            ( { model | openingHymn = value, help = setHelpOpeningHymn model.help "" }, Cmd.none )

        SetOpeningHymn (Err helpMsg) ->
            ( { model | help = setHelpOpeningHymn model.help helpMsg }, Cmd.none )

        SetInvocation (Ok value) ->
            ( { model | invocation = value, help = setHelpInvocation model.help "" }, Cmd.none )

        SetInvocation (Err helpMsg) ->
            ( { model | help = setHelpInvocation model.help helpMsg }, Cmd.none )

        SetSacramentHymn (Ok value) ->
            ( { model | sacramentHymn = value, help = setHelpSacramentHymn model.help "" }, Cmd.none )

        SetSacramentHymn (Err helpMsg) ->
            ( { model | help = setHelpSacramentHymn model.help helpMsg }, Cmd.none )

        SetClosingHymn (Ok value) ->
            ( { model | closingHymn = value, help = setHelpClosingHymn model.help "" }, Cmd.none )

        SetClosingHymn (Err helpMsg) ->
            ( { model | help = setHelpClosingHymn model.help helpMsg }, Cmd.none )

        SetBenediction (Ok value) ->
            ( { model | benediction = value, help = setHelpBenediction model.help "" }, Cmd.none )

        SetBenediction (Err helpMsg) ->
            ( { model | help = setHelpBenediction model.help helpMsg }, Cmd.none )

        SetSpeaker (Ok (speakerNum, name)) ->
            ( { model | speakers = set (speakerNum - 1) name model.speakers, help = setHelpSpeakers model.help "" }, Cmd.none )

        SetSpeaker (Err helpMsg) ->
            ( { model | help = setHelpSpeakers model.help helpMsg }, Cmd.none ) {-- Maybe redo speakers field of help...? --}

        SetSpecialMusicalNumber (Ok (spcMusicalNum, name)) ->
            ( { model | spcMusicalNumbers = set (spcMusicalNum - 1) name model.spcMusicalNumbers, help = setHelpSpcMusicalNumbers model.help "" }, Cmd.none )

        SetSpecialMusicalNumber (Err helpMsg) ->
            ( { model | help = setHelpSpcMusicalNumbers model.help helpMsg }, Cmd.none )

        MsgError ->
            ( model, Cmd.none )


type MatchResult 
    = Speaker
    | SpecialMusicalNumber
    | Other

        
type Value 
    = Int
    | String


type Msg
    = SetNumSpeakers (Result String Int)
    | SetNumMusic (Result String Int) 
    | SetWard (Result String String)
    | SetDate (Result String String)
    | SetTime (Result String String) 
    | SetConducting (Result String String)
    | SetOrganist (Result String String)
    | SetChorister (Result String String)
    | SetOpeningHymn (Result String String)
    | SetInvocation (Result String String)
    | SetSacramentHymn (Result String String)
    | SetClosingHymn (Result String String)
    | SetBenediction (Result String String)
    | SetSpeaker (Result (String, String) String)
    | SetSpecialMusicalNumber (Result (String, String) String)
    | MsgError


type alias Help = { numSpeakers : String
                    , numSpcMusic : String
                    , ward : String
                    , date : String
                    , time : String
                    , conducting : String
                    , organist : String
                    , chorister : String
                    , openingHymn : String
                    , invocation : String
                    , sacramentHymn : String
                    , speakers : String
                    , spcMusicalNumbers : String
                    , closingHymn : String
                    , benediction : String
                    }


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
                   , help : Help
                   }


main : Program Never Model Msg
main = 
    Html.program
        { init = ( initialModel, Cmd.none )
        , view = view
        , update = update
        , subscriptions = (\_ -> Sub.none)
        }
