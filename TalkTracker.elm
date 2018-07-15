module TalkTracker exposing (..)

import Html exposing (..)
import Html.Attributes exposing (id, class, classList, src, name, type_, title, href, style, alt, width, height, action, target, placeholder, required)

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
                 [ img [ class "image", src "https://orig00.deviantart.net/2a24/f/2015/091/7/6/no_more_art___salt_lake_city__utah_temple_by_0nuku-d8nxvhr.jpg", alt "Salt Lake City Temple", style [ ("width", "100%"), ("height", "800") ] ] []
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
              , form [] ( List.map viewInputField model.inputFields )
              , button [ class "button black section" ] [ i [ class "fa fa-paper-plane" ] [ text "SUBMIT" ] ]

        {-- About Section --}
              , div [ class "container padding-32", id "about" ]
                    [ h3 [ class "border-bottom border-light-grey padding-16" ] [ text "About" ] 
                    , p [] [ text "I'm a paragraph! Fill me with information about you and why you made this website!" ]
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




viewInputField : String -> Html Msg
viewInputField name =
    div [] [ input [ class "input section border", placeholder name, id (nameToId name) ] [] ]


nameToId : String -> String
nameToId name = 
    name
        |> String.toLower 
        |> String.split " " 
        |> String.join "-"
        |> (\string -> string ++ "-input")


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model = 
    case msg of
        _ -> 
            ( model, Cmd.none )


type Msg
    = NumSpeakers Int


initialModel : Model
initialModel = { inputFields = [ "Ward", "Date", "Time", "Conducting", "Organist", "Chorister", "Opening Hymn", "Invocation", "Sacrament Hymn", "First Speaker", "Second Speaker", "Third Speaker", "Closing Hymn", "Benediction" ] }


type alias Model = { inputFields : List String }


main : Program Never Model Msg
main = 
    Html.program
        { init = ( initialModel, Cmd.none )
        , view = view
        , update = update
        , subscriptions = (\_ -> Sub.none)
        }

