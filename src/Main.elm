module Main exposing (..)

-- Press buttons to increment and decrement a counter.
--
-- Read how it works:
--   https://guide.elm-lang.org/architecture/buttons.html
--

import Browser exposing (..)
import Css exposing (..)
import Html
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (class, css, href, src)
import Html.Styled.Events exposing (onClick)
import Model exposing (..)
import Svg.Styled.Attributes exposing (begin)
import Svg.Styled.Attributes exposing (direction)
import Array
import List exposing (foldl)
import List exposing (length)


-- MAIN


main : Program () Model Msg
main =  Browser.sandbox
    { view = view >> toUnstyled
    , update = update
    , init = init}
--   Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias Model = Model.Model


init : Model
init = Model.Model (StateConfig DEFAULT NoSound) 
    (Toolbar [Ton, Tin, Tch, SomeTch] NoSound) 
    (TrackModel "berimbau" (List.repeat 8 emptyBeat))



-- UPDATE


type Msg
  = AddSound Int Int
  | CnahgeAssets AssetStyle
  | SetActiveSound Sound
  | AddLine
  | RemoveLine

setSound : Sound -> Int -> Int -> List Beat -> List Beat
setSound snd beat idx beats = 
    let
        inner cur beatss = if (cur == beat) then 
                case beatss of
                ((BerimbauBeat x)::xs) -> case idx of
                    0 -> (BerimbauBeat {x | beat = snd}) :: xs
                    1 -> (BerimbauBeat {x | firstOf3 = snd}) :: xs
                    2 -> (BerimbauBeat {x | firstOf2 = snd}) :: xs
                    3 -> (BerimbauBeat {x | secondOf3 = snd}) :: xs
                    4 -> (BerimbauBeat {x | secondOf2 = snd}) :: xs
                    5 -> (BerimbauBeat {x | thirdOf3 = snd}) :: xs
                    _ -> beatss
                [] -> []
                else case beatss of
                    (x :: xs) -> x :: (inner (cur + 1) xs)
                    [] -> []
    in inner 0 beats

update : Msg -> Model -> Model
update msg model = case msg of
    SetActiveSound sound -> 
        let
            toolbar = model.toolbar
            newToolbar = {toolbar| sound = sound}
        in {model | toolbar = newToolbar}
    AddSound beat snd -> 
        let
            track = model.track
            new_track = {track| beats = setSound model.toolbar.sound beat snd track.beats}
        in {model| track = new_track}
    AddLine -> 
        let
            track = model.track
            new_track = {track | beats = (track.beats ++ (List.repeat 4 emptyBeat))}
        in  {model | track = new_track}
    RemoveLine -> 
        let
            track = model.track
            new_track = {track | beats = List.take (length track.beats - 4) track.beats}
        in  if (length model.track.beats <= 4) then model else {model | track = new_track}
    _ -> model



-- VIEW

soundBorder : Model -> Sound -> Style
soundBorder model sound = if (sound == model.toolbar.sound) then (border3 (px 5) solid (hex "E32636")) else (border3 (px 3) solid (hex "060606"))

soundSelectorMeessage : Model -> Sound -> Msg
soundSelectorMeessage model sound = if (model.toolbar.sound == sound) then SetActiveSound NoSound else SetActiveSound sound

indexedList : List a -> List (Int, a)
indexedList = 
    let
        inner : Int -> List a -> List (Int, a)
        inner idx xs = case xs of
            [] -> []
            (x :: xss) -> ((idx, x) :: (inner (idx + 1) xss))
    in inner 0

lineAdders : Html Msg
lineAdders = div [css [horizontalContainer, alignSelf right, marginLeft auto, marginRight (px 10)]] 
                        [ div [onClick RemoveLine, css [width (px 50), height (px 50), backgroundColor (hex "#e78284"), alignItems middle, justifyContent center]] [text "-"]
                        , div [onClick AddLine, css [width (px 50), height (px 50), backgroundColor (hex "#a6d189")]] [text "+"]]

view : Model -> Html Msg
view model = div [css [width (px 1000), padding (px 50), alignSelf center, alignItems center]]
            [ div [css [height (px 70), maxWidth (px 800), width (pct 100), padding (px 10), display inlineBlock, borderedBox, alignItems center]] 
                ((List.map (\sound -> div [ css 
                                            [ width (px 50)
                                            , height (px 50)
                                            , borderedBox
                                            , (soundBorder model sound)]
                                        , onClick (soundSelectorMeessage model sound)] [renderSound sound]) model.toolbar.sounds) ++ [lineAdders])
            , div [css 
                [ width (px 900)
                , height (pct 100)
                , display inlineBlock
                , alignItems flexEnd
                , margin2 auto auto
                , alignSelf center]
                ] 
                (List.map 
                    (\xs -> div [ css [ horizontalContainer
                                      , float left, margin (px 10)]
                                ] 
                                (List.map  (\(idx, beat) -> renderBeat idx beat) xs)
                    ) (partitionBy 4 (indexedList model.track.beats)))]
--   div [class "app"
--     [ div [class "toolbar"] [ text "-" ]
--     , div [class "tracks"] (List.map trackHtml model.tracks) 
--     ]

partitionBy: Int -> List a -> List (List a)
partitionBy sz xs = case xs of
    [] -> []
    (_ :: _) -> List.take sz xs :: partitionBy sz (List.drop sz xs) 

-- trackHtml : TrackModel -> Html Msg
-- trackHtml track = 
--     div [class "track-holder"] 
--     [ div [class "track-descriptor"] [text track.instrument]
--     , div [class "beats-holder"] (List.map renderBeat track.beats)
--     ]

renderBeat : Int -> Beat -> Html Msg
renderBeat idx (BerimbauBeat beat) = 
    div [class "horizontal-container", css [horizontalContainer, width (px 200), height (px 100), margin2 (px 0) (px -1)]]
        [ --(text (String.fromInt idx)),
        div [class "strong-beat", css [ width (pct 25)
                                      , height (pct 100)
                                      , backgroundColor (hex "#e78284")
                                      , borderedBox, alignItems end]
                                      , onClick (AddSound idx 0)] 
                                      [renderSound beat.beat]
        , div [class "vertical-container", css [height (pct 100), width (pct 75), margin4 (px 0) (px -1) (px -1) (px 0)]]
            [ div 
                [ class "horizontal-container"
                , css [ height (pct 50)
                      , width (pct 100)
                      , horizontalContainer]
                ]
                [ div [class "weak-sound", onClick (AddSound idx 2), css [width (pct 50), height (pct 100), borderedBox, backgroundColor (hex "#ffffff")]] [renderSound beat.firstOf2]
                , div [class "weak-sound", onClick (AddSound idx 4), css [width (pct 50), height (pct 100), borderedBox, backgroundColor (hex "#ffffff")]] [renderSound beat.secondOf2]
                ]
            , div [class "horizontal-container", css [height (pct 50), width (pct 100), horizontalContainer]]
                [ div [class "weak-sound", onClick (AddSound idx 1), css [width (pct 33), height (pct 100), backgroundColor (hex "#ffffff"), borderedBox, overflowX visible]] [renderSound beat.firstOf3]
                , div [class "off-beat", onClick (AddSound idx 3),   css [width (pct 33), height (pct 100), backgroundColor (hex "#99d1db"), borderedBox, overflowX visible]] [renderSound beat.secondOf3]
                , div [class "weak-sound", onClick (AddSound idx 5), css [width (pct 33), height (pct 100), backgroundColor (hex "#ffffff"), borderedBox, overflowX visible]] [renderSound beat.thirdOf3]
                ]
            ]
        ]

emptyBeat : Beat
emptyBeat = BerimbauBeat 
    { beat = NoSound
    , firstOf3 = NoSound
    , firstOf2 = NoSound
    , secondOf3 = NoSound
    , secondOf2 = NoSound
    , thirdOf3 = NoSound}

testBeat1 : Beat
testBeat1 = BerimbauBeat 
    { beat = Ton
    , firstOf3 = NoSound
    , firstOf2 = NoSound
    , secondOf3 = Tch
    , secondOf2 = NoSound
    , thirdOf3 = Tch}

testBeat2 : Beat
testBeat2 = BerimbauBeat 
    { beat = Tin
    , firstOf3 = NoSound
    , firstOf2 = NoSound
    , secondOf3 = Ton
    , secondOf2 = NoSound
    , thirdOf3 = NoSound}

testBeat3 : Beat
testBeat3 = BerimbauBeat 
    { beat = Ton
    , firstOf3 = SomeTch
    , firstOf2 = NoSound
    , secondOf3 = Tch
    , secondOf2 = NoSound
    , thirdOf3 = NoSound}

horizontalContainer : Css.Style
horizontalContainer = batch 
                        [ margin4 (px 0) (px 0) (px 0) (px 0)
                        , padding (px 0)
                        , displayFlex
                        ]

borderedBox : Css.Style
borderedBox = batch
    [displayFlex, border3 (px 2) solid (hex "060606"), margin4 (px -1) (px -1) (px 0) (px -1), overflowX visible]

renderSound : Sound -> Html Msg
renderSound sound = 
    case sound of
        Tch -> img [src "../assets/tch.svg", css [soundStyle]] []
        Ton -> img [src "../assets/ton.svg", css [soundStyle]] []
        Tin -> img [src "../assets/tin.svg", css [soundStyle]] []
        SomeTch -> img [src "../assets/sometch.svg", css [soundStyle]] []
        NoSound -> img [] []


soundStyle : Css.Style
soundStyle = batch 
    [width (pct 100)]