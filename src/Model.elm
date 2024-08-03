module Model exposing (..)
import Array exposing (Array)

type alias Model = 
    { config : StateConfig
    , toolbar: Toolbar
    , track : TrackModel
    }

type alias StateConfig =
    { assetStyle : AssetStyle
    , currentSound: BerimbauSound
    }

type alias Toolbar =
    { sounds: List Sound
    , sound: Sound
    }

type AssetStyle = DEFAULT | CUSTOM

type alias TrackModel =
    { instrument : String
    , beats : List Beat
    }

type Beat = BerimbauBeat (SoundsHolder BerimbauSound)

type alias SoundsHolder soundType = 
    { beat      : soundType
    , firstOf3  : soundType
    , firstOf2  : soundType
    , secondOf3 : soundType
    , secondOf2 : soundType
    , thirdOf3  : soundType
    }

type Instrument = Berimbau

type alias Sound = BerimbauSound

type BerimbauSound = Tch | Ton | Tin | SomeTch | NoSound