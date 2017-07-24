module SeatSaver exposing (..)

import Html exposing (..)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)

import Http
import Json.Decode as Decode exposing (Decoder, field, succeed)

apiUrlPrefix : String
apiUrlPrefix =
  "http://localhost:4000/api"

seatsUrl : String
seatsUrl =
  apiUrlPrefix ++ "/seats"


getSeats : (Result Http.Error (List Seat) -> msg) -> String -> Cmd msg
getSeats msg url =
  seatListDecoder
  |> Http.get url
  |> Http.send msg

requestSeats : Cmd Msg
requestSeats =
  getSeats FetchSeats seatsUrl

seatListDecoder : Decoder (List Seat)
seatListDecoder =
    Decode.list seatDecoder

seatDecoder : Decoder Seat
seatDecoder =
  Decode.map2 Seat
    (field "seatNo" Decode.int)
    (field "occupied" Decode.bool)



-- MODEL


type alias Seat =
  { seatNo : Int
  , occupied : Bool
  }

type alias Model =
  List Seat

initialModel : Model
initialModel =
  []


-- UPDATE


type Msg
  = Toggle Seat
  | FetchSeats (Result Http.Error (List Seat))

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Toggle seatToToggle ->
      let
        updateSeat seatFromModel =
          if seatFromModel.seatNo == seatToToggle.seatNo then
            { seatFromModel | occupied = not seatFromModel.occupied }
          else seatFromModel
      in
        ((List.map updateSeat model), Cmd.none)
    FetchSeats (Ok seats) ->
      let
        _ = Debug.log "It worked" seats
      in
        (seats, Cmd.none)
    FetchSeats (Err error) ->
      let
        _ = Debug.log "Oops!" error
      in
        (model, Cmd.none)


-- VIEW


view : Model -> Html Msg
view model =
  ul [ class "seats" ] (List.map seatItem model)

seatItem : Seat -> Html Msg
seatItem seat =
  let
    occupiedClass =
      if seat.occupied then "occupied" else "available"
  in
    li
      [
        class ("seat " ++ occupiedClass)
      , onClick (Toggle seat)
      ]
      [ text (toString seat.seatNo) ]

main : Program Never Model Msg
main =
  Html.program
    { init = ( initialModel, (getSeats FetchSeats seatsUrl) )
    , view = view
    , update = update
    , subscriptions = (\_ -> Sub.none)
    }

