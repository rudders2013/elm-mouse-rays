
import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Mouse
import Window


maxPathLength = 2  -- Define our path to be limited to two points.


-- MODEL

type alias MouseX = Int
type alias MouseY = Int
type alias Point = (MouseX, MouseY)            -- Mouse position (x,y).
type Model = NoPath                            -- No current path.
           | ActivePath   (List Point, Point)  -- An active path, plus an actively moving position.


-- UPDATE

getModelPoints : Model -> List Point
getModelPoints model =
  case model of
    NoPath              -> []   -- If no path is active, return an empty list.
    ActivePath (ps', _) -> ps'  -- Otherwise, retrieve the defined points...


update : (Point,Point) -> Model -> Model
update (p,movePoint) model =
  let
    isClick = p == movePoint             -- Check if this is a mouse click (defined by our mouse
                                         --   position being equal to our last click).
    ps = getModelPoints model            -- Get the active path points.
    len = List.length ps                 -- Get the length of the path.
    path = if isClick                    -- If this is a mouse click...
            then p :: ps                 --   ... extend the path, otherwise...
            else ps                      --   ... use the existing path.
  in
    if len < maxPathLength               -- Check if the path is currently incomplete.
      then ActivePath (path, movePoint)  -- If incomplete, update the current path.
      else NoPath                        -- Otherwise, update the completed path.


-- VIEW

lineStyle : LineStyle
lineStyle =
  { defaultLine          -- Extend the "default" definition.
      | width <- 10.0    -- Line width.
      , color <- blue    -- Assign the color.
      , cap   <- Round   -- The shape of the end points.
      , join  <- Smooth  -- The shape of the joints.
  }


drawLineSegments : (Int,Int) -> List Point -> Maybe Point -> Form
drawLineSegments (w,h) ps maybeMove =
  let
    points = case maybeMove of                           -- Check if a new move has occurred.
              Nothing -> ps                              -- If not, return the previous points.
              Just p  -> p :: ps                         -- Otherwise, prepend and return the
                                                         --   new position.
  in
    List.map (\(x,y) -> (toFloat x, toFloat -y)) points  -- Convert the mouse points to
                                                         --   "model" coordinates.
      |> path                                            -- Build a path from the points.
      |> traced lineStyle                                -- Trace the line with defined form.
      |> move (-(toFloat w) / 2, (toFloat h) / 2)        -- Move drawing from middle to upper
                                                         --   left ("screen" coordinates).


view : (Int,Int) -> Model -> Element
view (w,h) model =
  case model of
    NoPath             ->                                 -- If no path is currently defined...
      collage w h []                                      --   ... build an empty canvas.
    ActivePath (ps, p) ->                                 -- If an actively moving path is defined...
      collage w h [ drawLineSegments (w,h) ps (Just p) ]  --   ... draw the line segments, with the
                                                          --   active motion.


-- SIGNALS

sampleOnClick : Signal Point
sampleOnClick =
  Signal.sampleOn   -- Each time a...
    Mouse.clicks    --   ... mouse click occurs, return...
    Mouse.position  --   ... the mouse position.


mergeMouse : Signal (Point, Point)
mergeMouse =
  Signal.map2       -- Each time either event happens...
    (,)             --   ... collect both...
    sampleOnClick   --   ... the last mouse click...
    Mouse.position  --   ... and the current mouse position.


mousePositions : Signal Model
mousePositions =
  Signal.foldp      -- Fold each signal into an accumulated model...
    update          --   ... through the update function.
    NoPath          -- Start with an empty path.
    mergeMouse      -- Updates given by mouse position changes.


main : Signal Element
main =
  Signal.map2          -- Map two signals together...
    view               --   ... through the view function.
    Window.dimensions  -- Use updates to the window dimensions as the first signal.
    mousePositions     -- Use updates to mouse positions as the second signal.
