-- xiangqiboard
-- a haskell GUI for playing xiangqi - version 0.1.3 rc
--
-- Copyright 2007 Ralph Glass

module Main where

import Graphics.UI.Gtk
import Graphics.UI.Gtk.Gdk.Events
import Graphics.Rendering.Cairo
import System.IO
import System.Process
import System.Directory
import System.FilePath
import System.Time
import Control.Concurrent
import Monad
import List
import Maybe
import Char
import Data.IORef

license =
    "This program is free software; you can redistribute it and/or modify\n\
    \it under the terms of the GNU General Public License as published by\n\
    \the Free Software Foundation; either version 2 of the License, or\n\
    \(at your option) any later version.\n\
    \\n\
    \This program is distributed in the hope that it will be useful, but\n\
    \WITHOUT ANY WARRANTY; without even the implied warranty of\n\
    \MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the\n\
    \GNU General Public License for more details.\n\
    \\n\
    \You should have received a copy of the GNU General Public License \n\
    \along with this program; if not, write to the Free Software Foundation,\n\
    \Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA      "

uiDef =
  "<ui>\
  \  <menubar>\
  \     <menu name =\"File\" action=\"FileAction\">\
  \        <menuitem name=\"New\" action=\"NewAction\"/>\
  \        <menuitem name=\"Open\" action=\"OpenAction\"/>\
  \        <menuitem name=\"Save\" action=\"SaveAction\"/>\
  \        <separator/>\
  \        <menuitem name=\"Play Black\" action=\"GoAction\"/>\
  \        <separator/>\
  \        <menuitem name=\"Quit\" action=\"QuitAction\"/>\
  \     </menu>\
  \     <menu name =\"Move\" action=\"MovAction\">\
  \        <menuitem name=\"Undo\" action=\"UndoAction\"/>\
  \     </menu>\
  \     <menu name =\"Time\" action=\"TimeAction\">\
  \        <menuitem name=\"2 Seconds\" action=\"Sec2Action\"/>\
  \        <menuitem name=\"5 Seconds\" action=\"Sec5Action\"/>\
  \        <menuitem name=\"10 Seconds\" action=\"Sec10Action\"/>\
  \     </menu>\
  \     <menu name =\"View\" action=\"ViewAction\">\
  \        <menuitem name=\"toggle Sidebar\" action=\"ToggleSidebarAction\"/>\
  \        <menuitem name=\"Animation\" action=\"ToggleAnimationAction\"/>\
  \        <menuitem name=\"Blending\" action=\"ToggleBlendingAction\"/>\
  \     </menu>\
  \     <menu name =\"Help\" action=\"HelpAction\">\
  \        <menuitem name=\"Contents\" action=\"ContentsAction\"/>\
  \        <separator/>\
  \        <menuitem name=\"Info\" action=\"InfoAction\"/>\
  \     </menu>\
  \  </menubar>\
  \</ui>"

-- (last piece in piecesCharList is used for the highlighting cursor)
piecesCharListByEngine = [("HoiXiangqi 0.8.0","rnegkcpRNEGKCPO"),
                          ("HoiXiangqi 0.9.0","rnegkcpRNEGKCPO"),
                          ("HoiXiangqi 0.10.0","rnegkcpRNEGKCPO"),
                          ("HoiXiangqi 0.10.1","rnegkcpRNEGKCPO"),
                          ("HoiXiangqi 0.10.2","rnegkcpRNEGKCPO"),
                          ("HoiXiangqi 0.10.3 (Dec 28 2007 10:12:44)","rnefkopRNEFKOPH")]

--
-- Main Start
--

main :: IO ()
main = do

  initGUI

  window <- windowNew
  boardState <- newIORef("")
  sidebarState <- newIORef(True)
  moveAIState <- newIORef('O',(0,0))
  blendingFlag <- newIORef (False)
  highlightState <- newIORef('x',(9,9))
  animationState <- newIORef('R',((0,0),(0,0)))
  animationFlag <- newIORef (True)
  startTime <- getClockTime
  blendClockState <- newIORef (startTime)
  piecesStringState <- newIORef ("rnefkopRNEFKOPH")
  timeSettingState <- newIORef 5

  fileAct <- actionNew "FileAction" "Game" Nothing Nothing
  movAct <- actionNew "MovAction" "Move" Nothing Nothing
  timeAct <- actionNew "TimeAction" "Time" Nothing Nothing
  viewAct <- actionNew "ViewAction" "View" Nothing Nothing
  helpAct <- actionNew "HelpAction" "Help" Nothing Nothing

  newAct <- actionNew "NewAction" "New"
            (Just "Start a new game")
            (Just stockNew)
  openAct <- actionNew "OpenAction" "Open"
            (Just "Load a game")
            (Just stockOpen)
  saveAct <- actionNew "SaveAction" "Save"
            (Just "Save a game")
            (Just stockSave)
  goAct <- actionNew "GoAction" "Play Black"
            (Just "Play as Black")
            (Just stockUndo)
  quitAct <- actionNew "QuitAction" "Quit"
            (Just "Exit the program")
            (Just stockQuit)
  undoAct <- actionNew "UndoAction" "Undo"
             (Just "Undo Move")
             (Just stockUndo)
  contentsAct <- actionNew "ContentsAction" "Contents"
             (Just "")
             (Just stockHelp)
  infoAct <- actionNew "InfoAction" "Info"
            (Just "")
            (Just stockInfo)
  toggleSidebarAct <- actionNew "ToggleSidebarAction" "Sidebar on/off"
             (Just "")
             Nothing

  let   toggleAnimation te = do
        flag <- readIORef animationFlag
        writeIORef animationFlag (not flag)
        return ()
  let   toggleBlending te = do
        flag <- readIORef blendingFlag
        writeIORef blendingFlag (not flag)
        return ()
  let toggleAnimationAct = ToggleActionEntry "ToggleAnimationAction"
                                             "Toggle Animation"
                                              Nothing Nothing Nothing
                                              (toggleAnimation toggleAnimationAct)
                                              True

  let toggleBlendingAct = ToggleActionEntry "ToggleBlendingAction"
                                            "Toggle Blending"
                                            Nothing Nothing Nothing
                                            (toggleBlending toggleBlendingAct)
                                            False


  (inp, out, err, pid) <- runInteractiveProcess "hoixiangqi" ["-x"] Nothing Nothing

  let onRadioTimeChange ra = do
       rval <- radioActionGetCurrentValue ra
       let timeSt = show rval
       hPutStr inp ("st " ++ timeSt ++ "\n")
       return ()
  let radioTimeAct =
       [RadioActionEntry "Sec2Action" "2 seconds" Nothing Nothing Nothing 2,
        RadioActionEntry "Sec5Action" "5 seconds" Nothing Nothing Nothing 5,
        RadioActionEntry "Sec10Action" "10 seconds" Nothing Nothing Nothing 10]

  standardGroup <- actionGroupNew "standard"
  toggleGroup <- actionGroupNew "toogle"

  mapM_ (actionGroupAddAction standardGroup) [fileAct, movAct,timeAct,viewAct,helpAct]
  mapM_ (\act -> actionGroupAddActionWithAccel standardGroup act Nothing)
    [newAct,openAct,saveAct,goAct,quitAct,undoAct,
     toggleSidebarAct,contentsAct,infoAct]
  mapM_ (\act -> actionGroupAddToggleActions standardGroup act )
    [[toggleAnimationAct,toggleBlendingAct]]
  actionGroupAddRadioActions standardGroup radioTimeAct 2 onRadioTimeChange

  ui <- uiManagerNew
  mid <- uiManagerAddUiFromString ui uiDef
  uiManagerInsertActionGroup ui standardGroup 0
  (Just menuBar) <- uiManagerGetWidget ui "/ui/menubar"

  textview <- textViewNew
  entry <- entryNew
  canvas <- drawingAreaNew
  canvas `onSizeRequest` return (Requisition 300 300)
  boardtextbuffer <- textBufferNew Nothing
  scrolledWindow <- scrolledWindowNew Nothing Nothing
  scrolledWindow `onSizeRequest` return (Requisition 100 200)
  vBox <- vBoxNew False 0
  set vBox [boxHomogeneous := False]
  box <- hBoxNew False 0
  vboxRight <- vBoxNew False 0
  set vboxRight [boxHomogeneous := False]
  cairoTextContext <- cairoCreateContext Nothing
  myLayout <- layoutText cairoTextContext ""
  sidetextbuffer <- textBufferNew Nothing
  textViewSetBuffer textview sidetextbuffer

  boxPackStart vBox menuBar PackNatural 0
  boxPackStart vBox box PackGrow 0
  containerAdd box canvas
  containerAdd scrolledWindow textview
  boxPackStart vboxRight scrolledWindow PackGrow 0
  -- boxPackStart vboxRight entry PackNatural 0 -- entry deactivated for this release
  containerAdd box vboxRight
  containerAdd window vBox
  onDestroy window mainQuit

  imagelist <- mapM getPiecePixbufferByChar "rnegkcpRNEGKCPO" -- load svg to pixbuf list

  textBufferSetText sidetextbuffer "Hi! Xiangqi Engine started ..."

  let scroll = do
        end <- textBufferGetEndIter sidetextbuffer
        scrolled <- textViewScrollToIter textview end 0.4 Nothing
        sequence_ (replicate 1 (mainIterationDo True)) -- process gtk events
        return ()
  let print s = do
        textBufferInsertAtCursor sidetextbuffer (s ++ "\n")
        sc <- scroll
        return ()

  widgetShowNow canvas

  {- check engine version -}
  do
    (inpC, outC, errC, pidC) <- runInteractiveCommand "hoixiangqi -V"
    versionString <- hGetLine outC
    let engineString = versionString

    let checkPiecesString = filter (\x -> (fst x) == engineString) piecesCharListByEngine
    if checkPiecesString /= []
      then do
        writeIORef piecesStringState (snd $ head $ checkPiecesString)
      else do
        writeIORef piecesStringState "rnefkopRNEFKOPH"

  {- fork xiangqiengine process -}
    forkIO $ do
      hSetBuffering inp NoBuffering
      hSetBuffering out LineBuffering

      let newLine = do
            textBufferInsertAtCursor sidetextbuffer "\n"

      let pLine = do
            line <- hGetLine out
            print line

      let dLine = do
            line <- hGetLine out
            return ()

      let dLines n = do
            sequence_ (take n (repeat dLine))

      let pLines n = do
            sequence_ (take n (repeat pLine))

      let showboard = do
            hPutStr inp "show board\n"
            let getBoardLine = do
                  boardLine <- hGetLine out
                  dropLine <- hGetLine out
                  let s = ((drop 2 (filter (/='-') boardLine)) ++ "\n")
                  let s2 = (drop 2 (filter (/='-') boardLine))
                  addToBoardString boardState s2
            let getBoardLines n = do
                writeBoardString boardState ""
                sequence_ (take n (repeat getBoardLine))
            getBoardLines 10
            dropLine <- hGetLine out
            dropLine <- hGetLine out
            pLines 1
            scroll

      let getStringBoardPosByText text = do
            let x1 = fromJust (elemIndex (text !! 0) "abcdefghi")
            let y1 = 9 - digitToInt (text !! 1)
            let pos1 = (y1 * 9) + x1
            piece <- getStringBoardPos boardState pos1
            return piece

      let showEngineMove text = do
            let x1 = fromJust (elemIndex (text !! 0) "abcdefghi")
            let y1 = 9 - digitToInt (text !! 1)
            let pos1 = (y1 * 9) + x1
            let x2 = fromJust (elemIndex (text !! 2) "abcdefghi")
            let y2 = 9 - digitToInt (text !! 3)
            let pos2 = (y2 * 9) + x2
            piece <- getStringBoardPos boardState pos1

            writeIORef moveAIState ((head piece),(fromIntegral(x1),fromIntegral(y1)))
            now <- getClockTime
            writeIORef blendClockState (now)

            animationOn <- readIORef animationFlag
            if animationOn == True
              then do
                writeIORef animationState (head piece,((fromIntegral x1, fromIntegral y1),
                                                       (fromIntegral x2, fromIntegral y2)))
              else do
                return ()

            setStringBoardPos boardState pos1 "+" -- hide AI Move Origin

      let highlightPiece text = do
            let x1 = fromJust (elemIndex (text !! 0) "abcdefghi")
            let y1 = 9 - digitToInt (text !! 1)
            let pos1 = (y1 * 9) + x1
            piece <- getStringBoardPos boardState pos1
            writeIORef highlightState ((head piece),(fromIntegral(x1),fromIntegral(y1)))

      let pCheckedLine = do -- check for engine move response and mate
            line <- hGetLine out
            let testMove = take 4 line
            if testMove == "move"
               then do
                    let moveAI = drop 5 line
                    showEngineMove moveAI
                    sequence_ (replicate 5 (mainIterationDo True)) -- process gtk events
               else do
                    sequence_ []
            test <- hWaitForInput out 50 -- additional message (mate) available?
            if test
               then do
                    line <- hGetLine out
                    print line
                    layoutSetText myLayout line
                    sequence_ (replicate 10 (mainIterationDo True)) -- process gtk events
               else do
                    print line
                    layoutSetText myLayout ""

      let getSavedLines = do
            line <- hGetLine out
            if line == "--- end read game ---"
               then do
                 print line
               else do
                 print line
                 getSavedLines

      let getUnknownLines = do
            isLineAvailable <- hWaitForInput out 50
            if isLineAvailable == True
              then do
                line <- hGetLine out
                print line
                getUnknownLines
              else do
                return ()

      let showMove = do
            text <- entryGetText entry
            let x1 = fromJust (elemIndex (text !! 0) "abcdefghi")
            let y1 = 9 - digitToInt (text !! 1)
            let pos1 = (y1 * 9) + x1
            let x2 = fromJust (elemIndex (text !! 2) "abcdefghi")
            let y2 = 9 - digitToInt (text !! 3)
            let pos2 = (y2 * 9) + x2
            piece <- getStringBoardPos boardState pos1
            setStringBoardPos boardState pos1 "+"
            setStringBoardPos boardState pos2 piece
            sequence_ (replicate 10 (mainIterationDo True)) -- process gtk events

      let help = do
            hPutStr inp "help\n"
            pLines 46


      let new = do
            print "New game started."
            hPutStr inp "new\n"
            timeSetting <- readIORef timeSettingState
            let time = show timeSetting
            hPutStr inp ("st " ++ time ++ "\n")
            layoutSetText myLayout ""
            showboard

      let go = do
            hPutStr inp "go\n"
            sequence_ (replicate 16 (mainIterationDo True)) -- process gtk events
            pCheckedLine
            showboard

      let undo = do
            print "undo"
            hPutStr inp "remove\n"
            layoutSetText myLayout ""
            showboard

      let savegame = do
            string <- openSaveFileDialog window
            if string == ""
              then do
                return ()
              else do
                hPutStr inp ("savegame "++string++"\n")

      let loadgame = do
            new
            string <- openOpenFileDialog window
            if string == ""
              then do
                return ()
              else do
                hPutStr inp ("loadgame "++string++"\n")
                sequence_ (replicate 1 (mainIterationDo True)) -- process gtk events
                w <- hLookAhead out
                getSavedLines
                showboard
                timeSetting <- readIORef timeSettingState
                hPutStr inp ("st " ++ (show timeSetting) ++"\n")
                layoutSetText myLayout ""

      -- send command to engine and deal with response
      let move = do
            text <- entryGetText entry
            showMove
            mainIteration
            hPutStr inp (text++"\n")
            w <- hLookAhead out
            pCheckedLine

            -- Wait for Animation
            let wait = do
                  now <- getClockTime
                  start <- readIORef blendClockState
                  if (sinceTimeDouble now start) < 30
                    then do
                      sequence_ (replicate 2 (mainIterationDo True))
                      wait
                    else return ()
                  return ()
            wait
            showboard

      let checkmove = do
            text <- entryGetText entry
            if length text == 4
              then do
                   sequence_ [(print (text ++ " thinking ...")), move]
                   entrySetText entry ""
              else return ()

      let parseMove string = do
            text <- entryGetText entry
            let len = length text
            if len == 2
               then do
                 entrySetText entry (text ++ string)
                 return ()
               else do
                 entrySetText entry string
            checkmove

      onButtonPress canvas
         (\x -> if (eventButton x) == LeftButton
                   then do let ty = eventY x
                           let tx = eventX x
                           (step, offX) <- getScale canvas
                           let colX = (round ((tx - offX + (0.5 * step)) / step)) - 1
                           let rowY = (round ((ty + (0.5 * step)) / step))
                           if colX >= 0 && colX < 9 && rowY >= 0 && rowY <= 10
                             then do
                               let col = "abcdefghi" !! colX
                               let row = 10 - rowY
                               let movePart = ([col] ++ (show row))
                               piece <- getStringBoardPosByText movePart
                               let p = piece
                               test <- entryGetText entry
                               let t = test
                               if (length t) == 0 && p /= "+"
                                 then do
                                   highlightPiece movePart
                                   parseMove movePart
                                 else writeIORef highlightState ('x',(0,0))
                               if (length t) == 2
                                 then do
                                   parseMove movePart
                                 else return ()
                               return (eventSent x)
                             else return (eventSent x)
                   else return (eventSent x))

      {- todo: explicitly handle engine commands in future versions, entry deactivated for now
      onEntryActivate entry $ do
        text <- entryGetText entry
        print text
        if text == "help"
          then do
            print "Be careful: most commands not implemented in xiangqiboard"
            help
            scroll
          else do
            let supportedEntryCommands =
                  ["easy","hard","time"]
            if (text `elem` supportedEntryCommands)
              then do
                hPutStr inp (text++"\n")
                getUnknownLines
                newLine
                scroll
                showboard
              else do
                print "sorry, command not supported\n"
                scroll
                return ()
        entrySetText entry ""
        -}

      onActionActivate newAct new
      onActionActivate undoAct undo
      onActionActivate openAct loadgame
      onActionActivate saveAct savegame
      onActionActivate quitAct mainQuit
      onActionActivate contentsAct openHelpContents
      onActionActivate infoAct (openAboutDialog window license)
      onActionActivate goAct go
      onActionActivate toggleSidebarAct (toggleSidebarIO sidebarState vboxRight)

      newLine
      pLines 1
      getUnknownLines -- show engine startup message
      newLine
      scroll
      showboard
      new

  widgetShow vBox
  widgetShow box
  widgetShow canvas
  widgetShow window

  timeoutAdd (renderCanvas canvas myLayout boardState imagelist
                           startTime moveAIState blendClockState
                           highlightState animationState animationFlag
                           blendingFlag piecesStringState) 60 -- GTK render timeout

  mainGUI

--
-- Main End
--

--
-- GTK render callback
--

renderCanvas :: DrawingArea -> PangoLayout -> IORef (String) -> [Pixbuf]
                            -> ClockTime -> IORef(Char,(Double, Double))
                            -> IORef(ClockTime) -> IORef(Char,(Double,Double))
                            -> IORef(Char,((Double,Double),(Double,Double)))
                            -> IORef(Bool) -> IORef(Bool) -> IORef (String) -> IO Bool

renderCanvas canvas boardMessageLayout boardState imagelist startTime
             moveAIState blendClockState highlightState animationState
             animationFlag blendingFlag piecesStringState = do

  drawCanvas <- widgetGetDrawWindow canvas
  myGC <- gcNew drawCanvas
  scale <- (widgetGetSize canvas)
  let xscale = fst scale
  let yscale = snd scale
  let widgetWidth = fromIntegral(xscale)
  let widgetHeigth = fromIntegral(yscale)
  let wstep = getBoardWidthStep widgetWidth
  let hstep = getBoardHeightStep widgetHeigth
  let step = min hstep wstep
  let piecesize = 0.85
  let pscale = round (step * piecesize)
  let offsetx = (widgetWidth - (step * 9.0)) * 0.5
  let offsetpx = offsetx + ((step - (step * piecesize)) * 0.5)
  let offsetpy = (step - (step * piecesize))*0.5
  let wlist = take 9 (iterate (+step) ((step * 0.5)+offsetx))
  let hlist = take 10 (iterate (+step) (step * 0.5))
  let wlistLines = map (\x -> (x,step)) wlist
  let hlistLines = map (\x -> (x,(step,offsetx))) hlist
  let myscale i = do
        pixbufScaleSimple i pscale pscale InterpBilinear
  scaledImagelist <- mapM myscale imagelist

  piecesCharList <- readIORef piecesStringState
  let indexedScaledImagelist = zip piecesCharList scaledImagelist

  regio <- regionRectangle (Rectangle 0 0 xscale yscale)

  drawWindowBeginPaintRegion drawCanvas regio -- double buffering start
  renderWithDrawable drawCanvas $ do

    setSourceRGB 1 1 1
    rectangle 0 0 widgetWidth (step * 10.0)
    fillPreserve
    stroke

    mapM drawVerticalLine wlistLines
    mapM drawHorizontalLine hlistLines
    drawCastle (wlist !! 3) (hlist !! 0) step
    drawCastle (wlist !! 3) (hlist !! 7) step

  let drawPiece (c,(x,y)) = do
          let xx = round (x + offsetpx)
          let yy = round (y + offsetpy)
          let image = snd (head (filter (isPiece (c,0)) (indexedScaledImagelist)))
          drawPixbuf drawCanvas myGC image 0 0 xx yy pscale pscale RgbDitherNone 0 0

  let drawAnimatedPiece (c,(xUnscaled,yUnscaled)) = do
          let x = (xUnscaled * step)
          let y = (yUnscaled * step)
          let xx = round (x + offsetpx)
          let yy = round (y + offsetpy)
          let image = snd (head (filter (isPiece (c,0)) (indexedScaledImagelist)))
          drawPixbuf drawCanvas myGC image 0 0 xx yy pscale pscale RgbDitherNone 0 0

  let drawGhostPiece (c,(xUnscaled,yUnscaled)) = do
          let x = (xUnscaled * step)
          let y = (yUnscaled * step)
          let xx = round (x + offsetpx)
          let yy = round (y + offsetpy)
          let image = snd (head (filter (isPiece (c,0)) (indexedScaledImagelist)))
          blendImage <- pixbufCopy image
          pixbufFill blendImage 255 255 255 0
          pixbufComposite image blendImage 0 0 pscale pscale 0 0 1 1 InterpNearest 25
          drawPixbuf drawCanvas myGC blendImage 0 0 xx yy pscale pscale RgbDitherNone 0 0

  let drawHighlightedPiece (c,(xUnscaled,yUnscaled)) = do
        if c == 'x'
          then return ()
          else do
            let x = (xUnscaled * step)
            let y = (yUnscaled * step)
            let xx = round (x + offsetpx)
            let yy = round (y + offsetpy)
            let cursorImage = snd (head (filter (isPiece ((piecesCharList !! 14),0))
                                                          (indexedScaledImagelist)))
            drawPixbuf drawCanvas myGC cursorImage 0 0 xx yy pscale pscale RgbDitherNone 0 0

  -- generate coordinateslist [(0,0),(0,1)..]
  let coordy = foldl1 (++) (map (replicate 9) [0..9])
  let coordx = take 90 (cycle [0..8])
  let coordxy = zip coordx coordy
  let coordxyscaled = map (scalarmultrounded step) coordxy
  pieceList <- readBoardString boardState
  let preselectlist = (zip pieceList coordxyscaled)
  let drawlist = filter (isAnyPiece piecesCharList) preselectlist
  mapM drawPiece drawlist
  moveAIOrigin <- readIORef moveAIState

  now <- getClockTime
  moveTime <- readIORef blendClockState
  let time = sinceTimeDouble now moveTime

  -- show AI Move Blending
  blendingOn <- readIORef blendingFlag
  if blendingOn == True
    then do
      if time < 100
          then do
             drawGhostPiece moveAIOrigin
             return ()
          else do
             return ()
    else do
      return ()

  -- show AI Move Animation
  animationOn <- readIORef animationFlag
  if animationOn == True
    then do
      if time < 30
        then do
          animatedPiece <- readIORef animationState
          let animationPath = getSimpleAnimationPath animatedPiece time
          drawAnimatedPiece animationPath
        else do
          return ()
    else do
      return ()

  renderWithDrawable drawCanvas $ do
    setSourceRGB 0 0 0
    let textX = offsetx + (0.5*step)
    let textY = (4.7*step)
    moveTo textX textY
    showLayout boardMessageLayout

  highlighted <- readIORef highlightState
  drawHighlightedPiece highlighted

  drawWindowEndPaint drawCanvas -- double buffering end
  return True

--
-- Functions Start
--

scalarmultrounded a b = ((a * (fst b)), (a * (snd b)))

getBoardWidthStep :: Double -> Double
getBoardWidthStep width = width / 9.0

getBoardHeightStep :: Double -> Double
getBoardHeightStep height = height / 10.0

getScale :: DrawingArea -> IO (Double, Double)
getScale widget = do

         (scaleX,scaleY) <- (widgetGetSize widget)
         let width = fromIntegral scaleX
         let height = fromIntegral scaleY
         let widthStep = getBoardWidthStep width
         let heightStep = getBoardHeightStep height
         let step = min widthStep heightStep
         let offsetX = (width - (step * 9.0)) * 0.5
         let offsetY = 0
         return (step, offsetX)

--
-- Drawing Functions
--

drawCursorCircle :: Double -> Double -> Double -> Render ()
drawCursorCircle x y scale = do

  save
  setSourceRGBA 255 255 255 10
  arc x y scale 0 (2*pi)
  fillPreserve
  stroke
  restore

drawVerticalLine :: (Double,Double) -> Render ()
drawVerticalLine x = do

  save
  setSourceRGB 0 0 0
  let linewidth = 1
  setLineWidth linewidth
  moveTo (fst x) ((snd x) * 0.5)
  relLineTo 0 ((snd x) * 4.0)
  relMoveTo 0 (snd x)
  relLineTo 0 ((snd x) * 4.0)
  stroke
  restore

drawHorizontalLine :: (Double,(Double,Double)) -> Render ()
drawHorizontalLine y = do

  save
  setSourceRGB 0 0 0
  setLineWidth 1
  moveTo ((snd (snd y))+((fst (snd y)) * 0.5)) (fst y)
  relLineTo ((fst (snd y)) * 8.0) 0
  stroke
  restore

drawCastle :: Double -> Double -> Double -> Render ()
drawCastle x y step = do

  save
  setSourceRGB 0 0 0
  setLineWidth 1
  moveTo x y
  relLineTo (step * 2.0) (step * 2.0)
  moveTo x (y + step * 2.0)
  relLineTo (step * 2.0) (-(step * 2.0))
  stroke
  restore

getSimpleAnimationPath :: (Char,((Double,Double),(Double,Double)))
                          -> Double -> (Char,(Double,Double))
getSimpleAnimationPath animatedPiece time = (c,(xt,yt))
  where
    timeM = min time 25
    c = fst animatedPiece
    start = (fst (snd animatedPiece))
    end = (snd (snd animatedPiece))
    x0 = fst start
    y0 = snd start
    x1 = fst end
    y1 = snd end
    xt = x0 + (((x1-x0)/25)*timeM)
    yt = y0 + (((y1-y0)/25)*timeM)

--
-- File Functions
--

getPiecePixbufferByChar :: Char -> IO Pixbuf
getPiecePixbufferByChar c = do

    Just relativeDir <- findExecutable "xiangqiboard"
    let dataDir = combine (takeDirectory relativeDir) "../share/games/xiangqiboard/"
    let filename = dataDir ++ [c] ++ ".svg"

    --pixbufNewFromFileAtSize filename 256 256
    pixbufNewFromFileAtSize filename 127 127

--
-- Board Functions
--

getPosOfCoord :: (Int,Int) -> Int
getPosOfCoord (x1,y1) = y1 * 9 + x1

readBoardString :: IORef (String) -> IO String
readBoardString stringState = do
    string <- readIORef stringState
    return string

writeBoardString :: IORef (String) -> String -> IO ()
writeBoardString stringState string = do
   writeIORef stringState string

addToBoardString :: IORef (String) -> String -> IO ()
addToBoardString stringState string = do
   oldString <- readBoardString stringState
   let newString = oldString ++ string
   writeBoardString stringState newString

getStringBoardPos :: IORef (String) -> Int -> IO String
getStringBoardPos stringState pos = do
    fullString <- readIORef stringState
    let posString = fullString !! pos
    return [posString]

setStringBoardPos :: IORef (String) -> Int -> String -> IO ()
setStringBoardPos stringState pos string = do
    fullString <- readIORef stringState
    let (frontString,backString) = splitAt pos fullString
    let newString = frontString ++ string ++ (tail backString)
    writeIORef stringState newString

isPiece a b =
    if fst a == fst b
      then True
      else False

isAnyPiece :: String -> (Char,(Double,Double)) -> Bool
isAnyPiece piecesString a =
    if elem (fst a) piecesString
      then True
      else False

--
-- Time Functions Start
--

sinceTimeDouble :: ClockTime -> ClockTime -> Double
sinceTimeDouble endtime starttime = timeGone
  where
    myDiffTime = diffClockTimes endtime starttime
    timeGone = ((secondsDiff(myDiffTime) + (picosecondsDiff(myDiffTime) / 1e+12)) * 60)

picosecondsDiff :: TimeDiff -> Double
picosecondsDiff timediff = fromIntegral (getPico (timediff))

secondsDiff :: TimeDiff -> Double
secondsDiff timediff = fromIntegral (getSeco (timediff))

getPico (TimeDiff _ _ _ _ _ _ pico) = pico
getSeco (TimeDiff _ _ _ _ _ sec _ ) = sec

--
-- Time Functions End
--

toggleSidebarIO :: IORef (Bool) -> VBox -> IO ()
toggleSidebarIO state widget = do
   st <- readIORef state
   writeIORef state (not st)
   if st
     then do widgetShowAll widget
     else do widgetHideAll widget
   mainIteration
   return ()

--
-- GTK Dialogs Start
--

openOpenFileDialog :: Window -> IO String
openOpenFileDialog parentWindow = do
  dialog <- fileChooserDialogNew
            (Just $ "Load Game")
            (Just parentWindow)
            FileChooserActionOpen
            [("gtk-cancel",ResponseCancel),("gtk-open", ResponseAccept)]
  widgetShow dialog
  response <- dialogRun dialog
  ftext <- case response of
            ResponseAccept -> do Just fileName <- fileChooserGetFilename dialog
                                 return fileName
            ResponseCancel -> do return ""
            ResponseDeleteEvent -> do return ""
  widgetHide dialog
  return ftext

openSaveFileDialog :: Window -> IO String
openSaveFileDialog parentWindow = do
  dialog <- fileChooserDialogNew
            (Just $ "Save Game")
            (Just parentWindow)
            FileChooserActionSave
            [("gtk-cancel",ResponseCancel),("gtk-save", ResponseAccept)]
  widgetShow dialog
  response <- dialogRun dialog
  ftext <- case response of
            ResponseAccept -> do Just fileName <- fileChooserGetFilename dialog
                                 return fileName
            ResponseCancel -> do return ""
            ResponseDeleteEvent -> do return ""
  widgetHide dialog
  return ftext

openAboutDialog :: Window -> String -> IO ()
openAboutDialog parentwindow license = do
  dialog <- aboutDialogNew
  aboutDialogSetName dialog "XiangqiBoard"
  aboutDialogSetVersion dialog "0.1.3"
  aboutDialogSetCopyright dialog "Copyright 2007 Ralph Glass"
  aboutDialogSetComments dialog "a xiangqi GUI"
  aboutDialogSetLicense dialog (Just license)
  aboutDialogSetWebsite dialog "http://xiangqiboard.blogspot.com/"


  dialogRun dialog
  widgetDestroy dialog

--
-- GTK Dialogs End
--

openHelpContents :: IO ()
openHelpContents = do
    Just relativeDir <- findExecutable "xiangqiboard"
    let dataDir = combine (takeDirectory relativeDir) "../share/doc/xiangqiboard/"
    let filename = "file://" ++ dataDir ++ "xiangqiboard.xml"
    let commandString = "yelp " ++ filename ++ " > /dev/null"
    handle <- runCommand commandString
    return ()
