{-# LANGUAGE OverloadedStrings #-}
module NioFormHtml where

import Lucid
import Data.String.Conversions
import Control.Monad
import Data.String

import NioFormTypes
import ABH.Strings

data BootAlertType
   = BootAlertDanger
   | BootAlertWarn
   | BootAlertInfo
   | BootAlertSuccess

data ButtonSize = Normal | ExtraSmall

data ButtonType = PriBlue | InfoBlue | AlertRed

alertBox :: BootAlertType -> Html () -> Html ()
alertBox alertType alertVal =
  with div_ [class_ (mconcat ["alert alert-dismissable ", t])] $ do
    with
      button_
      [ type_ "button"
      , class_ "close"
      , data_ "dismiss"     "alert"
      , data_ "aria-hidden" "yes"
      ]
      ("")
    alertVal
 where
  t = case alertType of
    BootAlertDanger  -> "alert-danger"
    BootAlertWarn    -> "alert-warning"
    BootAlertInfo    -> "alert-info"
    BootAlertSuccess -> "alert-success"

friendlyError :: NioFieldError -> Html ()
friendlyError = p_ . fromString . friendlyError'

friendlyError' :: NioFieldError -> String
friendlyError' (NioFieldErrorV a) = show a

basicNioformHtml :: NioForm -> String -> Html ()
basicNioformHtml (NioForm nf) s = do
  with form_ [
    method_ "post"
    , action_ $ cs s
    , class_ "blockInputs"
    , enctype_ "multipart/form-data"
    ] $ do
      forM_ nf nioformHtmlField
      input_ [type_ "submit"]


nioformHtmlField :: NioFieldView -> Html ()
nioformHtmlField nfv@(NioFieldView _ _ ers nfi _) = do
        when (length ers > 0)
          (alertBox (BootAlertWarn)
            $ (p_ $ fromString ("Errors for " ++ (cs $ fvLabel nfv)))
            <> (mconcat $ friendlyError <$> ers)
          )
        <>
        case nfi of
          NioFieldInputSubmit v v' -> do
            input_ [
                type_ "submit"
              , name_ $ fromString $ cs v
              , id_ (fscs $ fvId nfv)
              , value_ $ fromString $ cs v'
              ]
          NioFieldInputFile -> do
            input_ [
                type_ "file"
              , name_ (fscs $ fvId nfv)
              , id_ (fscs $ fvId nfv)
              , value_ (fscs $ (case fvValue nfv of; NioFieldValS s -> s; NioFieldValM s -> unlines s))
              ]
          NioFieldInputHidden -> do
            input_ [
                type_ "hidden"
              , name_ (fscs $ fvId nfv)
              , id_ (fscs $ fvId nfv)
              , value_ $ fscs (case fvValue nfv of; NioFieldValS s -> s; NioFieldValM s -> unlines s)
              ]
          NioFieldInputText -> do
            with label_ 
              [for_ (fscs $ fvId nfv)]
              $ fscs $ fvLabel nfv
            span_ " "
            textarea_
              [ type_ "text"
              , name_ (fscs $ fvId nfv)
              , id_ (fscs $ fvId nfv)
              ] $ fscs $ (case fvValue nfv of; NioFieldValS s -> s; NioFieldValM s -> unlines s)
          NioFieldInputDigit -> do
            with label_ 
              [for_ (fscs $ fvId nfv)]
              $ fscs $ fvLabel nfv
            span_ " "
            input_ [
                type_ "text"
              , name_ (fscs $ fvId nfv)
              , id_ (fscs $ fvId nfv)
              , value_ (fscs $ (case fvValue nfv of; NioFieldValS s -> s; NioFieldValM s -> unlines s))
              ]
          NioFieldInputTextShort -> do
            with label_ 
              [for_ (fscs $ fvId nfv)]
              $ fscs $ fvLabel nfv
            span_ " "
            input_ [
                type_ "text"
              , name_ (fscs $ fvId nfv)
              , id_ (fscs $ fvId nfv)
              , value_ (fscs $ (case fvValue nfv of; NioFieldValS s -> s; NioFieldValM s -> unlines s))
              ]
          NioFieldInputBool c -> do
            with label_ 
              [for_ $ fromString $ c ++ (fscs $ fvId nfv)]
              $ fscs $ fvLabel nfv
            span_ " "
            input_ $ (if (case fvValue nfv of; NioFieldValS s -> c == s; NioFieldValM m -> elem c m) then (checked_ :) else id) [
                type_ "checkbox"
              , name_ (fscs $ fvId nfv)
              , id_ $ fromString $ c ++ (fscs $ fvId nfv)
              , value_ $ fscs c
              ]
          NioFieldInputLabled multi kv -> do
            with label_ 
              [for_ (fscs $ fvId nfv)]
              $ fscs $ fvLabel nfv
            span_ " "
            with select_ ([
                name_ (fscs $ fvId nfv)
              , id_ (fscs $ fvId nfv)
              ] ++ [multiple_ "multiple" | multi])
              $ sequence_ $ fmap
                (\(x',y') -> do
                  let y'' = ((case fvValue nfv of; NioFieldValS s -> pure s; NioFieldValM s -> s)) :: [String]
                  (with option_ $ [value_ y'] ++ [ selected_ "" | elem (cs y') y''])
                    $ fscs x'
                )
                kv
