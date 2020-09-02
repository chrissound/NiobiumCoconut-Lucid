{-# OPTIONS -Wno-unused-matches #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
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
nioformHtmlField nfv@(NioFieldView a b ers nfi _) = do
        when (length ers > 0)
          (alertBox (BootAlertWarn)
            $ (p_ $ fromString ("Errors for " ++ (cs $ fvLabel nfv)))
            <> (mconcat $ friendlyError <$> ers)
          )
        <>
        case nfi of
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
            br_ []
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
            br_ []
          NioFieldInputBool c -> do
            with label_ 
              [for_ ((fscs $ case fvValue nfv of; NioFieldValS s -> s; NioFieldValM s -> unlines s))]
              $ fscs $ fvLabel nfv
            span_ " "
            input_ $ (if c then (checked_ :) else id) [
                type_ "checkbox"
              , name_ (fscs $ fvId nfv)
              , id_ (fscs $ case fvValue nfv of; NioFieldValS s -> s; NioFieldValM s -> unlines s)
              , value_ (fscs $ case fvValue nfv of; NioFieldValS s -> s; NioFieldValM s -> unlines s)
              ]
            br_ []
          NioFieldInputLabled multi kv -> do
            with label_ 
              [for_ (fscs $ fvId nfv)]
              $ fscs $ fvLabel nfv
            span_ " "
            with select_ ([
                name_ (fscs $ fvId nfv)
              , id_ (fscs $ fvId nfv)
              ] ++ [multiple_ "multiple" | multi])
              $ sequence $ fmap
                (\(x',y') -> do
                  let y'' = ((case fvValue nfv of; NioFieldValS s -> pure s; NioFieldValM s -> s)) :: [String]
                  (with option_ $ [value_ y'] ++ [ selected_ "" | elem (cs y') y''])
                    $ fscs x'
                )
                kv
            br_ []
          _ -> error "Invalid input field..."
