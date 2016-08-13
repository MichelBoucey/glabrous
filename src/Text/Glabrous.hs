{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

-- | A minimalistic Mustache-like syntax, truly logic-less,
-- pure 'T.Text' template library
--
--    * Use only the simplest Mustache tag {{name}} called a variable.
--    * HTML agnostic
--

module Text.Glabrous
    (
    -- * Template
      Template (..)
    , Tag
    -- ** Get a 'Template'
    , fromText
    , readTemplateFile
    -- ** 'Template' operations
    , toText
    , isFinal
    , tagsOf
    , tagsRename
    , compress
    , writeTemplateFile
    -- * Context
    , Context (..)
    -- ** Get a 'Context'
    , initContext
    , fromList
    , fromTemplate
    -- ** 'Context' operations
    , setVariables
    , deleteVariables
    , unsetContext
    -- ** JSON 'Context' file
    , readContextFile
    , writeContextFile
    , initContextFile
    -- * Processing
    , process
    , processWithDefault
    , partialProcess
    , G.Result (..)
    , partialProcess'
    ) where

import           Data.Aeson
import           Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Lazy     as L
import qualified Data.HashMap.Strict      as H
import           Data.List                (uncons)
import qualified Data.Text                as T
import qualified Data.Text.IO             as I

import           Text.Glabrous.Internal
import           Text.Glabrous.Types      as G

-- | Optimize a 'Template' content after (many) 'partialProcess'(') rewriting(s).
compress :: Template -> Template
compress t =
    Template { content = go (content t) [] }
  where
    go ts !o = do
        let (a,b) = span isL ts
        if not (null a)
            then case uncons b of
                Just (c,d) -> go d (o ++ [concatL a] ++ [c])
                Nothing    -> o ++ [concatL a]
            else case uncons b of
                Just (c,d) -> go d (o ++ [c])
                Nothing    -> o
      where
        isL (Literal _) = True
        isL (Tag _)     = False
        concatL _ts =
            foldr trans (Literal "") _ts
          where
            trans (Literal a) (Literal b) = Literal (a `T.append` b)
            trans _           _           = undefined

-- | Build an empty 'Context'.
initContext :: Context
initContext = Context { variables = H.empty }

-- | Populate with variables and/or update variables in the given 'Context'.
--
-- >λ>setVariables [("something","something new"), ("about","Haskell")] context
-- >Context {variables = fromList [("etc.","..."),("about","Haskell"),("something","something new"),("name","")]}
setVariables :: [(T.Text,T.Text)] -> Context -> Context
setVariables ts c@Context{..} =
    case uncons ts of
        Just ((k,v),ts') ->
            setVariables ts' Context { variables = H.insert k v variables }
        Nothing          -> c

-- | Delete variables from a 'Context' by these names.
--
-- >λ>deleteVariables ["something"] context
-- >Context {variables = fromList [("etc.","..."),("about","Haskell"),("name","")]}
deleteVariables :: [T.Text] -> Context -> Context
deleteVariables ts c@Context{..} =
    case uncons ts of
        Just (k,ts') ->
            deleteVariables ts' Context { variables = H.delete k variables }
        Nothing      -> c

-- | Build a 'Context' from a list of 'Tag's and replacement 'T.Text's.
--
-- >λ>fromList [("something","something else"), ("etc.","...")]
-- >Context {variables = fromList [("etc.","..."),("something","something else")]}
--
fromList :: [(T.Text, T.Text)] -> Context
fromList ts = Context { variables = H.fromList ts }

-- | Extract 'Tag's from a 'Template' and build
-- a 'Context' with all its variables empty.
fromTemplate :: Template -> Context
fromTemplate t = setVariables ((\e -> (e,T.empty)) <$> tagsOf t) initContext

-- | Get a 'Context' from a file.
readContextFile :: FilePath -> IO (Maybe Context)
readContextFile f = decode <$> L.readFile f

-- | Write a 'Context' to a file.
--
-- @
-- {
--     "something": "something else",
--     "etc.": "..."
-- }
-- @
--
writeContextFile :: FilePath -> Context -> IO ()
writeContextFile f c = L.writeFile f $ encodePretty c

-- | Based on the given 'Context', write a
-- 'Context' file with all its variables empty.
--
-- @
-- {
--     "something": "",
--     "etc.": ""
-- }
-- @
--
initContextFile :: FilePath -> Context -> IO ()
initContextFile f Context{..} = L.writeFile f $
    encodePretty Context { variables = H.map (const T.empty) variables }

-- | Build 'Just' a (sub)'Context' made of unset variables
-- of the given context, or 'Nothing'.
--
-- >λ>unsetContext context
-- >Just (Context {variables = fromList [("name","")]})
--
unsetContext :: Context -> Maybe Context
unsetContext Context{..} = do
    let vs = H.filter (== T.empty) variables
    if vs /= H.empty
       then Just Context { variables = vs }
       else Nothing

-- | Get a 'Template' from a file.
readTemplateFile :: FilePath -> IO (Either String Template)
readTemplateFile f = fromText <$> I.readFile f

-- | Write a 'Template' to a file.
writeTemplateFile :: FilePath -> Template -> IO ()
writeTemplateFile f t = I.writeFile f $ toText t

-- | Output the content of the given 'Template'
-- as it is, with its 'Tag's, if they exist (no
-- 'Context' is processed).
toText :: Template -> T.Text
toText Template{..} =
    T.concat $ trans <$> content
  where
    trans (Literal c) = c
    trans (Tag k)     = T.concat ["{{",k,"}}"]

-- | Get the list of 'Tag's in the given 'Template'.
tagsOf :: Template -> [Tag]
tagsOf Template{..} =
    (\(Tag k) -> k) <$> filter isTag content
  where
    isTag (Tag _) = True
    isTag _       = False

tagsRename :: [(T.Text,T.Text)] -> Template -> Template
tagsRename ts Template{..} =
    Template { content = rename <$> content }
  where
    rename t@(Tag n) =
        case lookup n ts of
            Just r  -> Tag r
            Nothing -> t
    rename l@(Literal _) = l

-- | 'True' if a 'Template' has no more 'Tag'
-- inside and can be used as a final 'T.Text'.
isFinal :: Template -> Bool
isFinal Template{..} =
    allLiteral content
  where
    allLiteral t =
        case uncons t of
            Just (t',ts) ->
                case t' of
                    Literal _ -> allLiteral ts
                    Tag _     -> False
            Nothing      -> True

-- | Process, discard 'Tag's which are not in the 'Context'
-- and leave them without replacement text in the final 'T.Text'.
process :: Template -> Context -> T.Text
process = processWithDefault T.empty

-- | Process and replace missing variables in 'Context'
-- with the given default replacement 'T.Text'.
processWithDefault :: T.Text    -- ^ Default replacement text
                   -> Template
                   -> Context
                   -> T.Text
processWithDefault d Template{..} c = toTextWithContext (const d) c content

-- | Process a (sub)'Context' present in the given template, leaving
-- untouched, if they exist, other 'Tag's, to obtain a new template.
partialProcess :: Template -> Context -> Template
partialProcess Template{..} c =
    Template { content = transTags content c }
  where
    transTags ts Context{..} =
        trans <$> ts
      where
        trans i@(Tag k) =
            case H.lookup k variables of
                Just v  -> Literal v
                Nothing -> i
        trans t = t

-- | Process a (sub)'Context' present in the given template, and
-- get either a 'Final' 'T.Text' or a new 'Template' with the list
-- of its 'Tag's.
--
-- >λ>partialProcess' template context
-- >Partial {template = Template {content = [Literal "Some ",Tag "tags",Literal " are unused in this ",Tag "text",Literal "."]}, tags = ["tags","text"]}
partialProcess' :: Template -> Context -> G.Result
partialProcess' t c@Context{..} =
    case foldl trans ([],[]) (content t) of
        (f,[]) -> Final $ toTextWithContext (const T.empty) c f
        (p,p') -> G.Partial Template { content = p } p'
  where
    trans (!c',!ts) t' =
        case t' of
            Tag k     ->
                case H.lookup k variables of
                    Just v  -> (c' ++ [Literal v],ts)
                    Nothing -> (c' ++ [t'],ts ++ [k])
            Literal _ -> (c' ++ [t'],ts)

