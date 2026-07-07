-- | @(inherit BASE)@ for content definitions: a definition may name an
--   EARLIER definition as its base and only write the fields it changes.
--   Pure form-level expansion, run before compilation, so every schema gets
--   inheritance for free.
--
--   > (enemy slime … (stats (hp 30) (damage 10) (speed 60)) …)
--   > (enemy slime-red
--   >   (inherit slime)
--   >   (color 220 80 80)          ; overrides the base's color
--   >   (stats (hp 50))            ; merge-field: hp overridden, rest kept
--   >   (spawn-at 05-abyssgate 30 13))  ; spawn-at is never inherited
--
--   Rules:
--
--     * The base must be defined EARLIER (same file or an earlier-sorted
--       file) — this makes cycles impossible and keeps reading order =
--       dependency order.
--     * A child field replaces the base's field of the same name wholesale,
--       EXCEPT the caller-listed merge fields (e.g. @stats@) whose inner
--       forms merge per name, and the caller-listed RULE fields (e.g. an
--       NPC's @dialogue@) whose inner rules CONCATENATE — the child's rules
--       first (higher priority, rules are tried top to bottom), then the
--       base's; the base's @(default …)@ steps aside when the child brings
--       its own.
--     * Caller-listed drop fields (e.g. @spawn-at@) are never inherited —
--       placement is per-definition.
module Script.Inherit
  ( resolveInherits
  ) where

import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T

import Script.Sexp

-- | Expand @(inherit …)@ across definition bodies (each @id : fields@), in
--   order. Chains work naturally: a base that itself inherited is already
--   expanded when a later child names it.
resolveInherits
  :: String   -- ^ definition kind, for error messages ("enemy", "item"…)
  -> [Text]   -- ^ merge fields: inner forms merge per name (e.g. ["stats"])
  -> [Text]   -- ^ rule fields: inner rules concatenate child-first (e.g. ["dialogue"])
  -> [Text]   -- ^ drop fields: never inherited (e.g. ["spawn-at"])
  -> [[Sexp]] -- ^ definition bodies in file order
  -> Either String [[Sexp]]
resolveInherits kind mergeFields ruleFields dropFields bodies =
  reverse . snd <$> foldl step (Right (M.empty, [])) bodies
  where
    step acc body = do
      (seen, done) <- acc
      case body of
        (idForm : fields)
          | Just rawId <- sexpSymbol idForm -> do
              expanded <- expand seen rawId fields
              Right ( M.insert rawId expanded seen
                    , (idForm : expanded) : done )
        _ -> Right (seen, body : done)  -- malformed; the compiler reports it

    expand :: M.Map Text [Sexp] -> Text -> [Sexp] -> Either String [Sexp]
    expand seen rawId fields = case inheritsOf fields of
      []     -> Right fields
      [base] -> case M.lookup base seen of
        Nothing ->
          Left (kind <> " '" <> T.unpack rawId <> "' inherits unknown "
                <> kind <> " '" <> T.unpack base
                <> "' (bases must be defined earlier)")
        Just parentFields -> Right (mergeBodies parentFields childFields)
      _ -> Left (kind <> " '" <> T.unpack rawId
                 <> "': multiple (inherit …) forms")
      where
        childFields = [ f | f <- fields, fieldName f /= Just "inherit" ]

    inheritsOf fields = [ base | SList [SSym "inherit", SSym base] <- fields ]

    -- Parent fields first (child-overridden ones removed or merged), then
    -- the child's own fields (minus those already merged into a parent's).
    mergeBodies parentFields childFields = inherited <> overrides
      where
        childNames = [ n | Just n <- map fieldName childFields ]
        childOf n = [ c | c <- childFields, fieldName c == Just n ]

        inherited = concat
          [ result
          | p <- parentFields
          , Just n <- [fieldName p]
          , n `notElem` dropFields
          , let result
                  | n `notElem` childNames = [p]
                  | n `elem` mergeFields = map (mergeField p) (take 1 (childOf n))
                  | n `elem` ruleFields = map (mergeRules p) (take 1 (childOf n))
                  | otherwise = []  -- replaced wholesale by the child's
          ]

        overrides =
          [ c | c <- childFields
          , not (isMergedIntoParent c)
          ]
        isMergedIntoParent c = case fieldName c of
          Just n -> n `elem` (mergeFields <> ruleFields)
                    && any (\p -> fieldName p == Just n) parentFields
          Nothing -> False

    fieldName (SList (SSym n : _)) = Just n
    fieldName _                    = Nothing

    -- Merge one field's inner forms per name: the child's inners win.
    mergeField (SList (pn : pInner)) (SList (_ : cInner)) =
      let cNames = [ n | Just n <- map fieldName cInner ]
          kept = [ p | p <- pInner, maybe True (`notElem` cNames) (fieldName p) ]
      in SList (pn : kept <> cInner)
    mergeField p _ = p

    -- Concatenate rule lists child-first (rules are tried top to bottom, so
    -- the child's take priority); the parent's (default …) is dropped when
    -- the child brings its own.
    mergeRules (SList (pn : pInner)) (SList (_ : cInner)) =
      let childHasDefault = any ((== Just "default") . fieldName) cInner
          kept = [ p | p <- pInner
                 , not (childHasDefault && fieldName p == Just "default") ]
      in SList (pn : cInner <> kept)
    mergeRules p _ = p
