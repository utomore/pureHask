-- | Enemy definitions: compiling @assets/enemies/*.enemy@ files.
--
--   Behaviour is a DATA-DRIVEN BEHAVIOR TREE — no GUI editor needed, the
--   S-expression IS the tree (full reference in docs/DSL_GUIDE.md §6):
--
--   > (enemy warden
--   >   (name enemy.warden.name)
--   >   (stats (hp 90) (damage 22) (speed 80))
--   >   (behavior-tree
--   >     (select                              ; first child that succeeds wins
--   >       (sequence (hp-below 0.35) (flee 130))
--   >       (sequence (player-within 220) (chase 80))
--   >       (patrol 2 40)))
--   >   (spawn-at 05-abyssgate 50 12))
--
--   Node vocabulary:
--
--     * composites — @(select child…)@ (or), @(sequence child…)@ (and)
--     * conditions — @(player-within D)@ @(player-beyond D)@
--       @(hp-below FRACTION)@ @(cooldown-ready)@
--     * actions — @(chase SPEED)@ @(flee SPEED)@ @(patrol RADIUS-TILES SPEED)@
--       @(stop)@ @(shoot PROJECTILE-SPEED COOLDOWN)@
--
--   The tree is evaluated reactively every simulation sub-step by
--   "Sim.EnemyCore" — conditions gate, actions succeed. New capability
--   VERBS are added in code (typed, tested); new behaviour SHAPES are just
--   new trees in data.
--
--   The older @(behavior (patrol R) (aggro D) (attack …))@ shorthand still
--   works: it compiles into an equivalent tree ('sugarTree').
module Enemy.Script
  ( EnemyDef(..)
  , EnemyAttack(..)
  , BTNode(..)
  , BTCond(..)
  , BTAction(..)
  , sugarTree
  , compileEnemy
  , compileEnemies
  , loadEnemyDir
  ) where

import qualified Data.ByteString as BS
import Data.List (sort, isSuffixOf)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8')
import Linear (V2(..))
import System.Directory (listDirectory, doesDirectoryExist)
import System.FilePath ((</>))

import Core.Config (tileSize)
import Core.Lang (LangTable, langText)
import Core.Types (EnemyId(..))
import Script.Inherit (resolveInherits)
import Script.Sexp

-- | The (behavior …) shorthand's attack modes.
data EnemyAttack
  = AttackContact
  | AttackRanged { arCooldown :: !Double, arSpeed :: !Double }
  deriving (Eq, Show)

-- | A behavior-tree node. Pure data; evaluated in "Sim.EnemyCore".
data BTNode
  = BTSelect ![BTNode]    -- ^ first child that succeeds wins
  | BTSequence ![BTNode]  -- ^ all children must succeed, in order
  | BTCond !BTCond
  | BTAct !BTAction
  deriving (Eq, Show)

data BTCond
  = CondPlayerWithin !Double  -- ^ player centre within N pixels
  | CondPlayerBeyond !Double
  | CondHpBelow !Double       -- ^ own hp fraction strictly below (0..1)
  | CondCooldownReady
  deriving (Eq, Show)

data BTAction
  = ActChase !Double          -- ^ walk towards the player (px/s)
  | ActFlee !Double           -- ^ walk away from the player (px/s)
  | ActPatrol !Double !Double -- ^ radius px around home, speed px/s
  | ActStop
  | ActShoot !Double !Double  -- ^ projectile speed, cooldown seconds
  deriving (Eq, Show)

data EnemyDef = EnemyDef
  { edId     :: !EnemyId
  , edName   :: !Text
  , edColor  :: !(Int, Int, Int)
  , edSize   :: !(V2 Double)
  , edHp     :: !Double
  , edDamage :: !Double          -- ^ contact and projectile damage
  , edSpeed  :: !Double          -- ^ default walk speed for the shorthand
  , edTree   :: !BTNode
  , edSpawns :: ![(Text, V2 Double)]  -- ^ (level name, position px)
  } deriving (Eq, Show)

-- | The classic parameter trio as a tree — what @(behavior …)@ compiles to.
sugarTree :: Double -> Double -> Double -> EnemyAttack -> BTNode
sugarTree patrolPx aggro speed attack
  | aggro <= 0.0 = patrol
  | otherwise = BTSelect
      [ BTSequence (BTCond (CondPlayerWithin aggro) : engage)
      , patrol
      ]
  where
    patrol = BTAct (ActPatrol patrolPx speed)
    engage = case attack of
      AttackContact -> [BTAct (ActChase speed)]
      AttackRanged cooldown projSpeed ->
        [ BTAct (ActShoot projSpeed cooldown)
        , BTSelect
            [ BTSequence [ BTCond (CondPlayerBeyond (aggro * 0.6))
                         , BTAct (ActChase speed) ]
            , BTAct ActStop
            ]
        ]

compileEnemy :: LangTable -> [Sexp] -> Either String EnemyDef
compileEnemy _ [] = Left "enemy: (enemy …) without an id"
compileEnemy table (idForm : body) = do
  rawId <- maybe (Left "enemy: id must be a symbol") Right (sexpSymbol idForm)
  let ctx = "enemy '" <> T.unpack rawId <> "'"

  name <- case fieldOf "name" body >>= safeHead of
    Just form -> either (Left . ((ctx <> ": ") <>)) Right (langText table form)
    Nothing   -> Left (ctx <> ": missing (name …)")

  color <- case fieldOf "color" body of
    Just [SNum r, SNum g, SNum b] -> Right (round r, round g, round b)
    Just other -> Left (ctx <> ": bad color " <> show other)
    Nothing    -> Right (200, 80, 80)

  size <- case fieldOf "size" body of
    Just [SNum w, SNum h] -> Right (V2 w h)
    Just other -> Left (ctx <> ": bad size " <> show other)
    Nothing    -> Right (V2 24.0 24.0)

  statForms <- maybe (Left (ctx <> ": missing (stats …)")) Right
                 (fieldOf "stats" body)
  hp     <- numField ctx statForms "hp"
  damage <- numField ctx statForms "damage"
  speed  <- numField ctx statForms "speed"

  tree <- case (fieldOf "behavior-tree" body, fieldOf "behavior" body) of
    (Just [node], _) -> compileNode ctx node
    (Just other, _)  -> Left (ctx <> ": behavior-tree wants exactly one root node, got "
                              <> show (length other))
    (Nothing, Just behavior) -> compileSugar ctx behavior speed
    (Nothing, Nothing) -> Right (sugarTree (2.0 * tileSize) 0.0 speed AttackContact)

  spawns <- mapM (compileSpawn ctx) (formsNamed "spawn-at" body)
  if null spawns
    then Left (ctx <> ": needs at least one (spawn-at LEVEL X Y)")
    else Right EnemyDef
      { edId = EnemyId rawId, edName = name, edColor = color, edSize = size
      , edHp = hp, edDamage = damage, edSpeed = speed
      , edTree = tree, edSpawns = spawns
      }
  where
    safeHead (x : _) = Just x
    safeHead []      = Nothing

    numField :: String -> [Sexp] -> Text -> Either String Double
    numField ctx' forms key = case fieldOf key forms of
      Just [SNum n] -> Right n
      Just other    -> Left (ctx' <> ": bad " <> T.unpack key <> " " <> show other)
      Nothing       -> Left (ctx' <> ": stats missing (" <> T.unpack key <> " N)")

    compileSpawn ctx' forms = case forms of
      [SSym lvl, SNum tx, SNum ty] ->
        Right (lvl, V2 (tx * tileSize) (ty * tileSize))
      other -> Left (ctx' <> ": bad spawn-at " <> show other)

-- | The @(behavior …)@ shorthand: patrol radius + aggro + attack mode.
compileSugar :: String -> [Sexp] -> Double -> Either String BTNode
compileSugar ctx behavior speed = do
  patrol <- case fieldOf "patrol" behavior of
    Just [SNum r]  -> Right (r * tileSize)
    Just other     -> Left (ctx <> ": bad patrol " <> show other)
    Nothing        -> Right (2.0 * tileSize)
  aggro <- case fieldOf "aggro" behavior of
    Just [SNum d]  -> Right d
    Just other     -> Left (ctx <> ": bad aggro " <> show other)
    Nothing        -> Right 0.0
  attack <- case fieldOf "attack" behavior of
    Just [SSym "contact"] -> Right AttackContact
    Just [SSym "ranged", SNum cd, SNum spd] -> Right (AttackRanged cd spd)
    Just other -> Left (ctx <> ": bad attack " <> show other
                        <> " (want (attack contact) or (attack ranged CD SPEED))")
    Nothing -> Right AttackContact
  Right (sugarTree patrol aggro speed attack)

-- | One behavior-tree node form.
compileNode :: String -> Sexp -> Either String BTNode
compileNode ctx form = case form of
  SList (SSym "select" : kids)
    | not (null kids) -> BTSelect <$> mapM (compileNode ctx) kids
  SList (SSym "sequence" : kids)
    | not (null kids) -> BTSequence <$> mapM (compileNode ctx) kids
  SList [SSym "player-within", SNum d] -> Right (BTCond (CondPlayerWithin d))
  SList [SSym "player-beyond", SNum d] -> Right (BTCond (CondPlayerBeyond d))
  SList [SSym "hp-below", SNum f]      -> Right (BTCond (CondHpBelow f))
  SList [SSym "cooldown-ready"]        -> Right (BTCond CondCooldownReady)
  SList [SSym "chase", SNum s]         -> Right (BTAct (ActChase s))
  SList [SSym "flee", SNum s]          -> Right (BTAct (ActFlee s))
  SList [SSym "patrol", SNum r, SNum s] ->
    Right (BTAct (ActPatrol (r * tileSize) s))
  SList [SSym "stop"]                  -> Right (BTAct ActStop)
  SList [SSym "shoot", SNum spd, SNum cd] -> Right (BTAct (ActShoot spd cd))
  _ -> Left (ctx <> ": unknown behavior-tree node " <> show form)

-- | Compile every @(enemy …)@ form in a set of top-level forms, with
--   @(inherit BASE)@ expanded first (stats merge per-stat; spawn points are
--   never inherited — placement is per-enemy).
compileEnemies :: LangTable -> [Sexp] -> Either String [EnemyDef]
compileEnemies table forms = do
  bodies <- resolveInherits "enemy" ["stats"] [] ["spawn-at"]
              (formsNamed "enemy" forms)
  mapM (compileEnemy table) bodies

-- | Load and compile every @*.enemy@ file in a directory (sorted by name).
--   Files are parsed together so @(inherit …)@ works across files (bases
--   must sort earlier). A missing directory simply means no enemies.
loadEnemyDir :: LangTable -> FilePath -> IO (Either String [EnemyDef])
loadEnemyDir table dir = do
  exists <- doesDirectoryExist dir
  if not exists
    then pure (Right [])
    else do
      entries <- listDirectory dir
      let files = [ dir </> e | e <- sort entries, ".enemy" `isSuffixOf` e ]
      results <- mapM loadOne files
      pure (results `bindAll` (compileEnemies table . concat))
  where
    bindAll rs f = sequence rs >>= f
    loadOne path = do
      bytes <- BS.readFile path
      pure $ case decodeUtf8' bytes of
        Left err -> Left (path <> ": not valid UTF-8 (" <> show err <> ")")
        Right txt ->
          either (Left . ((path <> ": ") <>)) Right (parseSexps txt)
