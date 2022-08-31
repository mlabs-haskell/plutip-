{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

-- |
--  This module together with `Test.Plutip.Predicate` provides the way
--  to run assertions against the result of contract execution,
--  as well as funds at the wallet's UTxOs after contract being run.
--
--  Each test case starts with `assertExecution`, which accepts:
--
--    - description of test case
--    - initial funds distribution at wallets addresses (with optional Value assertions to be performed after the Contract run)
--    - contract to be tested (passed to `withContract`, more on this later)
--    - list of assertions to run against Contract return result, observable state and/or error
--
--  At least one TestWallet is required, this will be used as the own wallet for the contract. Any other
--  wallets can be used as other parties in transactions.
--
--  A TestWallet can be initialised with any positive number of lovelace, using the `initAda` or
--  `initLovelace`. In addition, the value in these wallets can be asserted after the contract
--  execution with `initAdaAssertValue` or `initAndAssertAda`. When `initAdaAssertValue` or `initAndAssertAda` used
--  to initiate wallets corresponding test case will be added automatically.
--
--  Each assertion in assertions list will become separate test case in `TestTree`,
--  however Contract will be executed only once.
--
--  E.g.:
--
--    > assertExecution
--    >   "Some Contract"                   -- Contract description
--    >   (initAda 100)                     -- wallets and initial funds for them (single wallet in this case)
--    >   (withContract $ \_ -> myContract) -- contract execution
--    >   [ shouldSucceed                   -- list of assertions
--    >   , not $ shouldYield someResult
--    >   , stateSatisfies "description" somePredicate
--    >   ]
--
--  To use multiple wallets, you can use the `Semigroup` instance of `TestWallets`. To reference the
--  wallet inside the contract, the following callback function is used together with `withContract`:
--  @[PaymentPubKeyHash] -> Contract w s e a@.
--
-- To display information useful for debugging together with test results use `assertExecutionWith`
-- and provide it with options:
--
--    - ShowBudgets, for displaying transaction execution budgets
--    - ShowTrace, for displaying contract execution trace
--    - ShowTraceButOnlyContext, like ShowTrace but filter what to show
--
--  Note that @[PaymentPubKeyHash]@ does not include the contract's own wallet,
--  for that you can use `Plutus.Contract.ownPaymentPubKeyHash` inside the Contract monad.
--
--  When contract supplied to test with `withContract`,
--  the 1st initiated wallet will be used as "own" wallet, e.g.:
--
--    > assertExecution  "Send some Ada"
--    >   (initAda 100 <> initAda 101 <> initAda 102)
--    >   (withContract $ \[pkh1, pkh2] ->
--    >     payToPubKey pkh1 (Ada.lovelaceValueOf amt))
--    >   [shouldSucceed]
--
--  Here:
--
--  - 3 wallets will be initialised with 100, 101 and 102 Ada respectively
--  - wallet with 100 Ada will be used as own wallet to run the contract
--  - `pkh1` - `PaymentPubKeyHash` of wallet with 101 Ada
--  - `pkh2` - `PaymentPubKeyHash` of wallet with 102 Ada
--
--
--  When contract supplied to test with `withContractAs`, wallet with provided index (0 based)
--  will be used as "own" wallet, e.g.:
--
--    > assertExecutionWith
--    >   [ShowBudgets, ShowTraceButOnlyContext ContractLog Error]
--    >   "Send some Ada"
--    >   (initAda 100 <> initAda 101 <> initAda 102)
--    >   (withContractAs 1 $ \[pkh0, pkh2] ->
--    >     payToPubKey pkh1 (Ada.lovelaceValueOf amt))
--    >   [shouldSucceed]
--
--  Here:
--
--    - 3 wallets will be initialised with 100, 101 and 102 Ada respectively
--    - wallet with 101 Ada will be used as own wallet to run the contract
--    - `pkh0` - `PaymentPubKeyHash` of wallet with 100 Ada
--    - `pkh2` - `PaymentPubKeyHash` of wallet with 102 Ada
--    - test result will additionaly show budget calculations and execution trace (but only contract logs)
--
--
--  If you have multiple contracts depending on each other, you can chain them together using
--  `withContract` and `withContractAs`:
--
--    > assertExecution
--    >   "Two contracts one after another"
--    >   (initAda 100 <> initAda 101)
--    >   ( do
--    >       void $ -- run something prior to the contract which result will be checked
--    >         withContract $
--    >           \[pkh1] -> payTo pkh1 10_000_000
--    >       withContractAs 1 $ -- run the contract which result will be checked
--    >         \[pkh1] -> payTo pkh1 10_000_000
--    >   )
--    >   [shouldSucceed]
--
--  Here two contracts are executed one after another.
--  Note that only execution result of the second contract will be tested.
module Test.Plutip.Contract (
  withContract,
  withContractAs,
  -- Wallet initialisation
  TestWallet (twInitDistribuition),
  -- initAda,
  -- withCollateral,
  -- initAndAssertAda,
  -- initAndAssertAdaWith,
  -- initAdaAssertValue,
  -- initAdaAssertValueWith,
  initLovelace,
  -- initAndAssertLovelace,
  -- initAndAssertLovelaceWith,
  -- initLovelaceAssertValue,
  -- initLovelaceAssertValueWith,
  -- Helpers
  ledgerPaymentPkh,
  ValueOrdering (VEq, VGt, VLt, VGEq, VLEq),
  assertValues,
  assertExecution,
  assertExecutionWith,
  ada,
  Wallets,
  WrappedContract,
) where

import BotPlutusInterface.Types (
  LogContext,
  LogLevel,
  LogLine (LogLine, logLineContext, logLineLevel),
  LogsList (getLogsList),
  sufficientLogLevel,
 )

import Control.Monad.Reader (MonadIO (liftIO), MonadReader (ask), ReaderT, runReaderT, void)
import Data.Bool (bool)
import Data.Kind (Type)
import Data.Row (Row)
import Data.Tagged (Tagged (Tagged))
import GHC.TypeLits (Nat)
import Ledger.Address (pubKeyHashAddress)
import Ledger.Value (Value)
import Plutus.Contract (Contract, waitNSlots)
import PlutusPrelude (render)
import Prettyprinter (Doc, Pretty (pretty), vcat, (<+>))
import Test.Plutip.Contract.Init (
  -- initAda,
  -- initAdaAssertValue,
  -- initAdaAssertValueWith,
  -- initAndAssertAda,
  -- initAndAssertAdaWith,
  -- initAndAssertLovelace,
  -- initAndAssertLovelaceWith,
  initLovelace,
  -- initLovelaceAssertValue,
  -- initLovelaceAssertValueWith,
  -- withCollateral,
 )
import Test.Plutip.Contract.Types (
  TestContract (TestContract),
  TestContractConstraints,
  TestWallet (twInitDistribuition),
  Wallets,
  ValueOrdering (VEq, VGEq, VGt, VLEq, VLt),
  NthWallet(nthWallet)
 )
import Test.Plutip.Contract.Values (assertValues, valueAt)
import Test.Plutip.Internal.BotPlutusInterface.Run (runContract)
import Test.Plutip.Internal.BotPlutusInterface.Wallet (BpiWallet, ledgerPaymentPkh)
import Test.Plutip.Internal.Types (
  ClusterEnv,
  ExecutionResult (contractLogs, outcome),
  budgets,
 )
import Test.Plutip.Options (TraceOption (ShowBudgets, ShowTrace, ShowTraceButOnlyContext))
import Test.Plutip.Predicate (Predicate, noBudgetsMessage, pTag)
import Test.Plutip.Tools (ada)
import Test.Plutip.Tools.Format (fmtTxBudgets)
import Test.Tasty (testGroup, withResource)
import Test.Tasty.Providers (IsTest (run, testOptions), TestTree, singleTest, testPassed)

type TestRunner (w :: Type) (e :: Type) (a :: Type) (idxs :: [Nat]) =
  ReaderT (ClusterEnv, Wallets idxs BpiWallet) IO (ExecutionResult w e (a, Wallets idxs Value))

type WrappedContract (w :: Type) (s :: Row Type) (e :: Type) (a :: Type) (idxs :: [Nat])
  = ReaderT (Wallets idxs BpiWallet) (Contract w s e) a


-- | When used with `withCluster`, builds `TestTree` from initial wallets distribution,
--  Contract and list of assertions (predicates). Each assertion will be run as separate test case,
--  although Contract will be executed only once.
--
-- > assertExecution
-- >   "Some Contract"                   -- Contract description
-- >   (initAda 100)                     -- wallets and initial funds for them (single wallet in this case)
-- >   (withContract $ \_ -> myContract) -- contract execution
-- >   [ shouldSucceed                   -- list of assertions
-- >   , not $ shouldYield someResult
-- >   , stateSatisfies "description" somePredicate
-- >   ]
--
-- @since 0.2
assertExecution ::
  forall (w :: Type) (e :: Type) (a :: Type) (idxs :: [Nat]).
  TestContractConstraints w e a idxs =>
  String ->
  TestRunner w e a idxs ->
  [Predicate w e a idxs] ->
  IO (ClusterEnv, Wallets idxs BpiWallet) -> TestTree
assertExecution = assertExecutionWith mempty

-- | Version of assertExecution parametrised with a list of extra TraceOption's.
--
-- > assertExecutionWith [ShowTrace, ShowBudgets]
--
-- to print additional transaction budget calculations and contract execution logs
assertExecutionWith ::
  forall (w :: Type) (e :: Type) (a :: Type) (idxs :: [Nat]).
  TestContractConstraints w e a idxs =>
  [TraceOption] ->
  String ->
  TestRunner w e a idxs ->
  [Predicate w e a idxs] ->
  IO (ClusterEnv, Wallets idxs BpiWallet) -> TestTree
assertExecutionWith options tag testRunner predicates =
  toTestGroup
  where
    toTestGroup :: IO (ClusterEnv, Wallets idxs BpiWallet) -> TestTree
    toTestGroup ioEnv =
      withResource (runReaderT testRunner =<< ioEnv) (const $ pure ()) $
        \ioRes -> testGroup tag ((toCase ioRes <$> predicates) <> ((`optionToTestTree` ioRes) <$> options))

    -- wraps IO with result of contract execution into single test
    toCase :: IO (ExecutionResult w e (a, Wallets idxs Value)) -> Predicate w e a idxs -> TestTree
    toCase ioRes p =
      singleTest (pTag p) (TestContract p ioRes)

    optionToTestTree :: TraceOption -> IO (ExecutionResult w e (a, Wallets idxs Value)) -> TestTree
    optionToTestTree = \case
      ShowBudgets -> singleTest "Budget stats" . StatsReport
      ShowTrace -> singleTest logsName . LogsReport DisplayAllTrace
      ShowTraceButOnlyContext logCtx logLvl ->
        singleTest logsName . LogsReport (DisplayOnlyFromContext logCtx logLvl)

    logsName = "BPI logs (PAB requests/responses)"

-- | Adds test case with assertions on values if any assertions were added
--  by `initAndAssert...` functions during wallets setup
--
-- @since 0.2

-- | Run a contract using the first wallet as own wallet, and return `ExecutionResult`.
-- This could be used by itself, or combined with multiple other contracts.
--
-- @since 0.2
withContract ::
  forall (w :: Type) (s :: Row Type) (e :: Type) (a :: Type) (idxs :: [Nat]).
  (TestContractConstraints w e a idxs, NthWallet 0 idxs) =>
  WrappedContract w s e a idxs ->
  TestRunner w e a idxs
withContract = withContractAs @0

-- | Run a contract using the nth wallet as own wallet, and return `ExecutionResult`.
-- This could be used by itself, or combined with multiple other contracts.
--
-- @since 0.2
withContractAs ::
  forall (idx :: Nat) (w :: Type) (s :: Row Type) (e :: Type) (a :: Type) (idxs :: [Nat]).
  (TestContractConstraints w e a idxs, NthWallet idx idxs) =>
  WrappedContract w s e a idxs ->
  TestRunner w e a idxs
withContractAs toContract = do
  (cEnv, wallets') <- ask
  let -- pick wallet for Contract's "own PKH", other wallets PKHs will be provided
      -- to the user in `withContractAs`
      ownWallet = nthWallet @idx wallets'

      collectValuesPkhs = fmap ledgerPaymentPkh wallets'

      valuesAtWallet :: (Contract w s e (Wallets idxs Value))
      valuesAtWallet =
        void (waitNSlots 1)
          >> traverse (valueAt . (`pubKeyHashAddress` Nothing)) collectValuesPkhs

  execRes <- liftIO $ runContract cEnv ownWallet (runReaderT toContract wallets')
  execValues <- liftIO $ runContract cEnv ownWallet valuesAtWallet

  case outcome execValues of
    Left _ -> fail "Failed to get values"
    Right values -> return $ execRes {outcome = (,values) <$> outcome execRes}

newtype StatsReport w e a idxs = StatsReport (IO (ExecutionResult w e (a, Wallets idxs Value)))

instance
  forall (w :: Type) (e :: Type) (a :: Type) (idxs :: [Nat]).
  TestContractConstraints w e a idxs =>
  IsTest (StatsReport w e a idxs)
  where
  run _ (StatsReport ioRes) _ =
    testPassed . mkDescription <$> ioRes
    where
      mkDescription runRes =
        let bs = budgets runRes
         in bool (fmtTxBudgets bs) noBudgetsMessage (null bs)

  testOptions = Tagged []

-- | Test case used internally for logs printing.
data LogsReport w e a idxs = LogsReport LogsReportOption (IO (ExecutionResult w e (a, Wallets idxs Value)))

-- | TraceOption stripped to what LogsReport wants to know.
data LogsReportOption
  = -- | Display all logs collected by BPI during contract execution.
    DisplayAllTrace
  | -- | Display filtered logs
    DisplayOnlyFromContext
      LogContext
      -- ^ upper bound on LogLevel
      LogLevel

instance
  forall (w :: Type) (e :: Type) (a :: Type) (idxs :: [Nat]).
  TestContractConstraints w e a idxs =>
  IsTest (LogsReport w e a idxs)
  where
  run _ (LogsReport option ioRes) _ =
    testPassed . ppShowLogs . contractLogs <$> ioRes
    where
      ppShowLogs =
        render
          . vcat
          . zipWith indexedMsg [0 ..]
          . map pretty
          . filterOrDont
          . getLogsList

      filterOrDont = case option of
        DisplayAllTrace ->
          id -- don't
        DisplayOnlyFromContext logCtx logLvl ->
          filter
            ( \LogLine {logLineContext, logLineLevel} ->
                logLineContext == logCtx
                  && sufficientLogLevel logLvl logLineLevel
            )

      indexedMsg :: Int -> Doc ann -> Doc ann
      indexedMsg i msg = pretty i <> pretty ("." :: String) <+> msg

  testOptions = Tagged []
