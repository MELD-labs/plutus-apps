{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE MonoLocalBinds     #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# OPTIONS_GHC -Wno-simplifiable-class-constraints #-}
{-# OPTIONS_GHC -fno-specialise #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas #-}
module Ledger.Constraints.OnChain
    ( checkScriptContext
    , checkOwnInputConstraint
    , checkOwnOutputConstraint
    ) where

import PlutusTx (ToData (toBuiltinData))
import PlutusTx.Prelude (AdditiveSemigroup ((+)), Bool (False, True), Eq ((==)), Maybe (Just), Ord ((<=), (>=)), all,
                         any, elem, isJust, maybe, traceIfFalse, ($), (&&), (.))

import Ledger qualified
import Ledger.Address (PaymentPubKeyHash (PaymentPubKeyHash, unPaymentPubKeyHash))
import Ledger.Constraints.TxConstraints (ScriptInputConstraint (ScriptInputConstraint, icTxOutRef),
                                         ScriptOutputConstraint (ScriptOutputConstraint, ocDatum, ocValue),
                                         TxConstraint (MustBeSignedBy, MustHashDatum, MustIncludeDatum, MustMintValue, MustPayToOtherScript, MustPayToPubKeyAddress, MustProduceAtLeast, MustSatisfyAnyOf, MustSpendAtLeast, MustSpendPubKeyOutput, MustSpendScriptOutput, MustValidateIn),
                                         TxConstraints (TxConstraints, txConstraints, txOwnInputs, txOwnOutputs))
import Ledger.Value qualified as Value
import Plutus.V1.Ledger.Address qualified as Address
import Plutus.V1.Ledger.Interval (contains)
import Plutus.V1.Ledger.Value (adaSymbol, adaToken, leq)
import Plutus.V2.Ledger.Contexts (ScriptContext (ScriptContext, scriptContextTxInfo),
                                  TxInInfo (TxInInfo, txInInfoOutRef, txInInfoResolved),
                                  TxInfo (txInfoData, txInfoInputs, txInfoMint, txInfoValidRange),
                                  TxOut (TxOut, txOutAddress, txOutDatum, txOutValue))
import Plutus.V2.Ledger.Contexts qualified as V

{-# INLINABLE checkScriptContext #-}
-- | Does the 'ScriptContext' satisfy the constraints?
checkScriptContext :: forall i o. ToData o => TxConstraints i o -> ScriptContext -> Bool
checkScriptContext TxConstraints{txConstraints, txOwnInputs, txOwnOutputs} ptx =
    traceIfFalse "Ld" -- "checkScriptContext failed"
    $ all (checkTxConstraint ptx) txConstraints
    && all (checkOwnInputConstraint ptx) txOwnInputs
    && all (checkOwnOutputConstraint ptx) txOwnOutputs

{-# INLINABLE checkOwnInputConstraint #-}
checkOwnInputConstraint :: ScriptContext -> ScriptInputConstraint a -> Bool
checkOwnInputConstraint ScriptContext{scriptContextTxInfo} ScriptInputConstraint{icTxOutRef} =
    let checkInput TxInInfo{txInInfoOutRef} =
            txInInfoOutRef == icTxOutRef -- TODO: We should also check the redeemer but we can't right now because it's hashed
    in traceIfFalse "L0" -- "Input constraint"
    $ any checkInput (txInfoInputs scriptContextTxInfo)

{-# INLINABLE checkOwnOutputConstraint #-}
checkOwnOutputConstraint
    :: ToData o
    => ScriptContext
    -> ScriptOutputConstraint o
    -> Bool
checkOwnOutputConstraint ctx@ScriptContext{scriptContextTxInfo} ScriptOutputConstraint{ocDatum, ocValue} =
    let hsh = V.findDatumHash (Ledger.Datum $ toBuiltinData ocDatum) scriptContextTxInfo
        checkValue val =
               Value.valueOf val adaSymbol adaToken >= Value.valueOf ocValue adaSymbol adaToken
            && Value.valueOf val adaSymbol adaToken <= Value.valueOf ocValue adaSymbol adaToken + Ledger.minAdaTxOut
            && Value.noAdaValue val == Value.noAdaValue ocValue
        checkOutput TxOut{txOutValue, txOutDatum=Ledger.OutputDatum datum} =
               checkValue txOutValue && (Ledger.Datum $ toBuiltinData ocDatum) == datum
        checkOutput TxOut{txOutValue, txOutDatum=Ledger.OutputDatumHash datumHash} =
               checkValue txOutValue && hsh == Just datumHash
        checkOutput _       = False
    in traceIfFalse "L1" -- "Output constraint"
    $ any checkOutput (V.getContinuingOutputs ctx)

{-# INLINABLE checkTxConstraint #-}
checkTxConstraint :: ScriptContext -> TxConstraint -> Bool
checkTxConstraint ctx@ScriptContext{scriptContextTxInfo} = \case
    MustIncludeDatum dv ->
        traceIfFalse "L2" -- "Missing datum"
        $ dv `elem` txInfoData scriptContextTxInfo
    MustValidateIn interval ->
        traceIfFalse "L3" -- "Wrong validation interval"
        $ interval `contains` txInfoValidRange scriptContextTxInfo
    MustBeSignedBy pkh ->
        traceIfFalse "L4" -- "Missing signature"
        $ scriptContextTxInfo `V.txSignedBy` unPaymentPubKeyHash pkh
    MustSpendAtLeast vl ->
        traceIfFalse "L5" -- "Spent value not OK"
        $ vl `leq` V.valueSpent scriptContextTxInfo
    MustProduceAtLeast vl ->
        traceIfFalse "L6" -- "Produced value not OK"
        $ vl `leq` V.valueProduced scriptContextTxInfo
    MustSpendPubKeyOutput txOutRef ->
        traceIfFalse "L7" -- "Public key output not spent"
        $ maybe False ((== Ledger.NoOutputDatum) . txOutDatum . txInInfoResolved) (V.findTxInByTxOutRef txOutRef scriptContextTxInfo)
    MustSpendScriptOutput txOutRef _ ->
        traceIfFalse "L8" -- "Script output not spent"
        -- Unfortunately we can't check the redeemer, because TxInfo only
        -- gives us the redeemer's hash, but 'MustSpendScriptOutput' gives
        -- us the full redeemer
        $ isJust (V.findTxInByTxOutRef txOutRef scriptContextTxInfo)
    MustMintValue mps _ tn v ->
        traceIfFalse "L9" -- "Value minted not OK"
        $ Value.valueOf (txInfoMint scriptContextTxInfo) (Value.mpsSymbol mps) tn == v
    MustPayToPubKeyAddress (PaymentPubKeyHash pk) _ mdv vl ->
        let outs = V.txInfoOutputs scriptContextTxInfo
            hsh dv = V.findDatumHash dv scriptContextTxInfo
            checkOutput (Just dv) TxOut{txOutDatum=Ledger.OutputDatumHash svh} = hsh dv == Just svh
            checkOutput (Just dv) TxOut{txOutDatum=Ledger.OutputDatum datum}   = dv == datum
            -- return 'True' by default meaning we fail only when the provided datum is not found
            checkOutput _ _                                                    = True
        in
        traceIfFalse "La" -- "MustPayToPubKey"
        $ vl `leq` V.valuePaidTo scriptContextTxInfo pk && any (checkOutput mdv) outs
    MustPayToOtherScript vlh dv vl ->
        let outs = V.txInfoOutputs scriptContextTxInfo
            hsh = V.findDatumHash dv scriptContextTxInfo
            addr = Address.scriptHashAddress vlh
            checkValue val address =
                   Value.valueOf val adaSymbol adaToken >= Value.valueOf vl adaSymbol adaToken
                && Value.valueOf val adaSymbol adaToken <= Value.valueOf vl adaSymbol adaToken + Ledger.minAdaTxOut
                && Value.noAdaValue val == Value.noAdaValue vl
                && address == addr
            checkOutput TxOut{txOutAddress, txOutValue, txOutDatum=Ledger.OutputDatumHash svh} =
                   checkValue txOutValue txOutAddress && hsh == Just svh
            checkOutput TxOut{txOutAddress, txOutValue, txOutDatum=Ledger.OutputDatum svh} =
                   checkValue txOutValue txOutAddress && dv == svh
            checkOutput _ = False
        in
        traceIfFalse "Lb" -- "MustPayToOtherScript"
        $ any checkOutput outs
    MustHashDatum dvh dv ->
        traceIfFalse "Lc" -- "MustHashDatum"
        $ V.findDatum dvh scriptContextTxInfo == Just dv
    MustSatisfyAnyOf xs ->
        traceIfFalse "Ld" -- "MustSatisfyAnyOf"
        $ any (all (checkTxConstraint ctx)) xs
