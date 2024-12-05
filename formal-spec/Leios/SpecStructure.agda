{-# OPTIONS --safe #-}

open import Leios.Prelude hiding (id)
open import Leios.Abstract
open import Leios.FFD
open import Leios.VRF

import Leios.Base
import Leios.Blocks
import Leios.KeyRegistration

module Leios.SpecStructure where

record SpecStructure : Type₁ where
  field a : LeiosAbstract

  open LeiosAbstract a public
  open Leios.Blocks a public

  field ⦃ IsBlock-Vote ⦄ : IsBlock (List Vote)
        ⦃ Hashable-IBHeaderOSig ⦄ : ∀ {b} → Hashable (IBHeaderOSig b) Hash
        ⦃ Hashable-PreEndorserBlock ⦄ : Hashable PreEndorserBlock Hash
        poolId : PoolID
        FFD' : FFDAbstract.Functionality ffdAbstract
        vrf' : LeiosVRF a

  open LeiosVRF vrf' public

  field sk-IB sk-EB sk-V : PrivKey
        pk-IB pk-EB pk-V : PubKey

  open Leios.Base a vrf' public

  field B' : BaseAbstract
        BF : BaseAbstract.Functionality B'
        initBaseState : BaseAbstract.Functionality.State BF

  open Leios.KeyRegistration a vrf' public

  field K' : KeyRegistrationAbstract
        KF : KeyRegistrationAbstract.Functionality K'

  module B   = BaseAbstract.Functionality BF
  module K   = KeyRegistrationAbstract.Functionality KF
  module FFD = FFDAbstract.Functionality FFD'
