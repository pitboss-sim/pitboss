module Main where

import Test.Hspec

import qualified Test.Pitboss.Integration.BoutFlowSpec as BoutFlowSpec
import qualified Test.Pitboss.Integration.BoutPlayerDecisionFlowSpec as BoutPlayerDecisionFlowSpec
import qualified Test.Pitboss.Integration.FullBoutSimulationRoundtripSpec as FullBoutSimulationRoundtripSpec
import qualified Test.Pitboss.Integration.DealerBlackjackSpec as DealerBlackjackSpec
import qualified Test.Pitboss.Integration.DoubleDownSpec as DoubleDownSpec
import qualified Test.Pitboss.Integration.SplitHandSpec as SplitHandSpec
import qualified Test.Pitboss.Integration.TimelineReconstructionSpec as TimelineReconstructionSpec
import qualified Test.Pitboss.Integration.PlayerDoubleDownWinSpec as PlayerDoubleDownWinSpec
import qualified Test.Pitboss.Integration.PlayerSplitAcesSpec as PlayerSplitAcesSpec
import qualified Test.Pitboss.Integration.PlayerBlackjackVsDealerSpec as PlayerBlackjackVsDealerSpec
import qualified Test.Pitboss.Integration.DealerSoft17HitBustSpec as DealerSoft17HitBustSpec
import qualified Test.Pitboss.Integration.ComplexPushVariationsSpec as ComplexPushVariationsSpec
import qualified Test.Pitboss.Integration.PlayerSurrenderSpec as PlayerSurrenderSpec
import qualified Test.Pitboss.Integration.SoftHandConversionSpec as SoftHandConversionSpec
import qualified Test.Pitboss.Integration.PlayerSplitPairsSpec as PlayerSplitPairsSpec
import qualified Test.Pitboss.Integration.InsuranceScenarioSpec as InsuranceScenarioSpec
import qualified Test.Pitboss.Integration.MultiSplitScenarioSpec as MultiSplitScenarioSpec
import qualified Test.Pitboss.Property.ChipPropertySpec as ChipPropertySpec
import qualified Test.Pitboss.Property.HandPropertySpec as HandPropertySpec
import qualified Test.Pitboss.Unit.Causality.DeltaSpec as DeltaSpec
import qualified Test.Pitboss.Unit.Causality.FiniteMapSpec as FiniteMapSpec
import qualified Test.Pitboss.Unit.Causality.UidSpec as UidSpec
import qualified Test.Pitboss.Unit.Domain.HandSpec as HandSpec
import qualified Test.Pitboss.Unit.Domain.RulesSpec as RulesSpec
import qualified Test.Pitboss.Unit.FSM.BoutPlayerHandSpec as BoutPlayerHandSpec
import qualified Test.Pitboss.Unit.Serialization.JsonSpec as JsonSpec
import qualified Test.Pitboss.Unit.Intent.ValidationSpec as ValidationSpec

main :: IO ()
main = hspec $ do
    describe "Integration.BoutFlow" BoutFlowSpec.spec
    describe "Integration.BoutPlayerDecisionFlow" BoutPlayerDecisionFlowSpec.spec
    describe "Integration.FullBoutSimulationRoundtrip" FullBoutSimulationRoundtripSpec.spec
    describe "Integration.DealerBlackjack" DealerBlackjackSpec.spec
    describe "Integration.DoubleDown" DoubleDownSpec.spec
    describe "Integration.SplitHand" SplitHandSpec.spec
    describe "Integration.TimelineReconstruction" TimelineReconstructionSpec.spec
    describe "Integration.PlayerDoubleDownWin" PlayerDoubleDownWinSpec.spec
    describe "Integration.PlayerSplitAces" PlayerSplitAcesSpec.spec
    describe "Integration.PlayerBlackjackVsDealer" PlayerBlackjackVsDealerSpec.spec
    describe "Integration.DealerSoft17HitBust" DealerSoft17HitBustSpec.spec
    describe "Integration.ComplexPushVariations" ComplexPushVariationsSpec.spec
    describe "Integration.PlayerSurrender" PlayerSurrenderSpec.spec
    describe "Integration.SoftHandConversion" SoftHandConversionSpec.spec
    describe "Integration.PlayerSplitPairs" PlayerSplitPairsSpec.spec
    describe "Integration.InsuranceScenario" InsuranceScenarioSpec.spec
    describe "Integration.MultiSplitScenario" MultiSplitScenarioSpec.spec
    describe "Property.ChipProperty" ChipPropertySpec.spec
    describe "Property.HandProperty" HandPropertySpec.spec
    describe "Unit.Causality.Delta" DeltaSpec.spec
    describe "Unit.Causality.FiniteMap" FiniteMapSpec.spec
    describe "Unit.Causality.Uid" UidSpec.spec
    describe "Unit.Domain.Hand" HandSpec.spec
    describe "Unit.Domain.Rules" RulesSpec.spec
    describe "Unit.FSM.BoutPlayerHand" BoutPlayerHandSpec.spec
    describe "Unit.Serialization.Json" JsonSpec.spec
    describe "Unit.Intent.Validation" ValidationSpec.spec
