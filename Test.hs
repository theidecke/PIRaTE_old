import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Test.QuickCheck
import Test.HUnit

import PIRaTE.Scene (
    prop_SensationPointSampler_nonzeroProb,
    prop_EmissionPointSampler_nonzeroProb,
    prop_ScatteringPointSampler_nonzeroProb,
    prop_SensationDirectionSampler_nonzeroProb,
    prop_EmissionDirectionSampler_nonzeroProb,
    prop_ScatteringDirectionSampler_nonzeroProb,
    prop_SensationDistanceSampler_nonzeroProb,
    prop_EmissionDistanceSampler_nonzeroProb,
    prop_ScatteringDistanceSampler_nonzeroProb
  )


main = defaultMain tests

tests = [
        testGroup "Point Samplers" [
          testProperty "Sensation  Point Sampler NonZeroProb" prop_SensationPointSampler_nonzeroProb,
          testProperty "Emission   Point Sampler NonZeroProb" prop_EmissionPointSampler_nonzeroProb,
          testProperty "Scattering Point Sampler NonZeroProb" prop_ScatteringPointSampler_nonzeroProb
        ],
        testGroup "Direction Samplers" [
          testProperty "Sensation  Direction Sampler NonZeroProb" prop_SensationDirectionSampler_nonzeroProb,
          testProperty "Emission   Direction Sampler NonZeroProb" prop_EmissionDirectionSampler_nonzeroProb,
          testProperty "Scattering Direction Sampler NonZeroProb" prop_ScatteringDirectionSampler_nonzeroProb
        ],
        testGroup "Distance Samplers" [
          testProperty "Sensation  Distance Sampler NonZeroProb" prop_SensationDistanceSampler_nonzeroProb,
          testProperty "Emission   Distance Sampler NonZeroProb" prop_EmissionDistanceSampler_nonzeroProb,
          testProperty "Scattering Distance Sampler NonZeroProb" prop_ScatteringDistanceSampler_nonzeroProb
        ]
    ]

