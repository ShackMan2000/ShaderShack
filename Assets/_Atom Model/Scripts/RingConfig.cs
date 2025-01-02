 
using System.Collections.Generic;
//using Sirenix.OdinInspector;
using UnityEngine;
using UnityEngine.Serialization;

//[InlineEditor]
[CreateAssetMenu]
public class RingConfig : ScriptableObject
{

    public float SpawnTimeRing5 = 14400;
    
  //  [ShowInInspector, ReadOnly]
    public float SpawnTimeFirstRing => SpawnTimeRing5 / 10000;

    public float RadiusFirstRing;
    public float RadiusChangePerRing;

    public float VisibilityRange = 0.2f;
    //public float RadiusBumpToMakeParticlesLookInsideTheRing;

    public float WaveHeightFirstRing = 0.1f;
    public float WaveHeightChangePerRing = 0.1f;

    public float DefaultVisibilityOfRangeToPreviousRing = 0.5f;
                 
    public float BaseRotationSpeedPerUnitRadius;


    public float RevealFusionTextureEarly = 0.6f;

    [Range( 0.5f, 1.5f)]
    public float TextureRotationSpeedMulti;
    
    [Range(0.1f, 1f)]
    public float WavesRotationsPerSec;
    
    [Range(0.5f, 1.5f)]
    public float ParticleSpeedMulti;


    
    //[BoxGroup("Fusion")]
    public AnimationCurve FusionCurve;

    //[BoxGroup("Fusion")]
    public float FusionTimeFirstRing;
   // [BoxGroup("Fusion")]
   public float FusionTimeIncreaseEachRing;
    
    //[BoxGroup("Fusion")]
    public AnimationCurve FusionCooldownCurve;
    //[BoxGroup("Fusion")]
    public float FusionCooldownTime = 1f;
    
    //[BoxGroup("Fusion")] 
    public float FusionSpeedMulti;

    //[BoxGroup("SpawnAnimation")]
    public float SpawnAnimationTimeFirstRing = 0.5f;
    //[BoxGroup("SpawnAnimation")]
    public float SpawnAnimationTimeIncreasePerRing = 1f;
}
