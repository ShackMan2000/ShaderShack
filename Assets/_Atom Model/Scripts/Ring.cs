using System;
using System.Collections;
using System.Collections.Generic;
//using Sirenix.OdinInspector;
using UnityEngine;
using UnityEngine.Rendering;
using UnityEngine.Serialization;

[Serializable]
public class Ring : MonoBehaviour
{
    public Material ringMaterial;
    [FormerlySerializedAs("overdriveMaterial")] public Material fusionMaterial;
    public Material particleMaterial;

    public Transform ParticlePrefab;
    public List<Transform> Particles = new();

    [SerializeField] Transform particleContainer;
    [SerializeField] Transform ringTexture;
    [SerializeField] Transform overdriveTexture;

    [SerializeField] RingController ringController;


    int activeParticlesCount;
    [FormerlySerializedAs("mergeTime")] public float fusionTime = 2f;

   // int particlesSpawnedDuringFusionBuildUp;
    // Dictionary<Transform, Material> particleMaterials;

    bool spawnRoutineRunning;
    bool fusionRoutineRunning;
    bool fusionBuildUpRunning;


    [SerializeField] RingConfig config;

    public int ID;

    // particles rotate by moving the parent transform
    // texture moves by being rotated
    // waves of the texture move via shader, set the float
    // just divide by 360
    // now lets have a method to set all via the ring controller


    bool liveUpdateTextureSpeed;
    

    public int ActiveParticlesCount
    {
        get => activeParticlesCount;

        private set
        {
            activeParticlesCount = value;
           
            for (int i = 0; i < Particles.Count; i++)
            {
                Particles[i].gameObject.SetActive(i < activeParticlesCount);
            }

            //  ringMaterial.SetFloat(Reveal, activeParticlesCount / 10f);
            ringMaterial.SetInt(ActiveParticlesCountProp, activeParticlesCount);
        }
    }


  //  [ShowInInspector, ReadOnly]
    float GetRadius()
    {
        float radius = config.RadiusFirstRing;
        // radius += config.RadiusBumpToMakeParticlesLookInsideTheRing;
        radius += config.RadiusChangePerRing * ID;

        return radius;
    }


    //[ShowInInspector, ReadOnly]
    float BaseRotationSpeed => config.BaseRotationSpeedPerUnitRadius / GetRadius();

    float ParticleRotationSpeed => BaseRotationSpeed * config.ParticleSpeedMulti;
    float TextureRotationSpeed => BaseRotationSpeed * config.TextureRotationSpeedMulti;

    public bool lockStartVisibility = true;
    [FormerlySerializedAs("lockOverdriveTextureVisibility")] public bool lockOverdriveTexture = true;

    static readonly int StartVisibility = Shader.PropertyToID("_StartVisibility");
    static readonly int MaxVisibility = Shader.PropertyToID("_MaxVisibility");
    static readonly int WaveHeightWorld = Shader.PropertyToID("_WaveHeightWorld");
    static readonly int WavesRotationsPerSec = Shader.PropertyToID("_WavesRotationsPerSec");

    static readonly int Fusion = Shader.PropertyToID("_Fusion");
    static readonly int Reveal = Shader.PropertyToID("_Reveal");
    static readonly int Scale = Shader.PropertyToID("_Scale");
    static readonly int ActiveParticlesCountProp = Shader.PropertyToID("_ActiveParticlesCount");

    float currentSpeedBoost = 1f;
    static readonly int SpawnProgress = Shader.PropertyToID("_SpawnProgress");


    [SerializeField] bool DEBUGCreateFreshCopyForSpawningEachTime;


    public void Initialize(RingController rc, int id)
    {
        ringController = rc;
        ID = id;


        //  CreateParticles();
        // SetParticlesPositionAndRotation();

        SetWaveSpeed();
        SetWaveHeight();

        SetFusionTime();
    }


    void SetFusionTime()
    {
        fusionTime = config.FusionTimeFirstRing + config.FusionTimeIncreaseEachRing * ID;
    }


    ////[Button]
    public void AssignMaterials()
    {
        ringTexture.GetComponent<MeshRenderer>().material = ringMaterial;
        overdriveTexture.GetComponent<MeshRenderer>().material = fusionMaterial;
    }


    Material uniqueParticleMaterialForSpawning;
    static readonly int RotationParticleZone = Shader.PropertyToID("_RotationParticleZone");

    Material GetUniqueParticleMaterialForSpawing()
    {
        if (uniqueParticleMaterialForSpawning == null || DEBUGCreateFreshCopyForSpawningEachTime)
        {
            uniqueParticleMaterialForSpawning = Instantiate(particleMaterial);
        }

        return uniqueParticleMaterialForSpawning;
    }


    ////[Button]
    public void ScaleRingForRadius()
    {
        float radius = GetRadius();
        ringTexture.transform.localScale = Vector3.one;
        ringMaterial.SetFloat(Scale, radius);

        if (!lockOverdriveTexture)
        {
            fusionMaterial.SetFloat(Scale, radius);
        }
    }


    ////[Button]
    public void SetVisibility()
    {
        float radius = GetRadius();

        float startVisibility = radius - config.VisibilityRange;


        ringMaterial.SetFloat(StartVisibility, startVisibility);
        ringMaterial.SetFloat(MaxVisibility, radius);


        if (!lockOverdriveTexture)
        {
            fusionMaterial.SetFloat(StartVisibility, startVisibility);
            fusionMaterial.SetFloat(MaxVisibility, radius);
        }
    }


    ////[Button]
    public void SetMaxVisibility()
    {
        float radius = GetRadius();

        ringMaterial.SetFloat(MaxVisibility, radius);

        if (!lockOverdriveTexture)
        {
            fusionMaterial.SetFloat(MaxVisibility, radius);
        }
    }


    ////[Button]
    public void SetWaveHeight()
    {
        float waveHeight = config.WaveHeightFirstRing + config.WaveHeightChangePerRing * ID;
        ringMaterial.SetFloat(WaveHeightWorld, waveHeight);
        fusionMaterial.SetFloat(WaveHeightWorld, waveHeight);
        particleMaterial.SetFloat(WaveHeightWorld, waveHeight);
    }


    //[Button]
    public void CreateParticles()
    {
        for (int i = Particles.Count - 1; i >= 0; i--)
        {
            DestroyImmediate(Particles[i].gameObject);
        }

        Particles.Clear();


        for (int i = 0; i < 10; i++)
        {
            var particle = Instantiate(ParticlePrefab, particleContainer);
            // rotate 
            particle.localRotation = Quaternion.Euler(new Vector3(0f, i * 36f, 0f));
            Particles.Add(particle);

            particle.GetComponent<MeshRenderer>().material = particleMaterial;
        }


        particleMaterial.SetFloat("_OffsetFromCenter", GetRadius());
        // particleMaterial.SetFloat(Fusion, 0f);
    }


    //[Button]
    public void SetParticlesPositionAndRotation()
    {
        for (int i = 0; i < Particles.Count; i++)
        {
            Particles[i].position = new Vector3(GetRadius(), 0f, 0f);
        }

        for (int i = 0; i < Particles.Count; i++)
        {
            float rotation = 36f * i;
            Particles[i].rotation = Quaternion.Euler(0, 0, 0);
            Particles[i].RotateAround(Vector3.zero, Vector3.up, rotation);
        }
    }


    public void SetActiveParticlesCount(int count)
    {
        ActiveParticlesCount = count;

        for (int i = 0; i < Particles.Count; i++)
        {
            Particles[i].gameObject.SetActive(i < count);
        }
    }


    void Update()
    {
        particleContainer.Rotate(Vector3.up, ParticleRotationSpeed * Time.deltaTime * currentSpeedBoost);
        ringTexture.Rotate(Vector3.up, TextureRotationSpeed * Time.deltaTime * currentSpeedBoost);

        float yRotationEuler = particleContainer.localRotation.eulerAngles.y;
        ringMaterial.SetFloat(RotationParticleZone, (1f - yRotationEuler / 360f));

        if (liveUpdateTextureSpeed)
        {
            SetWaveSpeed();
        }
    }


// combine this, so there's only ever one, maybe a bool to add without merging for the set up
// public void AddNewParticle()
// {
//     
//     if (fusionBuildUpRunning)
//     {
//         particlesSpawnedDuringFusionBuildUp++;
//         return;
//     }
//     
//     
//     ActiveParticlesCount++;
//
//     for (int i = 0; i < Particles.Count; i++)
//     {
//         Particles[i].gameObject.SetActive(i < ActiveParticlesCount);
//     }
//
//     if (ActiveParticlesCount == 10)
//     {
//         if (fusionRoutineRunning)
//         {
//             Debug.LogError("Merge routine was already running, this should not happen!");
//         }
//         else
//         {
//             StartCoroutine(FusionRoutine());
//         }
//     }
// }


    void SetWaveSpeed()
    {
        ringMaterial.SetFloat(WavesRotationsPerSec, config.WavesRotationsPerSec);
        particleMaterial.SetFloat(WavesRotationsPerSec, config.WavesRotationsPerSec);
        fusionMaterial.SetFloat(WavesRotationsPerSec, config.WavesRotationsPerSec);
    }


    IEnumerator FusionRoutine()
    {
        fusionRoutineRunning = true;
        fusionBuildUpRunning = true;

        float fusionTimeRunning = 0f;

        while (fusionTimeRunning < fusionTime)
        {
            fusionTimeRunning += Time.deltaTime;
            float t = fusionTimeRunning / fusionTime;
            t = config.FusionCurve.Evaluate(t);

            currentSpeedBoost = Mathf.Lerp(1f, config.FusionSpeedMulti, t);

            ringMaterial.SetFloat(Fusion, t);
            particleMaterial.SetFloat(Fusion, t);

            float fusionTextureReveal = Mathf.InverseLerp(0f, config.RevealFusionTextureEarly, t);
            fusionTextureReveal = Mathf.Clamp01(fusionTextureReveal);

            fusionMaterial.SetFloat(Reveal, fusionTextureReveal);

            yield return null;
        }


        Ring nextRing = ringController.GetNextRing(this);

        if (nextRing != null)
        {
            nextRing.SpawnParticle(this);
        }


        ActiveParticlesCount -= 10; //particlesSpawnedDuringFusionBuildUp;
        fusionBuildUpRunning = false;
        particleMaterial.SetFloat(Fusion, 0f);

      //  particlesSpawnedDuringFusionBuildUp = 0;

        float cooldownTimeLeft = config.FusionCooldownTime;

        while (cooldownTimeLeft > 0f)
        {
            cooldownTimeLeft -= Time.deltaTime;
            float t = Mathf.InverseLerp(config.FusionCooldownTime, 0f, cooldownTimeLeft);
            t = config.FusionCooldownCurve.Evaluate(t);

            currentSpeedBoost = Mathf.Lerp(1f, config.FusionSpeedMulti, t);

            ringMaterial.SetFloat(Fusion, t);

            float fusionTextureReveal = Mathf.InverseLerp(0f, config.RevealFusionTextureEarly, t);
            fusionTextureReveal = Mathf.Clamp01(fusionTextureReveal);

            fusionMaterial.SetFloat(Reveal, fusionTextureReveal);


            yield return null;
        }


        currentSpeedBoost = 1f;
        ringMaterial.SetFloat(Fusion, 0f);
        fusionMaterial.SetFloat(Reveal, 0f);


        fusionRoutineRunning = false;
    }






    public void SpawnParticle(Ring previousRing, bool animated = true)
    {
        // might also want to show the animation of spawning, and simply set it to fusion like the other ones, will be automatic anyway
        // if (fusionBuildUpRunning)
        // {
        //     //particlesSpawnedDuringFusionBuildUp++;
        //     return;
        // }


        // float offsetOfPreviousRing = 0f;
        //
        // if (previousRing != null)
        // {
        //     offsetOfPreviousRing = previousRing.GetRadius();
        // }


        ActiveParticlesCount++;


        // for the first ring this gets called too often now.
        // the issue is kinda that the spawning takes too long.
        // maybe just hard cap it so it never takes longer...
        // if (animated)
        // {
        //     if (spawnRoutineRunning)
        //     {
        //         // this shouldn't happen, it either means that it was triggered wrongly or the merge routine takes too long
        //         Debug.LogError("Trying to spawn a particle out of a merge while the routine is already running!");
        //     }
        //     else
        //     {
        //         StartCoroutine(SpawnRoutine(offsetOfPreviousRing));
        //     }
        // }


        if (ActiveParticlesCount == 10)
        {
            if (fusionRoutineRunning)
            {
                Debug.LogError("Merge routine was already running, this should not happen!");
            }
            else
            {
                StartCoroutine(FusionRoutine());
            }
        }
    }


    IEnumerator SpawnRoutine(float startOffset)
    {
        spawnRoutineRunning = true;


        Transform newParticle = Particles[ActiveParticlesCount];

        MeshRenderer particleRenderer = newParticle.GetComponent<MeshRenderer>();
        Material spawnMaterial = GetUniqueParticleMaterialForSpawing();
        particleRenderer.material = spawnMaterial;

        spawnMaterial.SetFloat(SpawnProgress, 0f);
        spawnMaterial.SetFloat(Fusion, 0f);

        newParticle.gameObject.SetActive(true);

        Vector3 directionToCenter = (newParticle.localPosition).normalized;

        float timeRunning = 0f;
        float timeToRun = SpawnAnimationTime;

        while (timeRunning < SpawnAnimationTime)
        {
            timeRunning += Time.deltaTime;
            float t = timeRunning / timeToRun;
            float offset = Mathf.Lerp(startOffset, GetRadius(), t);

            spawnMaterial.SetFloat(SpawnProgress, t);

            Vector3 offsetVector = directionToCenter * offset;

            newParticle.localPosition = offsetVector;

            yield return null;
        }

        particleRenderer.material = particleMaterial;


        ActiveParticlesCount++;

        if (ActiveParticlesCount == 10)
        {
            if (fusionRoutineRunning)
            {
                Debug.LogError("Merge routine was already running, this should not happen!");
            }
            else
            {
                StartCoroutine(FusionRoutine());
            }
        }

        spawnRoutineRunning = false;
    }

    public float SpawnAnimationTime
    {
        get
        {
            float time = config.SpawnAnimationTimeFirstRing + config.SpawnAnimationTimeIncreasePerRing * ID;

            if (ID == 0 && time > config.SpawnTimeFirstRing)
            {
                time = config.SpawnTimeFirstRing - 0.1f;
            }


            return time;
        }
    }


    //[Button]
    void SetThisAndAllPrevioursRingsTo9Particles()
    {
        ringController.SetThisAndAllPreviousRingsTo9Particles(this);
    }
}


// //[Button]
// public void SetStartVisibility()
// {
//     float radius = GetRadius();
//
//     float startVisibility = 0f;
//
//     if (ID == 0)
//     {
//         startVisibility = radius * config.DefaultVisibilityOfRangeToPreviousRing;
//     }
//     else
//     {
//         Ring ring = ringController.GetPreviousRing(this);
//         float previousRadius = ring.GetRadius();
//
//         float gapBetweenRadius = radius - previousRadius;
//         startVisibility = gapBetweenRadius * config.DefaultVisibilityOfRangeToPreviousRing + previousRadius;
//
//         ringMaterial.SetFloat(StartVisibility, startVisibility);
//         if (!lockOverdriveTexture)
//         {
//             fusionMaterial.SetFloat(StartVisibility, startVisibility);
//         }
//     }
// }