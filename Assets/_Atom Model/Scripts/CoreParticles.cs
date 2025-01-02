using System;
using System.Collections.Generic;
//using Sirenix.OdinInspector;
using UnityEngine;


public class CoreParticles : MonoBehaviour
{

    [SerializeField] Transform innerCoreParticlePrefab;
    [SerializeField] Transform outerCoreParticlePrefab;
    
    
    [SerializeField] List<Transform> innerCoreParticles;
    [SerializeField] List<Transform> outerCoreParticles;



    [Range(0f, 1f)]
    [SerializeField] float innerCoreActive;

    [SerializeField]
    int innerCoreActiveCount;
    
    [Range(0f, 1f)]
    [SerializeField] float outerCoreActive;
    
    [SerializeField]
    int outerCoreActiveCount;
    
    
    
    
    // create more spheres
    // a slider that adjusts how many of those are active, show the number above
    // do it for both
    // then try assigning the random to each one:
    // just use material properties for now
    // and only do it at runtime
    
    
    
    
    
    

    void Start()
    {
     
        SetRandomPostionForAllParticles();
    }

    void SetRandomPostionForAllParticles()
    {
        foreach (var innerCoreParticle in innerCoreParticles)
        {
            innerCoreParticle.localPosition = GetTinyRandomOffset();
        }
        
        foreach (var outerCoreParticle in outerCoreParticles)
        {
            outerCoreParticle.localPosition = GetTinyRandomOffset();
        }
    }

    Vector3 GetTinyRandomOffset()
    {
        float randomX = UnityEngine.Random.Range(-0.001f, 0.001f);
        float  randomY = UnityEngine.Random.Range(-0.001f, 0.001f);
        float randomZ = UnityEngine.Random.Range(-0.001f, 0.001f);
        return new Vector3(randomX, randomY, randomZ);
    }


   // [Button (ButtonSizes.Large)]
    void CreateMoreInnerCoreParticles()
    {
        for (int i = 0; i < 10; i++)
        {
            var newParticle = Instantiate(innerCoreParticlePrefab, transform);
            Vector3 randomOffset = GetTinyRandomOffset();
            newParticle.localPosition = randomOffset;
            
            innerCoreParticles.Add(newParticle);
        }
        
        AdjustInnerCoreParticles();
    }
    
    
    //[Button (ButtonSizes.Large)]
    void CreateMoreOuterCoreParticles()
    {
        for (int i = 0; i < 10; i++)
        {
            var newParticle = Instantiate(outerCoreParticlePrefab, transform);
            Vector3 randomOffset = GetTinyRandomOffset();
            newParticle.localPosition = randomOffset;
            
            outerCoreParticles.Add(newParticle);
        }
        
        AdjustOuterCoreParticles();
    }






    //[Button(ButtonSizes.Large)]
    void SetRandomOneForOuterCore()
    {
        foreach (var particle in outerCoreParticles)
        {
            particle.GetComponent<Renderer>().material.SetFloat("_RandomeOne", UnityEngine.Random.value);
        }
    }


    void OnValidate()
    {
        AdjustInnerCoreParticles();
        AdjustOuterCoreParticles();
    }
    
    
    

    void AdjustInnerCoreParticles()
    {
        innerCoreActiveCount = Mathf.RoundToInt(innerCoreActive * innerCoreParticles.Count);

        for (int i = 0; i < innerCoreParticles.Count; i++)
        {
            if (i < innerCoreActiveCount)
            {
                innerCoreParticles[i].gameObject.SetActive(true);
            }
            else
            {
                innerCoreParticles[i].gameObject.SetActive(false);
            }
        }
    }
    
    
    
    void AdjustOuterCoreParticles()
    {
        outerCoreActiveCount = Mathf.RoundToInt(outerCoreActive * outerCoreParticles.Count);

        for (int i = 0; i < outerCoreParticles.Count; i++)
        {
            if (i < outerCoreActiveCount)
            {
                outerCoreParticles[i].gameObject.SetActive(true);
            }
            else
            {
                outerCoreParticles[i].gameObject.SetActive(false);
            }
        }
    }
    
}