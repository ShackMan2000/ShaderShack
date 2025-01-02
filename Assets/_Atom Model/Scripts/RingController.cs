using System;
using System.Collections;
using System.Collections.Generic;
//using Sirenix.OdinInspector;
using TMPro;
using UnityEngine;
using UnityEngine.Serialization;
using Random = UnityEngine.Random;


public class RingController : MonoBehaviour
{

    [SerializeField] RingConfig config;


    public float totalSecondsPassed;

    public TextMeshProUGUI timeText;


  //  [ShowInInspector, ReadOnly]
    public string TimeAsString => TimeSpan.FromSeconds(totalSecondsPassed).ToString("hh':'mm':'ss");
    
    public List<Ring> allRings;

    
    public float spawnCounter;

    public bool startWithRandomParticles;
    public bool startFresh;


    void Start()
    {

        for (int i = 0; i < allRings.Count; i++)
        {
            allRings[i].Initialize(this, i);
        }


        if (startFresh)
        {
            totalSecondsPassed = 0;
            SaveTime();
            
            foreach (var ring in allRings)
            {
                ring.SetActiveParticlesCount(0);
            }
        }
        else if (startWithRandomParticles)
        {
            foreach (var ring in allRings)
            {
                ring.SetActiveParticlesCount(Random.Range(1, 9));
            }
        }
        else
        {
            LoadTime();
        }
        
        InvokeRepeating(nameof(SaveTime), 0, 1);
    }


    //[Button(ButtonSizes.Large)]
    void AdjustWaveHeightAllRings()
    {
        for (int i = 0; i < allRings.Count; i++)
        {
            allRings[i].SetWaveHeight();
        }
    }


    //[Button(ButtonSizes.Large)]
    void SetParticlesPositionAndRotationAllRings()
    {
        for (int i = 0; i < allRings.Count; i++)
        {
            allRings[i].SetParticlesPositionAndRotation();
        }
    }


    //[Button(ButtonSizes.Large)]
    void SetVisibilityForAllRings()
    {
        for (int i = 0; i < allRings.Count; i++)
        {
            allRings[i].SetVisibility();
        }
    }


    public bool isPaused;


    void LoadTime()
    {
        totalSecondsPassed = PlayerPrefs.GetFloat("totalTimePassed");
        AdjustRingsWhenAdjustingTime();
    }

    
    

    //[Button(ButtonSizes.Large)]
    void AdjustRingsWhenAdjustingTime()
    {
        // could just go through each ring and take the rest... ignore spawn time, less than smallest ring anyway...
        
        float timeLeft = totalSecondsPassed;
        
        
        for (int i = allRings.Count - 1; i >= 0; i--)
        {
            float timeForSpawningOneParticle = config.SpawnTimeFirstRing * Mathf.Pow(10, i);
            
            int particlesSpawned = Mathf.FloorToInt(timeLeft / timeForSpawningOneParticle);
            
            
            
            if(particlesSpawned > 9)
            {
                particlesSpawned = 9;
                Debug.LogError("Too many particles spawned for ring " + i);
            }
            
            timeLeft -= particlesSpawned * timeForSpawningOneParticle;
            allRings[i].SetActiveParticlesCount(particlesSpawned);
            
        
        }
        }

    //
    void SaveTime()
    {
        PlayerPrefs.SetFloat("totalTimePassed", totalSecondsPassed);
    }





    public int shouldHaveSpawnedTwice;
    
    void Update()
    {
        if (isPaused)
        {
            return;
        }

        totalSecondsPassed += Time.deltaTime;
        

        spawnCounter += Time.deltaTime;
        
        
        // a double spawn is unlikely the issue because it would just spawn another one in the next frame.
        // so potential causes are 
        if (spawnCounter > config.SpawnTimeFirstRing)
        {
            spawnCounter -= config.SpawnTimeFirstRing;
            
            
            SpawnParticle();
        }
        
    }


    
    
    void SpawnParticle()
    {
        allRings[0].SpawnParticle(null);
    }


    public void TogglePause()
    {
        isPaused = !isPaused;
    }



    //[Button(ButtonSizes.Large)]
    public void ResetTime()
    {
        totalSecondsPassed = 0;
        AdjustRingsWhenAdjustingTime();
        SaveTime();
    }
    
    
    //[Button]
    public void AddTimeInSeconds(float changeBy)
    {
        totalSecondsPassed += changeBy;
        
        if(totalSecondsPassed < 0)
        {
            totalSecondsPassed = 0;
        }
        
        AdjustRingsWhenAdjustingTime();
        
        SaveTime();
    }


    public void ShowTimeText()
    {
        StartCoroutine(ShowTimeTextCoroutine());
    }
    
    IEnumerator ShowTimeTextCoroutine()
    {
        int hours = Mathf.FloorToInt(totalSecondsPassed / 3600);
        int minutes = Mathf.FloorToInt((totalSecondsPassed % 3600) / 60);
        
        timeText.text = hours + "h " + minutes + "m";
        timeText.gameObject.SetActive(true);
        yield return new WaitForSeconds(2);
        timeText.gameObject.SetActive(false);
    }

    

    public Ring GetNextRing(Ring ring)
    {
        int index = allRings.IndexOf(ring);

        if (index == allRings.Count - 1)
        {
            Debug.LogError("Trying to get next ring but this one wasn't even in the list...");
            return null;
        }

        if (index + 1 == allRings.Count)
        {
            return null;
        }

        return allRings[index + 1];
    }

    public Ring GetPreviousRing(Ring ring)
    {
        int index = allRings.IndexOf(ring);

        if (index == 0)
        {
            return null;
        }

        return allRings[index - 1];
    }

    public void SetThisAndAllPreviousRingsTo9Particles(Ring ring)
    {
        int index = allRings.IndexOf(ring);

        for (int i = 0; i <= index; i++)
        {
            allRings[i].SetActiveParticlesCount(9);
        }
    }
}