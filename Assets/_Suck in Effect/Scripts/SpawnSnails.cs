using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class SpawnSnails : MonoBehaviour
{

    [SerializeField] SuckMyMesh suckableSnail;


    [SerializeField] float spawnPerSecond = 1f;
    [SerializeField] float spawnRadius;

    float counter;

  
    void Update()
    {
        counter += Time.deltaTime;
        if(counter >= 1f / spawnPerSecond)
        {
            counter = 0f;
            SpawnSnail();
        }
        
        
        
        
    }

    void SpawnSnail()
    {
        SuckMyMesh newSnail = Instantiate(suckableSnail, transform);
        
        //place inside radius, don't change own height
        newSnail.transform.position = transform.position + new Vector3(Random.Range(-spawnRadius, spawnRadius), 0f, Random.Range(-spawnRadius, spawnRadius));
        newSnail.transform.LookAt(new Vector3(transform.position.x, newSnail.transform.position.y, transform.position.z));
        

        newSnail.StartSuckRoutine(transform.position, 3f);
    }
}
