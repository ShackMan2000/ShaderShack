using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class CropSpawner : MonoBehaviour
{
   
    
    [SerializeField] GameObject cropPrefab;
    [SerializeField] float spawnRadius;
    [SerializeField] int spawnAmount = 10;
    
    
    List<GameObject> spawnedCrops = new List<GameObject>();

    // Update is called once per frame

    void Update()
    {
        //spaw when clicking mouse
        if (Input.GetMouseButtonDown(0))
        {
            SpawnCrop();
        }
    }
    
    
    
    //shoot a raycast from the mouse position into the scene, check if hit something with tag ground
    //if yes, spawn a crop there
    //if no, do nothing
    
    public void SpawnCrop()
    {
        Ray ray = Camera.main.ScreenPointToRay(Input.mousePosition);
        RaycastHit hit;
        if (Physics.Raycast(ray, out hit))
        {
            if (hit.collider.CompareTag("Ground"))
            {
                for (int i = 0; i < spawnAmount; i++)
                {
                    Vector3 randomPosition = hit.point + new Vector3(Random.Range(-spawnRadius, spawnRadius), 0f, Random.Range(-spawnRadius, spawnRadius));
                    
                    GameObject newCrop = Instantiate(cropPrefab, randomPosition, Quaternion.identity);
                    newCrop.transform.SetParent(transform);
                    //rotate crop randomyl around y axis
                    newCrop.transform.Rotate(0f, Random.Range(0f, 360f), 0f);
                    spawnedCrops.Add(newCrop);
                }
                
           
            }
        }
    }
}
