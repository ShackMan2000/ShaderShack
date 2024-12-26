using System;
using System.Collections;
using System.Collections.Generic;
using System.Data.Common;
using Unity.VisualScripting;
using UnityEngine;
using UnityEngine.Serialization;
using Random = UnityEngine.Random;

public class CropSpawner : MonoBehaviour
{
    [SerializeField] Transform cropPrefab;

    [FormerlySerializedAs("size")] [SerializeField]
    Vector2 fieldSize;

    [SerializeField] float density;

    [SerializeField] List<Transform> fencePrefabs = new List<Transform>();
    [SerializeField] float originalFenceLengthPerUnit = 2f;

    [SerializeField] float spawnRadius;
    [SerializeField] int spawnAmount = 10;
    [SerializeField] float offset = 0.1f;


    List<GameObject> spawnedCrops = new List<GameObject>();

    Vector3 fenceScale;
    // Update is called once per frame


    void Start()
    {
        fenceScale = fencePrefabs[0].localScale;
        originalFenceLengthPerUnit *= fenceScale.x;
    }


    //create at own pivot and in forward axis, x and z


    [ContextMenu("CreateField")]
    public void CreateField()
    {
        fenceScale = fencePrefabs[0].localScale;
        float adjustedFenceLength = originalFenceLengthPerUnit * fenceScale.x;

        if (fieldSize.x < 2 || fieldSize.y < 2)
        {
            Debug.LogError("Field size too small");
            return;
        }

        //how many fences fit in
        int xFences = Mathf.RoundToInt(fieldSize.x / adjustedFenceLength);
        int yFences = Mathf.RoundToInt(fieldSize.y / adjustedFenceLength);


        //adjust field size to fit fences

        Vector2 frameSize = new Vector2(xFences * adjustedFenceLength, yFences * adjustedFenceLength);

        //create field object
        GameObject fieldObject = new GameObject("Field " + fieldSize.x + "x" + fieldSize.y);
        Transform parent = fieldObject.transform;
        parent.position = transform.position;

        CreateRow(new Vector3(0f, 0f, 0f), new Vector3(frameSize.x, 0f, 0f), xFences);
        CreateRow(new Vector3(frameSize.x, 0f, 0f), new Vector3(frameSize.x, 0f, frameSize.y), yFences);
        CreateRow(new Vector3(frameSize.x, 0f, frameSize.y), new Vector3(0f, 0f, frameSize.y), xFences);

        //  CreateRow(new Vector3(0f, 0f, 0f), new Vector3(0f, 0f, fieldSize.y), yFences);
        CreateRow(new Vector3(0f, 0f, frameSize.y), new Vector3(0f, 0f, 0f), yFences);



        //Fences
        void CreateRow(Vector3 start, Vector3 target, int units)
        {
            for (int i = 0; i < units; i++)
            {
                Transform newFence = Instantiate(fencePrefabs[0], parent);


                Vector3 newPosition = new Vector3(Mathf.Lerp(start.x, target.x, i / (float)units), 0f, Mathf.Lerp(start.z, target.z, i / (float)units));

                newFence.localPosition = newPosition;

                Vector3 newTarget = transform.position + target;
                newFence.LookAt(newTarget);
            }
        }


        // - 2 to leave boarder close to fence
        int cropsPerX = Mathf.RoundToInt((fieldSize.x - 1) / density);
        int cropsPerY = Mathf.RoundToInt((fieldSize.y - 1) / density);


        for (int i = 0; i < cropsPerX; i++)
        {
            for (int j = 0; j < cropsPerY; j++)
            {
                Transform newWheat = Instantiate(cropPrefab, parent);
                
                Vector3 newPosition = new Vector3(i / density, 0f, j / density);
                newPosition += new Vector3(Random.Range(-offset, offset), 0f, Random.Range(-offset, offset));
                newPosition += new Vector3(1f, 0f, 1f);
                newWheat.localPosition = newPosition;
                
                newWheat.Rotate(0f, Random.Range(0f, 360f), 0f);
            }
        }
    }


    // void Update()
    // {
    //     //spaw when clicking mouse
    //     if (Input.GetMouseButtonDown(0))
    //     {
    //         SpawnCrop();
    //     }
    // }


    //shoot a raycast from the mouse position into the scene, check if hit something with tag ground
    //if yes, spawn a crop there
    //if no, do nothing

    // public void SpawnCrop()
    // {
    //     Ray ray = Camera.main.ScreenPointToRay(Input.mousePosition);
    //     RaycastHit hit;
    //     if (Physics.Raycast(ray, out hit))
    //     {
    //         if (hit.collider.CompareTag("Ground"))
    //         {
    //             for (int i = 0; i < spawnAmount; i++)
    //             {
    //                 Vector3 randomPosition = hit.point + new Vector3(Random.Range(-spawnRadius, spawnRadius), 0f, Random.Range(-spawnRadius, spawnRadius));
    //                 
    //                 GameObject newCrop = Instantiate(cropPrefab, randomPosition, Quaternion.identity);
    //                 newCrop.transform.SetParent(transform);
    //                 //rotate crop randomyl around y axis
    //                 newCrop.transform.Rotate(0f, Random.Range(0f, 360f), 0f);
    //                 spawnedCrops.Add(newCrop);
    //             }
    //             
    //        
    //         }
    //     }
    // }
}