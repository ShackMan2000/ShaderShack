using System;
using System.Collections;
using System.Collections.Generic;
using Unity.VisualScripting;
using UnityEngine;
using UnityEngine.PlayerLoop;
using UnityEngine.Serialization;

public class SuckMyMesh : MonoBehaviour
{

    float timePassed;

    [SerializeField] float variationOffset;

    public event Action StartedSuck; 
    
    
//   [SerializeField]  MeshRenderer meshRenderer;

    


    // start scuk routine to target 0 0 when hitting space
    void Update()
    {
        if (Input.GetKeyDown(KeyCode.Space))
        {
            StartSuckRoutine(Vector3.zero);
        }
    }
    
    
    public void StartSuckRoutine(Vector3 targetPosition, float speed = 1f)
    {
        StartCoroutine(SuckRoutine(targetPosition, speed));
    }
    

    IEnumerator SuckRoutine(Vector3 targetPosition, float speed)
    {
        float offset = UnityEngine.Random.Range(0f, variationOffset);
      //  meshRenderer.material.SetFloat("_VariationOffset", offset);
        
        
        StartedSuck?.Invoke();
        
        float distance = Vector3.Distance(transform.position, targetPosition);
        float moveTime = distance / speed;

        while (moveTime >= 0f)
        {
            transform.position = Vector3.MoveTowards(transform.position, targetPosition, speed * Time.deltaTime);
            moveTime -= Time.deltaTime;
            yield return null;
        }
    }
}
