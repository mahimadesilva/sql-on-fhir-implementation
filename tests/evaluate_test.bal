import ballerina/test;

@test:Config
function testEvaluatePatientDemographics() returns error? {
    json[] resources = [
        {
            "resourceType": "Patient",
            "id": "patient-1",
            "name": [
                {
                    "family": "Doe",
                    "given": ["John"]
                }
            ],
            "birthDate": "1980-01-01",
            "gender": "male"
        },
        {
            "resourceType": "Patient",
            "id": "patient-2",
            "name": [
                {
                    "family": "Smith",
                    "given": ["Jane"]
                }
            ],
            "birthDate": "1990-02-02",
            "gender": "female"
        }
    ];

    json viewDefinition = {
        "resourceType": "ViewDefinition",
        "name": "patient_demographics",
        "url": "http://myig.org/ViewDefinition/patient_demographics",
        "resource": "Patient",
        "select": [
            {
                "column": [
                    {
                        "path": "Patient.birthDate",
                        "name": "date_of_birth",
                        "type": "date"
                    },
                    {
                        "path": "Patient.gender",
                        "name": "gender",
                        "type": "string"
                    }
                ]
            }
        ]
    };

    json[] result = check evaluate(resources, viewDefinition);

    // Expected result should contain two records, one for each patient
    test:assertEquals(
            result.length(),
            2,
            msg = "Should return exactly 2 records for 2 patients"
        );

    // Verify the structure and content of the results
    json expectedResults = [
        {
            "date_of_birth": "1980-01-01",
            "gender": "male"
        },
        {
            "date_of_birth": "1990-02-02",
            "gender": "female"
        }
    ];

    test:assertEquals(
            result,
            expectedResults,
            msg = "Patient demographics should be extracted correctly"
        );
}
