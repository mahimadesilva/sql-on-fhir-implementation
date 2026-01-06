import ballerina/test;

// Test resources used across multiple tests
json[] testResources = [
    {
        "resourceType": "Patient",
        "id": "pt1",
        "name": [
            {
                "family": "F1.1"
            },
            {
                "family": "F1.2"
            }
        ],
        "contact": [
            {
                "telecom": [
                    {
                        "system": "phone"
                    }
                ],
                "name": {
                    "family": "FC1.1",
                    "given": ["N1", "N1`"]
                }
            },
            {
                "telecom": [
                    {
                        "system": "email"
                    }
                ],
                "gender": "unknown",
                "name": {
                    "family": "FC1.2",
                    "given": ["N2"]
                }
            }
        ]
    },
    {
        "resourceType": "Patient",
        "id": "pt2",
        "name": [
            {
                "family": "F2.1"
            },
            {
                "family": "F2.2"
            }
        ]
    },
    {
        "resourceType": "Patient",
        "id": "pt3"
    }
];

@test:Config
function testForEachNormal() returns error? {
    json viewDefinition = {
        "resource": "Patient",
        "status": "active",
        "select": [
            {
                "column": [
                    {
                        "name": "id",
                        "path": "id",
                        "type": "id"
                    }
                ]
            },
            {
                "forEach": "name",
                "column": [
                    {
                        "name": "family",
                        "path": "family",
                        "type": "string"
                    }
                ]
            }
        ]
    };

    json[] result = check evaluate(testResources, viewDefinition);

    json[] expected = [
        {
            "id": "pt1",
            "family": "F1.1"
        },
        {
            "id": "pt1",
            "family": "F1.2"
        },
        {
            "id": "pt2",
            "family": "F2.1"
        },
        {
            "id": "pt2",
            "family": "F2.2"
        }
    ];

    test:assertEquals(result, expected, msg = "forEach: normal - should create a row for each name element");
}

@test:Config
function testForEachOrNullBasic() returns error? {
    json viewDefinition = {
        "resource": "Patient",
        "status": "active",
        "select": [
            {
                "column": [
                    {
                        "name": "id",
                        "path": "id",
                        "type": "id"
                    }
                ]
            },
            {
                "forEachOrNull": "name",
                "column": [
                    {
                        "name": "family",
                        "path": "family",
                        "type": "string"
                    }
                ]
            }
        ]
    };

    json[] result = check evaluate(testResources, viewDefinition);

    json[] expected = [
        {
            "id": "pt1",
            "family": "F1.1"
        },
        {
            "id": "pt1",
            "family": "F1.2"
        },
        {
            "id": "pt2",
            "family": "F2.1"
        },
        {
            "id": "pt2",
            "family": "F2.2"
        },
        {
            "id": "pt3",
            "family": ()
        }
    ];
    test:assertEquals(result, expected, msg = "forEachOrNull: basic - should include rows with nulls when no elements exist");
}

@test:Config
function testForEachEmpty() returns error? {
    json viewDefinition = {
        "resource": "Patient",
        "status": "active",
        "select": [
            {
                "column": [
                    {
                        "name": "id",
                        "path": "id",
                        "type": "id"
                    }
                ]
            },
            {
                "forEach": "identifier",
                "column": [
                    {
                        "name": "value",
                        "path": "value",
                        "type": "string"
                    }
                ]
            }
        ]
    };

    json[] result = check evaluate(testResources, viewDefinition);

    json[] expected = [];

    test:assertEquals(result, expected, msg = "forEach: empty - should return empty array when no identifiers exist");
}

@test:Config
function testForEachTwoOnSameLevel() returns error? {
    json viewDefinition = {
        "resource": "Patient",
        "status": "active",
        "select": [
            {
                "forEach": "contact",
                "column": [
                    {
                        "name": "cont_family",
                        "path": "name.family",
                        "type": "string"
                    }
                ]
            },
            {
                "forEach": "name",
                "column": [
                    {
                        "name": "pat_family",
                        "path": "family",
                        "type": "string"
                    }
                ]
            }
        ]
    };

    json[] result = check evaluate(testResources, viewDefinition);

    json[] expected = [
        {
            "pat_family": "F1.1",
            "cont_family": "FC1.1"
        },
        {
            "pat_family": "F1.1",
            "cont_family": "FC1.2"
        },
        {
            "pat_family": "F1.2",
            "cont_family": "FC1.1"
        },
        {
            "pat_family": "F1.2",
            "cont_family": "FC1.2"
        }
    ];

    test:assertEquals(result, expected, msg = "forEach: two on the same level - should create cartesian product");
}

@test:Config
function testForEachTwoOnSameLevelEmptyResult() returns error? {
    json viewDefinition = {
        "resource": "Patient",
        "status": "active",
        "select": [
            {
                "column": [
                    {
                        "name": "id",
                        "path": "id",
                        "type": "id"
                    }
                ]
            },
            {
                "forEach": "identifier",
                "column": [
                    {
                        "name": "value",
                        "path": "value",
                        "type": "string"
                    }
                ]
            },
            {
                "forEach": "name",
                "column": [
                    {
                        "name": "family",
                        "path": "family",
                        "type": "string"
                    }
                ]
            }
        ]
    };

    json[] result = check evaluate(testResources, viewDefinition);

    json[] expected = [];

    test:assertEquals(result, expected, msg = "forEach: two on the same level (empty result) - should return empty when one forEach returns empty");
}

@test:Config
function testForEachOrNullNullCase() returns error? {
    json viewDefinition = {
        "resource": "Patient",
        "status": "active",
        "select": [
            {
                "column": [
                    {
                        "name": "id",
                        "path": "id",
                        "type": "id"
                    }
                ]
            },
            {
                "forEachOrNull": "identifier",
                "column": [
                    {
                        "name": "value",
                        "path": "value",
                        "type": "string"
                    }
                ]
            }
        ]
    };
    json[] result = check evaluate(testResources, viewDefinition);
    json[] expected = [
        {
            "id": "pt1",
            "value": ()
        },
        {
            "id": "pt2",
            "value": ()
        },
        {
            "id": "pt3",
            "value": ()
        }
    ];
    test:assertEquals(result, expected, msg = "forEachOrNull: null case - should return rows with nulls when no elements exist");
}

@test:Config
function testForEachForEachOrNullSameLevel() returns error? {
    json viewDefinition = {
        "resource": "Patient",
        "status": "active",
        "select": [
            {
                "column": [
                    {
                        "name": "id",
                        "path": "id",
                        "type": "id"
                    }
                ]
            },
            {
                "forEachOrNull": "identifier",
                "column": [
                    {
                        "name": "value",
                        "path": "value",
                        "type": "string"
                    }
                ]
            },
            {
                "forEach": "name",
                "column": [
                    {
                        "name": "family",
                        "path": "family",
                        "type": "string"
                    }
                ]
            }
        ]
    };
    json[] result = check evaluate(testResources, viewDefinition);
    json[] expected = [
        {
            "id": "pt1",
            "value": (),
            "family": "F1.1"
        },
        {
            "id": "pt1",
            "value": (),
            "family": "F1.2"
        },
        {
            "id": "pt2",
            "value": (),
            "family": "F2.1"
        },
        {
            "id": "pt2",
            "value": (),
            "family": "F2.2"
        }
    ];
    test:assertEquals(result, expected, msg = "forEach & forEachOrNull: same level - should handle both correctly");
}

// @test:Config
// function testNestedForEach() returns error? {
//     json viewDefinition = {
//         "resource": "Patient",
//         "status": "active",
//         "select": [
//             {
//                 "column": [
//                     {
//                         "name": "id",
//                         "path": "id",
//                         "type": "id"
//                     }
//                 ]
//             },
//             {
//                 "forEach": "contact",
//                 "select": [
//                     {
//                         "column": [
//                             {
//                                 "name": "contact_type",
//                                 "path": "telecom.system",
//                                 "type": "code"
//                             }
//                         ]
//                     },
//                     {
//                         "forEach": "name.given",
//                         "column": [
//                             {
//                                 "name": "name",
//                                 "path": "$this",
//                                 "type": "string"
//                             }
//                         ]
//                     }
//                 ]
//             }
//         ]
//     };

//     json[] result = check evaluate(testResources, viewDefinition);

//     json[] expected = [
//         {
//             "contact_type": "phone",
//             "name": "N1",
//             "id": "pt1"
//         },
//         {
//             "contact_type": "phone",
//             "name": "N1`",
//             "id": "pt1"
//         },
//         {
//             "contact_type": "email",
//             "name": "N2",
//             "id": "pt1"
//         }
//     ];

//     test:assertEquals(result, expected, msg = "nested forEach - should handle nested forEach correctly");
// }

// @test:Config
// function testNestedForEachSelectAndColumn() returns error? {
//     json viewDefinition = {
//         "resource": "Patient",
//         "status": "active",
//         "select": [
//             {
//                 "column": [
//                     {
//                         "name": "id",
//                         "path": "id",
//                         "type": "id"
//                     }
//                 ]
//             },
//             {
//                 "forEach": "contact",
//                 "column": [
//                     {
//                         "name": "contact_type",
//                         "path": "telecom.system",
//                         "type": "code"
//                     }
//                 ],
//                 "select": [
//                     {
//                         "forEach": "name.given",
//                         "column": [
//                             {
//                                 "name": "name",
//                                 "path": "$this",
//                                 "type": "string"
//                             }
//                         ]
//                     }
//                 ]
//             }
//         ]
//     };

//     json[] result = check evaluate(testResources, viewDefinition);

//     json[] expected = [
//         {
//             "contact_type": "phone",
//             "name": "N1",
//             "id": "pt1"
//         },
//         {
//             "contact_type": "phone",
//             "name": "N1`",
//             "id": "pt1"
//         },
//         {
//             "contact_type": "email",
//             "name": "N2",
//             "id": "pt1"
//         }
//     ];

//     test:assertEquals(result, expected, msg = "nested forEach: select & column - should handle both select and column in forEach");
// }

