import ballerina/test;

// Test resources for unionAll tests
json[] unionAllTestResources = [
    {
        "resourceType": "Patient",
        "id": "pt1",
        "telecom": [
            {
                "value": "t1.1",
                "system": "phone"
            },
            {
                "value": "t1.2",
                "system": "fax"
            },
            {
                "value": "t1.3",
                "system": "email"
            }
        ],
        "contact": [
            {
                "telecom": [
                    {
                        "value": "t1.c1.1",
                        "system": "pager"
                    }
                ]
            },
            {
                "telecom": [
                    {
                        "value": "t1.c2.1",
                        "system": "url"
                    },
                    {
                        "value": "t1.c2.2",
                        "system": "sms"
                    }
                ]
            }
        ]
    },
    {
        "resourceType": "Patient",
        "id": "pt2",
        "telecom": [
            {
                "value": "t2.1",
                "system": "phone"
            },
            {
                "value": "t2.2",
                "system": "fax"
            }
        ]
    },
    {
        "resourceType": "Patient",
        "id": "pt3",
        "contact": [
            {
                "telecom": [
                    {
                        "value": "t3.c1.1",
                        "system": "email"
                    },
                    {
                        "value": "t3.c1.2",
                        "system": "pager"
                    }
                ]
            },
            {
                "telecom": [
                    {
                        "value": "t3.c2.1",
                        "system": "sms"
                    }
                ]
            }
        ]
    },
    {
        "resourceType": "Patient",
        "id": "pt4"
    }
];

@test:Config
function testUnionAllBasic() returns error? {
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
                "unionAll": [
                    {
                        "forEach": "telecom",
                        "column": [
                            {
                                "name": "tel",
                                "path": "value",
                                "type": "string"
                            },
                            {
                                "name": "sys",
                                "path": "system",
                                "type": "code"
                            }
                        ]
                    },
                    {
                        "forEach": "contact.telecom",
                        "column": [
                            {
                                "name": "tel",
                                "path": "value",
                                "type": "string"
                            },
                            {
                                "name": "sys",
                                "path": "system",
                                "type": "code"
                            }
                        ]
                    }
                ]
            }
        ]
    };

    json[] result = check evaluate(unionAllTestResources, viewDefinition);

    json[] expected = [
        {
            "tel": "t1.1",
            "sys": "phone",
            "id": "pt1"
        },
        {
            "tel": "t1.2",
            "sys": "fax",
            "id": "pt1"
        },
        {
            "tel": "t1.3",
            "sys": "email",
            "id": "pt1"
        },
        {
            "tel": "t1.c1.1",
            "sys": "pager",
            "id": "pt1"
        },
        {
            "tel": "t1.c2.1",
            "sys": "url",
            "id": "pt1"
        },
        {
            "tel": "t1.c2.2",
            "sys": "sms",
            "id": "pt1"
        },
        {
            "tel": "t2.1",
            "sys": "phone",
            "id": "pt2"
        },
        {
            "tel": "t2.2",
            "sys": "fax",
            "id": "pt2"
        },
        {
            "tel": "t3.c1.1",
            "sys": "email",
            "id": "pt3"
        },
        {
            "tel": "t3.c1.2",
            "sys": "pager",
            "id": "pt3"
        },
        {
            "tel": "t3.c2.1",
            "sys": "sms",
            "id": "pt3"
        }
    ];

    test:assertEquals(result, expected, msg = "unionAll: basic - should combine results from telecom and contact.telecom");
}

@test:Config
function testUnionAllWithColumn() returns error? {
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
                ],
                "unionAll": [
                    {
                        "forEach": "telecom",
                        "column": [
                            {
                                "name": "tel",
                                "path": "value",
                                "type": "string"
                            },
                            {
                                "name": "sys",
                                "path": "system",
                                "type": "code"
                            }
                        ]
                    },
                    {
                        "forEach": "contact.telecom",
                        "column": [
                            {
                                "name": "tel",
                                "path": "value",
                                "type": "string"
                            },
                            {
                                "name": "sys",
                                "path": "system",
                                "type": "code"
                            }
                        ]
                    }
                ]
            }
        ]
    };

    json[] result = check evaluate(unionAllTestResources, viewDefinition);

    json[] expected = [
        {
            "tel": "t1.1",
            "sys": "phone",
            "id": "pt1"
        },
        {
            "tel": "t1.2",
            "sys": "fax",
            "id": "pt1"
        },
        {
            "tel": "t1.3",
            "sys": "email",
            "id": "pt1"
        },
        {
            "tel": "t1.c1.1",
            "sys": "pager",
            "id": "pt1"
        },
        {
            "tel": "t1.c2.1",
            "sys": "url",
            "id": "pt1"
        },
        {
            "tel": "t1.c2.2",
            "sys": "sms",
            "id": "pt1"
        },
        {
            "tel": "t2.1",
            "sys": "phone",
            "id": "pt2"
        },
        {
            "tel": "t2.2",
            "sys": "fax",
            "id": "pt2"
        },
        {
            "tel": "t3.c1.1",
            "sys": "email",
            "id": "pt3"
        },
        {
            "tel": "t3.c1.2",
            "sys": "pager",
            "id": "pt3"
        },
        {
            "tel": "t3.c2.1",
            "sys": "sms",
            "id": "pt3"
        }
    ];

    test:assertEquals(result, expected, msg = "unionAll + column - should work with column alongside unionAll");
}

@test:Config
function testUnionAllDuplicates() returns error? {
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
                ],
                "unionAll": [
                    {
                        "forEach": "telecom",
                        "column": [
                            {
                                "name": "tel",
                                "path": "value",
                                "type": "string"
                            },
                            {
                                "name": "sys",
                                "path": "system",
                                "type": "code"
                            }
                        ]
                    },
                    {
                        "forEach": "telecom",
                        "column": [
                            {
                                "name": "tel",
                                "path": "value",
                                "type": "string"
                            },
                            {
                                "name": "sys",
                                "path": "system",
                                "type": "code"
                            }
                        ]
                    }
                ]
            }
        ]
    };

    json[] result = check evaluate(unionAllTestResources, viewDefinition);

    json[] expected = [
        {
            "tel": "t1.1",
            "sys": "phone",
            "id": "pt1"
        },
        {
            "tel": "t1.2",
            "sys": "fax",
            "id": "pt1"
        },
        {
            "tel": "t1.3",
            "sys": "email",
            "id": "pt1"
        },
        {
            "tel": "t1.1",
            "sys": "phone",
            "id": "pt1"
        },
        {
            "tel": "t1.2",
            "sys": "fax",
            "id": "pt1"
        },
        {
            "tel": "t1.3",
            "sys": "email",
            "id": "pt1"
        },
        {
            "tel": "t2.1",
            "sys": "phone",
            "id": "pt2"
        },
        {
            "tel": "t2.2",
            "sys": "fax",
            "id": "pt2"
        },
        {
            "tel": "t2.1",
            "sys": "phone",
            "id": "pt2"
        },
        {
            "tel": "t2.2",
            "sys": "fax",
            "id": "pt2"
        }
    ];

    test:assertEquals(result, expected, msg = "unionAll: duplicates - should preserve duplicate rows");
}

@test:Config
function testUnionAllEmptyResults() returns error? {
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
                ],
                "unionAll": [
                    {
                        "forEach": "name",
                        "column": [
                            {
                                "name": "given",
                                "path": "given",
                                "type": "string"
                            }
                        ]
                    },
                    {
                        "forEach": "name",
                        "column": [
                            {
                                "name": "given",
                                "path": "given",
                                "type": "string"
                            }
                        ]
                    }
                ]
            }
        ]
    };

    json[] result = check evaluate(unionAllTestResources, viewDefinition);

    json[] expected = [];

    test:assertEquals(result, expected, msg = "unionAll: empty results - should return empty array when all union operands are empty");
}

@test:Config
function testUnionAllEmptyWithForEachOrNull() returns error? {
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
                ],
                "unionAll": [
                    {
                        "forEachOrNull": "name",
                        "column": [
                            {
                                "name": "given",
                                "path": "given",
                                "type": "string"
                            }
                        ]
                    },
                    {
                        "forEachOrNull": "name",
                        "column": [
                            {
                                "name": "given",
                                "path": "given",
                                "type": "string"
                            }
                        ]
                    }
                ]
            }
        ]
    };

    json[] result = check evaluate(unionAllTestResources, viewDefinition);

    json[] expected = [
        {
            "given": (),
            "id": "pt1"
        },
        {
            "given": (),
            "id": "pt1"
        },
        {
            "given": (),
            "id": "pt2"
        },
        {
            "given": (),
            "id": "pt2"
        },
        {
            "given": (),
            "id": "pt3"
        },
        {
            "given": (),
            "id": "pt3"
        },
        {
            "given": (),
            "id": "pt4"
        },
        {
            "given": (),
            "id": "pt4"
        }
    ];

    test:assertEquals(result, expected, msg = "unionAll: empty with forEachOrNull - should include null rows");
}

@test:Config
function testUnionAllForEachOrNullAndForEach() returns error? {
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
                ],
                "unionAll": [
                    {
                        "forEach": "name",
                        "column": [
                            {
                                "name": "given",
                                "path": "given",
                                "type": "string"
                            }
                        ]
                    },
                    {
                        "forEachOrNull": "name",
                        "column": [
                            {
                                "name": "given",
                                "path": "given",
                                "type": "string"
                            }
                        ]
                    }
                ]
            }
        ]
    };

    json[] result = check evaluate(unionAllTestResources, viewDefinition);

    json[] expected = [
        {
            "given": (),
            "id": "pt1"
        },
        {
            "given": (),
            "id": "pt2"
        },
        {
            "given": (),
            "id": "pt3"
        },
        {
            "given": (),
            "id": "pt4"
        }
    ];

    test:assertEquals(result, expected, msg = "unionAll: forEachOrNull and forEach - should combine forEach (empty) with forEachOrNull (nulls)");
}

@test:Config
function testUnionAllNested() returns error? {
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
                ],
                "unionAll": [
                    {
                        "forEach": "telecom[0]",
                        "column": [
                            {
                                "name": "tel",
                                "path": "value",
                                "type": "string"
                            }
                        ]
                    },
                    {
                        "unionAll": [
                            {
                                "forEach": "telecom[0]",
                                "column": [
                                    {
                                        "name": "tel",
                                        "path": "value",
                                        "type": "string"
                                    }
                                ]
                            },
                            {
                                "forEach": "contact.telecom[0]",
                                "column": [
                                    {
                                        "name": "tel",
                                        "path": "value",
                                        "type": "string"
                                    }
                                ]
                            }
                        ]
                    }
                ]
            }
        ]
    };

    json[] result = check evaluate(unionAllTestResources, viewDefinition);

    json[] expected = [
        {
            "id": "pt1",
            "tel": "t1.1"
        },
        {
            "id": "pt1",
            "tel": "t1.1"
        },
        {
            "id": "pt1",
            "tel": "t1.c1.1"
        },
        {
            "id": "pt2",
            "tel": "t2.1"
        },
        {
            "id": "pt2",
            "tel": "t2.1"
        },
        {
            "id": "pt3",
            "tel": "t3.c1.1"
        }
    ];

    test:assertEquals(result, expected, msg = "unionAll: nested - should handle nested unionAll correctly");
}

@test:Config
function testUnionAllOneEmptyOperand() returns error? {
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
                "unionAll": [
                    {
                        "forEach": "telecom.where(false)",
                        "column": [
                            {
                                "name": "tel",
                                "path": "value",
                                "type": "string"
                            },
                            {
                                "name": "sys",
                                "path": "system",
                                "type": "code"
                            }
                        ]
                    },
                    {
                        "forEach": "contact.telecom",
                        "column": [
                            {
                                "name": "tel",
                                "path": "value",
                                "type": "string"
                            },
                            {
                                "name": "sys",
                                "path": "system",
                                "type": "code"
                            }
                        ]
                    }
                ]
            }
        ]
    };

    json[] result = check evaluate(unionAllTestResources, viewDefinition);

    json[] expected = [
        {
            "id": "pt1",
            "sys": "pager",
            "tel": "t1.c1.1"
        },
        {
            "id": "pt1",
            "sys": "url",
            "tel": "t1.c2.1"
        },
        {
            "id": "pt1",
            "sys": "sms",
            "tel": "t1.c2.2"
        },
        {
            "id": "pt3",
            "sys": "email",
            "tel": "t3.c1.1"
        },
        {
            "id": "pt3",
            "sys": "pager",
            "tel": "t3.c1.2"
        },
        {
            "id": "pt3",
            "sys": "sms",
            "tel": "t3.c2.1"
        }
    ];

    test:assertEquals(result, expected, msg = "unionAll: one empty operand - should include results from non-empty operands only");
}

@test:Config
function testUnionAllColumnMismatch() returns error? {
    json viewDefinition = {
        "resource": "Patient",
        "status": "active",
        "select": [
            {
                "unionAll": [
                    {
                        "column": [
                            {
                                "name": "a",
                                "path": "id",
                                "type": "id"
                            },
                            {
                                "name": "b",
                                "path": "id",
                                "type": "id"
                            }
                        ]
                    },
                    {
                        "column": [
                            {
                                "name": "a",
                                "path": "id",
                                "type": "id"
                            },
                            {
                                "name": "c",
                                "path": "id",
                                "type": "id"
                            }
                        ]
                    }
                ]
            }
        ]
    };

    json[]|error result = evaluate(unionAllTestResources, viewDefinition);

    test:assertTrue(result is error, msg = "unionAll: column mismatch - should return error when column names differ");
}

@test:Config
function testUnionAllColumnOrderMismatch() returns error? {
    json viewDefinition = {
        "resource": "Patient",
        "status": "active",
        "select": [
            {
                "unionAll": [
                    {
                        "column": [
                            {
                                "name": "a",
                                "path": "id",
                                "type": "id"
                            },
                            {
                                "name": "b",
                                "path": "id",
                                "type": "id"
                            }
                        ]
                    },
                    {
                        "column": [
                            {
                                "name": "b",
                                "path": "id",
                                "type": "id"
                            },
                            {
                                "name": "a",
                                "path": "id",
                                "type": "id"
                            }
                        ]
                    }
                ]
            }
        ]
    };

    json[]|error result = evaluate(unionAllTestResources, viewDefinition);

    test:assertTrue(result is error, msg = "unionAll: column order mismatch - should return error when column order differs");
}
