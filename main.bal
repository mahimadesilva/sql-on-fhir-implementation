import ballerinax/health.fhir.r4utils.fhirpath;

// Function to merge two maps
function merge(json m1, json m2) returns json|error {
    json result = check m1.clone().mergeJson(m2);
    return result;
}

// Row product function - creates cartesian product of arrays of maps
public function rowProduct(json[][] parts) returns json[]|error {
    json[] result = [{}];

    foreach json[] partialRows in parts {
        json[] newResult = [];

        foreach json partialRow in partialRows {
            foreach json row in result {
                json merged = check merge(partialRow, row);
                newResult.push(merged);
            }
        }

        result = newResult;
    }

    return result;
}

public function normalize(json def) returns json|error {
    map<anydata> normalizedDef = check def.cloneWithType();
    if (normalizedDef.hasKey("forEach")) {
            normalizedDef["type"] = "forEach";
        }
        else {
            normalizedDef["type"] = "forEachOrNull";
        }

        // Move unionAll to select if it exists
        if (normalizedDef.hasKey("unionAll")) {
            json[] selectArray = check normalizedDef["select"].cloneWithType();
            json[] newSelect = [{"unionAll": normalizedDef["unionAll"]}.toJson()];
            newSelect.push(...selectArray);
            normalizedDef["select"] = newSelect;
            _ = normalizedDef.remove("unionAll");
        }

        // Move column to select if it exists
        if (normalizedDef.hasKey("column")) {
            json[] selectArray = check normalizedDef["select"].cloneWithType();
            json[] newSelect = [{"column": normalizedDef["column"]}.toJson()];
            newSelect.push(...selectArray);
            normalizedDef["select"] = newSelect;
            _ = normalizedDef.remove("column");
        }

        // Recursively normalize each item in select
        json[] selects = check normalizedDef["select"].cloneWithType();
        json[] normalizedSelects = [];
        foreach var s in selects {
            normalizedSelects.push(check normalize(s));
        }
        normalizedDef["select"] = normalizedSelects;
        return normalizedDef.toJson();
    }
    else if (normalizedDef.hasKey("select")) {
        normalizedDef["type"] = "select";
        json[] selects = check normalizedDef["select"].cloneWithType();
        json[] normalizedSelects = [];
        foreach var s in selects {
            normalizedSelects.push(check normalize(s));
        }
        normalizedDef["select"] = normalizedSelects;
        return normalizedDef.toJson();
    }
    else {
        if (normalizedDef.hasKey("column")) {
            normalizedDef["type"] = "column";
        }
        return normalizedDef.toJson();
    }
}

public function columnOperation(json selectExpression, json node) returns json[]|error {
    map<anydata> result = {};
    map<anydata> expression = <map<anydata>>selectExpression;

    if (!expression.hasKey("column")) {
        return error("No column specified in select expression");
    }
    map<anydata>[] columns = check expression["column"].cloneWithType();
    foreach var c in columns {
        if (!c.hasKey("path") || !(c["path"] is string)) {
            return error("Path is not specified or is not a string in column expression");
        }

        // TODO: Replace path expressions with constants specified in the view definition
        json[] vs = check fhirpath:getValuesFromFhirPath(node, <string>c["path"]);
        string recordKey = c.hasKey("name") && (c["name"] is string) ? <string>c["name"] : <string>c["path"];

        if c.hasKey("collection") && c["collection"] is boolean && <boolean>c["collection"] {
            result[recordKey] = vs;
        } else if vs.length() === 1 {
            result[recordKey] = vs[0];
        } else if vs.length() === 0 {
            result[recordKey] = ();
        }
        else {
            return error("Collection flag is false for path: " + <string>c["path"]);
        }
    }
    return [result.toJson()];
}

public function selectOperation(json selectExpression, json node) returns json[]|error {
    map<anydata> expression = <map<anydata>>selectExpression;
    if (!expression.hasKey("select")) {
        return error("No select specified in select expression");
    }

    // TODO: Implement filtering when "where" clause is specified

    if (expression.hasKey("resource")) {
        if (expression["resource"] !== node.resourceType) {
            return [];
        }
    }
    json[][] evalResult = [];
    foreach var s in <json[]>expression["select"] {
        json[] partialResult = check doEval(s, node);
        evalResult.push(partialResult);
    }

    return rowProduct(evalResult);
}

public function doEval(json selectExpression, json node) returns json[]|error {

    match check selectExpression.'type {
        "column" =>
        {
            return columnOperation(selectExpression, node);
        }
        "select" =>
        {
            return selectOperation(selectExpression, node);
        }
        _ =>
        {
            return [];
        }
    }

}

public function evaluate(json[] resources, json viewDefinition) returns json[]|error {
    // TODO: Validate view definition structure

    json noramalDef = check normalize(viewDefinition.clone());

    json[] results = [];
    foreach json 'resource in resources {
        json[] evalResult = check doEval(noramalDef, 'resource);
        // Accumulate results
        evalResult.forEach(function(json row) {
            results.push(row);
        });
    }

    return results;
}

public function main() returns error? {
}
