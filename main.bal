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

// Helper function to recursively normalize select array items
function normalizeArrayItems(json[] selects) returns json[]|error {
    json[] normalizedItems = [];
    foreach var s in selects {
        normalizedItems.push(check normalize(s));
    }
    return normalizedItems;
}

public function normalize(json def) returns json|error {
    map<anydata> normalizedDef = check def.cloneWithType();

    if (normalizedDef.hasKey("forEach") || normalizedDef.hasKey("forEachOrNull")) {
        // Initialize select array if it doesn't exist
        if (!normalizedDef.hasKey("select")) {
            normalizedDef["select"] = [];
        }

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
        normalizedDef["select"] = check normalizeArrayItems(selects);
        return normalizedDef.toJson();
    } else if normalizedDef.hasKey("select") && normalizedDef.hasKey("column") && normalizedDef.hasKey("unionAll") {
        // Normalize to select type
        normalizedDef["type"] = "select";
        json[] selects = check normalizedDef["select"].cloneWithType();
        json[] newSelects = [];
        newSelects.push({"column": normalizedDef["column"]}.toJson());
        newSelects.push({"unionAll": normalizedDef["unionAll"]}.toJson());
        newSelects.push(...selects);
        normalizedDef["select"] = newSelects;
        _ = normalizedDef.remove("column");
        _ = normalizedDef.remove("unionAll");

        // Recursively normalize each item in select
        normalizedDef["select"] = check normalizeArrayItems(newSelects);
        return normalizedDef.toJson();
    } else if normalizedDef.hasKey("select") && normalizedDef.hasKey("unionAll") {
        normalizedDef["type"] = "select";
        json[] selects = check normalizedDef["select"].cloneWithType();
        json[] newSelects = [];
        newSelects.push({"unionAll": normalizedDef["unionAll"]}.toJson());
        newSelects.push(...selects);
        normalizedDef["select"] = newSelects;
        _ = normalizedDef.remove("unionAll");

        // Recursively normalize each item in select
        normalizedDef["select"] = check normalizeArrayItems(newSelects);
        return normalizedDef.toJson();
    } else if normalizedDef.hasKey("select") && normalizedDef.hasKey("column") {
        normalizedDef["type"] = "select";
        json[] selects = check normalizedDef["select"].cloneWithType();
        json[] newSelects = [];
        newSelects.push({"column": normalizedDef["column"]}.toJson());
        newSelects.push(...selects);
        normalizedDef["select"] = newSelects;
        _ = normalizedDef.remove("column");
        // Recursively normalize each item in select
        normalizedDef["select"] = check normalizeArrayItems(newSelects);
        return normalizedDef.toJson();
    } else if normalizedDef.hasKey("column") && normalizedDef.hasKey("unionAll") {
        normalizedDef["type"] = "select";
        json[] newSelects = [];
        newSelects.push({"column": normalizedDef["column"]}.toJson());
        newSelects.push({"unionAll": normalizedDef["unionAll"]}.toJson());
        normalizedDef["select"] = newSelects;
        _ = normalizedDef.remove("column");
        _ = normalizedDef.remove("unionAll");
        // Recursively normalize each item in select
        normalizedDef["select"] = check normalizeArrayItems(newSelects);
        return normalizedDef.toJson();
    } else if (normalizedDef.hasKey("select")) {
        normalizedDef["type"] = "select";
        json[] selects = check normalizedDef["select"].cloneWithType();
        normalizedDef["select"] = check normalizeArrayItems(selects);
        return normalizedDef.toJson();
    } else {
        if (normalizedDef.hasKey("unionAll")) {
            normalizedDef["type"] = "unionAll";
            json[] unionAlls = check normalizedDef["unionAll"].cloneWithType();
            normalizedDef["unionAll"] = check normalizeArrayItems(unionAlls);
    }
        else if (normalizedDef.hasKey("column")) {
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

public function forEachOperation(json selectExpression, json node) returns json[]|error {
    map<anydata> expression = <map<anydata>>selectExpression;

    // Assert forEach is required
    if (!expression.hasKey("forEach")) {
        return error("forEach required");
    }

    string forEachPath = <string>expression["forEach"];

    // Evaluate FHIRPath expression to get nodes
    json[] nodes = check fhirpath:getValuesFromFhirPath(node, forEachPath);

    json[] results = [];

    // For each node, apply the select operation
    foreach json nodeItem in nodes {
        if (expression.hasKey("select")) {
            json selectExpr = {"select": expression["select"]}.toJson();
            json[] selectResults = check selectOperation(selectExpr, nodeItem);
            results.push(...selectResults);
        }
    }

    return results;
}

public function forEachOrNullOperation(json selectExpression, json node) returns json[]|error {
    map<anydata> expression = <map<anydata>>selectExpression;

    // Assert forEach is required
    if (!expression.hasKey("forEachOrNull")) {
        return error("forEachOrNull required");
    }

    string forEachOrNullPath = <string>expression["forEachOrNull"];

    // Evaluate FHIRPath expression to get nodes
    json[] nodes = check fhirpath:getValuesFromFhirPath(node, forEachOrNullPath);
    if nodes.length() == 0 {
        nodes = [{}];
    }

    json[] results = [];

    // For each node, apply the select operation
    foreach json nodeItem in nodes {
        if (expression.hasKey("select")) {
            json selectExpr = {"select": expression["select"]}.toJson();
            json[] selectResults = check selectOperation(selectExpr, nodeItem);
            results.push(...selectResults);
        }
    }

    return results;
}

// Helper function to check if all results have the same columns
function arraysUnique(json[] results) returns int {
    map<boolean> uniqueColumnSets = {};

    foreach var item in results {
        if item is map<anydata> {
            // Sort keys and create a string representation
            string columnSet = item.keys().sort().toString();
            uniqueColumnSets[columnSet] = true;
        }
    }

    return uniqueColumnSets.length();
}

public function unionAllOperation(json selectExpression, json node) returns json[]|error {
    map<anydata> expression = <map<anydata>>selectExpression;

    // Assert unionAll exists
    if (!expression.hasKey("unionAll")) {
        return error("unionAll is required");
    }

    // FlatMap: evaluate each unionAll element and flatten the results
    json[] result = [];
    foreach var d in <json[]>expression["unionAll"] {
        json[] partialResult = check doEval(d, node);
        result.push(...partialResult);
    }

    // TODO: ideally, this should be done during the validation
    // Validate that all results have the same columns
    int uniqueCount = arraysUnique(result);

    if uniqueCount > 1 {
        return error(string `Union columns mismatch: found ${uniqueCount} different column sets`);
    }

    return result;
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
        "forEach" =>
        {
            return forEachOperation(selectExpression, node);
        }
        "forEachOrNull" =>
        {
            return forEachOrNullOperation(selectExpression, node);
        }
        "unionAll" =>
        {
            return unionAllOperation(selectExpression, node);
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
