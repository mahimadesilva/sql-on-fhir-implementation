import ballerina/test;

@test:Config
function testRowProductBasic() returns error? {
    test:assertEquals(
            check rowProduct([
                        [{a: 1}, {a: 2}],
                        [{b: 1}, {b: 2}]
                    ]),
            [
                {b: 1, a: 1},
                {b: 1, a: 2},
                {b: 2, a: 1},
                {b: 2, a: 2}
            ], msg = "rowProduct returned unexpected result"
        );
}

@test:Config
function testRowProductWithEmptyArray() returns error? {
    test:assertEquals(
            check rowProduct([[{a: 1}, {a: 2}], []]),
            [],
            msg = "rowProduct with empty array should return empty result"
        );
}

@test:Config
function testRowProductWithEmptyObject() returns error? {
    test:assertEquals(
            check rowProduct([[{a: 1}, {a: 2}], [{}]]),
            [{a: 1}, {a: 2}],
            msg = "rowProduct with empty object should preserve original values"
        );
}

@test:Config
function testRowProductSingleArray() returns error? {
    test:assertEquals(
            check rowProduct([[{a: 1}, {a: 2}]]),
            [{a: 1}, {a: 2}],
            msg = "rowProduct with single array should return the array itself"
        );
}

