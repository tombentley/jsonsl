import jsonsl {
    parseType
}
import ceylon.test {
    test
}
test
shared void testParseType() {
    
    assert(`Anything` == parseType("ceylon.language::Anything"));
    assert(`Nothing` == parseType("ceylon.language::Nothing"));
    assert(`String` == parseType("ceylon.language::String"));
    assert(`Integer` == parseType("ceylon.language::Integer"));
    // type arguments
    assert(`List<Anything>` == parseType("ceylon.language::List<ceylon.language::Anything>"));
    assert(`List<Nothing>` == parseType("ceylon.language::List<ceylon.language::Nothing>"));
    assert(`List<String>` == parseType("ceylon.language::List<ceylon.language::String>"));
    assert(`List<Integer>` == parseType("ceylon.language::List<ceylon.language::Integer>"));
    assert(`Map<Integer,String>` == parseType("ceylon.language::Map<ceylon.language::Integer,ceylon.language::String>"));
    
    //unions
    assert(`String|Integer` == parseType("ceylon.language::String|ceylon.language::Integer"));
    assert(`String|List<String>` == parseType("ceylon.language::String|ceylon.language::List<ceylon.language::String>"));
    assert(`String|List<String|Integer>` == parseType("ceylon.language::String|ceylon.language::List<ceylon.language::String|ceylon.language::Integer>"));
}
