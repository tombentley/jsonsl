import ceylon.json {
    JsonObject=Object,
    JsonArray=Array,
    Builder,
    Visitor
}
import ceylon.test {
    test,
    assertEquals,
    fail,
    assertTrue
}

import jsonsl {
    Deserializer,
    jsonKey,
    Serializer,
    nested
}
import ceylon.language.meta {
    type
}
import ceylon.language.meta.model {
    ClassModel
}

"A point in ℝ²."
serializable class Point {
    shared Float x;
    shared Float y;
    shared new Point(Float x, Float y) {
        this.x = x;
        this.y = y;
    }
    /*shared new Polar(Float radius, Float theta) {
        this.x = radius*cos(theta);
        this.y = radius*sin(theta);
     }*/
    shared actual Boolean equals(Object other) {
        if (is Point other) {
            return this.x == other.x && this.y == other.y;
        } else {
            return false;
        }
    }
    shared actual Integer hash => this.x.hash ^ this.y.hash;
    
    shared actual default String string => "(``x``,``y``)";
}

"A named point in ℝ², defined in Cartesian coordinates."
serializable class NamedPoint(Float x, Float y, shared String name) extends Point(x,y){
    shared actual default String string => "\"``name``\"=``super.string``";
}

"A line segment between two points in ℝ²."
serializable class Line(shared nested Point start, shared nested Point end) {
    shared actual String string => "line from ``start`` to ``end``";
}
"A disk in ℝ²"
serializable class Disk(shared Point centre, shared Float radius) {
    shared actual String string => "Disk(``centre``,``radius``)";
}

serializable class Tip(shared String description) 
        extends Object() { // NOTE: NOT IDENTIFIABLE
    shared actual Boolean equals(Object that) {
        if (is Tip that) {
            return true;
        }
        else {
            return false;
        }
    }
    
    shared actual Integer hash {
        variable value hash = 1;
        return hash;
    }
    
    shared actual String string => "Tip(\"``description``\")";
}
serializable class Arrow(shared Tip tip, Point start, Point end) extends Line(start, end){
    
}

serializable class AmbiguousAttr(String s) {
    shared actual default String string => s;
    shared String leakS => s;
}
serializable class AmbiguousAttr2(s) extends AmbiguousAttr("sub"){
    // We end up with two value's with the name "s"
    jsonKey("s2")
    shared String s;
    shared actual String string => "AmbiguousAttr2(\"``s``\")";
}
serializable class AmbiguousAttrError(s) extends AmbiguousAttr("sub"){
    shared String s;
    shared actual String string => "AmbiguousAttr2(\"``s``\")";
}

"Test that :
 * [[jsonKey]] works for disambiguation during serialization
 * [[jsonKey]] works on deserialization
 * that classes with ambiguous attribute names are detected at serialization time."
test
shared void testAmbiguousAttributeNames() {
    variable value s = Serializer();
    s.add(AmbiguousAttr2("foo"));
    value json  = s.json;
    assertEquals("""[{"$":"0","$type":"test.jsonsl::AmbiguousAttr2","s":"sub","s2":"foo"}]""", json.string);
    value d = Deserializer();
    assert(is AmbiguousAttr2 got = d.parse(json).first);
    assertEquals("sub", got.leakS);
    assertEquals("foo", got.s);
    
    s = Serializer();
    try {
        s.add(AmbiguousAttrError("foo"));
        print(s.json);
        throw;
    } catch (AssertionError e) {
        assertEquals("ambiguous JSON key s for attribute test.jsonsl::AmbiguousAttrError.s. Use jsonKey annotation to disambiguate", e.message);
    }
}

serializable class Optional(shared Boolean? s) {
    shared actual String string => switch(s) case (null) "Optional(null)" case(true) "Optional(true)" case (false) "Optional(false)";
}

"Test we can round tip classes with optional attributes"
test
shared void testOptional() {
    value s = Serializer();
    s.add(Optional(true));
    s.add(Optional(false));
    s.add(Optional(null));
    value json = s.json;
    print(json);
    value d = Deserializer();
    value sequence = d.parse(json).sequence();
    assert(is Optional got1 = sequence[0]);
    assert(exists s1 = got1.s, s1);
    assert(is Optional got2 = sequence[1]);
    assert(exists s2 = got2.s, !s2);
    assert(is Optional got3 = sequence[2]);
    assert(! got3.s exists);
    
}


test
shared void test1() {
    value origin = NamedPoint(0.0, 0.0, "Origin");
    value x1y1 = NamedPoint(1.0, 1.0, "(1,1)");
    value pointyTip = Tip("pointy");
    
    value items = [
        //origin,
        //x1y1,
        Line(origin, x1y1),
        Arrow(pointyTip, origin, x1y1),
        Arrow(pointyTip, x1y1, origin),
        Disk(origin, 1.0)
    ];
    // Add the items to be serialized
    Serializer serializer = Serializer();
    for (item in items) {
        serializer.add(item);
    }
    // Now get the serialized items
    value b = Builder();
    serializer.serialize(b);
    assert(is JsonArray jsons = b.result);
    
    Deserializer deserializer = Deserializer();
    // now iterate the list adding the items to be deserialized
    for (jsonObject in jsons) {
        assert(is JsonObject jsonObject);
        deserializer.add(jsonObject);
    }
    // now iterate the deserialized items
    for (x in deserializer.items) {
        print(x);
    }
    // TODO assertions
}


serializable class Cyclic(shared Integer i) {
    shared late Cyclic other;
    shared actual String string => "Cyclic(``i``)";
}

"Tests we can handle cycles via references"
test
shared void testCycles() {
    value cyclic0 = Cyclic(0);
    cyclic0.other = cyclic0;
    value cyclic1 = Cyclic(1);
    
    value cyclic2 = Cyclic(2);
    cyclic1.other = cyclic2;
    cyclic2.other = cyclic1;
    
    value s = Serializer();
    s.add(cyclic0);
    s.add(cyclic1);
    String json = s.json;
    print(s.pretty);
    
    value d = Deserializer();
    value objs = d.parse(json);
    assert(objs.size == 2);
    for (o in objs) {
        assert(is Cyclic o);
        switch (o.i) 
        case (0) {
            assert(o.other === o);
        }
        case (1) {
            assert(o.other.i == 2);
            assert(o.other.other === o);
        }
        else {
            throw;
        }
    }
}

nested serializable class NestedClass(shared String name, shared NestedClass? child) {
    shared actual String string => "NestedClass(\"``name``\", ``child else "null"``)";
}

"Test we respect [[nested]] annotation on a class"
test
shared void testNestedClass() {
    value o = NestedClass("top", NestedClass("middle", NestedClass("bottom", null)));
    value s= Serializer();
    s.add(o);
    print(s.json);
    assertEquals("""[
                     {
                      "$": "0",
                      "$type": "test.jsonsl::NestedClass",
                      "name": "top",
                      "child": {
                       "$type": "test.jsonsl::NestedClass",
                       "name": "middle",
                       "child": {
                        "$type": "test.jsonsl::NestedClass",
                        "name": "bottom",
                        "child": null
                       }
                      }
                     }
                    ]""",
                s.pretty);
    
    value d = Deserializer();
    value deserialized = d.parse(s.json);
    assert(deserialized.size == 1);
    assert(is NestedClass obj = deserialized.first);
    assertEquals("top", obj.name);
    assert(exists obj1 = obj.child);
    assertEquals("middle", obj1.name);
    assert(exists obj2 = obj1.child);
    assertEquals("bottom", obj2.name);
    assert(! obj2.child exists);
}


serializable class NestedAttribute(shared String name, child) {
    // TODO nested attribute doesn't work on a non-initializer parameter
    // That's a bug
    nested shared NestedAttribute? child;
    shared actual String string => "NestedAttribute(\"``name``\", ``child else "null"``)";
}

"Test we respect [[nested]] annotation on an attribute"
test
shared void testNestedAttribute() {
    value o = NestedAttribute("top", NestedAttribute("middle", NestedAttribute("bottom", null)));
    value s= Serializer();
    s.add(o);
    print(s.pretty);
    assertEquals("""[
                     {
                      "$": "0",
                      "$type": "test.jsonsl::NestedAttribute",
                      "name": "top",
                      "child": {
                       "$type": "test.jsonsl::NestedAttribute",
                       "name": "middle",
                       "child": {
                        "$type": "test.jsonsl::NestedAttribute",
                        "name": "bottom",
                        "child": null
                       }
                      }
                     }
                    ]""", s.pretty);
    value d = Deserializer();
    value deserialized = d.parse(s.json);
    assert(deserialized.size == 1);
    assert(is NestedAttribute obj = deserialized.first);
    assertEquals("top", obj.name);
    assert(exists obj1 = obj.child);
    assertEquals("middle", obj1.name);
    assert(exists obj2 = obj1.child);
    assertEquals("bottom", obj2.name);
    assert(! obj2.child exists);
}

serializable class CyclicNested(String name) {
    nested shared late Anything child;
    shared actual String string => "CyclicNested(\"``name``\", ``child else "null"``)";
}

"what happens when we have cyclic data and nested -- presumably a stack overflow"
test
shared void testCyclicNested() {
    value o = CyclicNested("a");
    o.child = o;
    value s= Serializer();
    s.add(o);
    try {
        print(s.pretty);
        throw;
    } catch (AssertionError e) {
        assertEquals("cyclic data being serialized in nested format", e.message);
    }
}

test
shared void testArray() {
    Array<Anything> a = arrayOfSize<Anything>(5, null);
    a.set(1, "hello");
    a.set(2, 3);
    a.set(3, true);
    a.set(4, Point(1.0, 2.0));
    value s = Serializer();
    s.add(a);
    print(s.pretty);
    assertEquals("""[
                     {
                      "$": "0",
                      "$type": "ceylon.language::Array<ceylon.language::Anything>",
                      "size": 5,
                      "0": null,
                      "1": "hello",
                      "2": 3,
                      "3": true,
                      "4@": -1
                     },
                     {
                      "$": "-1",
                      "$type": "test.jsonsl::Point",
                      "x": 1.0,
                      "y": 2.0
                     }
                    ]""",
    s.pretty);
    value d = Deserializer();
    value deserialized = d.parse(s.pretty);
    print(deserialized);
    assert(deserialized.size == 1);
    assert(is Array<Anything> deserializedArray = deserialized.first);
    assert(!deserializedArray[0] exists);
    assert(is String hello=deserializedArray[1],
            "hello" == hello);
    assert(is Integer three=deserializedArray[2],
        3 == three);
    assert(is Boolean yes=deserializedArray[3],
        yes);
    assert(is Point oneTwo=deserializedArray[4]);
    print(oneTwo);
    assert(oneTwo == Point(1.0, 2.0));
}

test 
shared void testCyclicArray() {
    Array<Anything> a = arrayOfSize<Anything>(1, null);
    a.set(0, a);
    Array<Anything> b = arrayOfSize<Anything>(1, null);
    Array<Anything> c = arrayOfSize<Anything>(1, null);
    b.set(0, c);
    c.set(0, b);
    value s = Serializer();
    s.add(a);
    s.add(b);
    print(s.pretty);
    value d = Deserializer();
    value deserialized = d.parse(s.json).sequence();
    assert(deserialized.size == 2);
    assert(is Array<Anything> dsA = deserialized.first);
    assert(is Identifiable dsA1 = dsA[0]);
    assert(dsA === dsA1);
    
    assert(is Array<Anything> dsB = deserialized[1]);
    assert(is Array<Anything> dsC=dsB[0]);
    assert(is Array<Anything> dsB2=dsC[0]);
    assert(dsB===dsB2);
}

test
shared void testArrayOfInteger() {
    Array<Integer> a = arrayOfSize<Integer>(3, 1);
    a.set(1, 2);
    a.set(2, 3);
    value s = Serializer();
    s.add(a);
    assertEquals("""[
                     {
                      "$": "0",
                      "$type": "ceylon.language::Array<ceylon.language::Integer>",
                      "size": 3,
                      "0": 1,
                      "1": 2,
                      "2": 3
                     }
                    ]""",
        s.pretty);
    value d = Deserializer();
    value deserialized = d.parse(s.pretty);
    print(deserialized);
    assert(deserialized.size == 1);
    assert(is Array<Integer> deserializedArray = deserialized.first);
    assert(exists i1=deserializedArray[0], 1==i1);
    assert(exists i2=deserializedArray[1], 2==i2);
    assert(exists i3=deserializedArray[2], 3==i3);
}

serializable abstract class AbstractGeneric<T>(shared T t) {
    shared actual String string => "``type(this)``(``t else "null"``)";
}

serializable class Generic<T>(T t) extends AbstractGeneric<T>(t) {
    
}
serializable class GenericSub<T>(T t) extends Generic<T>(t) {
    
}
serializable class SubGeneric(String t) extends Generic<String>(t) {
    
}

test
shared void testGeneric() {
    value s = Serializer();
    s.add(Generic("generic"));
    s.add(GenericSub("generic-sub"));
    s.add(SubGeneric("sub-generic"));
    s.add(Generic(SubGeneric("generic(sub-generic)")));
    print(s.pretty);
    
    value d = Deserializer();
    value deserialized = d.parse(s.pretty).sequence();
    print(deserialized);
    
    assert(4 == deserialized.size);
    assert(is Generic<String> g1 = deserialized[0],
        g1.t == "generic");
    assert(is GenericSub<String> g2 = deserialized[1],
        g2.t == "generic-sub");
    assert(is SubGeneric g3 = deserialized[2],
        g3.t == "sub-generic");
    assert(is Generic<SubGeneric> g4 = deserialized[3],
        g4.t.t == "generic(sub-generic)");
}

class NotSerializable() {
    
}

"Check we throw a sensible exception when asked to serialize something we can't"
test
shared void testNotSerializable() {
    value s = Serializer();
    s.add(NotSerializable());
    try {
        print(s.pretty);
        throw Exception("Expected s.pretty to throw");
    } catch (AssertionError e) {
        assertTrue(e.message.startsWith("not an instance of a serializable class: test.jsonsl.NotSerializable@"));
    }
}

"Check we throw a sensible exception when asked to serialize something we can't"
test
shared void testObject() {
    value s = Serializer();
    s.add(larger);
    try {
        print(s.pretty);
        //throw Exception("Expected s.pretty to throw");
    } catch (AssertionError e) {
        assertEquals("not an instance of a serializable class: larger", e.message);
    }
    
    value d = Deserializer();
    value deserialized = d.parse(s.pretty);
    print(deserialized);
    assert(1 == deserialized.size);
}

test
shared void testArraySequence() {
    value s = Serializer();
    s.add(sequence{1, 2, 3});
    print(s.pretty);
    value d = Deserializer();
    value deserialized = d.parse(s.pretty);
    print(deserialized);
    assert(1 == deserialized.size);
    assert(is ArraySequence<Integer> as1 = deserialized.first);
    assert(exists as11 = as1[0], as11 == 1);
    assert(exists as12 = as1[1], as12 == 2);
    assert(exists as13 = as1[2], as13 == 3);
    
}

test
shared void testSpan() {
    value s = Serializer();
    s.add(1..10);
    print(s.pretty);
    value d = Deserializer();
    value deserialized = d.parse(s.pretty);
    assert(is Range<Integer> s1 = deserialized.first);
    assert(s1 == 1..10);
}

test
shared void testMeasure() {
    value s = Serializer();
    s.add(0:10);
    print(s.pretty);
    value d = Deserializer();
    value deserialized = d.parse(s.pretty);
    assert(is Range<Integer> s1 = deserialized.first);
    assert(s1 == 0:10);
}

/*test
shared void testEmpty() {
    value s = Serializer();
    s.add(empty);
    print(s.pretty);
    // TODO this doesn't work!
    assertEquals("""[
                     {
                      "$": "0",
                      "$type": "ceylon.language::empty"
                     }
                    ]""",
                    s.pretty);
    value d = Deserializer();
    value deserialized = d.parse(s.pretty);
    assert(deserialized.size == 1);
    assert(exists e = deserialized.first,
            e == empty);
}*/

test
shared void testTupleWithEmptyTail() {
    value s = Serializer();
    s.add([1, "hello", true, Point(1.0, 1.0)]);
    print(s.pretty);
    value d = Deserializer();
    value deserialized = d.parse(s.pretty);
    print( deserialized );
    assert(1 == deserialized.size);
    assert(is [Integer, String, Boolean, Point] t = deserialized.first);
    assert(1==t[0]);
    assert("hello"==t[1]);
    assert(true==t[2]);
    assert(Point(1.0, 1.0)==t[3]);
}

test
shared void testTupleWithSpanTail() {
    value s = Serializer();
    s.add([1, "hello", true, *(1..3)]);
    print(s.pretty);
    value d = Deserializer();
    value deserialized = d.parse(s.pretty);
    print( deserialized );
    assert(1 == deserialized.size);
    assert(is [Integer, String, Boolean, Integer+] t = deserialized.first);
    assert(1==t[0]);
    assert("hello"==t[1]);
    assert(true==t[2]);
    assert(1==t[3]);
    assert(exists t4=t[4], t4==2);
    assert(exists t5=t[5], t5==3);
    assert(is Range<Integer> x=t.rest.rest.rest);
}

test
shared void testSingleton() {
    value s = Serializer();
    s.add(Singleton(smaller));
    print(s.pretty);
    value d = Deserializer();
    value deserialized = d.parse(s.pretty);
    print( deserialized );
    assert(1 == deserialized.size);
    assert(is Singleton<Comparison> t = deserialized.first);
    assert(exists t0=t[0], smaller==t0);
}

serializable class Person(first, last, address) {
    shared String first;
    shared String last;
    nested shared variable Address? address;
    shared actual String string => "``first`` ``last`` living at ``address else "null"``";
}

nested
serializable class Address(houseNumberName,  
    street, town, country) {
    jsonKey("num")
    shared variable String houseNumberName;
    shared variable String street;
    shared variable String town;
    shared variable String country;
    shared actual String string => "``houseNumberName``, ``street``,
                                    ``town``,
                                    ``country``";
}

test
shared void testCustom() {
    value fdr = Person{
        first="Fred";
        last="Rooseveldt";
        address=Address{
            houseNumberName = "1600";
            street="Pennsylvania Avenue";
            town = "Washington, DC";
            country="USA";
        };
    };
    value s = Serializer{
        void addClassifier(Visitor visitor, ClassModel c) {return;}
    };
    //TODO Is it possible to substitute the defaulted value for a 
    // omitted value parameter on deserialisation? 
    //TODOWhat about on serialization? 
    s.add(fdr);
    print(s.pretty);
    value d = Deserializer{
        function classify(String? key, JsonObject o) {
            if (o.defines("first")) {
                return `Person`;
            } else if (o.defines("street") && o.defines("town")) {
                return `Address`;
            }
            throw;
        }
    };
    print (d.parse(s.pretty));
}


// TODO test language module things which are not Identifiable

test
shared void dupeId() {
    value fdr = Person{
        first="Fred";
        last="Rooseveldt";
        address=Address{
            houseNumberName = "1600";
            street="Pennsylvania Avenue";
            town = "Washington, DC";
            country="USA";
        };
    };
    value nixon = Person{
        first="Richard";
        last="Nixon";
        address=Address{
            houseNumberName = "1600";
            street="Pennsylvania Avenue";
            town = "Washington, DC";
            country="USA";
        };
    };
    variable value s = Serializer();
    s.add(fdr);
    print(s.pretty);
    value fdrJson = s.pretty.replaceFirst("[", "").replaceLast("]", "");
    s = Serializer();
    s.add(nixon);
    print(s.pretty);
    value nixonJson = s.pretty.replaceFirst("[", "").replaceLast("]", "");
    
    value d = Deserializer();
    value deser = d.parse("[``fdrJson``, ``nixonJson``]");
    print(deser);
    assert(1 == deser.size);
    assert(is Person fdr2 = deser.first);
    assert("Fred" == fdr2.first,
        "Rooseveldt" ==  fdr2.last);
}

test
shared void dupeId2() {
    value whiteHouse = Address{
            houseNumberName = "1600";
            street="Pennsylvania Avenue";
            town = "Washington, DC";
            country="USA";
        };
    value fdr = Person{
        first="Fred";
        last="Rooseveldt";
        address=whiteHouse;
    };
    variable value s = Serializer();
    s.add(fdr);
    value fdrJson = s.pretty.replaceFirst("[", "").replaceLast("]", "");
    s = Serializer();
    s.add(whiteHouse);
    value whiteHouseJson = s.pretty.replaceFirst("[", "").replaceLast("]", "");
    value d = Deserializer();
    value deser = d.parse("[``fdrJson``, ``whiteHouseJson``]");
    print(deser);
    assert(1 == deser.size);
    assert(is Person fdr2 = deser.first);
    assert("Fred" == fdr2.first,
        "Rooseveldt" ==  fdr2.last);
}
