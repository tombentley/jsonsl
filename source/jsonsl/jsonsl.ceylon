import ceylon.language.serialization {
    Reference,
    serialization,
    SerializableReference,
    SerializationContext,
    deserialization,
    DeserializableReference,
    RealizableReference,
    Deconstructed,
    Deconstructor
}
import ceylon.language.meta {
    type, 
    optionalAnnotation,
    typeLiteral
}
import ceylon.language.meta.model {
    Class,
    Type,
    ClassModel
}
import ceylon.language.meta.declaration {
    ValueDeclaration,
    ClassDeclaration,
    TypeParameter
}
import ceylon.json {
    JsonObject=Object,
    nil,
    JsonValue=Value,
    JsonArray=Array,
    JsonNull=NullInstance,
    jsonParse=parse,
    Visitor,
    StringEmitter
}
import ceylon.collection {
    HashMap,
    HashSet,
    IdentitySet
}

"""Annotation class for attributes that should be persisted "by value" 
   (i.e. copied into the JSON stream as a nested object/hash) 
   rather than "by reference" (i.e. an id .
   """
shared final annotation class Nested() satisfies OptionalAnnotation<Nested, ClassDeclaration|ValueDeclaration> {}

"""Annotation for instances that should be persisted "by value" 
   (i.e. copied into the JSON stream as a nested object) 
   rather than "by reference" (i.e. the attribute value is an 
   identifier that is a reference to another object elsewhere 
   in the JSON stream. 
   
   When applied to a class then any instance of that class will be serialized 
   by value.
   When applied to an attribute then the instance accessed *via that 
   attribute* will be serialized by value. If the instance is reachable 
   via another path through the object graph then it may also be 
   serialized by refernece if the attribute via which it is reached is 
   not annotated with `nested`. 
   """
// TODO semantics of a nested class for subclasses
shared annotation Nested nested() => Nested();


"Annotation class for attributes which need to override the JSON Object 
 key against which their value is stored.
 
 By default the attributes name is used."
shared final annotation class JsonKey(shared String key) satisfies OptionalAnnotation<JsonKey, ValueDeclaration> {}

"Annotation for attributes which need to override the JSON Object 
 key against which their value is stored.
 
 By default the attributes name is used."
shared annotation JsonKey jsonKey(String key) => JsonKey(key);

void defaultAddClassifier(Visitor visitor, ClassModel<Anything,Nothing> cls) {
    visitor.onKey("$type");
    visitor.onString(cls.string);
}

"Serializes objects to a form representible as a JSON hash."
shared class Serializer(
    "Callback to add type information to the JSON hash 
     when serializing an instance of the given class.
     [[Deserializer.classify]] supplies the inverse 
     function of determining the Ceylon Class for a given
     JSON hash during deserialization."
    Anything(Visitor,ClassModel) addClassifier = defaultAddClassifier
) {
    
    value context = serialization();
    
    "generator for ids for objects explicitly [[add]]ed to this serialization. 
     Such ids increment from 0."
    variable Integer explicit = 0;
    
    "generator for ids for objects included in the serialized stream because 
     they were reachable from explicitly added objects. 
     Such ids decrement from -1.
     We distinguish these from the explict case so that the 
     derserializer can return exactly the objects that were explicitly added."
    variable Integer implicit = -1;
    
    "generator for ids for objects which in the stream as nested hashes. 
     We don't even need to include the id in the stream for these, because
     by definition nothing can reference them so we can given them new
     ids during deserialization.
     Such ids increment from [[runtime.minIntegerValue]]."
    variable Integer nested = runtime.minIntegerValue;
    
    "Adds an instance to be serialized. 
     
     The given instance and every 
     instance reachable from it are added to the objects to be serialized.
     Instances which are reachable but not [[Identifiable]] 
     are added as nested JSON hashes, 
     otherwise attribute references are by reference."
    shared void add(Anything instance) {
        value id = explicit++;
        this.context.reference(id, instance);
    }
    
    "Serialize the objects previously [[add]]ed generating events 
     handled by the given visitor."
    shared void serialize(Visitor visitor) {
        object dtor satisfies Deconstructor {
            
            value serializing = IdentitySet<Identifiable>();
            
            //variable ClassDeclaration? cls = null;
            
            "Mutable state that changes as we recurse though the object graph"
            variable [ClassDeclaration, HashSet<String>]? state = null;
            
            function setInstance(Anything instance) {
                ClassModel cls = type(instance);
                addClassifier(visitor, cls);
                value oldState = this.state;
                this.state = [cls.declaration, HashSet<String>()];
                return oldState;
            }
            
            "Compute the key to use for the given attribute/"
            String computeKey(attribute, Boolean ref) {
                "The attribute we're putting"
                ValueDeclaration attribute;
                variable String jsonKey;
                if (exists key = optionalAnnotation(`JsonKey`, attribute)) {
                    jsonKey = key.key;
                } else {
                    jsonKey = attribute.name;
                }
                if (ref) {
                    jsonKey = jsonKey + "@";
                }
                value hashSet = (state else nothing)[1];
                Boolean hasKey = hashSet.contains(jsonKey);
                if (hasKey) {
                    throw AssertionError("ambiguous JSON key ``jsonKey`` "+
                        "for attribute ``attribute.qualifiedName``. "+
                            "Use jsonKey annotation to disambiguate");
                }
                hashSet.add(jsonKey);
                return jsonKey;
            }
            
            shared actual void putTypeArgument(TypeParameter typeParameter, Type typeArgument) {
                //TODO
            }
            
            function byReference<Instance>(Instance referredValue, ValueDeclaration? attribute) {
                switch (referredValue)
                case (is String|Integer|Float|Boolean|Null) {
                    return false;// these are always by value
                }
                else {
                    if (!referredValue is Identifiable) {
                        return false;
                    } else if (optionalAnnotation(`Nested`, (this.state else nothing)[0]) exists) {
                        return false;
                    } else if (exists attribute) {
                        if (optionalAnnotation(`Nested`, attribute) exists) {
                            return false;
                        } else {
                            return true;
                        }
                    } else {
                        return true;
                    }
                }
            }
            
            function key<Instance>(Instance referredValue, ValueDeclaration attribute) {
                variable value byRef = byReference(referredValue, attribute);
                switch (referredValue)
                case (is String|Integer|Float|Boolean|Null) {
                    visitor.onKey(computeKey(attribute, false));
                }
                else {
                    if (byRef) {
                        //a reference to another object in stream
                        visitor.onKey(computeKey(attribute, true));
                    } else {
                        // embedded object
                        visitor.onKey(computeKey(attribute, false));
                    }
                } /// Add the value
                return byRef;
            }
            
            void val<Instance>(Instance referredValue, Boolean byReference) {
                switch (referredValue)
                case (is String) {
                    visitor.onString(referredValue);
                }
                case (is Integer|Float) {
                    visitor.onNumber(referredValue);
                }
                case (is Boolean) {
                    visitor.onBoolean(referredValue);
                }
                case (is Null) {
                    visitor.onNull();
                }
                else {
                    // give embedded objects a negative id when adding them to the context
                    // that way we can omit them from also 
                    // being added as top level objects
                    value id = byReference then implicit-- else nested++;
                    assert (nested < implicit);
                    value referredReference = context.getReference(referredValue) else context.reference(id, referredValue);
                    if (byReference) { 
                        //a reference to another object in stream
                        assert(is Integer referredId = referredReference.id);
                        visitor.onNumber(referredId);
                    }
                    else { 
                        //an "embedded" object
                        visitor.onStartObject();
                        serializeRecursively(context, referredReference);
                        visitor.onEndObject();
                    }
                }
            }
            
            shared actual void putValue<Instance>(ValueDeclaration attribute, Instance referredValue) {
                value byReference=key(referredValue, attribute);// Add the value
                val(referredValue, byReference);
            }
            
            shared actual void putElement<Instance>(Integer index, Instance referenced) {
                value byRef  = byReference(referenced, null);
                visitor.onKey(byRef then index.string + "@" else index.string);
                val(referenced, byRef);
                
            }
            
            shared actual void putOuterInstance<Instance>(Instance v) {
                // TODO 
            }
            
            "A [[JSON Object|JsonObject]] for the given [[reference]] with a key 
             for the type and each of the attributes. 
             Non-[[Identifiable]] attribute values are serialized recursively."
            shared void serializeRecursively<Instance>(SerializationContext context, SerializableReference<Instance> reference) {
                if (serializing.contains(reference)) {
                    throw AssertionError("cyclic data being serialized in nested format");
                }
                if (is Identifiable reference) {
                    serializing.add(reference);
                }
                assert(is Integer id = reference.id);
                value savedState = this.setInstance(reference.instance());
                
                reference.serialize(this);
                
                this.state = savedState;
                if (is Identifiable reference) {
                    serializing.remove(reference);
                }
            }
        }// end of dtor
        
        visitor.onStartArray();
        for (ref in context) {
            if (is Integer id=ref.id , id>= implicit) {
                visitor.onStartObject();
                visitor.onKey("$");
                visitor.onString(ref.id.string);
                dtor.serializeRecursively(context, ref);
                visitor.onEndObject();
            }
        }
        visitor.onEndArray();
    }
    
    "[[serialize]]s the objects already added and returns the JSON string"
    shared String json {
        StringEmitter emitter = StringEmitter(false);
        serialize(emitter);
        return emitter.string;
    }
    
    "[[serialize]]s the objects already added and returns the pretty JSON string"
    shared String pretty {
        StringEmitter emitter = StringEmitter(true);
        serialize(emitter);
        return emitter.string;
    }
    
}

Class parseClassName(String className) {
    value type = parseModel(className);
    assert(is Class type);
    return type;
}
"The Ceylon Class that the given JSON hash encodes."
Class getClass(String? key, JsonObject obj) {
    assert(is String className = obj.get("$type"));
    value klass = parseModel(className);
    assert(is Class klass);
    return klass;
}


"Deserializes streams previously serialized by [[Serializer]],"
shared class Deserializer(
    "Determines the Ceylon Class for a given
     JSON hash being deserialized. This should use the 
     key(s) added by [[Serializer.addClassifier]] to 
     determine the type."
    //Class(String?,JsonObject) classify = getClass
    Class classify(
        "The key in the containing JSON hash of the hash being classified, 
         or null if the hash being classified is not contained within 
         another hash."
        String? key,
        "The JSON hash being classified."
        JsonObject hash) => getClass(key, hash)
) {
    
    value context = deserialization();
    
    "A cache memoising the result of [[attributeMap()]]."
    value attrMaps = HashMap<ClassDeclaration, Map<String,ValueDeclaration>>();
    
    "When we encounter a nested JSON hash we give it a synthetic id 
     (we have to because of the serialization API). It doesn't matter what it 
     is, because by definition nothing can refer to it."
    variable value anon = runtime.minIntegerValue; 
    
    "A map of JSON key name to ValueDeclaration for the given ClassDeclaration"
    Map<String,ValueDeclaration> attributeMap(ClassDeclaration klass) {
        // XXX could use structural sharing based on the class hierarchy
        if (exists r = attrMaps[klass]) {
            return r;
        } else {
            HashMap<String,ValueDeclaration> result = HashMap<String,ValueDeclaration>();
            variable ClassDeclaration? cd = klass;
            while (exists cde = cd) {
                for (ValueDeclaration attr in cde.declaredMemberDeclarations<ValueDeclaration>()) {
                    String name;
                    if (exists jsonKey = optionalAnnotation(`JsonKey`, attr)) {
                        name = jsonKey.key;
                    } else {
                        name = attr.name;
                    } 
                    result.put(name,attr);
                    // TODO Yuck: We put the attribute in the map twice 
                    result.put(name+"@",attr);
                }
                cd = cde.extendedType?.declaration;
            }
            attrMaps.put(klass, result);
            return result;
        }
    }
    
    "Adds a [[JSON Object|JsonObject]] to the set of objects to be deserialized."
    shared void add(JsonObject jsonObject) {
        assert(is String idStr = jsonObject.get("$"),
            exists id = parseInteger(idStr));
        Class c = classify(null,jsonObject);
        if (!exists r = context.getReference(id)) {
            // ignore dupe ids
            assert(is DeserializableReference<Anything> ref = context.reference(id, c));
            ref.deserialize(Dted(attributeMap(ref.clazz.declaration), jsonObject));
        }
    }
    
    "The objects that were explicitly [[Serializer.add]]ed 
     to the `Serializer`."
    shared {Anything*} items {
        return context.map<Anything>(function(ref) {
            if (is RealizableReference<Anything> ref) {
                if (is Integer id = ref.id,
                        id >= 0) {
                    // filter out the implicit objects 
                    return ref.instance();
                } else {
                    return null;
                }
            } else {
                throw;
            }
        }).coalesced;
    }
    
    "Parses the given string as a JSON array of serialized objects, 
     [[add]]s each object to this deserializer and returns the [[items]]."
    shared {Anything*} parse(String json) {
        value parsed = jsonParse(json);
        "Expected an Array of Objects"
        assert (is JsonArray parsed);
        for (jsonObject in parsed) {
            "Expected an Array of Objects"
            assert (is JsonObject jsonObject);
            add(jsonObject);
        }
        return items;
    }
    
    class Dted(Map<String,ValueDeclaration> attributeMap, JsonObject obj) satisfies Deconstructed {
        "The outer instance if the class of the instance is a 
         member class, otherwise null."
        shared actual Reference<Instance>? getOuterInstance<Instance>() {
            // TODO
            return nothing;
        }
        
        "The type argument of the given type parameter."
        throws (`class AssertionError`,
            "if the type argument is absent")
        shared actual Type getTypeArgument(TypeParameter typeParameter) {
            // TODO
            return nothing;
        }
        
        "The value of the given attribute."
        throws (`class AssertionError`,
            "if the value is absent")
        shared actual Instance|Reference<Instance> getValue<Instance>(ValueDeclaration attribute){
            String name;
            if (exists jsonKey = optionalAnnotation(`JsonKey`, attribute)) {
                name = jsonKey.key;
            } else {
                name = attribute.name;
            }
            JsonValue? val = obj.get(name);
            JsonValue? valRef = obj.get(name+"@");
            switch (val)
            case (is Null) {
                switch (valRef) 
                case (is Integer) {
                    assert(is Reference<Instance> r = context.getReference<Instance>(valRef));
                    return r;
                }
                case(is Null) {
                    throw Exception("Missing value for attribute ``attribute.qualifiedName`` using key ``name`` in JSON object ``obj``");
                } 
                else {
                    throw Exception("Unexpected JSON type ``type(valRef)`` for key ``name``@ in JSON object ``obj``");
                }
            } 
            case (is Integer) {
                assert(is Instance val);
                return val;
            } 
            case (is JsonObject) {
                // embedded object
                Class cx = classify(name, val);
                if (exists o = cx.declaration.objectValue){
                    if (is Instance ov = o.get()) {
                        return ov;
                    } else {
                        throw AssertionError("object ``o`` with value ``o.get() else "null"`` is not an instance of ``typeLiteral<Instance>()``");
                    }
                }
                assert(is DeserializableReference<Anything> xxx = context.reference(anon++, cx));
                assert(is Instance r = xxx.deserialize(Dted(outer.attributeMap(cx.declaration), val)).instance());
                
                return r;
                //throw;
            }
            case (is JsonNull ) {
                assert(is Instance result=null);
                return result;
            }
            case (is String|Float|Boolean ) {
                assert(is Instance val);
                return val;
            }
            case (is JsonArray) {
                throw;
            }
        }
        
        "The array element at the given index."
        shared actual Instance|Reference<Instance> getElement<Instance>(Integer index) {
            if (exists v=obj[index.string]) {
                switch (v)
                case (is String|Boolean|Integer|Float) {
                    assert(is Instance v);
                    return v;
                }
                case (nil) {
                    assert(is Instance n=null);
                    return n;
                }
                case (is JsonArray) {
                    
                }
                case (is JsonObject) {
                    // a nested object
                }
                throw;
            } else if (exists v = obj[index.string+"@"]) {
                assert(is Integer v);
                assert(is Reference<Instance> x = context.getReference(v));
                return x;
            }
            throw;
        }
        
        shared actual Iterator<[ValueDeclaration, Anything]> iterator() {
            return object satisfies Iterator<[ValueDeclaration, Anything]> {
                Iterator<String->JsonValue> i = obj.iterator(); 
                shared actual [ValueDeclaration, Anything]|Finished next() {
                    if (!is Finished n=i.next()) {
                        if (exists firstChar = n.key.first,
                            !firstChar.letter) {
                            // ignore $, $type, 0, 0$ etc
                            return next();
                        }
                        if (exists attribute = attributeMap[n.key]) {
                            return [attribute, getValue<Anything>(attribute)];
                        } else if (exists attribute = attributeMap[n.key+"@"]) {
                            return [attribute, getValue<Anything>(attribute)];
                        } else {
                            throw Exception("couldn't find key ``n.key`` in ``obj``");
                        }
                    } else {
                        return finished;
                    }
                }
            };
        }
    }
}
