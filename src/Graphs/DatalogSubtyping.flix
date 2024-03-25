mod Subtyping {

    type alias Program = {
        classes = List[String],
        finalClasses = List[String],
        interfaces = List[String],
        implementations = List[(String, String)],
        classExtends = List[(String, String)],
        interfaceExtends = List[(String, String)],
        methods = List[(String, String)]
    }

    def subtypes(program: Program): #{ Subtype(String, String), AvailableMethod(String, String), NonSupertype(String), ExtendsFinalClass(String, String) | r } =
        let rules = #{
            Subtype(x, x)       :- Class(x).
            Subtype(x, x)       :- Interface(x).
            Subtype(sub, super) :- Class(sub), Interface(super), Implementation(sub, super).
            Subtype(sub, super) :- Class(sub), Class(super), ClassExtends(sub, super).
            Subtype(sub, super) :- Interface(sub), Interface(super), InterfaceExtends(sub, super).
            Subtype(sub, super) :- Subtype(sub, a), Subtype(a, super).

            AvailableMethod(class, method) :- Method(t, method), Subtype(class, t).
            NonSupertype(class)            :- Class(class), Class(otherClass), not Subtype(otherClass, class), if class != otherClass.
            ExtendsFinalClass(sub, super)  :- FinalClass(super), Subtype(sub, super), if sub != super.
        };
        #(Subtype, AvailableMethod, NonSupertype, ExtendsFinalClass) -> rules <+> toPredicates(program)

    def toPredicates(program: Program): #{ Class(String), FinalClass(String), Interface(String), ClassExtends(String, String),  InterfaceExtends(String, String), Implementation(String, String), Method(String, String) | r } =
        let cs    = inject program.classes into Class;
        let fcs   = inject program.finalClasses into FinalClass;
        let is    = inject program.interfaces into Interface;
        let impls = inject program.implementations into Implementation;
        let ce    = inject program.classExtends into ClassExtends;
        let ies   = inject program.interfaceExtends into InterfaceExtends;
        let ms    = inject program.methods into Method;
        cs <+> fcs <+> is <+> ce <+> impls <+> ies <+> ms

    pub def subtypesDistinct(types: Program): Vector[(String, String)] =
        let v = query subtypes(types) select (x, y) from Subtype(x, y);
        Vector.filter(match (x, y) -> x != y, v)

    pub def methodsOf(tpe: String, types: Program): Vector[String] =
        query subtypes(types) select m from AvailableMethod(tpe, m)

    pub def nonSupertypes(types: Program): Vector[String] =
        query subtypes(types) select tpe from NonSupertype(tpe)

    pub def objectOnlyExtensions(types: Program): Vector[String] =
        let rules = #{
            ExtendsOnlyObject(class) :- ClassExtends(class, "java.lang.Object"), ClassExtends(class, otherClass), if otherClass == "java.lang.Object".
        };
        query rules, subtypes(types), toPredicates(types) select class from ExtendsOnlyObject(class)

    pub def isSubtype(subtype: String, possibleSupertype: String, types: Program): Bool =
        let v = query subtypes(types) select (x, y) from Subtype(x, y);
        not (Vector.filter(Eq.eq((subtype, possibleSupertype)), v) |> Vector.isEmpty)

    pub def hasMethod(tpe: String, method: String, types: Program): Bool =
        not (methodsOf(tpe, types) |> Vector.filter(Eq.eq(method)) |> Vector.isEmpty)

    pub def isNonSupertype(tpe: String, types: Program): Bool =
        not (nonSupertypes(types) |> Vector.filter(Eq.eq(tpe)) |> Vector.isEmpty)

    pub def extendsOnlyObject(tpe: String, types: Program): Bool =
        not (objectOnlyExtensions(types) |> Vector.filter(Eq.eq(tpe)) |> Vector.isEmpty)


    /////////////////////////////////////////////////
    // Tests                                       //
    /////////////////////////////////////////////////

    mod Test {

        @Test
        def testSubtypes01(): Bool =
            let result = Subtyping.subtypesDistinct(emptyFacts());
            Assert.eq(Vector#{}, result)

        @Test
        def testSubtypes02(): Bool =
            let result = Subtyping.subtypesDistinct(facts());
            let expected = Vector#{("au.content.Marker", "java.lang.Object"), ("java.lang.String", "java.io.Serializable"), ("java.lang.String", "java.lang.Object"), ("java.util.ArrayList", "java.lang.Object"), ("java.util.ArrayList", "java.util.List"), ("java.util.ArrayList", "java.util.Collection"), ("java.util.List", "java.util.Collection"), ("au.content.SpecialArrayList", "java.util.ArrayList"), ("au.content.SpecialArrayList", "java.lang.Object"), ("au.content.SpecialArrayList", "java.util.List"), ("au.content.SpecialArrayList", "java.util.Collection")} |> Vector.sort;
            Assert.eq(expected, result)

        @Test
        def testSubtype01(): Bool =
            let result = Subtyping.isSubtype("java.lang.String", "java.lang.Object", emptyFacts());
            Assert.eq(false, result)

        @Test
        def testSubtype02(): Bool =
            let result = Subtyping.isSubtype("java.lang.String", "java.lang.Object", facts());
            Assert.eq(true, result)

        @Test
        def testSubtype03(): Bool =
            let result = Subtyping.isSubtype("java.lang.Object", "java.lang.Object", facts());
            Assert.eq(true, result)

        @Test
        def testSubtype04(): Bool =
            let result = Subtyping.isSubtype("java.lang.Object", "java.lang.String", facts());
            Assert.eq(false, result)

        @Test
        def testSubtype05(): Bool =
            let result = Subtyping.isSubtype("java.util.ArrayList", "java.util.Collection", facts());
            Assert.eq(true, result)

        @Test
        def testSubtype06(): Bool =
            let result = Subtyping.isSubtype("au.content.SpecialArrayList", "java.util.Collection", facts());
            Assert.eq(true, result)

        @Test
        def testSubtype07(): Bool =
            let result = Subtyping.isSubtype("java.util.List", "java.util.List", facts());
            Assert.eq(true, result)

        @Test
        def testMethodsOf01(): Bool =
            let result = Subtyping.methodsOf("java.lang.String", facts());
            let expected = Vector#{"toString", "contains", "isEmpty"} |> Vector.sort;
            Assert.eq(expected, result)

        @Test
        def testMethodsOf02(): Bool =
            let result = Subtyping.methodsOf("java.util.ArrayList", facts());
            let expected = Vector#{"toString", "add"} |> Vector.sort;
            Assert.eq(expected, result)

        def emptyFacts(): Subtyping.Program = {
            classes = List#{},
            finalClasses = List#{},
            interfaces = List#{},
            implementations = List#{},
            classExtends = List#{},
            interfaceExtends = List#{},
            methods = List#{}
        }

        def facts(): Subtyping.Program = {
            classes = List#{
                "java.lang.Object",
                "java.lang.String",
                "java.util.ArrayList",
                "au.content.Marker",
                "au.content.SpecialArrayList"
            },
            finalClasses = List#{
                "java.lang.String"
            },
            interfaces = List#{
                "java.io.Serializable",
                "java.util.List",
                "java.util.Collection"
            },
            implementations = List#{
                ("java.lang.String", "java.io.Serializable"),
                ("java.util.ArrayList", "java.util.List")
            },
            classExtends = List#{
                ("java.lang.String", "java.lang.Object"),
                ("java.util.ArrayList", "java.lang.Object"),
                ("au.content.SpecialArrayList", "java.util.ArrayList"),
                ("au.content.Marker", "java.lang.Object")
            },
            interfaceExtends = List#{
                ("java.util.List", "java.util.Collection")
            },
            methods = List#{
                ("java.lang.Object", "toString"),
                ("java.util.List", "add"),
                ("java.lang.String", "contains"),
                ("java.lang.String", "isEmpty")
            }
        }
    }
}
