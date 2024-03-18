mod Subtyping {

    type alias Types[r : SchemaRow] = #{ Class(String), Interface(String), ClassExtends(String, String), ClassImplements(String, String), InterfaceExtends(String, String), Method(String, String) | r }

    def open(types: #{ Class(String), Interface(String), ClassExtends(String, String), ClassImplements(String, String), InterfaceExtends(String, String), Method(String, String) }): Types[r] = types

    def subtypeRules(): #{ IsSubtype(String, String), Class(String), Interface(String), ClassExtends(String, String), ClassImplements(String, String), InterfaceExtends(String, String) | r } = #{
        IsSubtype(sub, super) :- Class(sub), Class(super), if sub == super.
        IsSubtype(sub, super) :- Interface(sub), Interface(super), if sub == super.
        IsSubtype(sub, super) :- Class(sub), Class(super), ClassExtends(sub, super).
        IsSubtype(sub, super) :- Class(sub), Interface(super), ClassImplements(sub, super).
        IsSubtype(sub, super) :- Interface(sub), Interface(super), InterfaceExtends(sub, super).
        IsSubtype(sub, super) :- IsSubtype(sub, a), IsSubtype(a, super).
    }

    pub def subtypes(types: Types[#()]): Vector[(String, String)] = {
        query subtypeRules(), open(types) select (sub, super) from IsSubtype(sub, super)
    }

    pub def methodsOf(tpe: String, types: Types[#()]): Vector[String] =
        let rules = #{
            AvailableMethod(class, method) :- Method(t, method), IsSubtype(class, t).
        };
        query rules, subtypeRules(), open(types) select m from AvailableMethod(tpe, m)

    pub def isNonSupertype(tpe: String, types: Types[#()]): Bool =
        let rules = #{
            NonSupertype(class) :- Class(class), Class(otherClass), not IsSubtype(otherClass, class), if class != otherClass.
        };
        let result = query rules, subtypeRules(), open(types) select tpe from NonSupertype(tpe);
        not Vector.isEmpty(result)

    pub def hasInheritanceCycle(types: Types[#()]): Bool = ???

    pub def extendsOnlyObject(tpe: String, types: Types[#()]): Bool = ???

    pub def isSubtype(subtype: String, possibleSupertype: String, types: Types[#()]): Bool =
        not (subtypes(types) |> Vector.filter(Eq.eq((subtype, possibleSupertype))) |> Vector.isEmpty)

    pub def hasMethod(tpe: String, method: String, types: Types[#()]): Bool =
        not (methodsOf(tpe, types) |> Vector.filter(Eq.eq(method)) |> Vector.isEmpty)

}
