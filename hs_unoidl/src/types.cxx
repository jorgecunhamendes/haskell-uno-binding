/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */
/*
 * This file is part of the LibreOffice project.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
#include "types.hxx"

#include <iostream>

#include "module.hxx"
#include "utils.hxx"

using rtl::OUString;

bool isHsUnoType (OUString const & type) {
    return (type.compareTo("hsuno", 5) == 0);
}

bool isBasicType (OUString const & type) {
    return (type == "void"
            || type == "boolean"
            || type == "byte"
            || type == "short"
            || type == "unsigned short"
            || type == "long"
            || type == "unsigned long"
            || type == "hyper"
            || type == "unsigned hyper"
            || type == "float"
            || type == "double"
            || type == "char");
}

bool isSimpleType (OUString const & type) {
    return (isBasicType(type)
            || type == "string"
            || type == "type"
            || type == "any");
}

bool isPrimitiveType (OUString const & type) {
    return ((isSimpleType(type) && type != "any")
            || type == "enum");
}

bool isStringType (OUString const & type) {
    return (type == "string");
}

bool isSequenceType (OUString const & type) {
    return (type.compareTo("[]", 2) == 0);
}

OUString toCppType (OUString const & name)
{
    if (name.compareTo("hsuno ", 6) == 0)
        return name.copy(6);
    if (name == "hsuno_interface") return OUString("uno_Interface *");
    if (name == "hsuno_exception_ptr") return OUString("uno_Any **");
    if (name == "void") return OUString("void");
    if (name == "boolean") return OUString("bool");
    if (name == "char") return OUString("sal_Int8");
    if (name == "short") return OUString("sal_Int16");
    if (name == "long") return OUString("sal_Int32");
    if (name == "hyper") return OUString("sal_Int64");
    if (name == "float") return OUString("float");
    if (name == "double") return OUString("double");
    if (name == "string") return OUString("rtl::OUString");
    if (name == "type") return OUString("typelib_TypeDescription");
    //if (name == "type") return OUString("::css::uno::Type");
    if (name == "any") return OUString("uno_Any");
    OUString result;
    if (isSequenceType(name)) {
        result = "css::uno::Sequence< " + toCppType(name.copy(2)) + " >";
    } else {
        result = name.replaceAll(".","::");
    }
    return result;
}

OUString toHsType (OUString const & name, bool isInterface)
{
    if (name.compareTo("hsuno ", 6) == 0)
        return name.copy(6);
    if (name == "hsuno_interface") return OUString("UnoInterface");
    if (name == "hsuno_exception_ptr") return OUString("(Ptr Any)");
    if (name == "void") return OUString("()");
    if (name == "boolean") return OUString("Bool");
    if (name == "char") return OUString("Int8");
    if (name == "short") return OUString("Int16");
    if (name == "long") return OUString("Int32");
    if (name == "hyper") return OUString("Int64");
    if (name == "float") return OUString("Float");
    if (name == "double") return OUString("Double");
    if (name == "string") return OUString("Text");
    //if (name == "type") return OUString("TypeDescription");
    if (name == "type") return OUString("CssUnoTypePtr");
    if (name == "any") return OUString("Any");
    OUString result;
    if (name == "[]string") {
        result = "[" + toHsType(name.copy(2)) + "]";
    } else if (isSequenceType(name)) {
        result = "(Ptr (CSequence ()))";
        //result = "(Ptr (CSequence " + toHsType(name.copy(2)) + "))";
    } else if (isInterface) {
        result = "(Reference " + Module(name).getNameCapitalized() + ")";
    } else {
        result = "(Ptr " + Module(name).getNameCapitalized() + ")";
    }
    return result;
}

OUString toHsCppType (OUString const & name)
{
    if (name.compareTo("hsuno ", 6) == 0)
        return name.copy(6);
    if (name == "hsuno_interface") return OUString("(Ptr UnoInterface)");
    if (name == "hsuno_exception_ptr") return OUString("(Ptr AnyPtr)");
    if (name == "void") return OUString("()");
    if (name == "boolean") return OUString("Bool");
    if (name == "char") return OUString("Int8");
    if (name == "short") return OUString("Int16");
    if (name == "long") return OUString("Int32");
    if (name == "hyper") return OUString("Int64");
    if (name == "float") return OUString("Float");
    if (name == "double") return OUString("Double");
    if (name == "string") return OUString("OUStringPtr");
    if (name == "type") return OUString("CssUnoTypePtr");
    if (name == "any") return OUString("AnyPtr");
    OUString result;
    if (name == "[]string") {
        result = "(Ptr (CSequence OUString))";
    } else if (isSequenceType(name)) {
        result = "(Ptr (CSequence ()))";
        //result = "(Ptr (CSequence " + toHsCppType(name.copy(2)) + "))";
    } else {
        result = "(Ptr " + Module(name).getNameCapitalized() + ")";
    }
    return result;
}

OUString hsTypeCxxPrefix (OUString const & type)
{
    if (type == "boolean") return OUString("b");
    if (type == "char") return OUString("c");
    if (type == "short") return OUString("n");
    if (type == "long") return OUString("n");
    if (type == "hyper") return OUString("n");
    if (type == "float") return OUString("f");
    if (type == "double") return OUString("f");
    if (type == "string") return OUString("s");
    return OUString();
}

OUString toFunctionPrefix (OUString const & name)
{
    return name.replace('.','_');
}

std::vector< OUString > extractModulesFromName (OUString const & name)
{
    sal_Int32 i = 0;
    std::vector< OUString > modules;
    do {
        OUString id(name.getToken(0, '.', i));
        if (i != -1)
            modules.push_back(id);
    } while (i != -1);
    return modules;
}

/* vim:set shiftwidth=4 softtabstop=4 expandtab: */
