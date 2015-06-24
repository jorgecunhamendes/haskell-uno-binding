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

using rtl::OUString;

bool isPrimitiveType (OUString const & name)
{
    return (name == "bool" ||
            name == "char" ||
            name == "byte" ||
            name == "short" ||
            name == "long" ||
            name == "hyper" ||
            name == "float" ||
            name == "double" ||
            name == "string");
}

bool isStringType (OUString const & name)
{
    return (name == "string");
}

OUString toCppType (OUString const & name)
{
    if (name == "bool") return OUString("bool");
    if (name == "char") return OUString("sal_Int8");
    if (name == "short") return OUString("sal_Int16");
    if (name == "long") return OUString("sal_Int32");
    if (name == "hyper") return OUString("sal_Int64");
    if (name == "float") return OUString("float");
    if (name == "double") return OUString("double");
    if (name == "string") return OUString("rtl::OUString");
    return name.replaceAll(".","::");
}

OUString toHsType (OUString const & name)
{
    if (name == "bool") return OUString("Bool");
    if (name == "char") return OUString("Int8");
    if (name == "short") return OUString("Int16");
    if (name == "long") return OUString("Int32");
    if (name == "hyper") return OUString("Int64");
    if (name == "float") return OUString("Float");
    if (name == "double") return OUString("Double");
    if (name == "string") return OUString("Text");
    return name;
}

OUString toHsCppType (OUString const & name)
{
    if (name == "bool") return OUString("Bool");
    if (name == "char") return OUString("Int8");
    if (name == "short") return OUString("Int16");
    if (name == "long") return OUString("Int32");
    if (name == "hyper") return OUString("Int64");
    if (name == "float") return OUString("Float");
    if (name == "double") return OUString("Double");
    if (name == "string") return OUString("OUString");
    return name;
}

OUString hsTypeCxxPrefix (OUString const & type)
{
    if (type == "bool") return OUString("b");
    if (type == "char") return OUString("c");
    if (type == "short") return OUString("n");
    if (type == "long") return OUString("n");
    if (type == "hyper") return OUString("n");
    if (type == "float") return OUString("f");
    if (type == "double") return OUString("f");
    if (type == "string") return OUString("s");
    return type;
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
