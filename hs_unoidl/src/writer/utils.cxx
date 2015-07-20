/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */
/*
 * This file is part of the LibreOffice project.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
#include "writer/utils.hxx"

#include "rtl/ustrbuf.hxx"

#include "../types.hxx"

using rtl::OUString;
using rtl::OUStringBuffer;
using std::vector;

OUString cFunctionDeclaration(OUString name, vector< Parameter > params,
        OUString type)
{
    OUStringBuffer buf;
    buf.append("extern \"C\"\n");
    if (!isSimpleType(type) && !isHsUnoType(type) && !isSequenceType(type)) {
        buf.append("css::uno::Reference< ");
        buf.append(toCppType(type));
        buf.append(" >");
    } else {
        buf.append(toCppType(type));
    }
    if (!isBasicType(type) && !isHsUnoType(type))
        buf.append(" *");
    buf.append(" ");
    buf.append(name);
    buf.append("(");
    for (vector< Parameter >::const_iterator it (params.begin()) ;
            it != params.end() ; ++it)
    {
        if (it != params.begin())
            buf.append(", ");
        if (!isSimpleType(it->type) && !isHsUnoType(it->type) && !isSequenceType(it->type)) {
            buf.append("css::uno::Reference< ");
            buf.append(toCppType(it->type));
            buf.append(" >");
        } else {
            buf.append(toCppType(it->type));
        }
        if (!isBasicType(it->type) && !isHsUnoType(it->type))
            buf.append(" *");
        buf.append(" ");
        buf.append(it->name);
    }
    buf.append(")");
    return buf.makeStringAndClear();
}

void indent (std::ostream & out, int n) {
    while (n-- > 0)
        out << ' ';
}

/* vim:set shiftwidth=4 softtabstop=4 expandtab: */
