/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */
/*
 * This file is part of the LibreOffice project.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
#include "module.hxx"

#include <algorithm>

#include "rtl/ustrbuf.hxx"

using rtl::OUString;
using rtl::OUStringBuffer;

Module::Module (rtl::OUString module) {
    sal_Int32 i = 0;
    do {
        OUString id(module.getToken(0, '.', i));
        names.push_back(id);
    } while (i != -1);
}

Module Module::createSubModule (OUString const & name) {
  Module m (this);
  m.names.push_back(name);
  return m;
}

Module Module::getParent () const {
    Module m (this);
    m.names.pop_back();
    return m;
}

OUString Module::getName () const {
    return joinNamesWith(".");
}

OUString Module::getNameCapitalized () const {
    return joinWith(capitalizedNames(), ".");
}

OUString Module::asHeaderGuard () const {
    return joinNamesWith("_").toAsciiUpperCase();
}

OUString Module::asPath () const {
    return joinNamesWith("/");
}

OUString Module::asPathCapitalized () const {
    return joinWith(capitalizedNames(), "/");
}

OUString Module::asNamespace () const {
    return joinNamesWith("::");
}

OUString Module::joinNamesWith (OUString const & separator) const {
    return joinWith(names, separator);
}

std::vector< rtl::OUString > Module::capitalizedNames () const {
    std::vector< OUString > capitalizedNames;
    capitalizedNames.resize(names.size());
    std::transform(names.begin(), names.end(), capitalizedNames.begin(),
            &capitalize);
    return capitalizedNames;
}

OUString Module::joinWith (std::vector< OUString > strs,
        OUString const & separator)
{
    OUStringBuffer buf;
    for (std::vector< OUString >::const_iterator it(strs.begin()) ;
            it != strs.end() ; ++it)
    {
        if (it != strs.begin())
            buf.append(separator);
        buf.append(*it);
    }
    return buf.makeStringAndClear();
}

OUString Module::capitalize (OUString const & str) {
    sal_Unicode first (str[0]);
    sal_Unicode capital (first >= 97 && first <= 122 ? first -32 : first);
    return str.replaceAt(0, 1, OUString(capital));
}

/* vim:set shiftwidth=4 softtabstop=4 expandtab: */
