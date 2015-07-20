/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */
/*
 * This file is part of the LibreOffice project.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
#include "utils.hxx"

using rtl::OUString;

// constants
const OUString cxxFileExtension (".cpp");
const OUString hxxFileExtension (".hpp");
const OUString hsFileExtension (".hs");
const OUString functionPrefix ("hsuno_");
const OUString getterPrefix ("Get");
const OUString setterPrefix ("Set");
const OUString exceptionName ("e");
const OUString headerGuardPrefix ("HSUNO_");
const OUString headerGuardSuffix ("_H");
const OUString headerFileExtension (".hpp");
const OUString hsModulePrefix ("LibreOffice.");

OUString capitalize (OUString const & str) {
    sal_Unicode first (str[0]);
    sal_Unicode capital (first >= 97 && first <= 122 ? first - 32 : first);
    return str.replaceAt(0, 1, OUString(capital));
}

OUString decapitalize (OUString const & str) {
    sal_Unicode first (str[0]);
    sal_Unicode capital (first >= 65 && first <= 90 ? first + 32 : first);
    return str.replaceAt(0, 1, OUString(capital));
}

/* vim:set shiftwidth=4 softtabstop=4 expandtab: */
