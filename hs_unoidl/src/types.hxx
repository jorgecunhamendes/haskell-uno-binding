/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */
/*
 * This file is part of the LibreOffice project.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
#ifndef HSUNOIDL_TYPES_HXX
#define HSUNOIDL_TYPES_HXX

#include <vector>
#include "rtl/ustring.hxx"

bool isHsUnoType (rtl::OUString const & type);
bool isBasicType (rtl::OUString const & type);
bool isSimpleType (rtl::OUString const & type);
bool isPrimitiveType (rtl::OUString const & type);
bool isStringType (rtl::OUString const & type);
bool isSequenceType (rtl::OUString const & type);
rtl::OUString toCppType (rtl::OUString const & name);
rtl::OUString toHsType (rtl::OUString const & name);
rtl::OUString toHsCppType (rtl::OUString const & name);
rtl::OUString hsTypeCxxPrefix (rtl::OUString const & type);
rtl::OUString toFunctionPrefix (rtl::OUString const & name);
std::vector< rtl::OUString > extractModulesFromName (
        rtl::OUString const & name);

#endif /* HSUNOIDL_TYPES_HXX */
/* vim:set shiftwidth=4 softtabstop=4 expandtab: */
