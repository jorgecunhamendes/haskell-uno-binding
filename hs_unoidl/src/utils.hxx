/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */
/*
 * This file is part of the LibreOffice project.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
#ifndef HSUNOIDL_UTILS_HXX
#define HSUNOIDL_UTILS_HXX

#include "rtl/ustring.hxx"

extern const rtl::OUString cxxFileExtension;
extern const rtl::OUString hxxFileExtension;
extern const rtl::OUString hsFileExtension;
extern const rtl::OUString functionPrefix;
extern const rtl::OUString getterPrefix;
extern const rtl::OUString setterPrefix;
extern const rtl::OUString exceptionName;
extern const rtl::OUString headerGuardPrefix;
extern const rtl::OUString headerGuardSuffix;
extern const rtl::OUString headerFileExtension;
extern const rtl::OUString hsModulePrefix;

rtl::OUString capitalize (rtl::OUString const & str);
rtl::OUString decapitalize (rtl::OUString const & str);

#endif /* HSUNOIDL_UTILS_HXX */
/* vim:set shiftwidth=4 softtabstop=4 expandtab: */
