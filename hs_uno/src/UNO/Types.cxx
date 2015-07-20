/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */
/*
 * This file is part of the LibreOffice project.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

#include <com/sun/star/uno/Type.h>
#include "typelib/typedescription.h"

typelib_TypeDescription * css_uno_Type_getDescription (css::uno::TypeClass tc,
        const char * t)
{
    typelib_TypeDescription * td = 0;
    css::uno::Type(tc, t).getDescription(&td);
    return td;
}

/* vim:set shiftwidth=4 softtabstop=4 expandtab: */
