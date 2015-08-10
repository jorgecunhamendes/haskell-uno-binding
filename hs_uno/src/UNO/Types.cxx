/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */
/*
 * This file is part of the LibreOffice project.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

#include "rtl/ustring.hxx"
#include "typelib/typedescription.h"

extern "C"
sal_Int32 typelib_typedescription_getSize (typelib_TypeDescription const * td) {
    return td->nSize;
}

extern "C"
typelib_TypeDescription * hsuno_getTypeDescriptionByName (rtl::OUString const * psName) {
    typelib_TypeDescription * td = 0;
    typelib_typedescription_getByName(&td, psName->pData);
    return td;
}

/* vim:set shiftwidth=4 softtabstop=4 expandtab: */
