/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */
/*
 * This file is part of the LibreOffice project.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
#ifndef HSUNOIDL_WRITER_UTILS_HXX
#define HSUNOIDL_WRITER_UTILS_HXX

#include "rtl/ustring.hxx"
#include <ostream>
#include <vector>

#include "../entity.hxx"

typedef struct _Parameter {
    rtl::OUString type;
    rtl::OUString name;
} Parameter;

rtl::OUString cFunctionDeclaration(EntityList const & entities,
        rtl::OUString name, std::vector< Parameter > params,
        rtl::OUString type);

void indent (std::ostream & out, int n);

#endif /* HSUNOIDL_WRITER_UTILS_HXX */
/* vim:set shiftwidth=4 softtabstop=4 expandtab: */
