/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */
/*
 * This file is part of the LibreOffice project.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
#ifndef HSUNOIDL_FILE_HXX
#define HSUNOIDL_FILE_HXX

#include <fstream>
#include "rtl/ustring.hxx"

class File : public std::ofstream {
    public:
        File (rtl::OUString const & prefix, rtl::OUString const & path,
                rtl::OUString const & name);
        ~File ();
};

#endif /* HSUNOIDL_FILE_HXX */
/* vim:set shiftwidth=4 softtabstop=4 expandtab: */
