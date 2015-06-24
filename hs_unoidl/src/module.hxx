/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */
/*
 * This file is part of the LibreOffice project.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
#ifndef HSUNOIDL_MODULE_HXX
#define HSUNOIDL_MODULE_HXX

#include <vector>
#include "rtl/ustring.hxx"

class Module {
    public:
        Module () : names() {};
        Module (Module * m) : names(m->names) {};
        Module (rtl::OUString module);
        Module createSubModule (rtl::OUString const & name);
        rtl::OUString getName () const;
        rtl::OUString getNameCapitalized () const;
        rtl::OUString asHeaderGuard () const;
        rtl::OUString asPath () const;
        rtl::OUString asPathCapitalized () const;
        rtl::OUString asNamespace () const;
    private:
        std::vector< rtl::OUString > names;
        rtl::OUString joinNamesWith (rtl::OUString const & separator) const;
        std::vector< rtl::OUString > capitalizedNames () const;
        static rtl::OUString joinWith (std::vector< rtl::OUString > strs,
                rtl::OUString const & separator);
        static rtl::OUString capitalize (rtl::OUString const & str);
};

#endif /* HSUNOIDL_MODULE_HXX */
/* vim:set shiftwidth=4 softtabstop=4 expandtab: */
