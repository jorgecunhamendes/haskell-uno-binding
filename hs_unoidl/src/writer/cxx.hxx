/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */
/*
 * This file is part of the LibreOffice project.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
#ifndef HSUNOIDL_WRITER_CXX_HXX
#define HSUNOIDL_WRITER_CXX_HXX

#include "rtl/ustring.hxx"
#include <vector>

#include "entity.hxx"
#include "file.hxx"
#include "writer/utils.hxx"

class CxxWriter {
    public:
        CxxWriter(rtl::OUString const & fileurl) : out(fileurl) {};
        void writeOpening (Entity const & entity);
        void writePlainStructTypeEntity (Entity const & entity);
        void writeInterfaceTypeEntity (Entity const & entity);
        void writeSingleInterfaceBasedServiceEntity (Entity const & entity);
    private:
        File out;
};

#endif /* HSUNOIDL_WRITER_CXX_HXX */
/* vim:set shiftwidth=4 softtabstop=4 expandtab: */
