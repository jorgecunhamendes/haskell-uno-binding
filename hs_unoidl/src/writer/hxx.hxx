/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */
/*
 * This file is part of the LibreOffice project.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
#ifndef HSUNOIDL_WRITER_HXX_HXX
#define HSUNOIDL_WRITER_HXX_HXX

#include "rtl/ustring.hxx"
#include <vector>

#include "entity.hxx"
#include "file.hxx"
#include "writer/utils.hxx"
#include "writer/writer.hxx"

class HxxWriter : public Writer {
    public:
        HxxWriter(rtl::OUString const & fileurl, EntityRef const & entity)
            : Writer(fileurl, entity) {};
        HxxWriter(rtl::OUString const & fileurl, EntityRef const & entity,
                EntityList const & entities)
            : Writer(fileurl, entity, entities) {};
        void writeOpening ();
        void writeClosing ();
        void writePlainStructTypeEntity ();
        void writeInterfaceTypeEntity ();
        void writeSingleInterfaceBasedServiceEntity ();
};

#endif /* HSUNOIDL_WRITER_HXX_HXX */
/* vim:set shiftwidth=4 softtabstop=4 expandtab: */
