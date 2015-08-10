/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */
/*
 * This file is part of the LibreOffice project.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
#ifndef HSUNOIDL_WRITER_WRITER_HXX
#define HSUNOIDL_WRITER_WRITER_HXX

#include <map>
#include "rtl/ustring.hxx"

#include "entity.hxx"
#include "file.hxx"
#include "writer/utils.hxx"

class Writer {
    public:
        Writer(rtl::OUString const & fileurl, EntityRef const & entity)
            : out(fileurl), entity(entity), hasEntityList(false) {};
        Writer(rtl::OUString const & fileurl, EntityRef const & entity,
                EntityList const & entities)
            : out(fileurl), entity(entity), entities(entities),
            hasEntityList(true) {};
    protected:
        File out;
        const EntityRef entity;
        const EntityList entities;
        bool hasEntityList;

        void indent (int n) { ::indent(out, n); };
};

#endif /* HSUNOIDL_WRITER_WRITER_HXX */
/* vim:set shiftwidth=4 softtabstop=4 expandtab: */
