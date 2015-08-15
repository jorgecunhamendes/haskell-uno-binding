/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */
/*
 * This file is part of the LibreOffice project.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
#ifndef HSUNOIDL_WRITER_HS_HXX
#define HSUNOIDL_WRITER_HS_HXX

#include "rtl/ustring.hxx"
#include <set>

#include "entity.hxx"
#include "file.hxx"
#include "writer/utils.hxx"
#include "writer/writer.hxx"

class HsWriter : public Writer {
    public:
        HsWriter(rtl::OUString const & fileurl, EntityRef const & entity)
            : Writer(fileurl, entity) {};
        HsWriter(rtl::OUString const & fileurl, EntityRef const & entity,
                EntityList const & entities)
            : Writer(fileurl, entity, entities) {};
        // generic writer methods
        void writeOpening (std::set< rtl::OUString > const & deps
                = std::set< rtl::OUString >());
        void writeForeignImport (rtl::OUString & cfname, rtl::OUString & fname,
                std::vector< rtl::OUString > & params, rtl::OUString & rtype);
        //void writeFunctionType (std::vector< OUString > classes,
        //        rtl::OUString & fname, std::vector< Parameter > & params,
        //        rtl::OUString & rtype, bool io = true);
        void writeFunctionType (rtl::OUString & fname,
                std::vector< rtl::OUString > & classes,
                std::vector< Parameter > & params, rtl::OUString & rtype,
                bool io = true);
        void writeFunctionLHS (rtl::OUString & fname,
                std::vector< Parameter > & params);
        // UNO Entities
        // - plain struct type
        void writePlainStructTypeEntity ();
        // - interface type
        void writeInterfaceTypeEntity ();
        // - exception type
        void writeExceptionTypeEntity ();
        // - single-interface-based service type
        void writeSingleInterfaceBasedServiceEntity ();
        // UNO Entity module
        void writeModule ();
        // auxiliary methods
        std::set< rtl::OUString > plainStructTypeEntityDependencies ();
        std::set< rtl::OUString > interfaceTypeEntityDependencies ();
        std::set< rtl::OUString > singleInterfaceBasedServiceEntityDependencies ();
};

#endif /* HSUNOIDL_WRITER_HS_HXX */
/* vim:set shiftwidth=4 softtabstop=4 expandtab: */
