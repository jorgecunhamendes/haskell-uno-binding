/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */
/*
 * This file is part of the LibreOffice project.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
#include "writer.hxx"

#include <string>
#include <vector>
#include <fstream>
#include <ostream>
#include <iostream>

#include "osl/file.hxx"
#include "osl/process.h"
#include "rtl/ref.hxx"

#include "file.hxx"
#include "types.hxx"
#include "utils.hxx"
#include "writer/cxx.hxx"
#include "writer/hxx.hxx"
#include "writer/hs.hxx"
#include "writer/utils.hxx"

using rtl::OUString;

void writePlainStruct (EntityList const & entities, EntityRef const & entity) {
    const OUString filePath ("gen/" + Module(entity->type).asPathCapitalized());

    // cxx
    OUString cxxFilePath = filePath + cxxFileExtension;
    CxxWriter cxx (File::getFileUrlFromPath(cxxFilePath), entity, entities);
    cxx.writeOpening();
    cxx.writePlainStructTypeEntity();

    // hxx
    OUString hxxFilePath = filePath + hxxFileExtension;
    HxxWriter hxx (File::getFileUrlFromPath(hxxFilePath), entity, entities);
    hxx.writeOpening();
    hxx.writePlainStructTypeEntity();
    hxx.writeClosing();

    // hs
    OUString hsFilePath = filePath + hsFileExtension;
    HsWriter hs (File::getFileUrlFromPath(hsFilePath), entity, entities);
    hs.writeOpening(hs.plainStructTypeEntityDependencies());
    hs.writePlainStructTypeEntity();
}

void writeInterface (EntityList const & entities, EntityRef const & entity) {
    const OUString filePath ("gen/" + Module(entity->type).asPathCapitalized());

    // cxx
    OUString cxxFilePath = filePath + cxxFileExtension;
    CxxWriter cxx (File::getFileUrlFromPath(cxxFilePath), entity, entities);
    cxx.writeOpening();
    cxx.writeInterfaceTypeEntity();

    // hxx
    OUString hxxFilePath = filePath + hxxFileExtension;
    HxxWriter hxx (File::getFileUrlFromPath(hxxFilePath), entity, entities);
    hxx.writeOpening();
    hxx.writeInterfaceTypeEntity();
    hxx.writeClosing();

    // hs
    OUString hsFilePath = filePath + hsFileExtension;
    HsWriter hs (File::getFileUrlFromPath(hsFilePath), entity, entities);
    hs.writeOpening(hs.interfaceTypeEntityDependencies());
    hs.writeInterfaceTypeEntity();
}

void writeException (EntityRef const & entity)
{
    const OUString filePath ("gen/" + Module(entity->type).asPathCapitalized());

    // cxx
    OUString cxxFilePath = filePath + cxxFileExtension;
    CxxWriter cxx (File::getFileUrlFromPath(cxxFilePath), entity);
    cxx.writeOpening();

    // hxx
    OUString hxxFilePath = filePath + hxxFileExtension;
    HxxWriter hxx (File::getFileUrlFromPath(hxxFilePath), entity);
    hxx.writeOpening();
    hxx.writeClosing();

    // hs
    OUString hsFilePath = filePath + hsFileExtension;
    HsWriter hs (File::getFileUrlFromPath(hsFilePath), entity);
    hs.writeOpening();
}

void writeSingleInterfaceBasedService (EntityList const & entities,
        EntityRef const & entity)
{
    const OUString filePath ("gen/" + Module(entity->type).asPathCapitalized());

    // cxx
    OUString cxxFilePath = filePath + cxxFileExtension;
    CxxWriter cxx (File::getFileUrlFromPath(cxxFilePath), entity, entities);
    cxx.writeOpening();
    cxx.writeSingleInterfaceBasedServiceEntity();

    // hxx
    OUString hxxFilePath = filePath + hxxFileExtension;
    HxxWriter hxx (File::getFileUrlFromPath(hxxFilePath), entity, entities);
    hxx.writeOpening();
    hxx.writeSingleInterfaceBasedServiceEntity();
    hxx.writeClosing();

    // hs
    OUString hsFilePath = filePath + hsFileExtension;
    HsWriter hs (File::getFileUrlFromPath(hsFilePath), entity);
    hs.writeOpening(hs.singleInterfaceBasedServiceEntityDependencies());
    hs.writeSingleInterfaceBasedServiceEntity();
}

void writeModule (ModuleList const & modules, const EntityRef & entity) {
    const OUString filePath ("gen/" + Module(entity->type).asPathCapitalized());

    ModuleList::const_iterator entitiesIt = modules.find(entity->type);
    assert(entitiesIt != modules.end());
    EntityList entities = entitiesIt->second;

    OUString hsFilePath = filePath + hsFileExtension;
    HsWriter hs (File::getFileUrlFromPath(hsFilePath), entity, entities);
    hs.writeOpening();
    hs.writeModule();
}

/* vim:set shiftwidth=4 softtabstop=4 expandtab: */
