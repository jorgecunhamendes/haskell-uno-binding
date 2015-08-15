/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */
/*
 * This file is part of the LibreOffice project.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
#include "osl/file.hxx"
#include "rtl/process.h"
#include "rtl/ref.hxx"
#include "rtl/ustring.hxx"
#include "sal/main.h"
#include "sal/types.h"
#include "unoidl/unoidl.hxx"

#include <iostream>
#include <map>
#include <set>

#include "module.hxx"
#include "types.hxx"
#include "writer.hxx"

using ::rtl::OUString;

void badUsage () {
    std::cerr
        << "Usage:" << std::endl << std::endl
        << "  hs_unoidl -Ttype1:type2:...:typeN <registry>..."
        << std::endl << std::endl
        << ("where each <registry> is either a new- or legacy-format .rdb file,"
            " a single .idl")
        << std::endl
        << ("file, or a root directory of an .idl file tree.")
        << std::endl;
    std::exit(EXIT_FAILURE);
}

OUString getArgumentUri (OUString & arg) {
    OUString url;
    osl::FileBase::RC e1 = osl::FileBase::getFileURLFromSystemPath(arg, url);
    if (e1 != osl::FileBase::E_None) {
        std::cerr
            << "Cannot convert \"" << arg << "\" to file URL, error code "
            << +e1 << std::endl;
        std::exit(EXIT_FAILURE);
    }
    OUString cwd;
    oslProcessError e2 = osl_getProcessWorkingDir(&cwd.pData);
    if (e2 != osl_Process_E_None) {
        std::cerr
            << "Cannot obtain working directory, error code " << +e2
            << std::endl;
        std::exit(EXIT_FAILURE);
    }
    OUString abs;
    e1 = osl::FileBase::getAbsoluteFileURL(cwd, url, abs);
    if (e1 != osl::FileBase::E_None) {
        std::cerr
            << "Cannot make \"" << url
            << "\" into an absolute file URL, error code " << +e1 << std::endl;
        std::exit(EXIT_FAILURE);
    }
    return abs;
}

std::set< OUString > implementedInterfaces (
        rtl::Reference< unoidl::Manager > const & manager,
        OUString const & interface)
{
    std::set< OUString > interfaces;
    std::set< OUString > temp;
    std::set< OUString > notFound;
    temp.insert(interface);
    while (temp.size() > 0) {
        OUString iface (*temp.begin());
        temp.erase(temp.begin());
        rtl::Reference< unoidl::Entity > ent (manager->findEntity(iface));
        if (ent.is()) {
            switch (ent->getSort()) {
                case unoidl::Entity::SORT_INTERFACE_TYPE:
                    {
                        rtl::Reference< unoidl::InterfaceTypeEntity > ent2 (
                                static_cast< unoidl::InterfaceTypeEntity * >(
                                    ent.get()));
                        // mandatory direct bases
                        std::vector< unoidl::AnnotatedReference > bases
                            (ent2->getDirectMandatoryBases());
                        for (std::vector< unoidl::AnnotatedReference >::const_iterator
                                it2 (bases.begin()); it2 != bases.end(); ++it2)
                            if (interfaces.count(it2->name) == 0 &&
                                    notFound.count(it2->name) == 0)
                                temp.insert(it2->name);
                        // optional direct bases
                        std::vector< unoidl::AnnotatedReference > optionals
                            (ent2->getDirectOptionalBases());
                        for (std::vector< unoidl::AnnotatedReference >::const_iterator
                                it2 (optionals.begin()); it2 != optionals.end(); ++it2)
                            if (interfaces.count(it2->name) == 0 &&
                                    notFound.count(it2->name) == 0)
                                temp.insert(it2->name);
                    }
                    break;
                default:
                    // TODO check at runtime that this does not happen (debug)
                    break;
            }
            interfaces.insert(iface);
        } else {
            notFound.insert(iface);
            std::cerr << "Warning: could not find interface '" << iface << "'."
                << std::endl;
        }
    }
    return interfaces;
}

inline
void insertDependency (std::set< OUString > & dependencies,
        rtl::Reference< Entity > & entity, OUString const & type)
{
    OUString dependency (type);
    if (isSequenceType(type))
        dependency = type.copy(2);
    if (!isSimpleType(dependency)) {
        dependencies.insert(dependency);
        entity->dependencies.insert(dependency);
    }
}

inline
std::set< OUString > findDependencies (
        rtl::Reference< unoidl::Manager > const & manager,
        rtl::Reference< Entity > & entity)
{
    std::set< OUString > dependencies;
    switch (entity->unoidl->getSort()) {
        case unoidl::Entity::SORT_INTERFACE_TYPE:
            {
                rtl::Reference< unoidl::InterfaceTypeEntity > ent2 (
                        static_cast< unoidl::InterfaceTypeEntity * >(
                            entity->unoidl.get()));
                // dependencies from attribute types
                std::vector< unoidl::InterfaceTypeEntity::Attribute >
                    attributes = ent2->getDirectAttributes();
                for (std::vector< unoidl::InterfaceTypeEntity::Attribute
                        >::const_iterator it2 (attributes.begin()) ;
                        it2 != attributes.end() ; ++it2)
                {
                    insertDependency(dependencies, entity, it2->type);
                }
                // TODO dependencies from attribute getter/setter exceptions
                // dependencies from method types
                std::vector< unoidl::InterfaceTypeEntity::Method >
                    methods = ent2->getDirectMethods();
                for (std::vector< unoidl::InterfaceTypeEntity::Method
                        >::const_iterator it2 (methods.begin()) ;
                        it2 != methods.end() ; ++it2)
                {
                    // return type
                    insertDependency(dependencies, entity, it2->returnType);
                    // parameter types
                    std::vector< unoidl::InterfaceTypeEntity::Method::Parameter
                        > params = it2->parameters;
                    for (std::vector<
                            unoidl::InterfaceTypeEntity::Method::Parameter
                            >::const_iterator it3 (params.begin()) ;
                            it3 != params.end() ; ++it3)
                    {
                        insertDependency(dependencies, entity, it3->type);
                    }
                    // TODO dependencies from method exceptions
                }
            }
            break;
        // TODO
    }
    return dependencies;
}

inline
void updateImplementedInterfaces (
        rtl::Reference< unoidl::Manager > const & manager,
        rtl::Reference< Entity > & entity)
{
    switch (entity->unoidl->getSort()) {
        case unoidl::Entity::SORT_INTERFACE_TYPE:
            entity->interfaces = implementedInterfaces(manager, entity->type);
            break;
        case unoidl::Entity::SORT_SINGLE_INTERFACE_BASED_SERVICE:
            rtl::Reference< unoidl::SingleInterfaceBasedServiceEntity > ent2 (
                    static_cast< unoidl::SingleInterfaceBasedServiceEntity * >(
                        entity->unoidl.get()));
            entity->interfaces = implementedInterfaces(manager, ent2->getBase());
            break;
        // TODO
    }
}

inline
void updateImplementedInterfaces (
        rtl::Reference< unoidl::Manager > const & manager,
        EntityList & entities)
{
        for (EntityList::iterator it (entities.begin()) ;
                it != entities.end() ; ++it)
            updateImplementedInterfaces(manager, it->second);
}

inline
void generateCode (EntityList const & entities) {
    for (EntityList::const_iterator it (entities.begin()) ;
            it != entities.end() ; ++it)
    {
        switch (it->second->unoidl->getSort()) {
            case unoidl::Entity::SORT_PLAIN_STRUCT_TYPE:
                writePlainStruct(entities, it->second);
                break;
            case unoidl::Entity::SORT_EXCEPTION_TYPE:
                writeException(it->second);
                break;
            case unoidl::Entity::SORT_INTERFACE_TYPE:
                // do not generate code for 'com.sun.star.uno.XInterface'
                if (it->second->type != "com.sun.star.uno.XInterface")
                    writeInterface(entities, it->second);
                break;
            case unoidl::Entity::SORT_SINGLE_INTERFACE_BASED_SERVICE:
                writeSingleInterfaceBasedService(entities, it->second);
                break;
            default:
                std::cout << "Warning: entity not yet supported ["
                    << it->second->unoidl->getSort() << "]" << std::endl;
                break;
            // TODO
        }
    }
}

void generateModules (EntityList const & entities) {
    ModuleList modules;
    // populate modules
    for (EntityList::const_iterator it (entities.begin()) ;
            it != entities.end() ; ++it)
    {
        Module m (it->second->type);
        modules[m.getParent().getName()].insert(
                EntityList::value_type(it->second->type, it->second));
    }
    // write modules
    for (ModuleList::const_iterator it (modules.begin()) ;
            it != modules.end() ; ++it)
    {
        EntityRef entity = new Entity;
        entity->type = it->first;
        writeModule(modules, entity);
    }
}

SAL_IMPLEMENT_MAIN() {
    try {
        std::vector< OUString > providers;
        std::set< OUString > types;
        // process command arguments
        sal_uInt32 args = rtl_getAppCommandArgCount();
        if (args == 0)
            badUsage();
        for (sal_uInt32 i = 0 ; i < args ; ++i) {
            OUString arg;
            rtl_getAppCommandArg(i, &arg.pData);
            if (arg.compareTo("-T", 2) == 0) {
                sal_Int32 idx = 2;
                do {
                    types.insert(arg.getToken(0, ':', idx));
                } while (idx >= 0);
            } else {
                providers.push_back(getArgumentUri(arg));
            }
        }
        if (types.empty()) {
            std::cerr << "Error: no types specified." << std::endl;
            badUsage();
        }
        // add providers to the manager
        rtl::Reference< unoidl::Manager > manager (new unoidl::Manager);
        for (std::vector< OUString >::const_iterator it (providers.begin()) ;
                it != providers.end(); ++it)
            try {
                manager->addProvider(*it);
            } catch (unoidl::NoSuchFileException &) {
                std::cerr << "Input <" << *it << "> does not exist"
                    << std::endl;
            }
        // create entities list
        EntityList entities;
        std::set< OUString > notFound;
        for (std::set< OUString >::const_iterator it (types.begin()) ;
                it != types.end() ; ++it)
        {
            rtl::Reference< Entity > entity (new Entity);
            entity->unoidl = manager->findEntity(*it);
            entity->type = *it;
            if (entity->unoidl.is()) {
                entities.insert(std::pair< OUString, rtl::Reference< Entity > >
                        (entity->type, entity));
            } else {
                notFound.insert(entity->type);
                std::cerr << "Warning: could not find type '" << entity->type
                    << "'" << std::endl;
            }
        }
        // add type dependencies
        EntityList processing (entities);
        entities.clear();
        while (processing.size() > 0) {
            std::set< OUString > newTypes;
            for (EntityList::iterator it (processing.begin()) ;
                    it != processing.end() ; ++it)
            {
                std::set< OUString > deps (findDependencies(manager, it->second));
                newTypes.insert(deps.begin(), deps.end());
                entities.insert(*it);
            }
            processing.clear();
            for (std::set< OUString >::const_iterator it (newTypes.begin()) ;
                    it != newTypes.end() ; ++it)
            {
                if (notFound.count(*it) == 0 && entities.count(*it) == 0) {
                    rtl::Reference< Entity > entity (new Entity);
                    entity->unoidl = manager->findEntity(*it);
                    entity->type = *it;
                    if (entity->unoidl.is()) {
                        processing.insert(
                                std::pair< OUString, rtl::Reference< Entity > >
                                (entity->type, entity));
                    } else {
                        notFound.insert(entity->type);
                        std::cerr << "Warning: could not find type '" << entity->type
                            << "'" << std::endl;
                    }
                }
            }
        }
        // update implemented interfaces
        updateImplementedInterfaces(manager, entities);
        // generate code for each type
        generateCode(entities);
        // generate code for each module
        generateModules(entities);
        return EXIT_SUCCESS;
    } catch (unoidl::FileFormatException & e1) {
        std::cerr
            << "Bad input <" << e1.getUri() << ">: " << e1.getDetail()
            << std::endl;
        std::exit(EXIT_FAILURE);
    }
}
/* vim:set shiftwidth=4 softtabstop=4 expandtab: */
