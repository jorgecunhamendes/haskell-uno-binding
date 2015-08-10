/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */
/*
 * This file is part of the LibreOffice project.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
#include "entity.hxx"

#include <iostream>

using ::rtl::OUString;

Entity2::Entity2 (Module module, rtl::OUString name)
    : module(module), name(name), has_interfaces(false)
{}

std::set< OUString > Entity2::getInterfaces (
    std::map< OUString, Entity2 > entities)
{
    if (has_interfaces)
        return this->interfaces;
    // set up direct interfaces
    std::set< OUString > temp;
    switch (unoEntity->getSort()) {
        case unoidl::Entity::SORT_SINGLE_INTERFACE_BASED_SERVICE:
            {
                rtl::Reference<unoidl::SingleInterfaceBasedServiceEntity> ent =
                    static_cast<unoidl::SingleInterfaceBasedServiceEntity *>(this->unoEntity.get());
                temp.insert(ent->getBase());
            }
            break;
        default:
            break;
    }
    // find indirect interfaces
    std::set< OUString > notFound;
    while (temp.size() > 0) {
        rtl::OUString iface (*temp.begin());
        temp.erase(temp.begin());
        std::map< OUString, Entity2 >::iterator ifaceIt (entities.find(iface));
        if (ifaceIt == entities.end()) {
            notFound.insert(iface);
            std::cerr << "Entity not found: " << iface << std::endl;
        } else {
            Entity2 ent (ifaceIt->second);
            int sort = ent.unoEntity->getSort();
            switch (sort) {
                case unoidl::Entity::SORT_INTERFACE_TYPE:
                    {
                        // insert interface to the processed list
                        this->interfaces.insert(ent.module.createSubModule(ent.name).getName());
                        std::cout << "added: " << ent.module.createSubModule(ent.name).getName() << std::endl;
                        rtl::Reference<unoidl::InterfaceTypeEntity> ent2 =
                            static_cast<unoidl::InterfaceTypeEntity *>(ent.unoEntity.get());
                        // mandatory bases
                        std::vector< unoidl::AnnotatedReference > bases
                            (ent2->getDirectMandatoryBases());
                        for (std::vector< unoidl::AnnotatedReference >::const_iterator
                                it2 (bases.begin()); it2 != bases.end(); ++it2)
                        {
                            if (this->interfaces.count(it2->name) == 0 &&
                                    notFound.count(it2->name) == 0)
                            {
                                temp.insert(it2->name);
                                std::cout << "INDIRECT: " << it2->name << std::endl;
                            }
                        }
                        // optional bases
                        std::vector< unoidl::AnnotatedReference > optionals
                            (ent2->getDirectOptionalBases());
                        for (std::vector< unoidl::AnnotatedReference >::const_iterator
                                it2 (optionals.begin()); it2 != optionals.end(); ++it2)
                        {
                            if (this->interfaces.count(it2->name) == 0 &&
                                    notFound.count(it2->name) == 0)
                            {
                                temp.insert(it2->name);
                                std::cout << "INDIRECT OPT: " << it2->name << std::endl;
                            }
                        }
                    }
                    break;
                default:
                    std::cout << "Sort: " << sort << std::endl;
                    break;
            }
        }
    }
    has_interfaces = true;
    return this->interfaces;
}
/* vim:set shiftwidth=4 softtabstop=4 expandtab: */
