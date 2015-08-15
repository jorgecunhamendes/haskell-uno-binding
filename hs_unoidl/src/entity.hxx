/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */
/*
 * This file is part of the LibreOffice project.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
#ifndef HSUNOIDL_ENTITY_HXX
#define HSUNOIDL_ENTITY_HXX

#include <set>
#include <map>

#include "rtl/ustring.hxx"
#include "unoidl/unoidl.hxx"

#include "module.hxx"

struct Entity : public salhelper::SimpleReferenceObject {
    rtl::Reference< unoidl::Entity > unoidl;
    rtl::OUString type;
    std::set< rtl::OUString > interfaces;
    std::set< rtl::OUString > dependencies;

    inline Module getModule () const {
        return Module(type).getParent();
    }

    inline rtl::OUString getName () const {
        return Module(type).getLastName();
    }

    inline bool isStruct () const {
        return unoidl->getSort() == unoidl::Entity::SORT_PLAIN_STRUCT_TYPE;
    }

    inline bool isInterface () const {
        return unoidl->getSort() == unoidl::Entity::SORT_INTERFACE_TYPE;
    }
};

typedef rtl::Reference< Entity > EntityRef;

typedef std::map< rtl::OUString, EntityRef > EntityList;

typedef std::map< rtl::OUString, EntityList > ModuleList;

#endif /* HSUNOIDL_ENTITY_HXX */
/* vim:set shiftwidth=4 softtabstop=4 expandtab: */
