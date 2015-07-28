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

#include "rtl/ustring.hxx"
#include "unoidl/unoidl.hxx"

#include "module.hxx"

struct Entity {
    enum class Sorted { NO, ACTIVE, YES };

    explicit Entity(Entity const & e):
        entity(e.entity), module(e.module), name(e.name), relevant(e.relevant), sorted(Sorted::NO),
        written(false)
    {}

    explicit Entity(
        rtl::Reference<unoidl::Entity> const & theEntity, Module module,
        rtl::OUString const & name, bool theRelevant):
        entity(theEntity), module(&module), name(name), relevant(theRelevant), sorted(Sorted::NO),
        written(false)
    {}

    rtl::Reference<unoidl::Entity> const entity;
    Module module;
    rtl::OUString name;
    std::set<rtl::OUString> dependencies;
    std::set<rtl::OUString> interfaceDependencies;
    bool relevant;
    Sorted sorted;
    bool written;
};

#endif /* HSUNOIDL_ENTITY_HXX */
/* vim:set shiftwidth=4 softtabstop=4 expandtab: */
