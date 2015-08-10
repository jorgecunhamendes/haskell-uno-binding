/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */
/*
 * This file is part of the LibreOffice project.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
#ifndef HSUNOIDL_WRITER_HXX
#define HSUNOIDL_WRITER_HXX

#include "entity.hxx"

void writePlainStruct (EntityList const & entities, EntityRef const & entity);

void writeInterface (EntityList const & entities, EntityRef const & entity);

void writeException (EntityRef const & entity);

void writeSingleInterfaceBasedService (EntityList const & entities,
        EntityRef const & entity);

void writeInterfaceBasedSingleton (EntityRef const & entity);

void writeServiceBasedSingleton (EntityRef const & entity);

#endif /* HSUNOIDL_WRITER_HXX */
/* vim:set shiftwidth=4 softtabstop=4 expandtab: */
