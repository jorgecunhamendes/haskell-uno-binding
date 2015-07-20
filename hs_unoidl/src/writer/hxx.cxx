/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */
/*
 * This file is part of the LibreOffice project.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
#include "writer/hxx.hxx"

#include "unoidl/unoidl.hxx"

#include "../types.hxx"
#include "../utils.hxx"

using rtl::OUString;
using std::vector;

extern OUString entityHeaderGuardName (Entity const & entity); // FIXME

void HxxWriter::writeOpening (Entity const & entity) {
    const OUString headerGuardName (entityHeaderGuardName(entity));
    out << "#ifndef " << headerGuardName << std::endl;
    out << "#define " << headerGuardName << std::endl;
    out << std::endl;
    out << "#include \"rtl/ustring.hxx\"" << std::endl;
    out << "#include \"uno/any2.h\"" << std::endl;
    out << "#include \"uno/mapping.hxx\"" << std::endl;
}

void HxxWriter::writeClosing (Entity const & entity) {
    const OUString headerGuardName (entityHeaderGuardName(entity));
    out << std::endl;
    out << "#endif // " << headerGuardName << std::endl;
}

void HxxWriter::writePlainStructTypeEntity (Entity const & entity) {
    Module entityModule = entity.module.createSubModule(entity.name);
    OUString entityNameCapitalized (capitalize(entity.name));
    OUString fqn = entityModule.getName();
    OUString fqnCpp = entityModule.asNamespace();
    rtl::Reference< unoidl::PlainStructTypeEntity > ent (
            static_cast< unoidl::PlainStructTypeEntity * >(entity.entity.get()));

    out << "#include \"" << entityModule.asPath() << ".hpp\"" << std::endl;

    vector< unoidl::PlainStructTypeEntity::Member > members = ent->getDirectMembers();
    vector< Parameter > getterParams;
    getterParams.push_back({ "hsuno " + fqnCpp + " *", "o" + entity.name }); // FIXME hardcoded type

    // getters and setters
    for (vector< unoidl::PlainStructTypeEntity::Member >::const_iterator
            j(members.begin()) ; j != members.end() ; ++j)
    {
        // getter
        OUString getterName (functionPrefix + toFunctionPrefix(fqn)
                + "_get_" + j->name);
        OUString getterType (j->type);

        out << std::endl;
        out << cFunctionDeclaration(getterName, getterParams, j->type) << ";"
            << std::endl;

        // setter
        OUString setterName (functionPrefix + toFunctionPrefix(fqn)
                + "_set_" + j->name);
        OUString setterType ("void");
        vector< Parameter > setterParams;

        setterParams.push_back({ "hsuno " + fqnCpp + " *", "o" + entity.name }); // FIXME hardcoded type
        setterParams.push_back({ j->type, j->name });

        out << std::endl;
        out << cFunctionDeclaration(setterName, setterParams, setterType) << ";"
            << std::endl;
    }

    // constructor
    // TODO
}

void HxxWriter::writeInterfaceTypeEntity (Entity const & entity) {
    Module entityModule = entity.module.createSubModule(entity.name);
    OUString fqn = entity.module.createSubModule(entity.name).getName();
    rtl::Reference<unoidl::InterfaceTypeEntity> ent (
            static_cast<unoidl::InterfaceTypeEntity *>(entity.entity.get()));

    out << "#include \"" << entityModule.asPath() << ".hpp\"" << std::endl;

    vector< unoidl::InterfaceTypeEntity::Method > methods = ent->getDirectMethods();

    for (std::vector<unoidl::InterfaceTypeEntity::Method>::const_iterator
            j(methods.begin()); j != methods.end(); ++j)
    {
        OUString cMethodName (functionPrefix + toFunctionPrefix(fqn) + "_"
                + j->name);

        std::vector< Parameter > params;
        params.push_back({ OUString("hsuno_interface"), OUString("iface") });
        params.push_back({ OUString("hsuno_exception_ptr"), OUString("exception") });
        for (std::vector<unoidl::InterfaceTypeEntity::Method::Parameter>::const_iterator
                k(j->parameters.begin()) ; k != j->parameters.end() ; ++k)
            params.push_back({ k->type, k->name });

        out << std::endl;
        out << cFunctionDeclaration(cMethodName, params, j->returnType) << ";"
            << std::endl;
    }
}

void HxxWriter::writeSingleInterfaceBasedServiceEntity (Entity const & entity) {
    Module entityModule = entity.module.createSubModule(entity.name);
    rtl::Reference<unoidl::SingleInterfaceBasedServiceEntity> ent (
            static_cast<unoidl::SingleInterfaceBasedServiceEntity *>(entity.entity.get()));
    Module baseModule(ent->getBase());
    OUString baseFqn (baseModule.asNamespace());

    out << "#include \"" << entityModule.asPath() << ".hpp\"" << std::endl;

    OUString cMethodName (functionPrefix
            + toFunctionPrefix(entityModule.getName()) + "_create");
    vector< Parameter > params;
    params.push_back({OUString("css::uno::XComponentContext"), OUString("context")});

    out << std::endl;
    out << cFunctionDeclaration(cMethodName, params, baseFqn) << ";"
        << std::endl;

    // TODO write constructors
}
/* vim:set shiftwidth=4 softtabstop=4 expandtab: */
