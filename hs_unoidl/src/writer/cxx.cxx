/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */
/*
 * This file is part of the LibreOffice project.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
#include "writer/cxx.hxx"

#include "unoidl/unoidl.hxx"

#include "../types.hxx"
#include "../utils.hxx"

using rtl::OUString;
using std::vector;

void CxxWriter::writeOpening (Entity const & entity) {
    out << "#include \"" << capitalize(entity.name) << headerFileExtension
        << "\"" << std::endl;
    out << "#include \"UNO/Binary.hxx\"" << std::endl;
    out << "#include \"rtl/ref.hxx\"" << std::endl;
}

void CxxWriter::writePlainStructTypeEntity (Entity const & entity)
{
    OUString entityNameCapitalized (capitalize(entity.name));
    OUString fqn = entity.module.createSubModule(entity.name).getName();
    OUString fqnCpp = entity.module.createSubModule(entity.name).asNamespace();
    rtl::Reference< unoidl::PlainStructTypeEntity > ent (
            static_cast< unoidl::PlainStructTypeEntity * >(entity.entity.get()));

    OUString dataName (capitalize(entity.name));

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
        out << cFunctionDeclaration(getterName, getterParams, j->type) << " {"
            << std::endl;
        out << "    return " << (isBasicType(j->type) ? "" : "&")
            << "o" << entity.name << "->" << j->name << ";"
            << std::endl;
        out << "}"
            << std::endl;

        // setter
        OUString setterName (functionPrefix + toFunctionPrefix(fqn)
                + "_set_" + j->name);
        OUString setterType ("void");
        vector< Parameter > setterParams;

        setterParams.push_back({ "hsuno " + fqnCpp + " *", "o" + entity.name }); // FIXME hardcoded type
        setterParams.push_back({ j->type, j->name });

        out << std::endl;
        out << cFunctionDeclaration(setterName, setterParams, setterType) << " {"
            << std::endl;
        out << "    o" << entity.name << "->" << j->name << " = ";
        if (!isBasicType(j->type))
            out << "*";
        out << j->name << ";"
            << std::endl;
        out << "}"
            << std::endl;
    }

    // constructor
    // TODO
}

void CxxWriter::writeInterfaceTypeEntity (Entity const & entity) {
    Module entityModule = entity.module.createSubModule(entity.name);
    OUString fqn = entityModule.getName();
    OUString fqnCpp = entityModule.asNamespace();
    rtl::Reference<unoidl::InterfaceTypeEntity> ent (
            static_cast<unoidl::InterfaceTypeEntity *>(entity.entity.get()));

    vector< unoidl::InterfaceTypeEntity::Method > methods = ent->getDirectMethods();

    for (std::vector<unoidl::InterfaceTypeEntity::Method>::const_iterator
            j(methods.begin()); j != methods.end(); ++j)
    {
        OUString cMethodName (functionPrefix + toFunctionPrefix(fqn) + "_"
                + j->name);

        std::vector< Parameter > params;
        params.push_back({ OUString("hsuno_interface"), OUString("rIface") });
        params.push_back({ OUString("hsuno_exception_ptr"), OUString("exception") });
        for (std::vector<unoidl::InterfaceTypeEntity::Method::Parameter>::const_iterator
                k(j->parameters.begin()) ; k != j->parameters.end() ; ++k)
            params.push_back({ k->type, k->name });

        out << std::endl;
        out << cFunctionDeclaration(cMethodName, params, j->returnType) << " {"
            << std::endl;
        // prepare interface
        indent(out, 4);
        out << "css::uno::Reference< com::sun::star::uno::XInterface > * rIface0 ="
            << std::endl;
        indent(out, 8);
        out << "static_cast< css::uno::Reference< css::uno::XInterface > * >(rIface);"
            << std::endl;
        indent(out, 4);
        out << "css::uno::Reference< " << fqnCpp
            << " > rIface2 = css::uno::Reference< " << fqnCpp
            << " >( *rIface0, css::uno::UNO_QUERY);" << std::endl;
        /* indent(out, 4);
        out << "css::uno::Reference< " << fqnCpp << " > * rIface2 ="
            << std::endl;
        indent(out, 8);
        out << "static_cast< css::uno::Reference< " << fqnCpp
            << " > * >(rIface);" << std::endl; */
        indent(out, 4);
        out << "uno_Interface * iface =" << std::endl;
        indent(out, 8);
        out << "static_cast< uno_Interface * >(g_cpp2uno.mapInterface("
            << "rIface2.get(), cppu::UnoType< " << fqnCpp << " >::get()));"
            << std::endl;
        // result type
        if (j->returnType != "void") {
            indent(out, 4);
            if (isBasicType(j->returnType)) {
                out << toCppType(j->returnType) << " result;";
            } else if (isStringType(j->returnType)) {
                out << "rtl_uString * result = 0;";
            } else if (j->returnType == "any") {
                out << "uno_Any * result = new uno_Any;";
            } else if (isSequenceType(j->returnType)) {
                out << toCppType(j->returnType) << " * result = 0;";
            } else {
                // TODO Check non-primitive types for non-interface kinds
                out << "void * result = 0;";
            }
            out << std::endl;
        }
        // prepare arguments
        // TODO no need for arguments when there are no parameters
        indent(out, 4);
        out << "void * args [" << j->parameters.size() << "];" << std::endl;
        int argIdx = 0;
        for (std::vector<unoidl::InterfaceTypeEntity::Method::Parameter>::const_iterator
                k(j->parameters.begin()) ; k != j->parameters.end() ; ++k)
        {
            indent(out, 4);
            out << "args[" << argIdx << "] = ";
            if (isBasicType(k->type)) {
                out << "&" << k->name;
            } else if (isStringType(k->type)) {
                out << "const_cast<rtl_uString **>(&" << k->name << "->pData)";
            } else {
                // TODO test non-primitive type arguments
                out << k->name;
            }
            out << ";" << std::endl;
            ++argIdx;
        }
        // execute the call
        indent(out, 4);
        out << "makeBinaryUnoCall(iface, \"" << fqn << "::" << j->name
            << "\", ";
        if (j->returnType == "void")
            out << "NULL";
        else if (j->returnType == "any")
            out << "result";
        else
            out << "&result";
        out << ", args, exception);" << std::endl;
        // create result and return
        if (j->returnType != "void") {
            indent(out, 4);
            if (isBasicType(j->returnType)) {
                out << "return result;";
            } else if (isStringType(j->returnType)) {
                out << "return new rtl::OUString(result, SAL_NO_ACQUIRE);";
            } else if (j->returnType == "any" || isSequenceType(j->returnType)) {
                out << "return result;";
            } else {
                // TODO Check non-primitive types for non-interface kinds
                OUString ns = Module(j->returnType).asNamespace();
                out << "void * resultIface = g_uno2cpp.mapInterface(result, cppu::UnoType< "
                    << ns << " >::get());" << std::endl;
                indent(out, 4);
                out << "return new css::uno::Reference< " << ns
                    << " >(static_cast< " << ns << " * >(resultIface));";
            }
            out << std::endl;
        }
        out << "}" << std::endl;
    }
}

void CxxWriter::writeSingleInterfaceBasedServiceEntity (Entity const & entity) {
    Module entityModule = entity.module.createSubModule(entity.name);
    rtl::Reference<unoidl::SingleInterfaceBasedServiceEntity> ent (
            static_cast<unoidl::SingleInterfaceBasedServiceEntity *>(entity.entity.get()));
    Module baseModule(ent->getBase());
    OUString baseFqn (baseModule.asNamespace());

    OUString cMethodName (functionPrefix
            + toFunctionPrefix(entityModule.getName()) + "_create");
    vector< Parameter > params;
    params.push_back({OUString("css::uno::XComponentContext"), OUString("context")});

    out << std::endl;
    out << cFunctionDeclaration(cMethodName, params, baseFqn)
        << " {" << std::endl;
    indent(out, 4);
    out << "return new css::uno::Reference< " << baseFqn << " >("
        << entityModule.asNamespace() << "::create(*context));" << std::endl;
    out << "}" << std::endl;

    // TODO write constructors
}
/* vim:set shiftwidth=4 softtabstop=4 expandtab: */
