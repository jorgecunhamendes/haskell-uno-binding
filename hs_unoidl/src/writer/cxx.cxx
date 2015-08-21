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

void CxxWriter::writeOpening () {
    out << "#include \"" << capitalize(entity->getName()) << headerFileExtension
        << "\"" << std::endl;
    out << "#include \"UNO/Binary.hxx\"" << std::endl;
    out << "#include \"rtl/ref.hxx\"" << std::endl;
}

void CxxWriter::writePlainStructTypeEntity ()
{
    OUString name (entity->getName());
    OUString entityNameCapitalized (capitalize(name));
    OUString fqn = entity->type;
    OUString fqnCpp = Module(fqn).asNamespace();
    rtl::Reference< unoidl::PlainStructTypeEntity > ent (
            static_cast< unoidl::PlainStructTypeEntity * >(entity->unoidl.get()));

    OUString dataName (capitalize(name));

    vector< unoidl::PlainStructTypeEntity::Member > members = ent->getDirectMembers();
    vector< Parameter > getterParams;
    getterParams.push_back({ "hsuno " + fqnCpp + " *", "o" + name }); // FIXME hardcoded type

    // getters and setters
    for (vector< unoidl::PlainStructTypeEntity::Member >::const_iterator
            j(members.begin()) ; j != members.end() ; ++j)
    {
        bool isInterface = false;
        EntityList::const_iterator entIt = entities.find(j->type);
        if (entIt != entities.end() && entIt->second->isInterface())
            isInterface = true;
        // getter
        OUString getterName (functionPrefix + toFunctionPrefix(fqn)
                + "_get_" + j->name);
        OUString getterType (j->type);

        out << std::endl;
        out << cFunctionDeclaration(entities, getterName, getterParams,
                j->type) << " {" << std::endl;
        if (isInterface) {
            indent(4);
            out << toCppType(j->type) << " * p" << j->name << " = o" << name
                << "->" << j->name << ".get();" << std::endl;
            indent(4);
            out << "reinterpret_cast< css::uno::XInterface * >(p" << j->name
                << ")->acquire();" << std::endl;
            indent(4);
            out << "return p" << j->name << ";" <<std::endl;
        } else if (j->type == "type") {
            indent(4);
            out << "typelib_TypeDescription * td = 0;" << std::endl;
            indent(4);
            out << "o" << name << "->" << j->name << ".getDescription(&td);"
                << std::endl;
            indent(4);
            out << "return td;" << std::endl;
        } else {
            out << "    return ";
            if (!isBasicType(j->type))
                out << "&";
            out << "o" << name << "->" << j->name << ";" << std::endl;
        }
        out << "}" << std::endl;

        // setter
        OUString setterName (functionPrefix + toFunctionPrefix(fqn)
                + "_set_" + j->name);
        OUString setterType ("void");
        vector< Parameter > setterParams;

        setterParams.push_back({ "hsuno " + fqnCpp + " *", "o" + name }); // FIXME hardcoded type
        setterParams.push_back({ j->type, j->name });

        out << std::endl;
        out << cFunctionDeclaration(entities, setterName, setterParams,
                setterType) << " {" << std::endl;
        if (isInterface) {
            out << "    css::uno::Reference< " << toCppType(j->type) << " > r"
                << j->name << "(" << j->name << ");" << std::endl;
        }
        out << "    o" << name << "->" << j->name << " = ";
        if (j->type == "type") {
            out << "css::uno::Type(reinterpret_cast< typelib_TypeDescriptionReference * >("
                << j->name << "));" << std::endl;
        } else {
            if (!isBasicType(j->type) && !isInterface)
                out << "*";
            if (j->type == "any") {
                out << "static_cast< css::uno::Any * >(" << j->name << ");"
                    << std::endl;
            } else {
                if (isInterface)
                    out << "r";
                out << j->name << ";" << std::endl;
            }
        }
        out << "}"
            << std::endl;
    }

    // constructor
    // TODO
}

void CxxWriter::writeInterfaceTypeEntity () {
    OUString name (entity->getName());
    Module entityModule (entity->type);
    OUString fqn = entityModule.getName();
    OUString fqnCpp = entityModule.asNamespace();
    rtl::Reference<unoidl::InterfaceTypeEntity> ent (
            static_cast<unoidl::InterfaceTypeEntity *>(entity->unoidl.get()));

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

        bool isInterface = false;
        {
            EntityList::const_iterator entIt = entities.find(j->returnType);
            if (entIt != entities.end() && entIt->second->isInterface())
                isInterface = true;
        }
        out << std::endl;
        out << cFunctionDeclaration(entities, cMethodName, params,
                isInterface ? "hsuno_interface" : j->returnType) << " {" << std::endl;
        // result type
        if (j->returnType != "void") {
            indent(4);
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
        indent(4);
        out << "void * args [" << j->parameters.size() << "];" << std::endl;
        int argIdx = 0;
        for (std::vector<unoidl::InterfaceTypeEntity::Method::Parameter>::const_iterator
                k(j->parameters.begin()) ; k != j->parameters.end() ; ++k)
        {
            indent(4);
            out << "args[" << argIdx << "] = ";
            if (isBasicType(k->type)) {
                out << "&" << k->name;
            } else if (isStringType(k->type)) {
                out << "const_cast<rtl_uString **>(&" << k->name << "->pData)";
            } else {
                EntityList::const_iterator entIt = entities.find(k->type);
                if (entIt != entities.end() && entIt->second->isInterface())
                    out << "&";
                out << k->name;
            }
            out << ";" << std::endl;
            ++argIdx;
        }
        // execute the call
        indent(4);
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
            indent(4);
            if (isBasicType(j->returnType)) {
                out << "return result;";
            } else if (isStringType(j->returnType)) {
                out << "return new rtl::OUString(result, SAL_NO_ACQUIRE);";
            } else if (j->returnType == "any" || isSequenceType(j->returnType)) {
                out << "return result;";
            } else {
                if (isInterface) {
                    out << "return (uno_Interface *)result;";
                } else {
                    OUString ns = Module(j->returnType).asNamespace();
                    out << "return (" << ns << " *)result;";
                }
            }
            out << std::endl;
        }
        out << "}" << std::endl;
    }
}

void CxxWriter::writeSingleInterfaceBasedServiceEntity () {
    OUString cMethodName (functionPrefix + toFunctionPrefix(entity->type)
            + "_create");
    vector< Parameter > params;
    params.push_back({OUString("hsuno_interface"), OUString("pContext")});

    out << std::endl;
    out << cFunctionDeclaration(entities, cMethodName, params, "hsuno_interface")
        << " {" << std::endl;
    out << "return hsunoCreateInstanceWithContextFromAscii(\""
        << entity->type
        << "\", pContext);" << std::endl;

    out << "}" << std::endl;

    // TODO write constructors
}
/* vim:set shiftwidth=4 softtabstop=4 expandtab: */
