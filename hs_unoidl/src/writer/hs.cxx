/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */
/*
 * This file is part of the LibreOffice project.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
#include "hs.hxx"

#include "unoidl/unoidl.hxx"

#include "../types.hxx"
#include "../utils.hxx"

using std::vector;
using std::set;
using rtl::OUString;

void HsWriter::writeOpening (set< OUString > const & deps) {
    out << "module " << entity.module.createSubModule(entity.name).getNameCapitalized()
        << " where" << std::endl;
    out << std::endl;
    out << "import UNO" << std::endl;
    out << "import Temp" << std::endl; // FIXME this is temporary code for the document-loader example
    out << std::endl;
    out << "import Control.Applicative ((<$>))" << std::endl;
    out << "import Control.Monad (when)" << std::endl;
    out << "import Data.Int" << std::endl;
    out << "import Data.Text (Text)" << std::endl;
    out << "import Data.Word" << std::endl;
    out << "import Foreign" << std::endl;
    // imports for dependencies
    if (deps.size() > 0) {
        out << std::endl;
        for (set< OUString >::const_iterator it (deps.begin())
                ; it != deps.end(); ++it)
            out << "import " << *it << std::endl;
    }
    out << std::endl;
}

void HsWriter::writeForeignImport (OUString & cfname, OUString & fname,
        vector< OUString > & params, OUString & rtype)
{
    out << "foreign import ccall"
        << " \"" << cfname << "\" " << fname << std::endl;
    out << "    :: ";
    for (vector< OUString >::const_iterator it (params.begin()) ;
            it != params.end() ; ++it)
    {
        if (!isBasicType(*it))
            out << "Ptr ";
        out << toHsCppType(*it) << " -> ";
    }
    out << "IO ";
    if (!isBasicType(rtype))
        out << "(Ptr " << toHsCppType(rtype) << ")" << std::endl; 
    else
        out << toHsCppType(rtype) << std::endl; 
}

void HsWriter::writeFunctionType (OUString & fname,
        vector< Parameter > & params, OUString & rtype, bool io)
{
    out << fname << " :: ";
    for (vector< Parameter >::const_iterator it (params.begin())
            ; it != params.end() ; ++it)
        if (isBasicType(it->type) || isHsUnoType(it->type) || isStringType(it->type)) // FIXME Change this to something like "isPointerType".
            out << toHsType(it->type) << " -> ";
        else
            out << "(Ptr " << toHsType(it->type) << ") -> ";
    if (io)
        out << "IO ";
    if (isBasicType(rtype) || rtype.compareTo("hsuno ", 6) == 0)
        out << toHsType(rtype);
    else
        out << "(Ptr " << toHsType(rtype) << ")";
}

void HsWriter::writeFunctionLHS (OUString & fname, vector< Parameter > & params)
{
    out << fname;
    for (vector< Parameter >::const_iterator it (params.begin())
            ; it != params.end() ; ++it)
        out << " " << it->name;
    out << " =";
}

void HsWriter::writePlainStructTypeEntity ()
{
    OUString entityNameCapitalized (capitalize(entity.name));
    OUString fqn = entity.module.createSubModule(entity.name).getName();
    rtl::Reference< unoidl::PlainStructTypeEntity > ent (
            static_cast< unoidl::PlainStructTypeEntity * >(entity.entity.get()));

    OUString dataName (capitalize(entity.name));
    out << "data " << dataName << std::endl;

    vector< unoidl::PlainStructTypeEntity::Member > members = ent->getDirectMembers();
    vector< OUString > getterParams;
    getterParams.push_back(dataName);

    // getters and setters
    for (vector< unoidl::PlainStructTypeEntity::Member >::const_iterator
            j(members.begin()) ; j != members.end() ; ++j)
    {
        // getter
        OUString cGetterName (functionPrefix + toFunctionPrefix(fqn)
                + "_get_" + j->name);
        OUString hsGetterName ("c" + entityNameCapitalized + "_get_" + j->name);

        OUString getterType (j->type);
        out << std::endl;
        writeForeignImport(cGetterName, hsGetterName, getterParams, getterType);

        // setter
        OUString cSetterName (functionPrefix + toFunctionPrefix(fqn)
                + "_set_" + j->name);
        OUString hsSetterName ("c" + entityNameCapitalized + "_set_" + j->name);
        vector< OUString > setterParams;
        OUString setterType ("void");

        setterParams.push_back(dataName);
        setterParams.push_back(j->type);

        out << std::endl;
        writeForeignImport(cSetterName, hsSetterName, setterParams, setterType);
    }

    // constructor
    // TODO
}

void HsWriter::writeInterfaceTypeEntity () {
    rtl::Reference<unoidl::InterfaceTypeEntity> ent (
            static_cast<unoidl::InterfaceTypeEntity *>(entity.entity.get()));
    OUString entityName (entity.name);
    OUString entityNameCapitalized (capitalize(entity.name));
    OUString fqn = entity.module.createSubModule(entity.name).getName();
    vector< unoidl::InterfaceTypeEntity::Method > members = ent->getDirectMethods();

    // interface class
    out << "class Service a => " << entity.name << " a where";
    for (vector<unoidl::InterfaceTypeEntity::Method>::const_iterator
            m(members.begin()) ; m != members.end() ; ++m)
    {
        OUString hsMethodName (m->name);
        OUString hsForeignMethodName ("c" + entityNameCapitalized + "_" + m->name);
        //vector< OUString > classes;
        vector< Parameter > methodParams;
        vector< Parameter > params;
        OUString type (m->returnType);

        unsigned int level = 4;

        methodParams.push_back({ OUString("hsuno a"), OUString("a") });
        //methodParams.push_back({ "hsuno_exception_ptr", "exceptionPtr" });
        for (vector<unoidl::InterfaceTypeEntity::Method::Parameter>::const_iterator
                p(m->parameters.begin()) ; p != m->parameters.end() ; ++p) {
            OUString paramName (decapitalize(p->name));
            methodParams.push_back({ p->type, paramName });
            params.push_back({ p->type, paramName });
        }

        out << std::endl;
        indent(out, level);
        writeFunctionType(hsMethodName, methodParams, type);
        out << std::endl;
        indent(out, level);
        writeFunctionLHS(hsMethodName, methodParams);
        out << " do" << std::endl;
        level += 2;
        indent(out, level);
        out << "let iface = getInterface a" << std::endl;
        // prepare arguments
        std::vector< OUString > arguments;
        for (std::vector<unoidl::InterfaceTypeEntity::Method::Parameter>::const_iterator
                k(m->parameters.begin());
                k != m->parameters.end(); ++k)
        {
            OUString name (decapitalize(k->name));
            if (isStringType(k->type)) {
                OUString s (hsTypeCxxPrefix(k->type) + name);
                arguments.push_back(s);
                indent(out, level);
                out << s << " <- hs_text_to_oustring " << name << std::endl;
                // FIXME Check if the OUString is destructed.
                // If not, create a function "withOUStringText" that handles that.
            } else {
                arguments.push_back(name);
            }
        }
        // prepare exception pointer
        indent(out, level);
        out << "with nullPtr $ \\ exceptionPtr -> do" << std::endl;
        level += 2;
        // run method
        indent(out, level);
        out << "result <- " << hsForeignMethodName << " iface exceptionPtr";
        for (std::vector< OUString >::const_iterator
                k(arguments.begin()); k != arguments.end(); ++k)
        {
            out << " " << *k;
        }
        out << std::endl;
        // check for exceptions
        indent(out, level);
        out << "aException <- peek exceptionPtr" << std::endl;
        indent(out, level);
        out << "when (aException /= nullPtr) (error \"exceptions not yet implemented\")"
            << std::endl;
        // return
        if (isBasicType(m->returnType)) {
            indent(out, level);
            out << "return result" << std::endl;
        } else {
            OUString methodResult;
            methodResult = "methodResult";
            if (m->returnType == "string") {
                indent(out, level);
                out << "methodResult <- hs_oustring_to_text result" << std::endl;
                indent(out, level);
                out << "c_delete_oustring result" << std::endl;
            } else {
                //indent(out, level);
                //out << "error \"non-primitive types are not yet supported\"" << std::endl;
                methodResult = "result";
            }
            indent(out, level);
            out << "return " << methodResult << std::endl;
        }
    }

    // foreign imports
    for (vector<unoidl::InterfaceTypeEntity::Method>::const_iterator
            m(members.begin()) ; m != members.end() ; ++m)
    {
        OUString cMethodName (functionPrefix + toFunctionPrefix(fqn) + "_"
                + m->name);
        OUString hsMethodName ("c" + entityNameCapitalized + "_" + m->name);
        vector< OUString > params;
        OUString type (m->returnType);

        params.push_back("hsuno_interface");
        params.push_back("hsuno_exception_ptr");
        for (vector<unoidl::InterfaceTypeEntity::Method::Parameter>::const_iterator
                p(m->parameters.begin()) ; p != m->parameters.end() ; ++p)
            params.push_back(p->type);

        out << std::endl;
        writeForeignImport(cMethodName, hsMethodName, params, type);
    }
}

void HsWriter::writeSingleInterfaceBasedServiceEntity () {
    rtl::Reference<unoidl::SingleInterfaceBasedServiceEntity> ent (
            static_cast<unoidl::SingleInterfaceBasedServiceEntity *>(entity.entity.get()));
    OUString entityName (entity.name);
    OUString entityFullName (entity.module.getName() + "." + entityName);
    // entity module (including its name)
    Module eModule = entity.module.createSubModule(entity.name);
    // entity fully qualified name
    OUString eFQN (eModule.asNamespace());
    // entity base module
    Module eBaseModule(ent->getBase());
    // entity base fully qualified name
    OUString eBaseFQN(eBaseModule.asNamespace());

    out << "data " << capitalize(entity.name) << " = " << capitalize(entity.name)
        << " (Ptr UnoInterface)" << std::endl; // FIXME content should not be a UnoInterface
    out << std::endl;
    out << "instance Service " << capitalize(entity.name) << " where"
        << std::endl;
    out << "    getInterface (" << capitalize(entity.name) << " ptr) = ptr"
        << std::endl;
    out << std::endl;
    out << "instance " << capitalize(eBaseModule.getLastName()) << " "
        << capitalize(entity.name) << " where" << std::endl;

    // create method
    OUString hsImportMethodName ("c" + capitalize(entity.name) + "_create");
    {
        OUString hsMethodName (decapitalize(entity.name) + "Create");
        vector< Parameter > methodParams;
        methodParams.push_back({ OUString("hsuno Ptr Context"), OUString("context") });
        OUString methodType ("hsuno " + capitalize(entity.name));

        out << std::endl;
        writeFunctionType(hsMethodName, methodParams, methodType);

        out << std::endl;
        writeFunctionLHS(hsMethodName, methodParams);
        out << " " << capitalize(entity.name) << " <$> " << hsImportMethodName
            << " context" << std::endl;
    }

    // foreign import
    {
        OUString cMethodName (functionPrefix + toFunctionPrefix(entityFullName)
                + "_create");
        vector< OUString > importMethodParams;
        importMethodParams.push_back("hsuno Context");
        OUString methodType ("hsuno_interface");

        out << std::endl;
        writeForeignImport (cMethodName, hsImportMethodName, importMethodParams,
                methodType);
    }
}

set< OUString > HsWriter::plainStructTypeEntityDependencies () {
    set< OUString > deps;
    deps.insert(entity.module.getNameCapitalized());
    return deps;
}

set< OUString > HsWriter::interfaceTypeEntityDependencies () {
    set< OUString > deps;
    deps.insert(entity.module.getNameCapitalized());

    // dependencies from methods
    rtl::Reference<unoidl::InterfaceTypeEntity> ent (
            static_cast<unoidl::InterfaceTypeEntity *>(entity.entity.get()));
    vector< unoidl::InterfaceTypeEntity::Method > members =
        ent->getDirectMethods();
    for (vector<unoidl::InterfaceTypeEntity::Method>::const_iterator
            m(members.begin()) ; m != members.end() ; ++m)
    {
        // return type
        {
            OUString type (m->returnType);
            if (isSequenceType(type))
                type = type.copy(2);
            if (!isPrimitiveType(type) && type != "any") {
                Module dep (type);
                deps.insert(dep.getParent().getNameCapitalized());
                //deps.insert(dep.getNameCapitalized());
            }
        }
        // argument types
        for (std::vector<unoidl::InterfaceTypeEntity::Method::Parameter>::const_iterator
                k(m->parameters.begin());
                k != m->parameters.end(); ++k)
        {
            OUString type (k->type);
            if (isSequenceType(type))
                type = type.copy(2);
            if (!isPrimitiveType(type) && type != "any") {
                Module dep (type);
                deps.insert(dep.getParent().getNameCapitalized());
                //deps.insert(dep.getNameCapitalized());
            }
        }
    }
    return deps;
}

set< OUString > HsWriter::singleInterfaceBasedServiceEntityDependencies () {
    set< OUString > deps;
    rtl::Reference<unoidl::SingleInterfaceBasedServiceEntity> ent (
            static_cast<unoidl::SingleInterfaceBasedServiceEntity *>(entity.entity.get()));
    deps.insert(Module(ent->getBase()).getNameCapitalized());
    return deps;
}
/* vim:set shiftwidth=4 softtabstop=4 expandtab: */
