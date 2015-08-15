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
    out << "{-# LANGUAGE OverloadedStrings #-} " << std::endl;
    out << "module " << Module(entity->type).getNameCapitalized()
        << " where" << std::endl;
    out << std::endl;
    out << "import UNO" << std::endl;
    out << std::endl;
    out << "import Control.Applicative ((<$>))" << std::endl;
    out << "import Control.Monad (when)" << std::endl;
    out << "import Data.Int" << std::endl;
    out << "import Data.Text (Text)" << std::endl;
    out << "import Data.Word" << std::endl;
    out << "import Foreign" << std::endl;
    out << "import qualified Foreign.Concurrent as FC" << std::endl;
    //out << "import Foreign hiding (newForeignPtr)" << std::endl;
    //out << "import Foreign.Concurrent (newForeignPtr)" << std::endl;
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
        out << toHsCppType(*it) << " -> ";
    }
    out << "IO ";
    out << toHsCppType(rtype) << std::endl;
}

void HsWriter::writeFunctionType (OUString & fname,
        vector< OUString > & classes, vector< Parameter > & params,
        OUString & rtype, bool io)
{
    out << fname << " :: ";
    if (classes.size() > 0) {
        out << "(";
        for (vector< OUString >::const_iterator it (classes.begin()) ;
                it != classes.end() ; ++it)
        {
            if (it != classes.begin())
                out << ", ";
            out << *it;
        }
        out << ") => ";
    }
    for (vector< Parameter >::const_iterator it (params.begin())
            ; it != params.end() ; ++it)
        out << toHsType(it->type) << " -> ";
    if (io)
        out << "IO ";
    out << toHsType(rtype);
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
    OUString entityName (entity->getName());
    OUString entityNameCapitalized (capitalize(entityName));
    OUString fqn = entity->type;
    rtl::Reference< unoidl::PlainStructTypeEntity > ent (
            static_cast< unoidl::PlainStructTypeEntity * >(entity->unoidl.get()));

    OUString dataName (entityNameCapitalized);
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
            static_cast<unoidl::InterfaceTypeEntity *>(entity->unoidl.get()));
    OUString entityName (entity->getName());
    OUString entityNameCapitalized (capitalize(entityName));
    OUString fqn = entity->type;
    vector< unoidl::InterfaceTypeEntity::Method > members = ent->getDirectMethods();

    for (vector<unoidl::InterfaceTypeEntity::Method>::const_iterator
            m(members.begin()) ; m != members.end() ; ++m)
    {
        OUString hsMethodName (m->name);
        OUString hsForeignMethodName ("c" + entityNameCapitalized + "_" + m->name);
        vector< OUString > classes;
        vector< Parameter > methodParams;
        vector< Parameter > params;
        OUString type (m->returnType);

        unsigned int level = 0;

        methodParams.push_back({ fqn, OUString("rIface") });
        for (vector<unoidl::InterfaceTypeEntity::Method::Parameter>::const_iterator
                p(m->parameters.begin()) ; p != m->parameters.end() ; ++p) {
            OUString paramName (decapitalize(p->name));
            methodParams.push_back({ p->type, paramName });
            params.push_back({ p->type, paramName });
        }

        out << std::endl;
        indent(level);
        writeFunctionType(hsMethodName, classes, methodParams, type);
        out << std::endl;
        indent(level);
        writeFunctionLHS(hsMethodName, methodParams);
        out << " do" << std::endl;
        level += 2;
        // prepare arguments
        std::vector< OUString > arguments;
        for (std::vector<unoidl::InterfaceTypeEntity::Method::Parameter>::const_iterator
                k(m->parameters.begin());
                k != m->parameters.end(); ++k)
        {
            OUString argType (k->type);
            OUString name (decapitalize(k->name));
            if (isStringType(k->type)) {
                OUString s (hsTypeCxxPrefix(k->type) + name);
                arguments.push_back(s);
                indent(level);
                out << s << " <- hs_text_to_oustring " << name << std::endl;
                // FIXME Check if the OUString is destructed.
                // If not, create a function "withOUStringText" that handles that.
            } else if (argType == "any") {
                OUString s ("p" + name);
                arguments.push_back(s);
                indent(level);
                out << "withAny " << name << " $ \\ " << s << " -> do "
                    << std::endl;
                level += 2;
            } else {
                bool argIsInterface = false;
                {
                    EntityList::const_iterator entIt = entities.find(argType);
                    if (entIt != entities.end() && entIt->second->isInterface())
                        argIsInterface = true;
                }
                if (argIsInterface) {
                    OUString s ("p" + name);
                    arguments.push_back(s);
                    indent(level);
                    out << "withReference " << name << " $ \\ " << s
                        << " -> do " << std::endl;
                    level += 2;
                } else if (!isPrimitiveType(k->type) && !isSequenceType(k->type)) {
                    OUString s ("p" + name);
                    arguments.push_back(s);
                    indent(level);
                    out << "withForeignPtr " << name << " $ \\ " << s << " -> do "
                        << std::endl;
                    level += 2;
                } else {
                    arguments.push_back(name);
                }
            }
        }
        // get interface pointer
        indent(level);
        out << "withReference rIface $ \\ pIface -> do" << std::endl;
        level += 2;
        // prepare exception pointer
        indent(level);
        out << "with nullPtr $ \\ exceptionPtr -> do" << std::endl;
        level += 2;
        // run method
        indent(level);
        out << "result <- " << hsForeignMethodName << " pIface exceptionPtr";
        for (std::vector< OUString >::const_iterator
                k(arguments.begin()); k != arguments.end(); ++k)
        {
            out << " " << *k;
        }
        out << std::endl;
        // check for exceptions
        indent(level);
        out << "aException <- peek exceptionPtr" << std::endl;
        indent(level);
        out << "when (aException /= nullPtr) (error \"exceptions not yet implemented\")"
            << std::endl;
        bool isInterface = false;
        {
            EntityList::const_iterator entIt = entities.find(m->returnType);
            if (entIt != entities.end() && entIt->second->isInterface())
                isInterface = true;
        }
        // return
        indent(level);
        if (type == "void") {
            out << "return ()" << std::endl;
        } else if (type == "any") {
            out << "anyFromUno (castPtr result)" << std::endl;
        } else if (isBasicType(m->returnType)) {
            out << "return result" << std::endl;
        } else if (isInterface) {
            out << "mkReference result" << std::endl;
        } else {
            OUString methodResult;
            methodResult = "methodResult";
            if (m->returnType == "string") {
                out << "methodResult <- hs_oustring_to_text result" << std::endl;
                indent(level);
                out << "c_delete_oustring result" << std::endl;
                indent(level);
            } else if (m->returnType == "[]string") { // FIXME
                out << "fpResult <- FC.newForeignPtr result (sequenceRelease result)"
                    << std::endl;
                indent(level);
                out << "methodResult <- fromSequence fpResult" << std::endl;
                indent(level);
            } else if (isSequenceType(m->returnType)) { // FIXME
                methodResult = "result";
            } else {
                methodResult = "result";
            }
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

        params.push_back(fqn);
        params.push_back("hsuno_exception_ptr");
        for (vector<unoidl::InterfaceTypeEntity::Method::Parameter>::const_iterator
                p(m->parameters.begin()) ; p != m->parameters.end() ; ++p)
            params.push_back(p->type);

        out << std::endl;
        writeForeignImport(cMethodName, hsMethodName, params, type);
    }
}

void HsWriter::writeExceptionTypeEntity () {
}

void HsWriter::writeSingleInterfaceBasedServiceEntity () {
    rtl::Reference<unoidl::SingleInterfaceBasedServiceEntity> ent (
            static_cast<unoidl::SingleInterfaceBasedServiceEntity *>(entity->unoidl.get()));
    OUString entityName (entity->getName());
    OUString entityNameCapitalized (capitalize(entityName));
    OUString entityFullName (entity->type);
    // entity module (including its name)
    Module eModule = Module(entity->type);
    // entity fully qualified name
    OUString eFQN (eModule.asNamespace());
    // entity base module
    Module eBaseModule(ent->getBase());
    OUString sEntityBase (ent->getBase());
    OUString sEntityBaseLastNameCapitalized
        (capitalize(eBaseModule.getLastName()));
    // entity base fully qualified name
    OUString eBaseFQN(eBaseModule.asNamespace());

    // create method
    OUString hsImportMethodName ("c" + entityNameCapitalized + "_create");
    {
        OUString hsMethodName (decapitalize(entityName) + "Create");
        vector< OUString > classes;
        vector< Parameter > methodParams;
        methodParams.push_back({ OUString("com.sun.star.uno.XComponentContext"),
                OUString("rContext") });
        OUString methodType (sEntityBase);

        writeFunctionType(hsMethodName, classes, methodParams, methodType);

        out << std::endl;
        writeFunctionLHS(hsMethodName, methodParams);
        out << "withReference rContext $ \\ pContext -> " << std::endl;
        indent(4);
        out << hsImportMethodName << " pContext" << " >>= mkReference"
            << std::endl;
    }

    // foreign import
    {
        OUString cMethodName (functionPrefix + toFunctionPrefix(entityFullName)
                + "_create");
        vector< OUString > importMethodParams;
        importMethodParams.push_back("com.sun.star.uno.XComponentContext");
        OUString methodType (sEntityBase);

        out << std::endl;
        writeForeignImport (cMethodName, hsImportMethodName, importMethodParams,
                methodType);
    }
}

void HsWriter::writeModule () {
    assert(hasEntityList);

    for (EntityList::const_iterator it (entities.begin()) ;
            it != entities.end() ; ++it)
    {
        out << std::endl;
        out << "data " << it->second->getName() << std::endl;
        if (it->second->isInterface()) {
            out << "instance IsUnoType " << it->second->getName() << " where"
                << std::endl;
            indent(4);
            out << "getUnoTypeClass _ = Typelib_TypeClass_INTERFACE"
                << std::endl;
            indent(4);
            out << "getUnoTypeName _ = \"" << it->second->type << "\""
                << std::endl;
        }
    }
}

set< OUString > HsWriter::plainStructTypeEntityDependencies () {
    set< OUString > deps;
    deps.insert(entity->getModule().getNameCapitalized());
    return deps;
}

set< OUString > HsWriter::interfaceTypeEntityDependencies () {
    set< OUString > deps;
    deps.insert(entity->getModule().getNameCapitalized());

    // dependencies from methods
    rtl::Reference<unoidl::InterfaceTypeEntity> ent (
            static_cast<unoidl::InterfaceTypeEntity *>(entity->unoidl.get()));
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
            static_cast<unoidl::SingleInterfaceBasedServiceEntity *>(entity->unoidl.get()));
    deps.insert(Module(ent->getBase()).getParent().getNameCapitalized());
    deps.insert(Module(ent->getBase()).getNameCapitalized());
    deps.insert("Com.Sun.Star.Uno");
    return deps;
}
/* vim:set shiftwidth=4 softtabstop=4 expandtab: */
