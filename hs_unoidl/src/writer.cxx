/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */
/*
 * This file is part of the LibreOffice project.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
#include "writer.hxx"

#include <string>
#include <vector>
#include <fstream>
#include <ostream>
#include <iostream>

#include "osl/file.hxx"
#include "osl/process.h"
#include "rtl/ref.hxx"
#include "rtl/ustrbuf.hxx"

#include "file.hxx"
#include "types.hxx"
#include "utils.hxx"
#include "writer/cxx.hxx"
#include "writer/hxx.hxx"
#include "writer/hs.hxx"
#include "writer/utils.hxx"

using rtl::OUString;
using rtl::OUStringBuffer;

// temp
extern OUString openModulesFor(std::vector<OUString> & modules, OUString const & name);

// LOCAL FUNCTIONS
// exception functions
static void writeExceptionAux (std::ostream & cxx, std::ostream & hxx, std::ostream & hs,
        rtl::OUString const & name, rtl::Reference<unoidl::ExceptionTypeEntity> entity);
void writeExceptionCxxGetter(std::ostream & cxx, OUString const & entityName,
        OUString const & memberType, OUString const & memberName);
void writeExceptionHxxGetter(std::ostream & hxx, OUString const & entityName,
        OUString const & memberType, OUString const & memberName);
void writeFunctionDeclaration(std::ostream & out,
        OUString const & module,
        OUString const & functionName,
        OUString const & returnType,
        std::vector< OUString > const & parameters);
void writeExceptionHsGetter(std::ostream & hs, OUString const & entityName,
        OUString const & memberType, OUString const & memberName);
// single interface-based service functions
void writeSingleInterfaceBasedServiceAux (std::ostream & cxx,
        std::ostream & hxx, std::ostream & hs, Entity const & entity);
// interface-based singleton functions
void writeInterfaceBasedSingletonAux (std::ostream & cxx, std::ostream & hxx,
        std::ostream & hs, Entity const & entity);
// service-based singleton functions
void writeServiceBasedSingletonAux (std::ostream & cxx, std::ostream & hxx,
        std::ostream & hs, Entity const & entity);
// auxiliary functions
OUString entityHeaderGuardName (Entity const & entity);
OUString entityToHeaderGuardName (OUString const & entity);
OUString entityToHeaderFileName (OUString const & entity);
OUString entityToFileName (OUString const & entity);
OUString entityToInclude (OUString const & entity);

void writePlainStruct (Entity const & entity) {
    const OUString filePath ("gen/" + entity.module.asPathCapitalized() + "/"
            + entity.name);

    // cxx
    OUString cxxFilePath = filePath + cxxFileExtension;
    CxxWriter cxx (File::getFileUrlFromPath(cxxFilePath));
    cxx.writeOpening(entity);
    cxx.writePlainStructTypeEntity(entity);

    // hxx
    OUString hxxFilePath = filePath + hxxFileExtension;
    HxxWriter hxx (File::getFileUrlFromPath(hxxFilePath));
    hxx.writeOpening(entity);
    hxx.writePlainStructTypeEntity(entity);
    hxx.writeClosing(entity);

    // hs
    OUString hsFilePath = filePath + hsFileExtension;
    HsWriter hs (File::getFileUrlFromPath(hsFilePath), entity);
    hs.writeOpening(hs.plainStructTypeEntityDependencies());
    hs.writePlainStructTypeEntity();
}

void writeInterface (Entity const & entity) {
    const OUString filePath ("gen/" + entity.module.asPathCapitalized() + "/"
            + entity.name);

    // cxx
    OUString cxxFilePath = filePath + cxxFileExtension;
    CxxWriter cxx (File::getFileUrlFromPath(cxxFilePath));
    cxx.writeOpening(entity);
    cxx.writeInterfaceTypeEntity(entity);

    // hxx
    OUString hxxFilePath = filePath + hxxFileExtension;
    HxxWriter hxx (File::getFileUrlFromPath(hxxFilePath));
    hxx.writeOpening(entity);
    hxx.writeInterfaceTypeEntity(entity);
    hxx.writeClosing(entity);

    // hs
    OUString hsFilePath = filePath + hsFileExtension;
    HsWriter hs (File::getFileUrlFromPath(hsFilePath), entity);
    hs.writeOpening(hs.interfaceTypeEntityDependencies());
    hs.writeInterfaceTypeEntity();
}

void writeException (Entity const & entity)
{
    OUString name (entity.name);
    OUString entityFullName (entity.module.getName() + "." + name);
    const OUString cxxFileName(name + cxxFileExtension);
    const OUString hxxFileName(name + hxxFileExtension);
    const OUString hsFileName(name + hsFileExtension);

    File cxx ("gen/", entity.module.asPathCapitalized(), capitalize(cxxFileName));
    File hxx ("gen/", entity.module.asPathCapitalized(), capitalize(hxxFileName));
    File hs ("gen/", entity.module.asPathCapitalized(), capitalize(hsFileName));

    rtl::Reference<unoidl::ExceptionTypeEntity> ent2(
            static_cast<unoidl::ExceptionTypeEntity *>(entity.entity.get()));
    writeExceptionAux(cxx, hxx, hs, entityFullName, ent2);
}

static void writeExceptionAux (std::ostream & cxx, std::ostream & hxx, std::ostream & hs,
        rtl::OUString const & name, rtl::Reference<unoidl::ExceptionTypeEntity> entity)
{
    const OUString headerFileName = entityToHeaderFileName(name);
    const OUString headerGuardName = entityToHeaderGuardName(name);

    // cxx
    cxx << "#include \"" << headerFileName << "\"" << std::endl;
    cxx << std::endl;

    // hxx
    hxx << "#ifndef " << headerGuardPrefix << headerGuardName << headerGuardSuffix << std::endl;
    hxx << "#define " << headerGuardPrefix << headerGuardName << headerGuardSuffix << std::endl;
    hxx << std::endl;
    hxx << "#include \"" << entityToInclude(name) << "\"" << std::endl;
    hxx << std::endl;

    for (std::vector<unoidl::ExceptionTypeEntity::Member>::const_iterator
            j(entity->getDirectMembers().begin());
            j != entity->getDirectMembers().end(); ++j)
    {
        if (isPrimitiveType(j->type)) {
            // cxx
            writeExceptionCxxGetter(cxx, name, j->type, j->name);
            // hxx
            writeExceptionHxxGetter(hxx, name, j->type, j->name);
            // hs
            writeExceptionHsGetter(hs, name, j->type, j->name);
        } else {
            // TODO support for non-primitive types
        }
    }

    // hxx
    hxx << std::endl;
    hxx << "#endif // " << headerGuardPrefix << headerGuardName << headerGuardSuffix << std::endl;
}

void writeExceptionCxxGetter(std::ostream & cxx, OUString const & entityName,
        OUString const & memberType, OUString const & memberName)
{
    OUString cxxRetType (toCppType(memberType));
    if (isStringType(memberType))
        cxxRetType += " *";
    cxx << "extern \"C\"" << std::endl;
    cxx << cxxRetType << ' '
        << functionPrefix << toFunctionPrefix(entityName) << getterPrefix << memberName
        << "(" << toCppType(entityName) << " * " << exceptionName << ") "
        << "{" << std::endl;
    cxx << "    return ";
    if (isStringType(memberType))
        cxx << "&" ;
    cxx << exceptionName << "->" << memberName << ";"
        << std::endl;
    cxx << "}" << std::endl;
}

void writeExceptionHxxGetter(std::ostream & hxx, OUString const & entityName,
        OUString const & memberType, OUString const & memberName)
{
    OUString cxxRetType (toCppType(memberType));
    if (isStringType(memberType))
        cxxRetType += " *";
    hxx << "extern \"C\"" << std::endl;
    hxx << cxxRetType << ' '
        << functionPrefix << toFunctionPrefix(entityName) << getterPrefix << memberName
        << "(" << toCppType(entityName) << " * " << exceptionName << ");" << std::endl;
}

void writeFunctionDeclaration(std::ostream & out,
        OUString const & module,
        OUString const & functionName,
        OUString const & returnType,
        std::vector< OUString > const & parameters)
{
    out << "extern \"C\"" << std::endl;
    out << returnType << ' '
        << functionPrefix << toFunctionPrefix(module) << "_" << functionName;
    out << "(";
    for (std::vector< OUString >::const_iterator it (parameters.begin()) ;
            it != parameters.end() ; ++it) {
        if (it != parameters.begin())
            out << ", ";
        out << * it;
    }
    out << ")";
}

void writeExceptionHsGetter(std::ostream & hxx, OUString const & entityName,
        OUString const & memberType, OUString const & memberName)
{
    hxx << "TODO" << std::endl;
}

void writeSingleInterfaceBasedService (Entity const & entity) {
    const OUString filePath ("gen/" + entity.module.asPathCapitalized() + "/"
            + entity.name);

    // cxx
    OUString cxxFilePath = filePath + cxxFileExtension;
    CxxWriter cxx (File::getFileUrlFromPath(cxxFilePath));
    cxx.writeOpening(entity);
    cxx.writeSingleInterfaceBasedServiceEntity(entity);

    // hxx
    OUString hxxFilePath = filePath + hxxFileExtension;
    HxxWriter hxx (File::getFileUrlFromPath(hxxFilePath));
    hxx.writeOpening(entity);
    hxx.writeSingleInterfaceBasedServiceEntity(entity);
    hxx.writeClosing(entity);

    // hs
    OUString hsFilePath = filePath + hsFileExtension;
    HsWriter hs (File::getFileUrlFromPath(hsFilePath), entity);
    hs.writeOpening(hs.singleInterfaceBasedServiceEntityDependencies());
    hs.writeSingleInterfaceBasedServiceEntity();
}

void writeInterfaceBasedSingleton (Entity const & entity)
{
    OUString name (entity.name);
    OUString entityFullName (entity.module.getName() + "." + name);
    const OUString cxxFileName(name + cxxFileExtension);
    const OUString hxxFileName(name + hxxFileExtension);
    const OUString hsFileName(name + hsFileExtension);

    File cxx ("gen/", entity.module.asPathCapitalized(), capitalize(cxxFileName));
    File hxx ("gen/", entity.module.asPathCapitalized(), capitalize(hxxFileName));
    File hs ("gen/", entity.module.asPathCapitalized(), capitalize(hsFileName));

    writeInterfaceBasedSingletonAux(cxx, hxx, hs, entity);
}

void writeInterfaceBasedSingletonAux (std::ostream & cxx, std::ostream & hxx,
        std::ostream & hs, Entity const & entity)
{
    rtl::Reference<unoidl::InterfaceBasedSingletonEntity> ent (
            static_cast<unoidl::InterfaceBasedSingletonEntity *>(entity.entity.get()));
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

    // cxx
    cxx << "#include \"" << capitalize(entityName) << headerFileExtension
        << "\"" << std::endl;
    cxx << "#include \"" << "UNO/Binary.hxx" << "\"" << std::endl;
    cxx << "#include \"" << entity.module.asPath() << "/" << entityName
        << ".hpp\"" << std::endl;
    cxx << std::endl;
    writeFunctionDeclaration(cxx, eModule.getName(), "new", "void *", std::vector< OUString >());
    cxx << " {" << std::endl;
    indent(cxx, 4);
    cxx << "css::uno::Reference< " << eBaseFQN << " > * r = new "
        << "css::uno::Reference< " << eBaseFQN << " >("
        << eFQN << "::get(g_context));" << std::endl;
    indent(cxx, 4);
    cxx << "return static_cast< void * >(r);" << std::endl;
    cxx << "}" << std::endl;

    // hxx
    const OUString headerGuardName (entityHeaderGuardName(entity));
    hxx << "#ifndef " << headerGuardName << std::endl;
    hxx << "#define " << headerGuardName << std::endl;
    hxx << std::endl;
    writeFunctionDeclaration(hxx, eModule.getName(), "new", "void *", std::vector< OUString >());
    hxx << ";" << std::endl;
    hxx << std::endl;
    hxx << "#endif // " << headerGuardName << std::endl;

    // hs
    hs << "module " << eModule.getNameCapitalized() << " where" << std::endl;
    hs << std::endl;
    hs << "import " << eBaseModule.getNameCapitalized() << std::endl;
    hs << "import UNO" << std::endl;
    hs << std::endl;
    hs << "import Control.Applicative ((<$>))" << std::endl;
    hs << "import Foreign.Ptr" << std::endl;
    hs << std::endl;
    hs << "data " << capitalize(entity.name) << " = " << capitalize(entity.name)
       << " (Ptr UnoInterface)" << std::endl; // FIXME content should not be a UnoInterface
    hs << std::endl;
    hs << "instance Service " << capitalize(entity.name) << " where"
       << std::endl;
    hs << "    getInterface (" << capitalize(entity.name) << " ptr) = ptr"
       << std::endl;
    hs << std::endl;
    hs << "instance " << capitalize(eBaseModule.getLastName()) << " "
       << capitalize(entity.name) << " where" << std::endl;
    hs << std::endl;
    hs << entity.name << "New :: IO " << capitalize(entity.name) << std::endl;
    hs << entity.name << "New = " << capitalize(entity.name) << " <$> c"
       << capitalize(entity.name) << "_new" << std::endl;
    hs << std::endl;
    hs << "foreign import ccall \"" << functionPrefix
       << toFunctionPrefix(entityFullName) << "_new" << "\" c"
       << capitalize(entity.name) << "_new" << std::endl;
    hs << "    :: IO (Ptr UnoInterface)" << std::endl; // FIXME content should not be a UnoInterface
    hs << std::endl;
}

void writeServiceBasedSingleton (Entity const & entity)
{
    OUString name (entity.name);
    OUString entityFullName (entity.module.getName() + "." + name);
    const OUString cxxFileName(name + cxxFileExtension);
    const OUString hxxFileName(name + hxxFileExtension);
    const OUString hsFileName(name + hsFileExtension);

    File cxx ("gen/", entity.module.asPathCapitalized(), capitalize(cxxFileName));
    File hxx ("gen/", entity.module.asPathCapitalized(), capitalize(hxxFileName));
    File hs ("gen/", entity.module.asPathCapitalized(), capitalize(hsFileName));

    writeServiceBasedSingletonAux(cxx, hxx, hs, entity);
}

void writeServiceBasedSingletonAux (std::ostream & cxx, std::ostream & hxx,
        std::ostream & hs, Entity const & entity)
{
    rtl::Reference<unoidl::ServiceBasedSingletonEntity> ent (
            static_cast<unoidl::ServiceBasedSingletonEntity *>(entity.entity.get()));
    OUString entityName (entity.name);
    OUString entityFullName (entity.module.getName() + "." + entityName);

    std::cout << "TODO" << std::endl;
    // TODO
}

OUString entityHeaderGuardName (Entity const & entity)
{
    OUStringBuffer buf;
    buf.append(headerGuardPrefix);
    buf.append(entity.module.createSubModule(entity.name).asHeaderGuard());
    buf.append(headerGuardSuffix);
    return buf.makeStringAndClear();
}

OUString entityToHeaderGuardName (OUString const & entity)
{
    return entity.replace('.', '_').toAsciiUpperCase();
}

OUString entityToHeaderFileName (OUString const & entity)
{
    // FIXME only return the name and the file, dropping its parent directory
    return entity.replace('.', '/') + headerFileExtension;
}

OUString entityToFileName (OUString const & entity)
{
    return entity.replace('.', '/');
}

OUString entityToInclude (OUString const & entity)
{
    return entity.replace('.', '/') + headerFileExtension;
}

/* vim:set shiftwidth=4 softtabstop=4 expandtab: */
