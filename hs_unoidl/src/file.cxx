/* -*- Mode: C++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */
/*
 * This file is part of the LibreOffice project.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
#include "file.hxx"

#include <iostream>

#include "osl/file.hxx"
#include "osl/process.h"

using rtl::OUString;

const OUString pathSeparator("/");

File::File (OUString const & url) {
    initialize(url);
}

File::File (OUString const & prefix, OUString const & path,
        OUString const & name)
{
    OUString relPath = prefix + pathSeparator + path + pathSeparator + name;
    OUString abs = getFileUrlFromPath(relPath);

    initialize(abs);
}

File::~File () {
    this->close();
}

OUString File::getFileUrlFromPath (rtl::OUString const & path)
{
    OUString url;
    osl::FileBase::RC e1 = osl::FileBase::getFileURLFromSystemPath(path, url);
    if (e1 != osl::FileBase::E_None) {
        std::cerr
            << "Cannot convert \"" << path << "\" to file URL, error code "
            << +e1 << std::endl;
        std::exit(EXIT_FAILURE);
    }
    OUString cwd;
    oslProcessError e2 = osl_getProcessWorkingDir(&cwd.pData);
    if (e2 != osl_Process_E_None) {
        std::cerr
            << "Cannot obtain working directory, error code " << +e2
            << std::endl;
        std::exit(EXIT_FAILURE);
    }
    OUString abs;
    e1 = osl::FileBase::getAbsoluteFileURL(cwd, url, abs);
    if (e1 != osl::FileBase::E_None) {
        std::cerr
            << "Cannot make \"" << url
            << "\" into an absolute file URL, error code " << +e1 << std::endl;
        std::exit(EXIT_FAILURE);
    }
    return abs;
}

void File::initialize(rtl::OUString const & url) {
    // create path for file
    sal_Int32 end = url.lastIndexOf('/');
    OUString dir (url.copy(0, end));
    osl::Directory::createPath(dir);

    // open file
    OUString absPath;
    osl::FileBase::getSystemPathFromFileURL(url, absPath);
    this->open(absPath.toUtf8().getStr(), std::ofstream::trunc);
}

/* vim:set shiftwidth=4 softtabstop=4 expandtab: */
