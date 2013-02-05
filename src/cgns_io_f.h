c * ------------------------------------------------------------------------- *
c * CGNS - CFD General Notation System (http://www.cgns.org)                  *
c * CGNS/MLL - Mid-Level Library header file                                  *
c * Please see cgnsconfig.h file for this local installation configuration    *
c * ------------------------------------------------------------------------- *
c
c * ------------------------------------------------------------------------- *
c
c  This software is provided 'as-is', without any express or implied warranty.
c  In no event will the authors be held liable for any damages arising from
c  the use of this software.
c
c  Permission is granted to anyone to use this software for any purpose,
c  including commercial applications, and to alter it and redistribute it
c  freely, subject to the following restrictions:
c
c  1. The origin of this software must not be misrepresented; you must not
c     claim that you wrote the original software. If you use this software
c     in a product, an acknowledgment in the product documentation would be
c     appreciated but is not required.
c
c  2. Altered source versions must be plainly marked as such, and must not
c     be misrepresented as being the original software.
c
c  3. This notice may not be removed or altered from any source distribution.
c
c * ------------------------------------------------------------------------- *
c
c
c     file open modes
c
      integer CGIO_MODE_READ, CGIO_MODE_WRITE, CGIO_MODE_MODIFY
      parameter (CGIO_MODE_READ   = 0)
      parameter (CGIO_MODE_WRITE  = 1)
      parameter (CGIO_MODE_MODIFY = 2)
c
c     database file types
c
      integer CGIO_FILE_NONE, CGIO_FILE_ADF, CGIO_FILE_HDF5,
     &        CGIO_FILE_ADF2
      parameter (CGIO_FILE_NONE = 0)
      parameter (CGIO_FILE_ADF  = 1)
      parameter (CGIO_FILE_HDF5 = 2)
      parameter (CGIO_FILE_ADF2 = 3)
c
c     dimension limits
c
      integer CGIO_MAX_DATATYPE_LENGTH, CGIO_MAX_DIMENSIONS,
     &        CGIO_MAX_NAME_LENGTH, CGIO_MAX_LABEL_LENGTH,
     &        CGIO_MAX_VERSION_LENGTH, CGIO_MAX_ERROR_LENGTH,
     &        CGIO_MAX_LINK_DEPTH, CGIO_MAX_FILE_LENGTH,
     &        CGIO_MAX_LINK_LENGTH
      parameter (CGIO_MAX_DATATYPE_LENGTH = 2)
      parameter (CGIO_MAX_DIMENSIONS      = 12)
      parameter (CGIO_MAX_NAME_LENGTH     = 32)
      parameter (CGIO_MAX_LABEL_LENGTH    = 32)
      parameter (CGIO_MAX_VERSION_LENGTH  = 32)
      parameter (CGIO_MAX_ERROR_LENGTH    = 80)
      parameter (CGIO_MAX_LINK_DEPTH      = 100)
      parameter (CGIO_MAX_FILE_LENGTH     = 1024)
      parameter (CGIO_MAX_LINK_LENGTH     = 4096)

