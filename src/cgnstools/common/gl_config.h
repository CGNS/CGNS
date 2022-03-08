#ifndef CMAKE_BUILD
#   include "CGNSconfig.h"
#endif
#if defined(HAVE_WINDOWS_H) && defined(_WIN32)
#   include <windows.h>
#endif
#ifdef HAVE_GL_GL_H
#   include <GL/gl.h>
#elif defined(HAVE_OPENGL_GL_H)
#   include <OpenGL/gl.h>
#else
#   error no gl.h
#endif
#ifdef HAVE_GL_GLU_H
#   include <GL/glu.h>
#elif defined(HAVE_OPENGL_GLU_H)
#   include <OpenGL/glu.h>
#else
#   error no glu.h
#endif
#ifdef HAVE_GL_GLX_H
#   include <GL/glx.h>
#elif defined(HAVE_OPENGL_GLX_H)
#   include <OpenGL/glx.h>
#else
#   error no glx.h
#endif
