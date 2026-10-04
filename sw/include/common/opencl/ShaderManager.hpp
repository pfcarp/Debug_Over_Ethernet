#pragma once

#include <string>
#include <epoxy/gl.h>

class ShaderManager {
  public:
    static bool isOpenGLES();
    static std::string getVertexShaderSource();
    static std::string getFragmentShaderSource();
    static GLuint loadShaders();
    static GLuint compileShader(GLenum type, const std::string& source);
};
