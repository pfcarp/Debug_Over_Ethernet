#include "opencl/ShaderManager.hpp"

#include <glib.h>
#include <string>


bool ShaderManager::isOpenGLES() {
  static bool checked = false;
  static bool isES = false;

  if (!checked) {
    const char* version = (const char*)glGetString(GL_VERSION);
    const char* shader_version = (const char*)glGetString(GL_SHADING_LANGUAGE_VERSION);

    if (version != nullptr) {
      std::string version_str(version);
      isES = (version_str.find("ES") != std::string::npos);
      printf("%s\n", version);
      printf("%s\n", shader_version);
    }
    else {
      g_warning("Could not determine OpenGL version");
    }
    checked = true;
  }
  return isES;
}

std::string ShaderManager::getVertexShaderSource() {
    if (isOpenGLES()) {
        return R"glsl(
#version 300 es
precision highp float;
layout(location = 0) in vec2 aPos;
layout(location = 1) in vec4 aColor;
out vec4 vColor;
uniform mat4 uViewProjection;
uniform float uZoomScale;
void main() {
    gl_Position = uViewProjection * vec4(aPos, 0.0, 1.0);
    vColor = aColor;
    gl_PointSize = 10.0;
}
)glsl";
    } else {
        return R"glsl(
#version 330 core
layout(location = 0) in vec2 aPos;
layout(location = 1) in vec4 aColor;
out vec4 vColor;
uniform mat4 uViewProjection;
uniform float uZoomScale;
void main() {
    gl_Position = uViewProjection * vec4(aPos, 0.0, 1.0);
    vColor = aColor;
    gl_PointSize = 10.0;
}
)glsl";
    }
}

std::string ShaderManager::getFragmentShaderSource() {
    if (isOpenGLES()) {
        return R"glsl(
#version 300 es
precision highp float;
in vec4 vColor;
out vec4 FragColor;
uniform float uRadius;
uniform bool uIsPoint; // Add this

void main() {
    if (uIsPoint) {
        vec2 coord = gl_PointCoord - vec2(0.5);
        if (length(coord) > 0.5) discard;
        if (length(coord) > uRadius) discard;
    }
    FragColor = vColor;
}
)glsl";
    } else {
        return R"glsl(
#version 330 core
in vec4 vColor;
out vec4 FragColor;
uniform float uRadius;
uniform bool uIsPoint; // Add this

void main() {
    if (uIsPoint) {
        vec2 coord = gl_PointCoord - vec2(0.5);
        if (length(coord) > 0.5) discard;
        if (length(coord) > uRadius) discard;
    }
    FragColor = vColor;
}
)glsl";
    }
}

GLuint ShaderManager::compileShader(GLenum type, const std::string& source) {
    const char* source_cstr = source.c_str();
    GLuint shader = glCreateShader(type);
    glShaderSource(shader, 1, &source_cstr, NULL);
    glCompileShader(shader);
    GLint success;
    glGetShaderiv(shader, GL_COMPILE_STATUS, &success);
    if (!success) {
        GLchar infoLog[512];
        glGetShaderInfoLog(shader, 512, NULL, infoLog);
        g_warning("Shader compilation failed: %s", infoLog);
        return 0;
    }
    return shader;
}

GLuint ShaderManager::loadShaders() {
    GLuint vertex_shader = compileShader(GL_VERTEX_SHADER, getVertexShaderSource());
    GLuint fragment_shader = compileShader(GL_FRAGMENT_SHADER, getFragmentShaderSource());
    if (!vertex_shader || !fragment_shader) return 0;

    GLuint program = glCreateProgram();
    glAttachShader(program, vertex_shader);
    glAttachShader(program, fragment_shader);
    glLinkProgram(program);
    GLint success;
    glGetProgramiv(program, GL_LINK_STATUS, &success);
    if (!success) {
        GLchar infoLog[512];
        glGetProgramInfoLog(program, 512, NULL, infoLog);
        g_warning("Shader linking failed: %s", infoLog);
        return 0;
    }
    glDeleteShader(vertex_shader);
    glDeleteShader(fragment_shader);
    return program;
}
