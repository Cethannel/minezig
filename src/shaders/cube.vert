#version 450

layout(binding = 0) uniform vs_params {
    mat4 mvp;
};

layout(location = 0) in vec4 pos;
layout(location = 1) in vec2 texcoord0;
layout(location = 2) in vec3 normal0;
layout(location = 3) in vec3 modifierColor0;

layout(location = 0) out vec2 uv;
layout(location = 1) out vec3 normal;
layout(location = 2) out vec3 modifierColor;

void main() {
    gl_Position = mvp * pos;
    uv = texcoord0;
    normal = normal0;
    modifierColor = modifierColor0;
}
