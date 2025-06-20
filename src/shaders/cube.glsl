//------------------------------------------------------------------------------
//  Shader code for texcube-sapp sample.
//
//  NOTE: This source file also uses the '#pragma sokol' form of the
//  custom tags.
//------------------------------------------------------------------------------
#pragma sokol @header const m = @import("zlm")
#pragma sokol @ctype mat4 m.Mat4

#pragma sokol @vs vs
layout(binding = 0) uniform vs_params {
    mat4 mvp;
};

in vec4 pos;
in vec2 texcoord0;
in vec3 normal0;
in vec3 modifierColor0;

out vec2 uv;
out vec3 normal;
out vec3 modifierColor;

void main() {
    gl_Position = mvp * pos;
    uv = texcoord0;
    normal = normal0;
    modifierColor = modifierColor0;
}
#pragma sokol @end

#pragma sokol @fs fs
layout(binding = 0) uniform texture2D tex;
layout(binding = 0) uniform sampler smp;

in vec2 uv;
in vec3 normal;
in vec3 modifierColor;
out vec4 frag_color;

void main() {
    vec4 text = texture(sampler2D(tex, smp), uv);
    vec4 outColor = text;
    if (text.r == text.g && text.g == text.b) {
        outColor *= vec4(modifierColor, 1.0);
    }
    if (text.r == text.g) {
        outColor *= vec4(normal, 1.0);
    }
    frag_color = outColor;
}
#pragma sokol @end

#pragma sokol @program texcube vs fs
