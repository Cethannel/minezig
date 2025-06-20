//------------------------------------------------------------------------------
//  Shader code for texcube-sapp sample.
//
//  NOTE: This source file also uses the '#pragma sokol' form of the
//  custom tags.
//------------------------------------------------------------------------------
#pragma sokol @header const m = @import("zlm")
#pragma sokol @ctype mat4 m.Mat4

#pragma sokol @vs vs
layout(binding=0) uniform vs_params {
    mat4 mvp;
};

in vec4 pos;
in vec2 texcoord0;

out vec2 uv;

void main() {
    gl_Position = mvp * pos;
    uv = texcoord0;
}
#pragma sokol @end

#pragma sokol @fs fs

in vec2 uv;
out vec4 frag_color;

void main() {
    frag_color = vec4(vec3(0.0), 1.0);

    if (uv.x == 0) {
	    frag_color = vec4(vec3(1.0), 1.0);
    }
}
#pragma sokol @end

#pragma sokol @program selector vs fs
