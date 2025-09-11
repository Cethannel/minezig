#version 450

layout(location = 0) in vec2 uv;
layout(location = 1) in vec3 normal;
layout(location = 2) in vec3 modifierColor;

layout(location = 0) out vec4 frag_color;

layout(binding = 1) uniform sampler2D texSampler;

void main() {
    vec4 text = texture(texSampler, uv);
    vec4 outColor = text;
    if (text.r == text.g && text.g == text.b) {
        outColor *= max(vec4(modifierColor, 1.0), vec4(1.0));
    }
    float modifier = 1.0;
    if (normal.x != 0.0) {
    	modifier = 0.9;
    }
    if (normal.y != 0.0) {
    	modifier = 0.8;
    }
    if (normal.z != 0.0) {
    	modifier = 0.85;
    }
    outColor.rgb *= modifier;
    frag_color = outColor;
}
