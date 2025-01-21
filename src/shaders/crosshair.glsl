#pragma sokol @vs vs

in vec4 pos;
in vec2 texcoord0;

out vec2 uv;

void main() {
    gl_Position = pos;
    uv = texcoord0;
}
#pragma sokol @end

#pragma sokol @fs fs

in vec2 uv;
out vec4 frag_color;

void main() {
    frag_color = vec4(0.0);

    if (uv.x > 0.4 && uv.x < 0.6) {
	frag_color = vec4(1.0);
    }

    if (uv.y > 0.4 && uv.y < 0.6) {
	frag_color = vec4(1.0);
    }
}
#pragma sokol @end

#pragma sokol @program crosshair vs fs
