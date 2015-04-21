#version 330 core

in vec2 uv;

out vec4 fragColor;

void main() {
  fragColor = vec4(uv.x, uv.y, 0.0, 1.0);
}
