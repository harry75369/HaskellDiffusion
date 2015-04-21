#version 330 core

in vec2 vertexPos;
in vec2 texCoords;

out vec2 uv;

void main() {
  gl_Position = vec4(vertexPos, 0.0, 1.0);
  uv = texCoords;
}
