#version 460 core

layout(location = 0) in vec4 position;
uniform vec2 offset;

void main() {
  gl_Position = position + vec4(offset.x, -offset.y, 0.0, 0.0);
}
