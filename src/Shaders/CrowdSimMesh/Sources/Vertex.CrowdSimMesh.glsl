#version 450

layout (location = 0) in vec4 in_Position;
layout (location = 1) in float walkable;

flat out float VSOUT_Walkable;

void main(void) {
	VSOUT_Walkable = walkable;
    gl_Position = vec4(in_Position.x, in_Position.y, in_Position.z, 1.0);
    }
