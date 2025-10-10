#version 450

layout (points) in; // 入力プリミティブ
layout (triangle_strip, max_vertices = 4) out; // 出力プリミティブ

flat in float VSOUT_Walkable[];
flat out float GSOUT_Walkable;

uniform mat4 ProjMat;
uniform mat4 ModelMat;
uniform float CellSize;

void main() {
	GSOUT_Walkable = VSOUT_Walkable[0];

    vec4 cellvertex = gl_in[0].gl_Position;
	gl_Position = ProjMat * ModelMat * cellvertex;
	EmitVertex();	
	
	cellvertex = vec4(gl_in[0].gl_Position.x + CellSize, gl_in[0].gl_Position.y, gl_in[0].gl_Position.z, 1.0);
	gl_Position = ProjMat * ModelMat * cellvertex;
	EmitVertex();
	
	cellvertex = vec4(gl_in[0].gl_Position.x, gl_in[0].gl_Position.y, gl_in[0].gl_Position.z + CellSize, 1.0);
	gl_Position = ProjMat * ModelMat * cellvertex;
	EmitVertex();
	
	cellvertex = vec4(gl_in[0].gl_Position.x + CellSize, gl_in[0].gl_Position.y, gl_in[0].gl_Position.z + CellSize, 1.0);
	gl_Position = ProjMat * ModelMat * cellvertex;
	EmitVertex();
	
    EndPrimitive();
    }
	