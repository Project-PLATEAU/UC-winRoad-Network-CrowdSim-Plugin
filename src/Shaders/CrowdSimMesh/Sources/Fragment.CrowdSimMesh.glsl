#version 450

uniform sampler2D texture;
uniform float CellSize;
flat in float GSOUT_Walkable;
out vec4 out_Color;

void main(void){
    if(GSOUT_Walkable == 0.00) // 歩行不可
        {
        out_Color = vec4(0.49, 0.49, 0.49, 0.70);
        }
    else if(GSOUT_Walkable == 1.00) // 歩行可能
        {
		out_Color = vec4(0.69, 0.87, 0.95, 0.70);
        }
	else if(GSOUT_Walkable == 2.00) // 横断歩道（青信号）
        {
        out_Color = vec4(0.20, 0.80, 0.20, 0.70);
        }
	else // 横断歩道（赤信号）
        {
		out_Color = vec4(0.94, 0.51, 0.00, 0.70);
        }
    }
