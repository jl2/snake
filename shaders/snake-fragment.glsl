#version 400 core

// uniform int maxIterations;
in vec2 complexCoordinate;

out vec4 Color;

void main (void)
{
     Color = vec4(complexCoordinate.y, complexCoordinate.x, 0.0, 1.0);
}
