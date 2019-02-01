#version 150 compatibility
#define SMF_TEXSQR_SIZE 1024.0

###CUSTOM_DEFINITIONS###

uniform ivec2 texSquare;
uniform vec4 lightDir;       // mapInfo->light.sunDir

uniform vec2 normalTexGen;   // either 1.0/mapSize (when NPOT are supported) or 1.0/mapSizePO2
uniform vec2 mapTexGen; // 1.0/mapSize
#if (HAVE_INFOTEX == 1)
	uniform vec2 infoTexGen;     // 1.0/(pwr2map{x,z} * SQUARE_SIZE)
#endif
#if (HAVE_SHADOWS == 1)
	uniform mat4 shadowMat;
	uniform vec4 shadowParams;
#endif


out Data {
	vec4 vertexWorldPos;
	vec3 viewDir;
	vec2 diffuseTexCoords;
	vec2 normalTexCoords;
	vec2 mapTexCoords;
#if (HAVE_INFOTEX == 1)
	vec2 infoTexCoords;
#endif
#if (HAVE_SHADOWS == 1)
	//mat4 invShadowMat;
	vec4 shadowTexCoord;
#endif
	float fogFactor;
};

void main() {
	// calc some lighting variables
	viewDir = vec3(gl_ModelViewMatrixInverse * vec4(0.0, 0.0, 0.0, 1.0));
	viewDir = viewDir - gl_Vertex.xyz;

	vertexWorldPos = gl_Vertex;

	// calc texcoords
	diffuseTexCoords = (floor(vertexWorldPos.xz) / SMF_TEXSQR_SIZE) - vec2(texSquare);
	normalTexCoords = vertexWorldPos.xz * normalTexGen;
	mapTexCoords = vertexWorldPos.xz * mapTexGen;
#if (HAVE_INFOTEX == 1)
	infoTexCoords = vertexWorldPos.xz * infoTexGen;
#endif

#if (HAVE_SHADOWS == 1)
	//invShadowMat = inverse(shadowMat);
	shadowTexCoord = shadowMat * vertexWorldPos;
	#if 1
		shadowTexCoord.xy = shadowTexCoord.xy + 0.5;
	#else
		shadowTexCoord.xy *= (inversesqrt(abs(shadowTexCoord.xy) + shadowParams.zz) + shadowParams.ww);
		shadowTexCoord.xy += shadowParams.xy;
	#endif
#endif

	// transform vertex pos
	gl_Position = gl_ModelViewProjectionMatrix * gl_Vertex;
	gl_ClipVertex = gl_ModelViewMatrix * gl_Vertex;

#ifndef DEFERRED_MODE
	// emulate linear fog
	float fogCoord = length(gl_ClipVertex.xyz);
	fogFactor = (gl_Fog.end - fogCoord) * gl_Fog.scale; // gl_Fog.scale == 1.0 / (gl_Fog.end - gl_Fog.start)
	fogFactor = clamp(fogFactor, 0.0, 1.0);
#endif
}

