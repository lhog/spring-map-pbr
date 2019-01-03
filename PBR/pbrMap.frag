#version 150 compatibility
/***********************************************************************/
// Engine defines
#define SMF_TEXSQUARE_SIZE 1024.0
#ifdef DEFERRED_MODE
	#define GBUFFER_NORMTEX_IDX 0
	#define GBUFFER_DIFFTEX_IDX 1
	#define GBUFFER_SPECTEX_IDX 2
	#define GBUFFER_EMITTEX_IDX 3
	#define GBUFFER_MISCTEX_IDX 4
#endif

/***********************************************************************/
// Custom defines
###CUSTOM_DEFINES###


/***********************************************************************/
// Consts

const float SMF_SHALLOW_WATER_DEPTH     = 10.0;
const float SMF_SHALLOW_WATER_DEPTH_INV = 1.0 / SMF_SHALLOW_WATER_DEPTH;

const float MINROUGHNESS = 0.04;


/***********************************************************************/
// Samplers

uniform sampler2D diffuseTex;
uniform sampler2D terrainNormalsTex;
uniform sampler2D brdfLUTTex;
uniform samplerCube reflectionTex;

###SAMPLER_UNIFORMS###
#define tex(num) tex##num

// Uniforms
uniform vec2 normalTexGen;   // either 1.0/mapSize (when NPOT are supported) or 1.0/mapSizePO2
uniform vec2 mapTexGen; // 1.0/mapSize
uniform vec2 infoTexGen;     // 1.0/(pwr2map{x,z} * SQUARE_SIZE)

uniform vec4 lightDir;
uniform vec3 cameraPos;

uniform float infoTexIntensityMul;

#ifdef HAVE_SHADOWS
	//uniform sampler2DShadow shadowTex;
	uniform sampler2D shadowTex;
	uniform mat4 shadowMat;
	uniform vec4 shadowParams;
#endif

#ifdef SMF_WATER_ABSORPTION
	uniform vec2 mapHeights; // min & max height on the map
	uniform vec3 waterMinColor;
	uniform vec3 waterBaseColor;
	uniform vec3 waterAbsorbColor;
#endif

// Varyings
in Data {
	vec4 vertexWorldPos;
	vec2 diffuseTexCoords;
	vec2 normalTexCoords;
	vec2 mapTexCoords;
	vec2 infoTexCoords;

	vec3 halfDir;
	float fogFactor;
};

/***********************************************************************/
// Custom Code
###CUSTOM_CODE###


/***********************************************************************/
// Helper functions
#line 20081

vec3 GetTerrainNormal(vec2 uv) {
	vec3 normal;
	normal.xz = texture2D(terrainNormalsTex, uv).ra;
	normal.y  = sqrt(1.0 - dot(normal.xz, normal.xz));
	return normal;
}

// Updated and fixed version of
// https://github.com/SaschaWillems/Vulkan-glTF-PBR/blob/master/data/shaders/pbr_khr.frag#L205-L217
// Gets metallic factor from specular workflow inputs
float ConvertToMetalness(vec3 diffuse, vec3 specular, float maxSpecular, float specularF0) {
	const vec3 LUMA = vec3(0.2126, 0.7152, 0.0722);
	float perceivedDiffuse = dot(diffuse, LUMA);
	float perceivedSpecular = dot(specular, LUMA);

	if (perceivedSpecular < specularF0) {
		return 0.0;
	}
	float a = specularF0;
	float b = perceivedDiffuse * (1.0 - maxSpecular) / (1.0 - specularF0) + perceivedSpecular - 2.0 * specularF0;
	float c = specularF0 - perceivedSpecular;
	float D = max(b * b - 4.0 * a * c, 0.0);
	return clamp((-b + sqrt(D)) / (2.0 * a), 0.0, 1.0);
}


/***********************************************************************/
// main()

struct MaterialInfo {
	vec3 baseColor;
	vec3 emissionColor;
	vec3 blendNormal;

	float weight;

	float height;
	float pomScale;
	float occlusion;
	float specularF0;
	float roughness;
	float metalness;
};

###MATERIALS_VAR###

void GetMaterialInfo() {
	###MATERIAL_WEIGHTS###

	###MATERIAL_PARAMS###
}

void main() {

	GetMaterialInfo();

	gl_FragColor = texture(tex20, mapTexCoords);

	#ifdef DEFERRED_MODE
		gl_FragData[GBUFFER_NORMTEX_IDX] = vec4((normal + vec3(1.0, 1.0, 1.0)) * 0.5, 1.0);
		gl_FragData[GBUFFER_DIFFTEX_IDX] = diffuseCol + detailCol;
		gl_FragData[GBUFFER_SPECTEX_IDX] = specularCol;
		gl_FragData[GBUFFER_EMITTEX_IDX] = emissionCol;
		gl_FragData[GBUFFER_MISCTEX_IDX] = vec4(0.0, 0.0, 0.0, 0.0);

		// linearly transform the eye-space depths, might be more useful?
		// gl_FragDepth = gl_FragCoord.z / gl_FragCoord.w;
	#else
		gl_FragColor.rgb = mix(gl_Fog.color.rgb, gl_FragColor.rgb, fogFactor);
	#endif
}

