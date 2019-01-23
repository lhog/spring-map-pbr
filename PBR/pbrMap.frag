#version 150 compatibility
/***********************************************************************/
// Engine definitions
#define SMF_TEXSQUARE_SIZE 1024.0
#ifdef DEFERRED_MODE
	#define GBUFFER_NORMTEX_IDX 0
	#define GBUFFER_DIFFTEX_IDX 1
	#define GBUFFER_SPECTEX_IDX 2
	#define GBUFFER_EMITTEX_IDX 3
	#define GBUFFER_MISCTEX_IDX 4
#endif

/***********************************************************************/
// Custom definitions
###CUSTOM_DEFINITIONS###


/***********************************************************************/
// Consts

const float SMF_SHALLOW_WATER_DEPTH     = 10.0;
const float SMF_SHALLOW_WATER_DEPTH_INV = 1.0 / SMF_SHALLOW_WATER_DEPTH;

const float M_PI = 3.1415926535897932384626433832795028841971693993751058209749445923078164062;
const float M_PI2 = M_PI * 2.0;

const float MIN_ROUGHNESS = 0.04;
const float MIN_SPECULAR_F0 = 0.0001;

const vec3 LUMA = vec3(0.2126, 0.7152, 0.0722);


/***********************************************************************/
// Samplers

uniform sampler2D diffuseTex;
uniform sampler2D terrainNormalsTex;
uniform sampler2D brdfTex;
uniform samplerCube reflectionTex;

###SAMPLER_UNIFORMS###
#define tex(num) tex##num

// Uniforms
uniform vec2 normalTexGen;   // either 1.0/mapSize (when NPOT are supported) or 1.0/mapSizePO2
uniform vec2 mapTexGen; // 1.0/mapSize

uniform vec4 lightDir;
uniform vec3 lightColor = SUN_COLOR;

uniform float groundShadowDensity; //useful for NdotL shading as well, so keep it unconditional

uniform float gameFrame;

#if (HAVE_INFOTEX == 1)
	uniform sampler2D infoTex;
	uniform vec2 infoTexGen;     // 1.0/(pwr2map{x,z} * SQUARE_SIZE)
	uniform float infoTexIntensityMul;
#endif

#if (HAVE_SHADOWS == 1)
	uniform sampler2DShadow shadowTex;
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
	vec3 viewDir;
	vec2 diffuseTexCoords;
	vec2 normalTexCoords;
	vec2 mapTexCoords;
#if (HAVE_INFOTEX == 1)
	vec2 infoTexCoords;
#endif
#if (HAVE_SHADOWS == 1)
	vec4 shadowTexCoord;
#endif
	float fogFactor;
} fromVS;

/***********************************************************************/
// Custom Code

###CUSTOM_CODE###

#line 10091

/***********************************************************************/
// Gamma forward and inverse correction procedures

//inspired by https://github.com/tobspr/GLSL-Color-Spaces/blob/master/ColorSpaces.inc.glsl
const vec3 SRGB_INVERSE_GAMMA = vec3(2.2);
const vec3 SRGB_GAMMA = vec3(1.0 / 2.2);
const vec3 SRGB_ALPHA = vec3(0.055);
const vec3 SRGB_MAGIC_NUMBER = vec3(12.92);
const vec3 SRGB_MAGIC_NUMBER_INV = vec3(1.0) / SRGB_MAGIC_NUMBER;

float fromSRGB(float srgbIn) {
	#if (FAST_GAMMA == 1)
		float rgbOut = pow(srgbIn, SRGB_INVERSE_GAMMA.x);
	#else
		float bLess = step(0.04045, srgbIn);
		float rgbOut1 = srgbIn * SRGB_MAGIC_NUMBER_INV.x;
		float rgbOut2 = pow((srgbIn + SRGB_ALPHA.x) / (1.0 + SRGB_ALPHA.x), 2.4);
		float rgbOut = mix( rgbOut1, rgbOut2, bLess );
	#endif
	return rgbOut;
}

vec3 fromSRGB(vec3 srgbIn) {
	#if (FAST_GAMMA == 1)
		vec3 rgbOut = pow(srgbIn.rgb, SRGB_INVERSE_GAMMA);
	#else
		vec3 bLess = step(vec3(0.04045), srgbIn.rgb);
		vec3 rgbOut1 = srgbIn.rgb * SRGB_MAGIC_NUMBER_INV;
		vec3 rgbOut2 = pow((srgbIn.rgb + SRGB_ALPHA) / (vec3(1.0) + SRGB_ALPHA), vec3(2.4));
		vec3 rgbOut = mix( rgbOut1, rgbOut2, bLess );
	#endif
	return rgbOut;
}

vec4 fromSRGB(vec4 srgbIn) {
	#if (FAST_GAMMA == 1)
		vec3 rgbOut = pow(srgbIn.rgb, SRGB_INVERSE_GAMMA);
	#else
		vec3 bLess = step(vec3(0.04045), srgbIn.rgb);
		vec3 rgbOut1 = srgbIn.rgb * SRGB_MAGIC_NUMBER_INV;
		vec3 rgbOut2 = pow((srgbIn.rgb + SRGB_ALPHA) / (vec3(1.0) + SRGB_ALPHA), vec3(2.4));
		vec3 rgbOut = mix( rgbOut1, rgbOut2, bLess );
	#endif
	return vec4(rgbOut, srgbIn.a);
}

float toSRGB(float rgbIn) {
	#if (FAST_GAMMA == 1)
		float srgbOut = pow(rgbIn, SRGB_GAMMA.x);
	#else
		float bLess = step(0.0031308, rgbIn);
		float srgbOut1 = rgbIn * SRGB_MAGIC_NUMBER.x;
		float srgbOut2 = (1.0 + SRGB_ALPHA.x) * pow(rgbIn, 1.0/2.4) - SRGB_ALPHA.x;
		float srgbOut = mix( srgbOut1, srgbOut2, bLess );
	#endif
	return srgbOut;
}

vec3 toSRGB(vec3 rgbIn) {
	#if (FAST_GAMMA == 1)
		vec3 srgbOut = pow(rgbIn.rgb, SRGB_GAMMA);
	#else
		vec3 bLess = step(vec3(0.0031308), rgbIn.rgb);
		vec3 srgbOut1 = rgbIn.rgb * SRGB_MAGIC_NUMBER;
		vec3 srgbOut2 = (vec3(1.0) + SRGB_ALPHA) * pow(rgbIn.rgb, vec3(1.0/2.4)) - SRGB_ALPHA;
		vec3 srgbOut = mix( srgbOut1, srgbOut2, bLess );
	#endif
	return srgbOut;
}

vec4 toSRGB(vec4 rgbIn) {
	#if (FAST_GAMMA == 1)
		vec3 srgbOut = pow(rgbIn.rgb, SRGB_GAMMA);
	#else
		vec3 bLess = step(vec3(0.0031308), rgbIn.rgb);
		vec3 srgbOut1 = rgbIn.rgb * SRGB_MAGIC_NUMBER;
		vec3 srgbOut2 = (vec3(1.0) + SRGB_ALPHA) * pow(rgbIn.rgb, vec3(1.0/2.4)) - SRGB_ALPHA;
		vec3 srgbOut = mix( srgbOut1, srgbOut2, bLess );
	#endif
	return vec4(srgbOut, rgbIn.a);
}

/***********************************************************************/
// Forward and inverse tone mapping operators

vec3 ACESFilmicTM(in vec3 x) {
	float a = 2.51;
	float b = 0.03;
	float c = 2.43;
	float d = 0.59;
	float e = 0.14;
	return (x * (a * x + b)) / (x * (c * x + d) + e);
}

// https://twitter.com/jimhejl/status/633777619998130176
vec3 FilmicHejl2015(in vec3 x) {
    vec4 vh = vec4(x, 1.0); //1.0 is hardcoded whitepoint!
    vec4 va = 1.425 * vh + 0.05;
    vec4 vf = (vh * va + 0.004) / (vh * (va + 0.55) + 0.0491) - 0.0821;
    return vf.rgb / vf.www;
}

vec3 Uncharted2TM(in vec3 x) {
	const float A = 0.15;
	const float B = 0.50;
	const float C = 0.10;
	const float D = 0.20;
	const float E = 0.02;
	const float F = 0.30;
	const float W = 11.2;
	const float white = ((W * (A * W + C * B) + D * E) / (W * (A * W + B) + D * F)) - E / F;

	x *= vec3(2.0); //exposure bias

	vec3 outColor = ((x * (A * x + C * B) + D * E) / (x * (A * x + B) + D * F)) - E / F;
	outColor /= white;

	return outColor;
}

vec3 FilmicTM(in vec3 x) {
	vec3 outColor = max(vec3(0.0), x - vec3(0.004));
	outColor = (outColor * (6.2 * outColor + 0.5)) / (outColor * (6.2 * outColor + 1.7) + 0.06);
	return fromSRGB(outColor); //sadly FilmicTM outputs gamma corrected colors, so need to reverse that effect
}

//https://mynameismjp.wordpress.com/2010/04/30/a-closer-look-at-tone-mapping/ (comments by STEVEM)
vec3 SteveMTM1(in vec3 x) {
	const float a = 10.0; /// Mid
	const float b = 0.3; /// Toe
	const float c = 0.5; /// Shoulder
	const float d = 1.5; /// Mid

	return (x * (a * x + b)) / (x * (a * x + c) + d);
}

vec3 SteveMTM2(in vec3 x) {
	const float a = 1.8; /// Mid
	const float b = 1.4; /// Toe
	const float c = 0.5; /// Shoulder
	const float d = 1.5; /// Mid

	return (x * (a * x + b)) / (x * (a * x + c) + d);
}

vec3 LumaReinhardTM(in vec3 x) {
	float luma = dot(x, LUMA);
	float toneMappedLuma = luma / (1.0 + luma);
	return x * vec3(toneMappedLuma / luma);
}

vec3 ReinhardTM(in vec3 x) {
	return x / (vec3(1.0) + x);
}

vec3 LogTM(vec3 c) {
	const float limit = 2.2;
	const float contrast = 0.35;

	c = log(c + 1.0) / log(limit + 1.0);
	c = clamp(c, 0.0, 1.0);

	c = mix(c, c * c * (3.0 - 2.0 * c), contrast);
	c = pow(c, vec3(1.05,0.9,1));

	return c;
}

vec3 RomBinDaHouseTM(vec3 c) {
	c = exp( -1.0 / ( 2.72 * c + 0.15 ) );
	return c;
}

vec3 expExpand(in vec3 x, in float cutoff, in float mul) {
	float xL = dot(x, LUMA);

	float cutEval = step(cutoff, xL);

	//float yL = (1.0 - cutEval) * xL + cutEval * (exp(mul * xL) - exp(mul * cutoff) + cutoff);
	float yL = mix(xL, exp(mul * xL) - exp(mul * cutoff) + cutoff, cutEval);
	return x * yL / xL;
}

/***********************************************************************/
// Dithering functions

//https://www.shadertoy.com/view/MslGR8

#define DITHER_STATIC 0
#define DITHER_ANIMATED 1

#define DITHER DITHER_ANIMATED

vec3 Dither1(vec3 input) {
	//vec2 seed = gl_FragCoord.xy;
	vec2 seed = gl_FragCoord.xy * 200.0;
#if (DITHER == DITHER_ANIMATED)
	seed += 1337.0 * fract(gameFrame / 100.0);
#endif

	const vec3 magic = vec3( 0.06711056, 0.00583715, 52.9829189 );
	float rnd = fract( magic.z * fract( dot( seed, magic.xy ) ) );
	#if 1
		return input + vec3(rnd, 1.0-rnd, rnd)/255.0;
	#else
		return input + vec3(rnd/255.0);
	#endif
}

vec3 Dither2(vec3 input) {
	// Iestyn's RGB dither (7 asm instructions) from Portal 2 X360, slightly modified for VR
	vec2 seed = gl_FragCoord.xy;
#if (DITHER == DITHER_ANIMATED)
	seed += 1337.0 * fract(gameFrame / 100.0);
#endif

    vec3 vDither = vec3( dot( vec2( 171.0, 231.0 ), seed ) );
    vDither = fract( vDither / vec3( 103.0, 71.0, 97.0 ) );
    return input + (vDither / 255.0);
}

vec3 Dither3(vec3 input) {
	//note: from comment by CeeJayDK
	float dither_bit = 8.0; //Bit-depth of display. Normally 8 but some LCD monitors are 7 or even 6-bit.

	vec2 seed = gl_FragCoord.xy;
#if (DITHER == DITHER_ANIMATED)
	seed += 1337.0 * fract(gameFrame / 100.0);
#endif

	//Calculate grid position
	float grid_position = fract( dot( seed - vec2(0.5,0.5) , vec2(1.0 / 16.0, 10.0 / 36.0) + 0.25 ) );

	//Calculate how big the shift should be
	float dither_shift = (0.25) * (1.0 / (pow(2.0, dither_bit) - 1.0));

	//Shift the individual colors differently, thus making it even harder to see the dithering pattern
	vec3 dither_shift_RGB = vec3(dither_shift, -dither_shift, dither_shift); //subpixel dithering

	//modify shift acording to grid position.
	dither_shift_RGB = mix(2.0 * dither_shift_RGB, -2.0 * dither_shift_RGB, grid_position); //shift acording to grid position.

	//shift the color by dither_shift
	return input + 0.5/255.0 + dither_shift_RGB;
}

/***********************************************************************/
// Material and vectors struct

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


//TODO review if all vectors here are in use
struct VectorDotsInfo {
	float NdotL;			// cos angle between normal and light direction
	float NdotV;			// cos angle between normal and view direction
	float NdotH;			// cos angle between normal and half vector
	float LdotV;			// cos angle between light direction and view direction
	float LdotH;			// cos angle between light direction and half vector
	float VdotH;			// cos angle between view direction and half vector
};

/***********************************************************************/
// Shadow Mapping Stuff
#if (HAVE_SHADOWS == 1)
float GetShadowCoeff(vec4 shadowCoords, float NdotL) {
	NdotL = clamp(NdotL, 0.0, 1.0);
	const float cb = 0.00005;
	//float bias = cb * tan(acos(NdotL));
	float bias = cb * sqrt (1.0 - NdotL * NdotL) / NdotL; // same as above, but cheaper!
	bias = clamp(bias, 0.01 * cb, 5.0 * cb);

	float coeff = 0.0;

	#if (SHADOW_SAMPLES == 1)
		coeff = textureProj( shadowTex, shadowCoords + vec4(0.0, 0.0, bias, 0.0) );
	#else
		const int ssHalf = int(floor(float(SHADOW_SAMPLES)/2.0));
		const float ssSum = float((ssHalf + 1) * (ssHalf + 1));

		shadowCoords += vec4(0.0, 0.0, -bias, 0.0);

		for( int x = -ssHalf; x <= ssHalf; x++ ) {
			float wx = float(ssHalf - abs(x) + 1) / ssSum;
			for( int y = -ssHalf; y <= ssHalf; y++ ) {
				float wy = float(ssHalf - abs(y) + 1) / ssSum;
				coeff += wx * wy * textureProjOffset ( shadowTex, shadowCoords, ivec2(x, y) );
			}
		}
	#endif

	coeff  = (1.0 - coeff);
	coeff *= smoothstep(0.1, 1.0, coeff);

	coeff *= groundShadowDensity;
	return (1.0 - coeff);
}
#endif


/***********************************************************************/
// PBR stuff

// Updated and fixed version of
// https://github.com/SaschaWillems/Vulkan-glTF-PBR/blob/master/data/shaders/pbr_khr.frag#L205-L217
// Gets metalness factor from specular workflow inputs
float ConvertToMetalness(vec3 diffuse, vec3 specular, float maxSpecular, float specularF0) {
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

// Here comes updated version of https://github.com/SaschaWillems/Vulkan-glTF-PBR/blob/master/data/shaders/pbr.frag
//

// Normal (Microfacet) Distribution function --------------------------------------
// Standard across every implementation so far
float D_GGX(float NdotH, float roughness4) {
	float denom = NdotH * NdotH * (roughness4 - 1.0) + 1.0;
	return roughness4/(M_PI * denom*denom);
}

#define PBR_SCHLICK_SMITH_GGX_THIN 1
#define PBR_SCHLICK_SMITH_GGX_THICK 2

// Geometric Shadowing (Occlusion) function --------------------------------------
#if (PBR_SCHLICK_SMITH_GGX == PBR_SCHLICK_SMITH_GGX_THIN)
	// Thinner, more concentrated lobe. Equation 4 of https://blog.selfshadow.com/publications/s2013-shading-course/karis/s2013_pbs_epic_notes_v2.pdf
	// Same is used to generate BRDF LUT
	float G_SchlickSmithGGX(float NdotL, float NdotV, float roughness, float roughness4) {
		float k = roughness + 1.0;
		k = k * k / 8.0;
		float GL = NdotL / (NdotL * (1.0 - k) + k);
		float GV = NdotV / (NdotV * (1.0 - k) + k);
		return GL * GV;
	}
#elif (PBR_SCHLICK_SMITH_GGX == PBR_SCHLICK_SMITH_GGX_THICK)
	// Wider, more spread lobe. Used in Khronos reference PBR implementation
	float G_SchlickSmithGGX(float NdotL, float NdotV, float roughness, float roughness4) {
		float GL = 2.0 * NdotL / (NdotL + sqrt(roughness4 + (1.0 - roughness4) * (NdotL * NdotL)));
		float GV = 2.0 * NdotV / (NdotV + sqrt(roughness4 + (1.0 - roughness4) * (NdotV * NdotV)));
		return GL * GV;
	}
#endif

// Fresnel function ----------------------------------------------------
// Represent specular reflectivity
vec3 F_Schlick(float VdotX, vec3 R0, vec3 R90) {
	return R0 + (R90 - R0) * pow( clamp(1.0 - VdotX, 0.0, 1.0), 5.0 );
}

vec3 F_Schlick(float VdotX, float R0, vec3 R90) {
	return R0 + (R90 - R0) * pow( clamp(1.0 - VdotX, 0.0, 1.0), 5.0 );
}

float F_Schlick(float VdotX, float R0, float R90) {
	return R0 + (R90 - R0) * pow( clamp(1.0 - VdotX, 0.0, 1.0), 5.0 );
}

vec3 F_Schlick(float VdotX, vec3 R0) {
	return R0 + (vec3(1.0) - R0) * pow( clamp(1.0 - VdotX, 0.0, 1.0), 5.0 );
}

vec3 F_Schlick(float VdotX, float R0) {
	return R0 + (vec3(1.0) - R0) * pow( clamp(1.0 - VdotX, 0.0, 1.0), 5.0 );
}

vec3 F_Schlick_Gaussian(float VdotX, vec3 R0, vec3 R90) {
	//Spherical Gaussian Approximation, Used in UE4(?)
	//Reference: Seb. Lagarde's Blog (seblagarde.wordpress.com)
	vec3 k = vec3(exp2((-5.55473 * VdotX - 6.98316) * VdotX));
	return R0 + (R90 - R0) * k;
}

// https://github.com/AndreaMelle/unitypbr/blob/master/Assets/pbr.cginc
vec3 F_CookTorrance(float VdotX, vec3 R0) {
	vec3 sqrtR0 = sqrt(R0);
    vec3 n = (1.0 + sqrtR0) / (1.0 - sqrtR0);
    vec3 g = sqrt(n * n + VdotX * VdotX - 1.0);

    vec3 part1 = (g - VdotX)/(g + VdotX);
    vec3 part2 = ((g + VdotX) * VdotX - 1.0f)/((g - VdotX) * VdotX + 1.0f);

    return max( vec3(0.0), 0.5 * part1 * part1 * (1.0 + part2 * part2) );
}

/*
vec3 F_SchlickR(float cosTheta, vec3 F0, float roughness)
{
	return F0 + (max(vec3(1.0 - roughness), F0) - F0) * pow(1.0 - cosTheta, 5.0);
}
*/

#define PBR_DIFFUSE_LAMBERT 1
#define PBR_DIFFUSE_BURLEY_GOOGLE 2
#define PBR_DIFFUSE_BURLEY_GODOT 3
#define PBR_DIFFUSE_OREN_NAYAR_GODOT 4

// Directional light Diffuse
#if (PBR_BRDF_DIFFUSE == PBR_DIFFUSE_LAMBERT)
	vec3 Diffuse(vec3 diffColor, float roughness, VectorDotsInfo vd) {
		return diffColor / M_PI;
	}
#elif (PBR_BRDF_DIFFUSE == PBR_DIFFUSE_BURLEY_GOOGLE)
	vec3 Diffuse(vec3 diffColor, float roughness, VectorDotsInfo vd) {
		float f90 = 0.5 + 2.0 * roughness * vd.LdotH * vd.LdotH;
		float lightScatter = F_Schlick(vd.NdotL, 1.0, f90);
		float viewScatter  = F_Schlick(vd.NdotV, 1.0, f90);
		return diffColor * lightScatter * viewScatter / M_PI;
	}
#elif (PBR_BRDF_DIFFUSE == PBR_DIFFUSE_BURLEY_GODOT)
	vec3 Diffuse(vec3 diffColor, float roughness, VectorDotsInfo vd) {
			float FD90_minus_1 = 2.0 * vd.LdotH * vd.LdotH * roughness - 0.5;
			float FdV = 1.0 + FD90_minus_1 * pow(vd.NdotV, 5.0);
			float FdL = 1.0 + FD90_minus_1 * pow(vd.NdotL, 5.0);
			return (diffColor / M_PI) * FdV * FdL;
	}
#elif (PBR_BRDF_DIFFUSE == PBR_DIFFUSE_OREN_NAYAR_GODOT)
	vec3 Diffuse(vec3 diffColor, float roughness, VectorDotsInfo vd) {
			float s = vd.LdotV - vd.NdotL * vd.NdotV;
			float t = mix(1.0, max(vd.NdotL, vd.NdotV), step(0.0, s));

			float sigma2 = roughness * roughness; // TODO: this needs checking
			vec3 A = 1.0 + sigma2 * (-0.5 / (sigma2 + 0.33) + 0.17 * diffColor / (sigma2 + 0.13));
			float B = 0.45 * sigma2 / (sigma2 + 0.09);

			return diffColor * (A + vec3(B) * s / t) / M_PI;
	}
#endif

#define PBR_F_SCHLICK_KHRONOS 1
#define PBR_F_SCHLICK_SASCHA 2
#define PBR_F_SCHLICK_GOOGLE 3
#define PBR_F_SCHLICK_GAUSSIAN 4
#define PBR_F_COOK_TORRANCE 5

#define PBR_R90_METHOD_STD 1
#define PBR_R90_METHOD_GOOGLE 2

vec3 GetPBR(MaterialInfo mat, VectorDotsInfo vd, vec3 N, vec3 R) {

	// sanitize inputs
	float roughness = clamp(mat.roughness, MIN_ROUGHNESS, 1.0);
	float metalness = clamp(mat.metalness, 0.0, 1.0);

	// additional vars
	float roughness2 = roughness * roughness;
	float roughness4 = roughness2 * roughness2; // roughness^4
	vec3 F0 = vec3(mat.specularF0);

	// break down the base color
	vec3 baseDiffuseColor = mat.baseColor * (vec3(1.0) - F0) * (1.0 - metalness);
	vec3 baseSpecularColor = mix(F0, mat.baseColor, vec3(metalness));

	#if (PBR_R90_METHOD == PBR_R90_METHOD_STD)
		float maxReflectance = max(max(baseSpecularColor.r, baseSpecularColor.g), baseSpecularColor.b);
		float reflectance90 = clamp(maxReflectance / mat.specularF0, 0.0, 1.0); // bugged way...
		//float reflectance90 = clamp(maxReflectance * 25.0, 0.0, 1.0);
	#elif (PBR_R90_METHOD == PBR_R90_METHOD_GOOGLE)
		float reflectance90 = clamp(dot(F0, vec3(50.0 * 0.33)), 0.0, 1.0);
	#endif

	vec3 specularEnvironmentR0 = baseSpecularColor;
	vec3 specularEnvironmentR90 = vec3(reflectance90);

	// D = Normal distribution (Distribution of the microfacets)
	float D = D_GGX(vd.NdotH, roughness4);

	// F = Fresnel factor (Reflectance depending on angle of incidence)
	#if (PBR_F_SCHLICK == PBR_F_SCHLICK_KHRONOS) // Khronos & learnopengl
		vec3 F = F_Schlick(vd.VdotH, specularEnvironmentR0, specularEnvironmentR90);
	#elif (PBR_F_SCHLICK == PBR_F_SCHLICK_SASCHA) // SaschaWillems. Likely mistake
		vec3 F = F_Schlick(vd.NdotV, specularEnvironmentR0, specularEnvironmentR90);
	#elif (PBR_F_SCHLICK == PBR_F_SCHLICK_GOOGLE) // Google, same as Khronos?
		vec3 F = F_Schlick(vd.LdotH, specularEnvironmentR0, specularEnvironmentR90);
	#elif (PBR_F_SCHLICK == PBR_F_SCHLICK_GAUSSIAN)
		vec3 F = F_Schlick_Gaussian(vd.LdotH, specularEnvironmentR0, specularEnvironmentR90);
	#elif (PBR_F_SCHLICK == PBR_F_COOK_TORRANCE)
		vec3 F = F_CookTorrance(vd.LdotH, specularEnvironmentR0);
	#endif

	//F= vec3(0.001);

	// G = Geometric shadowing term (Microfacets shadowing)
	float G = G_SchlickSmithGGX(vd.NdotL, vd.NdotV, roughness, roughness4);

	// Calculation of analytical lighting contribution
	vec3 sunDiffuseContrib = (1.0 - F) * Diffuse(baseDiffuseColor, roughness, vd);
	vec3 sunSpecContrib = F * G * D / (4.0 * vd.NdotL * vd.NdotV);

	// Obtain final intensity as reflectance (BRDF) scaled by the energy of the light (cosine law)
	vec3 sunLitDiffColor = vd.NdotL * lightColor * sunDiffuseContrib;
	vec3 sunLitSpecColor = vd.NdotL * lightColor * sunSpecContrib;

	// Image Based Lighting
	ivec2 reflectionTexSize = textureSize(reflectionTex, 0);
	float reflectionTexMaxLOD = log2(float(max(reflectionTexSize.x, reflectionTexSize.y)));
	float specularLOD = reflectionTexMaxLOD * roughness;
	specularLOD += IBL_SPECULAR_LOD_BIAS;

	#if (IBL_DIFFUSECOLOR_STATIC == 1)
		vec3 iblDiffuseLight = IBL_DIFFUSECOLOR;
	#else
		// It's wrong to sample diffuse irradiance from reflection texture.
		// But alternative (convolution to irradiance) is too performance hungry (???)
		// Sample from "blurry" 16x16 texels mip level, so it looks more or less like irradiance
		vec3 iblDiffuseLight = texture(reflectionTex, N, reflectionTexMaxLOD - 4.0).rgb;
		iblDiffuseLight = IBL_GAMMACORRECTION(iblDiffuseLight);
		#if (IBL_INVERSE_TONEMAP == 1)
			float avgDLum = dot(LUMA, textureLod(reflectionTex, N, reflectionTexMaxLOD).rgb);
			iblDiffuseLight = expExpand(iblDiffuseLight, avgDLum, IBL_INVERSE_TONEMAP_MUL);
		#endif
	#endif

	#if (IBL_SPECULARCOLOR_STATIC == 1)
		vec3 iblSpecularLight = IBL_SPECULARCOLOR;
	#else
		// Get reflection with respect to surface roughness
		vec3 iblSpecularLight = texture(reflectionTex, R, specularLOD).rgb;
		iblSpecularLight = IBL_GAMMACORRECTION(iblSpecularLight);
		#if (IBL_INVERSE_TONEMAP == 1)
			float avgSLum = dot(LUMA, textureLod(reflectionTex, R, reflectionTexMaxLOD).rgb);
			iblSpecularLight = expExpand(iblSpecularLight, avgSLum, IBL_INVERSE_TONEMAP_MUL);
		#endif
	#endif

	iblDiffuseLight = IBL_SCALE_DIFFUSE(iblDiffuseLight);
	iblSpecularLight = IBL_SCALE_SPECULAR(iblSpecularLight);

	//sanitize Lights
	iblDiffuseLight = max(vec3(0.0), iblDiffuseLight);
	iblSpecularLight = max(vec3(0.0), iblSpecularLight);

	vec2 brdf = textureLod(brdfTex, vec2(vd.NdotV, 1.0 - roughness), 0.0).xy;

	vec3 iblLitDiffColor = iblDiffuseLight * baseDiffuseColor;
	vec3 iblLitSpecColor = iblSpecularLight * (baseSpecularColor * brdf.x + brdf.y);

	//return N;

	//TODO: figure out which one looks better
	float occlusion = mix(1.0, groundShadowDensity, 1.0 - mat.occlusion);
	#if 0
		return (sunLitDiffColor + iblLitDiffColor) * occlusion + (sunLitSpecColor + iblLitSpecColor);
	#else
		return (sunLitDiffColor + sunLitSpecColor) + (iblLitDiffColor + iblLitSpecColor) * occlusion;
	#endif
	//return iblLitSpecColor;
}

/***********************************************************************/
// Main variables and base functions

vec2 diffuseTexCoords;
vec2 normalTexCoords;
vec2 mapTexCoords;
#if (HAVE_INFOTEX == 1)
	vec2 infoTexCoords;
#endif

vec3 terrainWorldNormal;

#define MAT_COUNT ###MATERIALS_COUNT###
MaterialInfo material[MAT_COUNT];

vec2 flipUV(vec2 uv) {
	return vec2(uv.x, 1.0 - uv.y);
}

#define DXUnpackNormals(xyz) ((2.0 * xyz - 1.0) * vec3(1.0, -1.0, 1.0))
#define OGLUnpackNormals(xyz) (2.0 * xyz - 1.0)

void FillMaterialWeightsHeights() {
###MATERIAL_WEIGHTS_HEIGHTS###
}

void FillMaterialParams() {
###MATERIAL_PARAMS###
}

vec3 GetTerrainNormal(vec2 uv) {
	vec3 normal;
	normal.xz = texture2D(terrainNormalsTex, uv).ra;
	normal.y  = sqrt(1.0 - dot(normal.xz, normal.xz));
	return normal;
}

// RNM - Already unpacked
// https://www.shadertoy.com/view/4t2SzR
vec3 NormalBlendUnpackedRNM(vec3 n1, vec3 n2) {
	n1 += vec3(0.0, 0.0, 1.0);
	n2 *= vec3(-1.0, -1.0, 1.0);

    return n1 * dot(n1, n2) / n1.z - n2;
}

#line 20711

void main() {

	diffuseTexCoords = fromVS.diffuseTexCoords;
	normalTexCoords = fromVS.normalTexCoords;
	mapTexCoords = fromVS.mapTexCoords;
#if (HAVE_INFOTEX == 1)
	infoTexCoords = fromVS.infoTexCoords;
#endif

	FillMaterialWeightsHeights();
	{
		float weightsSum = 0.0;
		for (int i = 0; i < MAT_COUNT; ++i) {
			// TODO: review float(material[i].weight >= WEIGHT_CUTOFF) impact
			//weightsSum += material[i].weight * float(material[i].weight >= WEIGHT_CUTOFF);
			weightsSum += material[i].weight;
		}

		#if (HAS_DEFAULT_SPLAT == 1)
			// default splat goes always in [0]. Only defined if sum(non-default splat weights) < 1.0, otherwise it's 0.0
			material[0].weight = 1.0 - min(weightsSum, 1.0);
		#endif

		// Sum of weights might go over 1.0, so normalize splat weights in such case
		float weightsSumOr1 = max(weightsSum, 1.0);
		for (int i = 0; i < MAT_COUNT; ++i) {
			material[i].weight /= weightsSumOr1;
		}
	}

	// https://raw.githubusercontent.com/Zylann/godot_heightmap_plugin/master/addons/zylann.hterrain/shaders/simple4_lite.shader
	#if (MAT_BLENDING_HEIGHT == 1)
	{
		const float dh = MAT_BLENDING_HEIGHT_SMOOTHNESS;
		const float minusInf = -1.0 / 0.0;

		// use of single hMax is incorrect, but fast and not too detrimental to the final blending result.
		// see link above how maximums should be calculated
		float hMax = minusInf;

		for (int i = 0; i < MAT_COUNT; ++i) {
			material[i].weight += material[i].height; // h = bumps + splat;
			hMax = max(hMax, material[i].weight);
			material[i].weight += dh; // vec4 d = h + dh;
		}

		float weightsSum = 0.0;
		for (int i = 0; i < MAT_COUNT; ++i) {
			material[i].weight = clamp(material[i].weight - hMax, 0.0, 1.0);
			weightsSum += material[i].weight;
		}

		for (int i = 0; i < MAT_COUNT; ++i) {
			material[i].weight /= weightsSum;
		}
	}
	#endif

	//gl_FragColor.rg = vec2(material[0].weight, material[1].weight);
	//return;

	vec4 vertexWorldPos = fromVS.vertexWorldPos;

	// TODO: reduce ammount of calculations
	vec3 terrainWorldNormal = GetTerrainNormal(mapTexCoords);
	vec3 worldTangent = normalize( cross(terrainWorldNormal, vec3(0.0, 0.0, 1.0)));
	vec3 worldBitangent = normalize( cross(terrainWorldNormal, worldTangent) );

	mat3 worldTBN = mat3(worldTangent, worldBitangent, terrainWorldNormal); //from tangent space to world space

	#if (TERRAIN_NORMAL_BLEND_DO == 1)
		terrainWorldNormal = worldTBN * TERRAIN_NORMAL_BLEND;
		// TODO: find out if results are same below
		#if 1 // Gram-Schmidt process.
			worldTangent = normalize(worldTangent - terrainWorldNormal * dot(terrainWorldNormal, worldTangent));
		#else // Regular process
			worldTangent = normalize( cross(terrainWorldNormal, vec3(0.0, 0.0, 1.0)));
		#endif
		worldBitangent = normalize( cross(terrainWorldNormal, worldTangent) );
		worldTBN = mat3(worldTangent, worldBitangent, terrainWorldNormal);
	#endif

	mat3 invWorldTBN = transpose(worldTBN); //from world space to tangent space

	#if (POM_MAXSTEPS > 0)
		// TODO POM

		//these 4 below will be affected by POM (see above)
		diffuseTexCoords = fromVS.diffuseTexCoords;
		normalTexCoords = fromVS.normalTexCoords;
		mapTexCoords = fromVS.mapTexCoords;
		#if (HAVE_INFOTEX == 1)
			infoTexCoords = fromVS.infoTexCoords;
		#endif
	#endif

	FillMaterialParams();

	// TODO: recalc TBN with new terrainNormal?


	// TODO: move these three to Vertex Shader?
	vec3 V = normalize(fromVS.viewDir);	// Vector from surface point to camera
	// TODO: figure out if normalize is required
	vec3 L = normalize(lightDir.xyz);	// Vector from surface point to light
	vec3 H = normalize(L + V);			// Half vector between both l and v

	gl_FragColor = vec4(0.0, 0.0, 0.0, 1.0);
	VectorDotsInfo vdi = VectorDotsInfo(
		0.0,							//vdi.NdotL
		0.0,							//vdi.NdotV
		0.0,							//vdi.NdotH
		clamp(dot(L, V), 0.0, 1.0),		//vdi.LdotV
		clamp(dot(L, H), 0.0, 1.0),		//vdi.LdotH
		clamp(dot(V, H), 0.0, 1.0)		//vdi.VdotH
	);

	float shadowMix = 0.0;
	float shadowN = 0.0;

	vec3 emissionColor = vec3(0.0);

	for (int i = 0; i < MAT_COUNT; ++i) {
		// TODO: review (material[i].weight >= WEIGHT_CUTOFF) impact
		// bool enoughWeight = (material[i].weight >= WEIGHT_CUTOFF);
		// TODO: do branchless?
		if (material[i].weight > 0.0) {
			vec3 blendNormalTBN = material[i].blendNormal;
			#if 1
				vec3 N = worldTBN * blendNormalTBN; //blended already because of TBN transformation?
			#else
				vec3 terrainNormalTBN = invWorldTBN * terrainWorldNormal; //vec3(0, 1, 0) ?
				vec3 N = worldTBN * NormalBlendUnpackedRNM(terrainNormalTBN, blendNormalTBN);
			#endif

			//N = terrainWorldNormal;
			N = normalize(N);

			vec3 R = -normalize(reflect(V, N));

			float NdotLu = dot(N, L);

			vdi.NdotL = clamp(NdotLu, 0.001, 1.0);
			vdi.NdotV = clamp(abs(dot(N, V)), 0.001, 1.0);
			vdi.NdotH = clamp(dot(N, H), 0.0, 1.0);

			shadowMix = max(shadowMix, smoothstep(0.0, 0.5, NdotLu) * material[i].weight);
			shadowN = max(shadowN, mix(1.0 - groundShadowDensity, 1.0, shadowMix));

			gl_FragColor.rgb += GetPBR(material[i], vdi, N, R) * material[i].weight;// * smoothstep(0.0, WEIGHT_CUTOFF, material[i].weight);
			emissionColor += material[i].emissionColor * material[i].weight;// * smoothstep(0.0, WEIGHT_CUTOFF, material[i].weight);
		}
	}

	float shadowG = 1.0;

	#if (HAVE_SHADOWS == 1)
		float NdotL = dot(terrainWorldNormal, L); //too lazy to carry the real NdotL from the prev loop
		// TODO: figure out performance implications of conditional
		#if 0
			if (NdotL > 0.0) {
				shadowG = GetShadowCoeff(fromVS.shadowTexCoord, NdotL);
			}
		#else
			shadowG = GetShadowCoeff(fromVS.shadowTexCoord, NdotL);
		#endif
	#endif

	//gl_FragColor.rg = vec2(1.0 - shadowN, 1.0 - shadowG);
	//gl_FragColor.b = 0.0;
	//return;

	float shadow = mix(shadowN, shadowG, shadowMix);
	gl_FragColor.rgb *= shadow;

	gl_FragColor.rgb += emissionColor;

	//gl_FragColor.rgb = material[0].baseColor;
	//gl_FragColor.rgb =  BlendNormals(worldTBN * UnpackNormals(material[0].blendNormal));
	//gl_FragColor.rgb = terrainWorldNormal;
	//gl_FragColor.rgb = vec3( dot(terrainWorldNormal, H) );
	//gl_FragColor.rgb = vec3( L );

	//vec3 N = terrainWorldNormal;
	//vec3 R = -normalize(reflect(V, N));
	//gl_FragColor.rgb = textureLod(reflectionTex, N, 0.0).rgb;
















	#ifdef DEFERRED_MODE
		gl_FragData[GBUFFER_NORMTEX_IDX] = vec4((normal + vec3(1.0)) * 0.5, 1.0);
		gl_FragData[GBUFFER_DIFFTEX_IDX] = diffuseCol + detailCol;
		gl_FragData[GBUFFER_SPECTEX_IDX] = specularCol;
		gl_FragData[GBUFFER_EMITTEX_IDX] = emissionCol;
		gl_FragData[GBUFFER_MISCTEX_IDX] = vec4(0.0, 0.0, 0.0, 0.0);

		// linearly transform the eye-space depths, might be more useful?
		// gl_FragDepth = gl_FragCoord.z / gl_FragCoord.w;
	#else
		gl_FragColor.rgb = mix(gl_Fog.color.rgb, gl_FragColor.rgb, fromVS.fogFactor);
		gl_FragColor.rgb = OUTPUT_EXPOSURE(gl_FragColor.rgb);
		gl_FragColor.rgb = OUTPUT_TONEMAPPING(gl_FragColor.rgb);
		gl_FragColor.rgb = OUTPUT_GAMMACORRECTION(gl_FragColor.rgb);
	#endif
}

