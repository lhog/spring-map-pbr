#version 150 compatibility

const uint NUM_SAMPLES = 1024u;
const float M_PI = 3.1415926535897932384626433832795028841971693993751058209749445923078164062;

uniform vec2 texSize;

//https://www.shadertoy.com/view/4djSRW
// Hash without Sine
#define HASHSCALE1 .1031

//----------------------------------------------------------------------------------------
//  1 out, 2 in...
float hash12(vec2 p)
{
	vec3 p3  = fract(vec3(p.xyx) * HASHSCALE1);
    p3 += dot(p3, p3.yzx + 19.19);
    return fract((p3.x + p3.y) * p3.z);
}

vec2 hammersley2d(uint i, uint N)
{
	// Radical inverse based on http://holger.dammertz.org/stuff/notes_HammersleyOnHemisphere.html
	uint bits = (i << 16u) | (i >> 16u);
	bits = ((bits & 0x55555555u) << 1u) | ((bits & 0xAAAAAAAAu) >> 1u);
	bits = ((bits & 0x33333333u) << 2u) | ((bits & 0xCCCCCCCCu) >> 2u);
	bits = ((bits & 0x0F0F0F0Fu) << 4u) | ((bits & 0xF0F0F0F0u) >> 4u);
	bits = ((bits & 0x00FF00FFu) << 8u) | ((bits & 0xFF00FF00u) >> 8u);
	float rdi = float(bits) * 2.3283064365386963e-10;
	return vec2(float(i) /float(N), rdi);
}

#define RESTRICT_BRANCHING

// Based on http://blog.selfshadow.com/publications/s2013-shading-course/karis/s2013_pbs_epic_slides.pdf
vec3 importanceSample_GGX(vec2 Xi, float roughness, vec3 normal)
{
	// Maps a 2D point to a hemisphere with spread based on roughness
	float alpha = roughness * roughness;
	float phi = 2.0 * M_PI * Xi.x + hash12(normal.xz) * 0.1;
	float cosTheta = sqrt((1.0 - Xi.y) / (1.0 + (alpha * alpha - 1.0) * Xi.y));
	float sinTheta = sqrt(1.0 - cosTheta * cosTheta);
	vec3 H = vec3(sinTheta * cos(phi), sinTheta * sin(phi), cosTheta);

	// Tangent space
	#ifdef RESTRICT_BRANCHING
		vec3 up = mix(vec3(1.0, 0.0, 0.0), vec3(0.0, 0.0, 1.0), vec3( abs(normal.z) < 0.999));
	#else
		vec3 up = abs(normal.z) < 0.999 ? vec3(0.0, 0.0, 1.0) : vec3(1.0, 0.0, 0.0);
	#endif
	vec3 tangentX = normalize(cross(up, normal));
	vec3 tangentY = normalize(cross(normal, tangentX));

	// Convert to world Space
	return normalize(tangentX * H.x + tangentY * H.y + normal * H.z);
}

#define G_OPTION 3

// Geometric Shadowing function
float G_SchlicksmithGGX(float dotNL, float dotNV, float roughness)
{
	#if (G_OPTION == 1)
		float k = (roughness * roughness) / 2.0;
	#elif (G_OPTION == 2)
		float k = (roughness + 1.0);
		//k = k * k;
	#elif (G_OPTION == 3)
		float k = (roughness + 1.0);
		k = k * k / 8.0;
	#elif (G_OPTION == 4)
		float k = (0.5 + 0.5 * roughness);
		k = k * k;
	#endif
	float GL = dotNL / (dotNL * (1.0 - k) + k);
	float GV = dotNV / (dotNV * (1.0 - k) + k);
	return GL * GV;
}

vec2 BRDF(float NoV, float roughness)
{
	// Normal always points along z-axis for the 2D lookup
	const vec3 N = vec3(0.0, 0.0, 1.0);
	vec3 V = vec3(sqrt(1.0 - NoV * NoV), 0.0, NoV); //normalized

	vec2 LUT = vec2(0.0);
	for(uint i = 0u; i < NUM_SAMPLES; i++) {
		vec2 Xi = hammersley2d(i, NUM_SAMPLES);
		vec3 H = importanceSample_GGX(Xi, roughness, N);
		vec3 L = 2.0 * dot(V, H) * H - V;

		float dotNL = max(dot(N, L), 0.0);
		float dotVH = max(dot(V, H), 0.0);
		float dotNV = clamp(dot(N, V), 0.0000001, 1.0);
		float dotNH = clamp(dot(H, N), 0.0000001, 1.0);

		#ifdef RESTRICT_BRANCHING
			float G = G_SchlicksmithGGX(dotNL, dotNV, roughness);
			float G_Vis = (G * dotVH) / (dotNH * dotNV);
			float Fc = pow(1.0 - dotVH, 5.0);
			vec2 lutAdd = mix( vec2(0.0), vec2((1.0 - Fc) * G_Vis, Fc * G_Vis), vec2(float(dotNL > 0.0)) );
			LUT += lutAdd;
		#else
			if (dotNL > 0.0) {
				float G = G_SchlicksmithGGX(dotNL, dotNV, roughness);
				float G_Vis = (G * dotVH) / (dotNH * dotNV);
				float Fc = pow(1.0 - dotVH, 5.0);
				LUT += vec2((1.0 - Fc) * G_Vis, Fc * G_Vis);
			}
		#endif
	}
	return LUT / vec2( float(NUM_SAMPLES) );
}

void main() {
	//NdotV, roughness
	vec2 inUV = gl_FragCoord.xy / texSize;
	gl_FragColor = vec4(BRDF(inUV.x, 1.0 - inUV.y), 0.0, 1.0);
}