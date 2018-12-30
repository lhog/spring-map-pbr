struct MaterialInfo {
	vec3 baseColor;
	vec3 emissionColor;
	vec3 normals;

	float height;
	float pom;
	float occlusion;
	float specularF0;
	float roughness;
	float metallic;
};

MaterialInfo GetMaterialInfo%%SPLAT_NUM%%(vec2 mapUV) {

}

//#define AVOID_SPLATSWEIGHT_BRANCHING

#ifdef AVOID_SPLATSWEIGHT_BRANCHING
const MaterialInfo emptyMaterial = MaterialInfo(
	vec3(0.0), // baseColor
	vec3(0.0), // emissionColor
	vec3(0.0), // normals

	0.0, // height
	0.0, // pom
	0.0, // occlusion
	0.0, // specularF0
	0.0, // roughness
	0.0  // metallic
);
#endif

vec3 BlendHeightWeight(vec3 tex1, vec3 tex2, float height1, float height2, float a1, float a2) {
    const float depth = 0.2; // less - sharper transitions, more - smoother transitions
    float ma = max(a1 + height1, a2 + height2) - depth;

    float b1 = max(a1 + height1 - ma, 0.0);
    float b2 = max(a2 + height2 - ma, 0.0);

    return (tex1 * b1 + tex2 * b2) / (b1 + b2);
}

vec3 BlendWeight(vec3 tex1, vec3 tex2, float a1, float a2) {
	return tex1 * a1 + tex2 * a2;
}

float splatWeights[%%SPLAT_COUNT%%];

void FillSplatsWeights(vec2 straightMapUV) {
	vec2 flippedMapUV = vec2(straightMapUV.s, 1.0 - straightMapUV.t); //flip vertical component
	
	#ifdef HAS_DEFAULT_SPLAT
		splatWeights[0] = 0.0; // Don't trust compiler to assign this to 0.0 for us. Better safe, than sorry
	#endif
	%%READ_WEIGHT_TEXTURES%%
	
	float weightsSum = 0.0;
	for (int i = 0; i < %%SPLAT_COUNT%%; ++i) {
		weightsSum += splatWeights[i];
	}
	
	#ifdef HAS_DEFAULT_SPLAT
		// default splat goes always in [0]. Only defined if sum(non-default splat weights) < 1.0, otherwise it's 0.0
		splatWeights[0] = 1.0 - min(weightsSum, 1.0);
	#endif
	
	// Sum of weights might go over 1.0, so normalize splat weights in such case
	float weightsSumOr1 = max(weightsSum, 1.0);
	for (int i = 0; i < %%SPLAT_COUNT%%; ++i) {
		splatWeights[i] /= weightsSumOr1;
	}	
}


const ignoreWeight = 5.0/255.0; //ignore <= than ~2% inclusions

MaterialInfo MaterialsPreBlend(vec2 mapUV) {
	MaterialInfo mi;
	float splatWeight;
	bool ignoreCond;
	
	MaterialInfo result;

	splatWeight = splatWeights[%%SPLAT_NUM%%];
	cond = splatWeight <= ignoreWeight;
#ifndef AVOID_SPLATSWEIGHT_BRANCHING
	if (cond) {
		mi = GetMaterialInfo%%SPLAT_NUM%%(mapUV);
	}
#else
	mi = GetMaterialInfo%%SPLAT_NUM%%(mapUV);
	mi = mix(mi, emptyMaterial, float(cond));
#endif
	
	result.baseColor = mix()

}