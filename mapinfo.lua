--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- mapinfo.lua
--

local mapinfo = {
	name        = "Enceladus",
	shortname   = "Enceladus",
	description = "This ice planet has high geothermal activity and a subsurface liquid ocean. SW vs NE Teams.",
	author      = "TheMooseIsLoose",
	version     = "1a",
	--mutator   = "deployment";
	mapfile   = "maps/iceworld.smf", --// location of smf/sm3 file (optional)
	modtype     = 3, --// 1=primary, 0=hidden, 3=map
	depend      = {},
	replace     = {},

	--startpic   = "", --// deprecated
	--StartMusic = "", --// deprecated

	maphardness     = 200,
	notDeformable   = false,
	gravity         = 80,
	tidalStrength   = 50,
	maxMetal        = 0.5,
	extractorRadius = 100.0,
	voidWater       = false,
	autoShowMetal   = false,


	smf = {
		minheight = 2,
		maxheight = 486,
		smtFileName0 = "maps/iceworld.smt",
		minimapTex = "icemini.dds",
		typemapTex = "icetype.png",
		--smtFileName1 = "",
		--smtFileName.. = "",
		--smtFileNameN = "",
	},

	sound = {
		--// Sets the _reverb_ preset (= echo parameters),
		--// passfilter (the direct sound) is unchanged.
		--//
		--// To get a list of all possible presets check:
		--//   https://github.com/spring/spring/blob/master/rts/System/Sound/OpenAL/EFXPresets.cpp
		--//
		--// Hint:
		--// You can change the preset at runtime via:
		--//   /tset UseEFX [1|0]
		--//   /tset snd_eaxpreset preset_name   (may change to a real cmd in the future)
		--//   /tset snd_filter %gainlf %gainhf  (may    "   "  "  "    "  "   "    "   )
		preset = "mountains",

		passfilter = {
			--// Note, you likely want to set these
			--// tags due to the fact that they are
			--// _not_ set by `preset`!
			--// So if you want to create a muffled
			--// sound you need to use them.
			gainlf = 1.0,
			gainhf = 1.0,
		},

		reverb = {
			--// Normally you just want use the `preset` tag
			--// but you can use handtweak a preset if wanted
			--// with the following tags.
			--// To know their function & ranges check the
			--// official OpenAL1.1 SDK document.

			--density
			--diffusion
			--gain
			--gainhf
			--gainlf
			--decaytime
			--decayhflimit
			--decayhfratio
			--decaylfratio
			--reflectionsgain
			--reflectionsdelay
			--reflectionspan
			--latereverbgain
			--latereverbdelay
			--latereverbpan
			--echotime
			--echodepth
			--modtime
			--moddepth
			--airabsorptiongainhf
			--hfreference
			--lfreference
			--roomrollofffactor
		},
	},

	resources = {
		--grassBladeTex = "",
		--grassShadingTex = "",
		detailTex = "detailtexbright.bmp",
		specularTex = "icespec.dds",
		splatDetailTex = "splat.dds",
		splatDistrTex = "icedist.dds",
		--skyReflectModTex = "icesky.dds",
		splatDetailNormalDiffuseAlpha = 1,
		splatDetailNormalTex1 = "Zsplatnorm2.dds";
		splatDetailNormalTex2 = "rock_269_highpass_dnts.dds";
		splatDetailNormalTex3 = "rock_46_highpass_dnts.dds";
		splatDetailNormalTex4 = "sand_195_highpass_dnts.dds";
		detailNormalTex = "icenorm.dds",
		--lightEmissionTex = "",
	},

	splats = {
		texScales = {0.004, 0.007, 0.008, 0.0015},
		texMults  = {0.18, 0.36, 0.36, 0.026},
	},

	atmosphere = {
		minWind      = 5.0,
		maxWind      = 30.0,

		fogStart     = 0.58,
		fogEnd       = 0.85,
		fogColor     = {0.04, 0.075, 0.04},

		sunColor     = {1, 1, 1},
		skycolor     = {0.2, 0.4, 0.4},
		skyDir       = {0.1, 0.0, 0.5},
		--skyBox       = "spacey.dds",

		cloudDensity = 0.025,
		cloudColor   = {0.9, 0.9, 0.7},
	},

	lighting = {
		--// dynsun
		sunStartAngle = 0.0,
		sunOrbitTime  = 1440.0,
		sundir        = { -0.1, 0.215, -0.32 },

		--// unit & ground lighting
		groundAmbientColor  = {0.65, 0.65, 0.65},
		groundDiffuseColor  = {1.15, 1.15, 1.15},
		groundSpecularColor = {0.4, 0.4, 0.4},
		groundShadowDensity = 0.5,
		unitAmbientColor    = {0.4, 0.4, 0.4},
		unitDiffuseColor    = {1.45, 1.45, 1.45},
		unitSpecularColor   = {0.9, 0.9, 0.9},
		unitShadowDensity   = 0.8,

		specularExponent    = 50.0,
	},

	water = {
		damage =  0.0,

		repeatX = 0.0,
		repeatY = 0.0,

		absorb    = {0.004, 0.004, 0.002},
		baseColor = {0.4, 0.6, 0.8},
		minColor  = {0.1, 0.1, 0.23},

		ambientFactor  = 1.3,
		diffuseFactor  = 1.0,
		specularFactor = 0.5,
		specularPower  = 2.0,

		planeColor = {0.0, 0.01, 0.01},

		surfaceColor  = {0.25, 0.85, 0.85},
		surfaceAlpha  = 0.02,
		diffuseColor  = {0.4, 0.7, 1.0},
		specularColor = {0.5, 0.5, 0.5},

		fresnelMin   = 0.3,
		fresnelMax   = 1.0,
		fresnelPower = 4.0,

		reflectionDistortion = 1.0,

		blurBase      = 2.0,
		blurExponent = 1.5,

		perlinStartFreq  =  8.0,
		perlinLacunarity = 3.0,
		perlinAmplitude  =  0.9,
		windSpeed = 1.0, --// does nothing yet

		shoreWaves = true,
		forceRendering = false,

		--// undefined == load them from resources.lua!
		--texture =       "",
		--foamTexture =   "",
		--normalTexture = "",
		--caustics = {
		--	"",
		--	"",
		--},
	},

	teams = {
		[0] = {startPos = {x = 2033, z = 852}},
		[1] = {startPos = {x = 10134, z = 852}},
		[2] = {startPos = {x = 0, z = 0}},
		[3] = {startPos = {x = 0, z = 0}},
	},

	terrainTypes = {
		[0] = {
			name = "Default",
			hardness = 1.0,
			receiveTracks = true,
			moveSpeeds = {
				tank  = 1.0,
				kbot  = 1.0,
				hover = 1.0,
				ship  = 1.0,
			},
		},
		[255] = {
			name = "Ice",
			hardness = 0.33,
			receiveTracks = false,
			moveSpeeds = {
				tank  = 1.0,
				kbot  = 1.0,
				hover = 1.0,
				ship  = 1.0,
			},
		},
	},

	custom = {
		fog = {
			color    = {0.6, 0.6, 0.6},
			height   = "11%", --// allows either absolue sizes or in percent of map's MaxHeight
			fogatten = 0.008,
		},
		precipitation = {
			weather = "snow",
			density   = 17500,
			size      = 1.2,
			speed     = 20,
			windscale = 1,
			texture   = 'LuaUI/effects/snowflake.png',
		},
		pbr = {
			enabled = true,
			textures = {
				[2] = "Ice_001/Ice_001_COLOR.jpg",
				[3] = "Ice_001/Ice_001_SPEC.jpg",
				[4] = "Ice_001/Ice_001_NRM.jpg",
				[5] = "Ice_001/Ice_001_OCC.jpg",
				
				[6] = "Rocks_006_2K/Rocks06_col.jpg",
				[7] = "Rocks_006_2K/Rocks06_nrm.jpg",
				[8] = "Rocks_006_2K/Rocks06_rgh.jpg",
				[9] = "Rocks_006_2K/Rocks06_AO.jpg",
				
				[10] = "Ice_001/Ice_001_DISP.png",
				[11] = "Rocks_006_2K/Rocks06_disp.jpg",

				--[3] = "Blue_Ice_001_SD/Blue_Ice_001_COLOR.jpg",
				[20] = "icedist.dds",
			},
			definitions = {
				["SUN_COLOR"] = "vec3(1.0)",
				["SHADOW_SAMPLES"] = "1",
				--["WEIGHT_CUTOFF"] = "0.3",
				["MAT_BLENDING_HEIGHT_SMOOTHNESS"] = "0.2",
				--["IBL_DIFFUSECOLOR"] = "vec3(0.6, 0.77, 0.77)",
				--["IBL_SPECULARCOLOR"] = "vec3(0.6, 0.77, 0.77)",
				--["IBL_DIFFUSECOLOR"] = "vec3(1.0)",
				--["IBL_SPECULARCOLOR"] = "vec3(1.0)",
				
				--["OUTPUT_EXPOSURE(preExpColor)"] = "1.4 * preExpColor",
				--["OUTPUT_TONEMAPPING(preTMColor)"] = "SteveMTM2(preTMColor)"
			},
			splats = {
				{
					workflow = "SPECULAR",
					--specularF0 = "0.01",
					--weight = "1.0",
					--weight = "texture(tex20, (mapTexCoords)).g",
					weight = "mapTexCoords.x",
					diffuseColor = "fromSRGB(texture(tex2, 15.0 * mapTexCoords).rgb)",
					specularColor = "fromSRGB(texture(tex3, 15.0 * mapTexCoords).rgb)",
					occlusion = "texture(tex5, 15.0 * mapTexCoords).r",
					--blendNormal = "OGLUnpackNormals(texture(tex4, 15.0 * mapTexCoords).xyz)",
					--blendNormalStrength = "vec3(0.0)",
					roughness = "0.0",
					
					--height = "texture(tex10, 15.0 * mapTexCoords).r",
					height = "0.5",
				},
		
				{
					workflow = "METALNESS",
					--weight = "texture(tex20, (mapTexCoords)).r",
					--weight = "1.0",
					weight = "1.0 - mapTexCoords.x",
					baseColor = "texture(tex6, 2.0 * mapTexCoords).rgb",
					blendNormal = "DXUnpackNormals(texture(tex7, 2.0 * mapTexCoords).xyz)",
					roughness = "texture(tex8, 2.0 * mapTexCoords).r",
					
					occlusion = "texture(tex9, 2.0 * mapTexCoords).r",
					metalness = "0.0",
					
					height = "texture(tex11, 2.0 * mapTexCoords).r",
				},
				--{ },
				--{ },
			},
		},
	},
}

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- Helper

local function lowerkeys(ta)
	local fix = {}
	for i,v in pairs(ta) do
		if (type(i) == "string") then
			if (i ~= i:lower()) then
				fix[#fix+1] = i
			end
		end
		if (type(v) == "table") then
			lowerkeys(v)
		end
	end

	for i=1,#fix do
		local idx = fix[i]
		ta[idx:lower()] = ta[idx]
		ta[idx] = nil
	end
end

lowerkeys(mapinfo)

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- Map Options

if (Spring and Spring.GetMapOptions) then
	local function tmerge(t1, t2)
		for i,v in pairs(t2) do
			if (type(v) == "table") then
				t1[i] = t1[i] or {}
				tmerge(t1[i], v)
			else
				t1[i] = v
			end
		end
	end

	getfenv()["mapinfo"] = mapinfo
		local files = VFS.DirList("mapconfig/mapinfo/", "*.lua")
		table.sort(files)
		for i=1,#files do
			local newcfg = VFS.Include(files[i])
			if newcfg then
				lowerkeys(newcfg)
				tmerge(mapinfo, newcfg)
			end
		end
	getfenv()["mapinfo"] = nil
end

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

return mapinfo

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------