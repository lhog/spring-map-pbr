function gadget:GetInfo()
	return {
		name      = "Map PBR Shader",
		desc      = "Applies PBR to map surface",
		author    = "ivand",
		date      = "2018-2019",
		license   = " ",
		layer     = 1000,
		enabled   = true,
	}
end

if (gadgetHandler:IsSyncedCode()) then
	return
end

local LuaShader = VFS.Include("libs/lsg/LuaShader.lua")
local GenBRDFLUT = VFS.Include("PBR/GenBrdfLut.lua")

local function GetNextPowerOf2(tbl)
	local npot = {}
	for i = 1, #tbl do
		npot[i] = math.pow(2, math.ceil(math.log(tbl[i])/math.log(2)))
	end

	return unpack(npot)
end

Table = Table or {}

function Table.Echo(data, name, indent)
	indent = indent or ""
	name = name or "Table.Echo"
	Spring.Echo(indent .. name .. " = {")
	if data then
		for name, v in pairs(data) do
			local ty =  type(v)
			if ty == "table" then
				Table.Echo(v, name, indent .. "    ")
			elseif ty == "boolean" then
				Spring.Echo(indent .. name .. " = " .. (v and "true" or "false"))
			else
				Spring.Echo(indent .. name .. " = " .. tostring(v))
			end
		end
	else
		Spring.Echo(indent .. name .. " = " .. tostring(v))
	end
	Spring.Echo(indent .. "}")
end

function Table.RestoreKeysCase(inputTable, keysTable)
	local insertKeys = {}
	local deleteKeys = {}
	for ki, vi in pairs(inputTable) do
		for kk, vk in pairs(keysTable) do
			if string.lower(ki) == string.lower(kk) and ki ~= kk  then
				--Spring.Echo("Matching keys", ki, kk)
				insertKeys[kk] = vi
				table.insert(deleteKeys, ki)
				break
			end
		end
	end

	for ik, iv in pairs(insertKeys) do
		inputTable[ik] = iv
		--Spring.Echo("Inserting key/value", ik, iv)
		--Table.Echo(iv, ik)
		if type(iv) == "table" then
			Table.RestoreKeysCase(inputTable[ik], keysTable[ik])
		end
	end

	for _, ik in ipairs(deleteKeys) do
		--Spring.Echo("Deleting key", ik)
		inputTable[ik] = nil
	end

	return inputTable
end

function Table.MergeWithDefaults(inputTable, defaultsTable)
	for kd, vd in pairs(defaultsTable) do
		local vdt = type(vd)
		if inputTable[kd] then
			if vdt == "table" then
				inputTable[kd] = Table.MergeWithDefaults(inputTable[kd], vd)
			else
				--leave all inputTable scalars intact
			end
		else
			if vdt == "table" then
				inputTable[kd] = Table.MergeWithDefaults({}, vd)
			else
				inputTable[kd] = vd
			end
		end
	end
	return inputTable
end

function Table.Merge(originalTable, overrideTable)
    for k, v in pairs(overrideTable) do
        if type(v) == 'table' then
            local sv = type(originalTable[k])
            if sv == 'table' or sv == 'nil' then
                if sv == 'nil' then
                    originalTable[k] = {}
                end
                Table.Merge(originalTable[k], v)
            end
        elseif originalTable[k] == nil then
            originalTable[k] = v
        end
    end
    return originalTable
end

-- Below are examples of how "get" statement might look like
-- get = "[0].rgb" -- sample 0th texture and take RGB channels
-- get = "[10].a" -- sample 10th texture and take A channel
-- get = "[20].aaaa" -- sample 20th texture and take A value 4 times to form RGBA value in shader
local pbrSplatDefaults = {
	workflow = "metallic", -- either "metallic" or "specular"
	uvMult = {1.0, 1.0}, -- texture coordinates multiplier. Map-wide UV (0.0 --> 1.0) will be multiplied by this value and then sampled at the result. Default vec2(1.0).

	-- Reference to splats distribution map
	distrMap = {
		scale = 1.0, -- multiplier to the texture value or a value in case get == nil
		get = nil, -- reference to splats distribution map. In case of get == nil, this is the default splat. Implementation won't allow for more than one default splat. Default splat is optional.
		gammaCorrection = false, -- Defaults to false. Don't change unless you know what you are doing!
	},

	-- Also known as albedo or diffuse (in PBR sense). 
	-- "An albedo map defines the color of diffused light. One of the biggest differences between an albedo map in a PBR system and a traditional diffuse map is the lack of directional light or ambient occlusion. 
	--  Directional light will look incorrect in certain lighting conditions, and ambient occlusion should be added in the separate AO slot."
	baseColorMap = {
		scale = {1.0, 1.0, 1.0}, -- acts as a color if tex unit is unused or as a multiplier if tex unit is present. Defaults to vec3(1.0).
		get = nil, -- coold be for example "[2].rgb". Takes samples from 2nd texture in textures array.
		gammaCorrection = true, -- Artists see colors in sRGB, but we need colors in linear space. Therefore this defaults to true.
	},

	-- Tangent space normal map
	normalMap = {
		scale = {1.0, 1.0, 1.0}, -- scale for normals sampled from normalMapTex. Use to scale one direction of normals or increase/reduce normal values of this material in the mix. Defaults to vec3(1.0)
		get = nil, --If you use DDS and see some weird moar/acne like artifacts, use uncompressed DDS instead.
		gammaCorrection = false, -- Defaults to false. Don't change unless you know what you are doing!
	},

	-- Used for height based materials blending.
	bumpMap = {
		-- the next three params are applied like this: (1) Invert(if any) (2) Bias (3) Scale
		invert = false, -- invert height value, i.e. height = (1.0 - height).
		bias = 0.0,  -- Bias to height value. Use to lower or raise height value of this material. Can be useful for height based materials blending
		scale = 1.0, -- Scale factor for height.
		pomScale = 0.02, -- Set to non-nil to enable Parallax Occlusion Mapping. Reduce in case of severe texture distortions or layering.
		pomMaxSteps = 32, -- Maximal number of iterations spent by Parallax Occlusion Mapping algorithm on one screen pixel.
		get = nil, --expects one grayscale channel
		gammaCorrection = false, -- Defaults to false. Don't change unless you know what you are doing!
	},


	-- Emission color map:
	--   1: multiplier to baseColor
	--   3: add blended on top of the shaded color
	--   4: lerp blended with the shaded color
	emissionMap1 = {
		scale = 1.0, -- acts as a value if tex unit is unused or as a multiplier if tex unit is present. Defaults to 1.0.
		get = nil, -- expects a grayscale channel. This will act as a multiplier to baseColorMap
		gammaCorrection = false, -- Don't do gammaCorrection if 1 channel emissive is used.
	},
	emissionMap3 = {
		scale = {1.0, 1.0, 1.0}, -- acts as a color if tex unit is unused or as a multiplier if tex unit is present. Defaults to vec3(1.0).
		get = nil, -- expects RGB channels.
		gammaCorrection = true, -- Defaults to true, because you might provide RGB channels (see baseColorMap.gammaCorrection).
	},
	emissionMap4 = {
		scale = {1.0, 1.0, 1.0, 1.0}, -- acts as a color if tex unit is unused or as a multiplier if tex unit is present. Defaults to vec4(1.0).
		get = nil, -- expects RGBA channels. A-channel represents rate of mix between shaded color and emissive color.
		gammaCorrection = true, -- Defaults to true, because you might provide RGB channels (see baseColorMap.gammaCorrection). If 1 channel emissive is used don't do gammaCorrection.
	},

	-- Ambient occlusion
	occlusionMap = {
		scale = 1.0, --acts as a multiplier or a base value (if get is nil) Defaults to 1.0.
		get = nil, -- expects a grayscale channel. Note 1.0 means unoccluded, 0.0 - shadowed.
		gammaCorrection = false, -- Defaults to false. Don't change unless you know what you are doing!
	},

	-- Specular F0: a linear grayscale texture for Fresnel values (non-metals). Can be used in both metallic and specular workflows
	-- Most often than not, this map is unused.
	-- DO NOT MIX IT UP with specularMap below
	specularF0Map = {
		scale = 0.04, --acts as a multiplier or a base value (if get is nil). Defaults to 0.04.
		get = nil, -- expects a grayscale channel.
		gammaCorrection = false, -- Defaults to false. Don't change unless you know what you are doing!
	},

	-- Supports either a roughness or glossiness map. Can be used in both metallic and specular workflows.
	roughnessMap = {
		invert = false, -- invert i.e. roughness = (1.0 - glossiness). Do so in case when supplied map represents glossiness. Default to false
		scale = 1.0, --acts as a multiplier or a base value (if get is nil) Defaults to 1.0.
		get = nil, -- expects a grayscale channel.
		gammaCorrection = false, -- Defaults to false. Don't change unless you know what you are doing!
	},

	-- Metalness workflow. Only considered if workflow = "metallic"
	metallicMap = {
		scale = 1.0, --acts as a multiplier or a base value (if get is nil) Defaults to 1.0.
		get = nil, -- expects a grayscale channel.
		gammaCorrection = false, -- Defaults to false. Don't change unless you know what you are doing!
	},

	-- Specular workflow. Only considered if workflow = "specular"
	specularMap = {
		scale = {1.0, 1.0, 1.0}, --acts as a multiplier or a base value (if get is nil) Defaults to 1.0.
		get = nil, -- expects RGB channels.
		gammaCorrection = false, -- Defaults to true, because you might provide RGB channels (see baseColorMap.gammaCorrection).
	},
}

local pbrMapDefaults = {
	fastGamma = false, --default is false i.e. more precise method
	exposure = 1.0, -- RGB color multiplier before color is feed to tonemapper or to user's screen (if tonemap is nil)
	toneMapping = nil, --valid values are "aces", "uncharted2", "filmic", "reinhard", "log", "romBinDaHouse", "lumaReinhard", "hejl2015", "steveM1", "steveM2".
	gammaCorrection = true, -- do gamma correction (RGB-->sRGB) on the final color.
	iblMap = {
		invToneMapExp = nil, -- can be some value to enable poor man's SDR to HDR mapping (inverse tonemapping)
		scale = {1.0, 1.0}, --{diffuse, specular} IBL scale. Acts as a multiplier to base values.
		gammaCorrection = false, -- Artists see colors in sRGB, but we need colors in linear space. Therefore this defaults to true.
	},
	splats = {
	},
	textures = {
	},
	debug = {
	},
}

local DefinitionGenerators = {
	["pomScale"] = true,
	["pomMaxSteps"] = true,
	["invToneMapExp"] = true,
}

local function ParseSubtable(prefix, mapName, mapValue, uniformsFloat, uniformsSampler, definitions, ignoreList)
	if not prefix then prefix = "" end
	if not mapName then mapName = "" end
	if not ignoreList then ignoreList = {} end

	local camelPrefix = prefix:gsub("^%l", string.lower)
	local capitalPrefix = prefix:gsub("%l", string.upper)

	local camelMapName = mapName:gsub("^%l", string.upper)
	local capitalMapName = mapName:gsub("%l", string.upper)

	local definitionKeyTemplate = ""
	local capitalizeUniformKey = false

	if capitalPrefix ~= "" then
		capitalizeUniformKey = capitalizeUniformKey or true
		definitionKeyTemplate = definitionKeyTemplate .. "%s_"
	else
		definitionKeyTemplate = definitionKeyTemplate .. "%s"
	end

	if mapName ~= "" then
		capitalizeUniformKey = capitalizeUniformKey or true
		definitionKeyTemplate = definitionKeyTemplate .. "%s_"
	else
		definitionKeyTemplate = definitionKeyTemplate .. "%s"
	end

	--Spring.Echo("definitionKeyTemplate", definitionKeyTemplate)

	local definitionKeyPrefix = string.format( definitionKeyTemplate, capitalPrefix, capitalMapName)
	local uniformKeyPrefix = string.format( "%s%s", camelPrefix, camelMapName)

	for k, v in pairs(mapValue) do

		local definitionKey = definitionKeyPrefix .. k:gsub("%l", string.upper)

		local uniformKey
		if capitalizeUniformKey then
			uniformKey = uniformKeyPrefix .. k:gsub("^%l", string.upper)
		else
			uniformKey = uniformKeyPrefix .. k
		end

		--Spring.Echo("uniformKey", uniformKey)
		--Spring.Echo("definitionKey", definitionKey)

		local onIgnoreList = false

		for _, v in ipairs(ignoreList) do
			if k:find(v) ~= nil then
				onIgnoreList = true
				break;
			end
		end

		--Spring.Echo("#0")
		valType = type(v)
		--Spring.Echo("#0", mapName, valType, k)
		if not onIgnoreList then
			if valType == "number" or valType == "table" then --scalars and arrays of scalars
				--Spring.Echo("#0000")
				if DefinitionGenerators[k] then
					-- Sometimes we want a number to be placed in #define
					definitions[definitionKey] = v
					--Spring.Echo("#1")
				else
					-- Most often we want number or vector to go to uniforms
					uniformsFloat[uniformKey] = v
					--Spring.Echo("#2")
				end
			elseif valType == "boolean" then -- generate definition in case of boolean set to true
				definitions[definitionKey] = tostring( (v and 1) or 0 )
				--Spring.Echo("#3")
			elseif valType == "string" then
				if v == "get" then
					definitions[definitionKey] = string.format( "%s%s", "texels", v)
					--Spring.Echo("#4")
				else
					local definitionVal = string.format( "%s_%s", definitionKey, v:gsub("%l", string.upper) )
					definitions[definitionKey] = definitionVal
					--Spring.Echo("#5")
				end
			else
				--ignore everything else
			end
		end
	end
end


local function ParseTextures(pbrMap)
	local TEXTURE_DIR = "maps/"

	local boundTexUnits = {}
	local flippedTexUnits = {}

	for tu, texDef in pairs(pbrMap.textures) do
		if texDef and texDef.name and texDef.name:len() >= 1 then
			local texDefName = texDef.name
			if texDefName ~= "$" then
				--filter out gl.Texture options to get filename
				local s, e = string.find(texDefName, ":.-:")
				local texOpt = ""
				if s and e then
					texOpt = string.sub(texDefName, s, e)
				end
				local fileName = string.gsub(texDefName, ":.-:", "")
				local newFilePath = TEXTURE_DIR .. fileName
				if VFS.FileExists(newFilePath) then
					boundTexUnits[tu] = texOpt .. newFilePath --keep :{opts}:
				else
					Spring.Echo(string.format( "[%s]: Failed to find PBR texture file (%s) to be bound to texture unit %d", gadget:GetInfo().name, newFilePath, tu ))
				end
			else
				boundTexUnits[tu] = texDefName --bind $blabla textures unconditionally
			end

			flippedTexUnits[tu] = texDef.flip
		end
	end

	return boundTexUnits, flippedTexUnits
end

local function ParseDistrMapOfSplats(pbrMap, boundTexUnits, flippedTexUnits)
	local splatsReadBodyTbl = {}
	local defaultSplatFound = false
	local tabPrefix = "\t"

	for splatName, splatDef in pairs(pbrMap.splats) do
		local splatDefDistrMap = splatDef.distrMap
		if not (splatDefDistrMap and splatDefDistrMap.get) then
			if not defaultSplatFound then
				defaultSplatFound = true
			else
				Spring.Echo("Error, more than 1 default splat specified")
			end
		end
	end

	local splatNum = (defaultSplatFound and 1) or 0

	for splatName, splatDef in pairs(pbrMap.splats) do
		local splatDefDistrMap = splatDef.distrMap
		if splatDefDistrMap and splatDefDistrMap.get then
			local scale = splatDefDistrMap.scale
			local texUnitNum = tonumber( string.match(splatDefDistrMap.get, "%[(%d-)%]") )
			if not boundTexUnits[texUnitNum] then
				Spring.Echo("Error, attempt to reference texture unit that was not bound")
			end
			local texUnitChannel = string.match(splatDefDistrMap.get, "%.(%a+)")
			local straightOrFlipped = ( (flippedTexUnits[texUnitNum] and "flippedMapUV") or "straightMapUV" )
			table.insert( splatsReadBodyTbl, string.format("%ssplatWeights[%d] = %.1f * texture(tex%d, %s, lodBias).%s;", tabPrefix, splatNum, scale, texUnitNum, straightOrFlipped, texUnitChannel) )
			splatNum = splatNum + 1
		end
	end

	local splatsReadBodyStr = table.concat(splatsReadBodyTbl, " \n")
	return splatsReadBodyStr, defaultSplatFound, splatNum
end


local function ParseEverything(pbrMap)
	local boundTexUnits, flippedTexUnits = ParseTextures(pbrMap)
	--Table.Echo({boundTexUnits = boundTexUnits, flippedTexUnits = flippedTexUnits}, "boundTexUnits, flippedTexUnits")

	local splatsReadBodyStr, defaultSplatFound, splatsCount = ParseDistrMapOfSplats(pbrMap, boundTexUnits, flippedTexUnits)
	--Spring.Echo(splatsReadBodyStr)
	--Spring.Echo("defaultSplatFound", defaultSplatFound)
	--Spring.Echo("splatsCount", splatsCount)
end

local function ParseFlagsAndUniforms(pbrMap, boundTexUnits, flippedTexUnits)
	local definitions = {}
	local uniformsSampler = {}
	local uniformsFloat = {}

	local ignoreList1 = {
		"splats", "textures", "debug"
	}

	ParseSubtable(nil, nil, pbrMap, uniformsFloat, uniformsSampler, definitions, ignoreList1)

	--[[
	for paramName, paramValue in pairs(pbrMap) do
		local paramValueType = type(paramValue)
		if paramName ~= "splats" and paramName ~= "textures" then --skip splats and textures
			if paramName == "iblMap" and paramValueType == "table" then

			elseif paramDefType == "boolean" and paramValue then
				table.insert(definitions, string.format( "#define %s", string.upper(paramName) ))
			elseif paramDefType == "number" then
				table.insert(uniformsFloat, {
					[paramName] = paramValue
				})
			end

		end
	end
	]]--
	Table.Echo(pbrMap.splats, "pbrMap.splats")

	local splatNum = 0
	for splatName, splatDef in pairs(pbrMap.splats) do
		--Spring.Echo("pbrMap.splats", splatName, splatDef)
		if type(splatDef) == "table" then
			Spring.Echo("!!!!!!!!!!splats!!!!!!!!!!")
			ParseSubtable("splats", tostring(splatNum), splatDef, uniformsFloat, uniformsSampler, definitions, nil)
			splatNum = splatNum + 1
		end
	end

	Table.Echo(uniformsFloat, "uniformsFloat")
	Table.Echo(uniformsSampler, "uniformsSampler")
	Table.Echo(definitions, "definitions")

end

local function ParsePbrMapParams()
	local mapInfo = VFS.Include("mapinfo.lua", nil, VFS.MAP)
	local pbrMap = (mapInfo.custom or {}).pbr
	if not pbrMap then
		return nil
	end

	if not pbrMap.enabled or not pbrMap.textures or not pbrMap.splats then
		return nil
	end

	-- replace lowercased values with camelCase. Whereever we can.
	pbrMap = Table.RestoreKeysCase(pbrMap, pbrMapDefaults)

	-- merge pbrMap with defaults
	pbrMap = Table.MergeWithDefaults(pbrMap, pbrMapDefaults)

	local splatsList = {}
	for splatName, _ in pairs(pbrMap.splats) do
		table.insert(splatsList, splatName)
	end

	-- need to do ^^^^^^ because pairs() are unrealiable if something is changed during looping over the table
	-- for splatName, splatDef in pairs(pbrMap.splats) do << --- doesn't work
	for _, splatName in ipairs(splatsList) do
		-- replace lowercased values with camelCase. Whereever we can.
		pbrMap.splats[splatName] = Table.RestoreKeysCase(pbrMap.splats[splatName], pbrSplatDefaults)
		-- merge splatDef with defaults
		pbrMap.splats[splatName] = Table.MergeWithDefaults(pbrMap.splats[splatName], pbrSplatDefaults)
	end

	Table.Echo(pbrMap, "pbrMap")
	ParseEverything(pbrMap)

	return true
end


local function GetFlagsTexturesUniforms()
	local mapInfo = VFS.Include("mapinfo.lua", nil, VFS.MAP)
	--GG.TableEcho(mapinfo, "mapinfo")
	local mapResources = mapInfo.resources
	local mapLighting = mapInfo.lighting
	local mapSplats = mapInfo.splats
	local mapWater = mapInfo.water
	local pbrMap = (mapInfo.custom or {}).pbr

	local definitions = {}
	local uniformsFloat = {}
	local uniformsInt = {}
	local textures = {}

	local pwr2mapx, pwr2mapy = GetNextPowerOf2({Game.mapSizeX, Game.mapSizeZ})

	table.insert(definitions, "#version 150 compatibility")
	table.insert(definitions, "#define NOSPRING")

	local hasDNTS = false
	local hasSplats = false
	local hasSplatsDistr = false

	--GG.TableEcho(mapResources, "mapResources")

	for k, v in pairs(mapResources) do
		if 		k == string.lower("detailTex") then
			--unconditional ...
		elseif 	k ==  "splatDetailTex" then
			hasSplats = true
		elseif 	k ==  string.lower("specularTex") then
			table.insert(definitions, "#define SMF_SPECULAR_LIGHTING")
		elseif 	k ==  string.lower("splatDistrTex") then
			hasSplatsDistr = true
		elseif 	k ==  string.lower("skyReflectModTex") then
			table.insert(definitions, "#define SMF_SKY_REFLECTIONS")
		elseif 	k ==  string.lower("splatDetailNormalTex1") then
			hasDNTS = true
		elseif 	k ==  string.lower("splatDetailNormalTex2") then
			hasDNTS = true
		elseif 	k ==  string.lower("splatDetailNormalTex3") then
			hasDNTS = true
		elseif 	k ==  string.lower("splatDetailNormalTex4") then
			hasDNTS = true
		elseif 	k ==  string.lower("splatDetailNormalDiffuseAlpha") then
			if v and v == 1.0 then
				table.insert(definitions, "#define SMF_DETAIL_NORMAL_DIFFUSE_ALPHA")
			end
		elseif 	k ==  string.lower("detailNormalTex") then
			table.insert(definitions, "#define SMF_BLEND_NORMALS")
		elseif 	k ==  string.lower("lightEmissionTex") then
			table.insert(definitions, "#define SMF_LIGHT_EMISSION")
		elseif 	k ==  string.lower("parallaxHeightTex") then
			table.insert(definitions, "#define SMF_PARALLAX_MAPPING")
		end
	end

	-- HAVE_INFOTEX
	if Spring.GetMapDrawMode() ~= nil then
		table.insert(definitions, "#define HAVE_INFOTEX")
		uniformsFloat["infoTexGen"] = { 1.0 / pwr2mapx, 1.0 / pwr2mapy }
	end

	-- HAVE_SHADOWS
	if Spring.HaveShadows() then
		table.insert(definitions, "#define HAVE_SHADOWS")
	end

	-- SMF_WATER_ABSORPTION
	-- SMF_VOID_WATER
	if gl.GetMapRendering("voidWater") then
		table.insert(definitions, "#define SMF_VOID_WATER")
	else
		table.insert(definitions, "#define SMF_WATER_ABSORPTION")
	end

	-- SMF_VOID_GROUND
	if gl.GetMapRendering("voidGround") then
		table.insert(definitions, "#define SMF_VOID_GROUND")
	end

	-- SMF_DETAIL_TEXTURE_SPLATTING
	if hasSplats and hasSplatsDistr then
		table.insert(definitions, "#define SMF_DETAIL_TEXTURE_SPLATTING")
	end

	-- SMF_DETAIL_NORMAL_TEXTURE_SPLATTING
	if hasDNTS and hasSplatsDistr then
		table.insert(definitions, "#define SMF_DETAIL_NORMAL_TEXTURE_SPLATTING")
	end

	-- SSMF_UNCOMPRESSED_NORMALS
	-- always compressed by default? Represent geometry normals!

	-- BASE_DYNAMIC_MAP_LIGHT
	-- MAX_DYNAMIC_MAP_LIGHTS
	-- ^^ not supported yet, figure out what to do with them. NOSPRING defines these values

	-- DEFERRED_MODE
	-- ^^ not supported yet, figure out what to do with it

	for k, v in pairs(mapSplats) do
		if 		k == string.lower("texScales") then
			uniformsFloat["splatTexScales"] = v
		elseif 	k == string.lower("texMults") then
			uniformsFloat["splatTexMults"] = v
		end
	end

	for k, v in pairs(mapWater) do
		if 		k == string.lower("minColor") then
			uniformsFloat["waterMinColor"] = v
		elseif 	k == string.lower("baseColor") then
			uniformsFloat["waterBaseColor"] = v
		elseif	k == string.lower("absorb") then
			uniformsFloat["waterAbsorbColor"] = v
		end
	end

	for k, v in pairs(mapLighting) do
		if 		k == string.lower("groundAmbientColor") then
			uniformsFloat["groundAmbientColor"] = { gl.GetSun("ambient") }
		elseif 	k == string.lower("groundDiffuseColor") then
			uniformsFloat["groundDiffuseColor"] = { gl.GetSun("diffuse") }
		elseif	k == string.lower("groundSpecularColor") then
			uniformsFloat["groundSpecularColor"] = { gl.GetSun("specular") }
		elseif	k == string.lower("groundSpecularExponent") then
			uniformsFloat["groundSpecularExponent"] = gl.GetSun("specularExponent")
		elseif	k == string.lower("groundShadowDensity") then
			uniformsFloat["groundShadowDensity"] = gl.GetSun("shadowDensity")
		end
	end

	if gl.HasExtension("GL_ARB_texture_non_power_of_two") then
		uniformsFloat["normalTexGen"] = { 1.0/Game.mapSizeX, 1.0/Game.mapSizeZ }
	else
		uniformsFloat["normalTexGen"] = { 1.0/pwr2mapx, 1.0/pwr2mapy }
	end

	uniformsFloat["specularTexGen"] = { 1.0/Game.mapSizeX, 1.0/Game.mapSizeZ }

	--uniformsFloat["infoTexIntensityMul"] = ((Spring.GetMapDrawMode() == "metal") and 1.0 or 0.0) + 1.0

	local texUnitUniforms = {
		diffuseTex = 0,
		detailTex = 2,
		shadowTex = 4,
		normalsTex = 5,
		specularTex = 6,
		splatDetailTex = 7,
		splatDistrTex = 8,
		skyReflectModTex = 10,
		blendNormalsTex = 11,
		lightEmissionTex = 12,
		parallaxHeightTex = 13,
		infoTex = 14,
		splatDetailNormalTex1 = 15,
		splatDetailNormalTex2 = 16,
		splatDetailNormalTex3 = 17,
		splatDetailNormalTex4 = 18,
	}

	for k, v in pairs(texUnitUniforms) do
		uniformsInt[k] = v
	end

	for k, v in pairs(definitions) do
		definitions[k] = v .. "\n"
	end

	return definitions, uniformsFloat, uniformsInt, textures
end


--  Gadget Global Vars  --
local fwdShaderObjValid
local fwdShaderObj

local updateHeights

local oldSunPos = {-1, -1, -1}
local updateSunPos

local firstTime
local genBrdfLut
-- /Gadget Global Vars/ --

local function InitGlobalVars()
	fwdShaderObjValid = false
	fwdShaderObj = nil

	firstTime = true
	updateHeights = true

	oldSunPos = {-1, -1, -1}
	updateSunPos = false

	genBRDFLUT = nil
end


function gadget:Initialize()
	if ParsePbrMapParams() == nil then
		Spring.Echo("Map PBR is not enabled on this map, unloading gadget")
		gadgetHandler:RemoveGadget()
		return
	else
		Spring.Echo("Map PBR is enabled on this map, loading gadget")
	end

	InitGlobalVars()

	local BRDFLUT_TEXDIM = 512 --512 is BRDF LUT texture size
	genBrdfLut = GenBRDFLUT(BRDFLUT_TEXDIM)
	genBrdfLut:Initialize()

	local definitions, uniformsFloat, uniformsInt, textures = GetFlagsTexturesUniforms()

	--GG.TableEcho({ definitions, uniformsFloat, uniformsInt, textures }, "GetFlagsTexturesUniforms")


	local vertCode = VFS.LoadFile("PBR/pbrMap.vert", VFS.MAP)
	local fragCode = VFS.LoadFile("PBR/pbrMap.frag", VFS.MAP)

	--Spring.Echo(vertCode, fragCode)

	fwdShaderObj = LuaShader({
		definitions = definitions,
		vertex = vertCode,
		fragment = fragCode,
		uniformFloat = uniformsFloat,
		uniformInt = uniformsInt,

	}, "PBR Map Shader (Forward)")
	fwdShaderObjValid = fwdShaderObj:Initialize()

	if fwdShaderObjValid then
		Spring.SetMapShader(fwdShaderObj:GetHandle(), 0)
	end
end

function gadget:Update(dt)
	local newSunX, newSunY, newSunZ = gl.GetSun("pos")
	if (newSunX ~= oldSunPos[1] or newSunY ~= oldSunPos[2] or newSunZ ~= oldSunPos[3]) then
		--Spring.Echo("updateSunPos", newSunX, newSunY, newSunZ)
		oldSunPos = { newSunX, newSunY, newSunZ }
		updateSunPos = true
	end
end

function gadget:UnsyncedHeightMapUpdate()
	updateHeights = true
end


local function UpdateSomeUniforms()
	if firstTime then
		genBrdfLut:Execute(false)
		firstTime = false
	end

	fwdShaderObj:ActivateWith( function()

		if updateHeights then
			fwdShaderObj:SetUniformFloat("mapHeights", Spring.GetGroundExtremes())
			updateHeights = false
		end

		if updateSunPos then
			local sunPosX, sunPosY, sunPosZ = gl.GetSun("pos")
			fwdShaderObj:SetUniformFloatAlways("lightDir", sunPosX, sunPosY, sunPosZ, 0.0)
			updateSunPos = false
		end

		local drawMode = Spring.GetMapDrawMode() or "nil"
		fwdShaderObj:SetUniformFloat("infoTexIntensityMul", ((drawMode == "metal") and 1.0 or 0.0) + 1.0)
		--fwdShaderObj:SetUniformFloat("cameraPos", Spring.GetCameraPosition())
	end)
end

local function BindTextures()
	-- diffuseTex = 0,
	-- ^^ Is bound by engine

	-- detailTex = 2,
	gl.Texture(2, "$detail")

	-- shadowTex = 4,
	gl.Texture(4, "$shadow")

	-- normalsTex = 5,
	gl.Texture(5, "$normals")

	-- specularTex = 6,
	gl.Texture(6, "$ssmf_specular")

	-- splatDetailTex = 7,
	gl.Texture(7, "$ssmf_splat_detail")

	-- splatDistrTex = 8,
	gl.Texture(8, "$ssmf_splat_distr")

	-- skyReflectModTex = 10,
	gl.Texture(10, "$sky_reflection")

	-- blendNormalsTex = 11,
	gl.Texture(11, "$ssmf_normals")

	-- lightEmissionTex = 12,
	gl.Texture(12, "$ssmf_emission")

	-- parallaxHeightTex = 13,
	gl.Texture(13, "$ssmf_parallax")

	-- infoTex = 14,
	gl.Texture(14, "$info")

	-- splatDetailNormalTex1 = 15,
	gl.Texture(15, "$ssmf_splat_normals:0")
	-- splatDetailNormalTex2 = 16,
	gl.Texture(16, "$ssmf_splat_normals:1")
	-- splatDetailNormalTex3 = 17,
	gl.Texture(17, "$ssmf_splat_normals:2")
	-- splatDetailNormalTex4 = 18,
	gl.Texture(18, "$ssmf_splat_normals:3")
end

function gadget:GotChatMsg(msg, player)
	local pbrmapFound = string.find(msg, "pbrmap")
	if pbrmapFound == nil then
		pbrmapFound = string.find(msg, "mappbr")
	end
	local pbrmapNumStart, pbrmapNumEnd = string.find(msg, "%d+")
	local pbrmapNum
	if pbrmapNumStart then
		pbrmapNum = string.sub(msg, pbrmapNumStart, pbrmapNumEnd)
	end

	local pbrmapReloadFound = string.find(msg, "reload")

	--Spring.Echo(msg, pbrmapFound, pbrmapNumStart, pbrmapNumEnd, pbrmapNum, pbrmapReloadFound)
	if pbrmapFound and (pbrmapNum or pbrmapReloadFound) then
		if 		pbrmapNum == "0" then
			Spring.Echo("Disabling map PBR")
			Spring.SetMapShader(0, 0)
		elseif	pbrmapNum == "1" then
			if fwdShaderObjValid then
				Spring.Echo("Enabling map PBR")
				Spring.SetMapShader(fwdShaderObj:GetHandle(), 0)
			end
		elseif	pbrmapReloadFound then
			Spring.Echo("Reloading map PBR gadget")
			gadget:Shutdown()
			gadget:Initialize()
			--gadgetHandler:ToggleGadget(gadget:GetInfo().name)
		end
	end
end


function gadget:DrawWorldShadow()
	--Spring.Echo("gadget:DrawWorldShadow()")
	fwdShaderObj:ActivateWith( function()
		fwdShaderObj:SetUniformMatrixAlways("shadowMat", gl.GetMatrixData("shadow"))
		fwdShaderObj:SetUniformFloat("shadowParams", gl.GetShadowMapParams())
	end)

end

-- Shadow matrix and shadow params are wrong here!!!
-- Use gadget:DrawWorldShadow() for shadows instead
-- TODO make sure DrawGroundPreForward is mapped
function gadget:DrawGroundPreForward()
	if fwdShaderObjValid then
		CallAsTeam(Spring.GetMyTeamID(),  function()
			UpdateSomeUniforms()
			--BindTextures()
			-- ^^ somehow bound by engine
		end)
	end
end



function gadget:Shutdown()
	genBrdfLut:Finalize()
	if fwdShaderObjValid then
		fwdShaderObj:Finalize()

		fwdShaderObjValid = false
		fwdShaderObj = nil

		Spring.SetMapShader(0, 0)
	end
end
