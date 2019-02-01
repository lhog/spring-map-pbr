function gadget:GetInfo()
	return {
		name      = "Map PBR Shader",
		desc      = "Applies PBR to map surface",
		author    = "ivand",
		date      = "2018-2019",
		license   = "PD",
		layer     = 0,
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

function Table.ToString(tbl, format)
	local result = ""
	for _, v in ipairs(tbl) do
		result = result .. string.format(format, v)
	end
	return result
end

local pbrSplatDefaults = {
	workflow = "METALNESS",

	weight = "0.0",

	diffuseColor = "vec3(0.0)", -- Only in use when workflow == SPECULAR. Use fromSRGB() in case textures are in non-linear (sRGB) space.
	specularColor = "vec3(0.0)", -- Only in use when workflow == SPECULAR. Use fromSRGB() in case textures are in non-linear (sRGB) space.

	baseColor = "vec3(0.0)", -- Only in use when workflow == METALNESS. Use fromSRGB() in case textures are in non-linear (sRGB) space.

	blendNormal = "vec3(0.0, 0.0, 1.0)", -- TBN space normals, that will be blended into map normals

	height = "0.0", -- Used for height based blending and POM
	pomScale = "0.0",
	emissionColor = "vec3(0.0)", -- Use fromSRGB() in case textures are in non-linear (sRGB) space.
	occlusion = "1.0", -- 1.0 is unoccluded.
	specularF0 = "0.04", -- More often than not you won't be using this. Decrease to 0.0 (clamped to MIN_SPECULAR_F0) to get rid of reflections at grazing angles for non-metallic materials.
	roughness = "0.0", -- To convert from glossiness use 1.0 - <glossiness>

	metalness = "0.0", -- Only in use when workflow == METALNESS.
}

local function LuaToGLSL(luaData)
	local ldt = type(luaData)
	if ldt == "table" then
		N = #luaData
		if (N == 1) then
			return string.format("%.1f", luaData[1])
		elseif (N == 2) then
			return string.format("vec2(%.1f, %.1f)", luaData[1], luaData[2])
		elseif (N == 3) then
			return string.format("vec3(%.1f, %.1f, %.1f)", luaData[1], luaData[2], luaData[3])
		elseif (N == 4) then
			return string.format("vec4(%.1f, %.1f, %.1f, %.1f)", luaData[1], luaData[2], luaData[3], luaData[4])
		else
			return nil
		end
	elseif ldt == "number" then
		return string.format("%.1f", luaData)
	else
		return string.format("%.1f", tonumber(luaData))
	end
end

local pbrMapDefaultDefinitions = {
	["SUN_COLOR"] = LuaToGLSL({gl.GetSun("diffuse")}),
	["TERRAIN_NORMAL_BLEND"] = "", -- Detailed Terrain Normals Texture. Optional, Must be in TBN space
	["FAST_GAMMA"] = "0", -- Faster gamma correction makes darker area apear brighter. Doesn't look to good.
	["WEIGHT_CUTOFF"] = "10.0/255.0",
	["POM_MAXSTEPS"] = "32",
	["PBR_SCHLICK_SMITH_GGX"] = "PBR_SCHLICK_SMITH_GGX_THIN", -- PBR_SCHLICK_SMITH_GGX_THICK seems to give less glossiness on non-metallic surfaces
	["PBR_BRDF_DIFFUSE"] = "PBR_DIFFUSE_LAMBERT",  -- Others give almost same picture, but are more expensive
	["MAT_BLENDING_HEIGHT_SMOOTHNESS"] = "", -- weights based blending if empty, height based blending with height smoothness factor otherwise
	["PBR_F_SCHLICK"] = "PBR_F_SCHLICK_KHRONOS",
	["PBR_R90_METHOD"] = "PBR_R90_METHOD_GOOGLE",
	["OUTPUT_EXPOSURE(preExpColor)"] = "preExpColor",
	["OUTPUT_TONEMAPPING(preTMColor)"] = "preTMColor", -- See full list of TM operators in the shader code
	["OUTPUT_GAMMACORRECTION(preGammaColor)"] = "toSRGB(preGammaColor)",
	["SHADOW_SAMPLES"] = "3", -- number of shadow map samples, "1" will revert to standard spring shadows
	["IBL_SPECULAR_LOD_BIAS"] = "0", -- positive number will make all cubemap reflections blurry by this LOD value.
	["IBL_DIFFUSECOLOR"] = "",  -- replaces IBL diffuse sampling result with color value defined here
	["IBL_SPECULARCOLOR"] = "", -- replaces IBL specular sampling result with color value defined here
	["IBL_GAMMACORRECTION(color)"] = "color", --change to "fromSRGB(color)" if you feel IBL gamma correction is required
	["IBL_INVERSE_TONEMAP_MUL"] = "", -- expExpand() mul param
	["IBL_SCALE_DIFFUSE(color)"] = "color",
	["IBL_SCALE_SPECULAR(color)"] = "color",
}

local pbrMapDefaults = {
	definitions = {},
	customCode = "",
	splats = {},
	textures = {},
	debug = {},
}

local function ParseTextures(pbrMap)
	local TEXTURE_DIR = "maps/"

	local boundTexUnits = {}

	for tu, texName in pairs(pbrMap.textures) do
		local tun = tonumber(tu)

		if boundTexUnits[tun] then
			Spring.Echo(string.format( "[%s]: Texture unit (%d) is referenced more than once", gadget:GetInfo().name, tun ))
		end

		if texName:len() >= 1 then
			if texName:find("%$") then
				if gl.TextureInfo(texName) then
					boundTexUnits[tun] = texName
				else
					Spring.Echo(string.format( "[%s]: Failed to find PBR Lua texture (%s) to be bound to texture unit %d", gadget:GetInfo().name, texName, tun ))
				end
			else
				--filter out gl.Texture options to get filename
				local s, e = string.find(texName, ":.-:")
				local texOpt = ""
				if s and e then
					texOpt = string.sub(texName, s, e)
				end
				local fileName = string.gsub(texName, ":.-:", "")
				local newFilePath = TEXTURE_DIR .. fileName
				if VFS.FileExists(newFilePath) then
					boundTexUnits[tun] = texOpt .. newFilePath --keep :{opts}:
				else
					Spring.Echo(string.format( "[%s]: Failed to find PBR texture file (%s) to be bound to texture unit %d", gadget:GetInfo().name, newFilePath, tun ))
				end
			end
		end
	end

	return boundTexUnits
end

local specularWFSplatTemplate =
[[	{
		const float epsilon = 1e-6;

		vec3 diffuseColor = ###DIFFUSE_COLOR###;
		vec3 specularColor = ###SPECULAR_COLOR###;
		float maxSpecular = max(max(specularColor.r, specularColor.g), specularColor.b);

		float specularF0 = clamp(###SPECULAR_F0###, MIN_SPECULAR_F0, 1.0);
		float metalness = ConvertToMetalness(diffuseColor, specularColor, maxSpecular, specularF0);

		vec3 baseColorDiffusePart = diffuseColor * ((1.0 - maxSpecular) / (1 - specularF0) / max(1.0 - metalness, epsilon));
		vec3 baseColorSpecularPart = specularColor - (vec3(specularF0) * (1.0 - metalness) * (1.0 / max(metalness, epsilon)));

		vec3 baseColor = mix(baseColorDiffusePart, baseColorSpecularPart, metalness * metalness);
		baseColor = clamp( baseColor, vec3(0.0), vec3(1.0) );

		material[###MAT_NUM###].baseColor = baseColor;

		material[###MAT_NUM###].blendNormal = ###BLEND_NORMAL###;

		material[###MAT_NUM###].pomScale = ###POM_SCALE###;
		material[###MAT_NUM###].emissionColor = ###EMISSION_COLOR###;
		material[###MAT_NUM###].occlusion = ###OCCLUSION###;
		material[###MAT_NUM###].specularF0 = specularF0;
		material[###MAT_NUM###].roughness = ###ROUGHNESS###;

		material[###MAT_NUM###].metalness = metalness;
	}]]

local metalnessWFSplatTemplate =
[[	{
		material[###MAT_NUM###].baseColor = ###BASE_COLOR###;

		material[###MAT_NUM###].blendNormal = ###BLEND_NORMAL###;

		material[###MAT_NUM###].pomScale = ###POM_SCALE###;
		material[###MAT_NUM###].emissionColor = ###EMISSION_COLOR###;
		material[###MAT_NUM###].occlusion = ###OCCLUSION###;
		material[###MAT_NUM###].specularF0 = clamp(###SPECULAR_F0###, MIN_SPECULAR_F0, 1.0);
		material[###MAT_NUM###].roughness = ###ROUGHNESS###;

		material[###MAT_NUM###].metalness = ###METALNESS###;
	}]]


local function ParseSplats(pbrMap)
	local hasDefaultSplat = false
	local splatsCode = ""
	local splatsWeightHeightCode = ""

	local splatNum = 0
	for splatId, splatDef in pairs(pbrMap.splats) do
		if splatDef.weight == pbrSplatDefaults.weight then
			if splatNum ~= 0 then
				Spring.Echo("Error: Default splat must be first in the splats array")
			else
				if hasDefaultSplat == false then
					hasDefaultSplat = true
				else
					Spring.Echo("Error: Too many default splats are defined")
				end
			end
		end

		splatsWeightHeightCode = splatsWeightHeightCode .. string.format("\tmaterial[%d].weight = %s;\n", splatNum, splatDef.weight)
		splatsWeightHeightCode = splatsWeightHeightCode .. string.format("\tmaterial[%d].height = %s;\n", splatNum, splatDef.height)

		local splatCode = ""

		if splatDef.workflow == "METALNESS" then
			splatCode = string.format("%s", metalnessWFSplatTemplate)

			splatCode = splatCode:gsub("###MAT_NUM###", splatNum)

			splatCode = splatCode:gsub("###BASE_COLOR###", splatDef.baseColor)

			splatCode = splatCode:gsub("###BLEND_NORMAL###", splatDef.blendNormal)

			splatCode = splatCode:gsub("###POM_SCALE###", splatDef.pomScale)
			splatCode = splatCode:gsub("###EMISSION_COLOR###", splatDef.emissionColor)
			splatCode = splatCode:gsub("###OCCLUSION###", splatDef.occlusion)
			splatCode = splatCode:gsub("###SPECULAR_F0###", splatDef.specularF0)
			splatCode = splatCode:gsub("###ROUGHNESS###", splatDef.roughness)

			splatCode = splatCode:gsub("###METALNESS###", splatDef.metalness)

		elseif splatDef.workflow == "SPECULAR" then
			splatCode = string.format("%s", specularWFSplatTemplate)

			splatCode = splatCode:gsub("###MAT_NUM###", splatNum)

			splatCode = splatCode:gsub("###DIFFUSE_COLOR###", splatDef.diffuseColor)
			splatCode = splatCode:gsub("###SPECULAR_COLOR###", splatDef.specularColor)

			splatCode = splatCode:gsub("###BLEND_NORMAL###", splatDef.blendNormal)

			splatCode = splatCode:gsub("###POM_SCALE###", splatDef.pomScale)
			splatCode = splatCode:gsub("###EMISSION_COLOR###", splatDef.emissionColor)
			splatCode = splatCode:gsub("###OCCLUSION###", splatDef.occlusion)
			splatCode = splatCode:gsub("###SPECULAR_F0###", splatDef.specularF0)
			splatCode = splatCode:gsub("###ROUGHNESS###", splatDef.roughness)

		else
			Spring.Echo("Error: Wrong workflow name")
		end

		splatsCode = splatsCode .. "\n\n" .. splatCode
		splatNum = splatNum + 1
	end

	return splatsCode, splatsWeightHeightCode, hasDefaultSplat, splatNum
end

local function ParseEverything()
	local mapInfo = VFS.Include("mapinfo.lua", nil, VFS.MAP)
	local pbrMap = (mapInfo.custom or {}).pbr
	if not pbrMap then
		Spring.Echo("Map PBR is not enabled on this map, unloading gadget")
		gadgetHandler:RemoveGadget()
	end

	if not pbrMap.enabled or not pbrMap.textures or not pbrMap.splats then
		Spring.Echo("Map PBR is not enabled on this map, unloading gadget")
		gadgetHandler:RemoveGadget()
	end

	Spring.Echo("Map PBR is enabled on this map, loading gadget")

	-- replace lowercased values with camelCase. Whereever we can.
	pbrMap = Table.RestoreKeysCase(pbrMap, pbrMapDefaults)
	-- merge pbrMap with defaults
	pbrMap = Table.MergeWithDefaults(pbrMap, pbrMapDefaults)

	-- replace lowercased values with camelCase. Whereever we can.
	pbrMap.definitions = Table.RestoreKeysCase(pbrMap.definitions, pbrMapDefaultDefinitions)
	-- merge pbrMap with defaults
	pbrMap.definitions = Table.MergeWithDefaults(pbrMap.definitions, pbrMapDefaultDefinitions)

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


	local boundTexUnits, samplerTypes = ParseTextures(pbrMap)

	local samplerUniformsCode = ""
	for tun, _ in pairs(boundTexUnits) do
		samplerUniformsCode = samplerUniformsCode .. string.format("uniform sampler2D tex%d;\n", tun)
	end

	local splatsCode, splatsWeightHeightCode, hasDefaultSplat, splatCount = ParseSplats(pbrMap)
	pbrMap.definitions["HAS_DEFAULT_SPLAT"] = (hasDefaultSplat and "1") or "0"

	if pbrMap.definitions["TERRAIN_NORMAL_BLEND"] ~= "" then
		pbrMap.definitions["TERRAIN_NORMAL_BLEND_DO"] = "1"
	end

	if pbrMap.definitions["IBL_INVERSE_TONEMAP_MUL"] ~= "" then
		pbrMap.definitions["IBL_INVERSE_TONEMAP"] = "1"
	end

	if pbrMap.definitions["IBL_DIFFUSECOLOR"] ~= "" then
		pbrMap.definitions["IBL_DIFFUSECOLOR_STATIC"] = "1"
	end

	if pbrMap.definitions["IBL_SPECULARCOLOR"] ~= "" then
		pbrMap.definitions["IBL_SPECULARCOLOR_STATIC"] = "1"
	end

	if pbrMap.definitions["MAT_BLENDING_HEIGHT_SMOOTHNESS"] ~= "" then
		pbrMap.definitions["MAT_BLENDING_HEIGHT"] = "1"
	end

	-- Standard spring definitions
	-- HAVE_INFOTEX
	if Spring.GetMapDrawMode() ~= nil then
		pbrMap.definitions["HAVE_INFOTEX"] = "1"
	end

	-- HAVE_SHADOWS
	if Spring.HaveShadows() then
		pbrMap.definitions["HAVE_SHADOWS"] = "1"
	end


	local customDefinitions = ""
	for defKey, defVal in pairs(pbrMap.definitions) do
		customDefinitions = customDefinitions .. string.format("#define %s %s\n", defKey, defVal)
	end

	return samplerUniformsCode, splatsCode, splatsWeightHeightCode, customDefinitions, pbrMap.customCode, splatCount, boundTexUnits
end



--  Gadget Global Vars  --
local fwdShaderObjValid = false
local fwdShaderObj = nil

local firstTime = true
local updateHeights = true

local oldSunPos = {-1, -1, -1}
local updateSunPos = false

local genBrdfLut = nil
local boundTexUnits = nil
-- /Gadget Global Vars/ --

function gadget:Initialize()

	local samplerUniformsCode, splatsCode, splatsWeightHeightCode, customDefinitions, customCode, splatCount

	samplerUniformsCode, splatsCode, splatsWeightHeightCode,
	customDefinitions, customCode, splatCount, boundTexUnits = ParseEverything()

	local boundSamplers = {}
	for tun, _ in pairs(boundTexUnits) do
		boundSamplers[string.format("tex%d", tun)] = tun
	end

	boundSamplers["diffuseTex"] = 0

	boundSamplers["shadowTex"] = 27
	boundSamplers["shadowTexDepth"] = 27
	boundSamplers["infoTex"] = 28
	boundSamplers["terrainNormalsTex"] = 29
	boundSamplers["reflectionTex"] = 30
	boundSamplers["brdfTex"] = 31

	--Table.Echo(boundSamplers, "boundSamplers")

	uniformsFloat = {}
	local pwr2mapx, pwr2mapy = GetNextPowerOf2({Game.mapSizeX, Game.mapSizeZ})

	uniformsFloat["mapTexGen"] = { 1.0/Game.mapSizeX, 1.0/Game.mapSizeZ }
	uniformsFloat["infoTexGen"] = { 1.0 / pwr2mapx, 1.0 / pwr2mapy }

	if gl.HasExtension("GL_ARB_texture_non_power_of_two") then
		uniformsFloat["normalTexGen"] = { 1.0/Game.mapSizeX, 1.0/Game.mapSizeZ }
	else
		uniformsFloat["normalTexGen"] = { 1.0/pwr2mapx, 1.0/pwr2mapy }
	end

	uniformsFloat["groundShadowDensity"] = gl.GetSun("shadowDensity")

	local BRDFLUT_TEXDIM = 512 --512 is BRDF LUT texture size
	genBrdfLut = GenBRDFLUT(BRDFLUT_TEXDIM)
	genBrdfLut:Initialize()

	local vertCodeTmpl = VFS.LoadFile("PBR/pbrMap.vert", VFS.MAP)
	local fragCodeTmpl = VFS.LoadFile("PBR/pbrMap.frag", VFS.MAP)

	local vertCode = string.format("%s", vertCodeTmpl)
	vertCode = vertCode:gsub("###CUSTOM_DEFINITIONS###", customDefinitions)

	local fragCode = string.format("%s", fragCodeTmpl)
	fragCode = fragCode:gsub("###CUSTOM_DEFINITIONS###", customDefinitions)
	fragCode = fragCode:gsub("###SAMPLER_UNIFORMS###", samplerUniformsCode)
	fragCode = fragCode:gsub("###CUSTOM_CODE###", customCode)
	fragCode = fragCode:gsub("###MATERIALS_COUNT###", splatCount)
	fragCode = fragCode:gsub("###MATERIAL_WEIGHTS_HEIGHTS###", splatsWeightHeightCode)
	fragCode = fragCode:gsub("###MATERIAL_PARAMS###", splatsCode)

	Spring.Echo("vertCode = \n\n\n\n", vertCode)
	Spring.Echo("fragCode = \n\n\n\n", fragCode)

	fwdShaderObj = LuaShader({
		definitions = definitions,
		vertex = vertCode,
		fragment = fragCode,
		uniformFloat = uniformsFloat,
		uniformInt = boundSamplers,

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


local SHADOW_CAMERA_ID = 2
local function UpdateSomeUniforms()
	if firstTime then
		--genBrdfLut:Execute(true)
		gl.PushPopMatrix(function()
			gl.MatrixMode(GL.PROJECTION); gl.LoadIdentity();
			gl.MatrixMode(GL.MODELVIEW); gl.LoadIdentity();
			genBrdfLut:Execute(false)
		end)
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
			Spring.Echo("sunPos", sunPosX, sunPosY, sunPosZ)
			updateSunPos = false
		end

		local drawMode = Spring.GetMapDrawMode() or "nil"
		fwdShaderObj:SetUniformFloat("infoTexIntensityMul", ((drawMode == "metal") and 1.0 or 0.0) + 1.0)

		--local gf = Spring.GetGameFrame()
		--fwdShaderObj:SetUniformFloat("gameFrame", gf)

		local lightProjNear, lightProjFar = gl.GetViewRange(SHADOW_CAMERA_ID)
		fwdShaderObj:SetUniformFloat("lightProjNF", lightProjNear, lightProjFar)
	end)
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
			--Spring.Echo("Reloading map PBR gadget")
			--gadget:Shutdown()
			--gadget:Initialize()
			--gadgetHandler:ToggleGadget(gadget:GetInfo().name)
		end
	end
end

function gadget:DrawWorldShadow()
	--Spring.Echo("gadget:DrawWorldShadow()")
	fwdShaderObj:ActivateWith( function()
		Spring.Echo("shadowMat", gl.GetMatrixData("shadow"))
		fwdShaderObj:SetUniformMatrixAlways("shadowMat", gl.GetMatrixData("shadow"))
		fwdShaderObj:SetUniformFloat("shadowParams", gl.GetShadowMapParams())
	end)

end

local function BindTextures()
	gl.Texture(27, "$shadow")
	gl.Texture(28, "$info")
	gl.Texture(29, "$normals")
	gl.Texture(30, "$reflection")
	gl.Texture(31, genBrdfLut:GetTexture())

	if boundTexUnits then
		for tun, def in pairs(boundTexUnits) do
			gl.Texture(tun, def)
		end
	end
end

-- Shadow matrix and shadow params are wrong here!!!
-- Use gadget:DrawWorldShadow() for shadows instead
-- TODO make sure DrawGroundPreForward is mapped
function gadget:DrawGroundPreForward()
	if fwdShaderObjValid then
		CallAsTeam(Spring.GetMyTeamID(),  function()
			UpdateSomeUniforms()
			BindTextures()
		end)
	end
end

function gadget:Shutdown()
	genBrdfLut:Finalize()
	if fwdShaderObjValid then
		fwdShaderObj:Finalize()
		Spring.SetMapShader(0, 0)
	end
end
