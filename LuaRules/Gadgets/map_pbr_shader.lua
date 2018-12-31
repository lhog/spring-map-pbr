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

local pbrSplatDefaults = {
	weight = "0.0;",
	baseColor = "vec3(0.0);",
	normals = "vec3(0.0);",
	bump = "0.0;",
	pomMaxSteps = "32;",
	pomScale = "0.0;",
	emissionColor = "vec3(0.0);",
	occlusion = "1.0;",
	specularF0 = "0.04;",
	roughness = "0.0;", -- to convert from glossiness use 1.0 - <glossiness>
	metalness = "0.0;", -- in case of specular WF, use ConvertToMetalness() call
}

local pbrMapDefaultDefinitions = {
	["HAS_DEFAULT_SPLAT"] = "0", -- Default splat (if used) is always the first splat in splats array
	["FAST_GAMMA"] = "0",
	["DO_POM"] = "1",
	["MAT_BLENDING"] = "MAT_BLENDING_WEIGHT",
	["EXPOSURE(preExpColor)"] = "preExpColor",
	["TONEMAPPING(preTMColor)"] = "preTMColor", -- See full list of TM operators in the shader code.
	["IBL_INVERSE_TONEMAP(color)"] = "color", -- Might want to use expExpand(). See details in the shader code
	["IBL_SCALE_DIFFUSE(color)"] = "color",
	["IBL_SCALE_SPECULAR(color)"] = "color",
}

local pbrMapDefaults = {
	finalColor = "toSRGB(postTMColor);",
	definitions = pbrMapDefaultDefinitions,
	customCode = "",
	splats = {},
	textures = {
		"%%%BRDF%%%" = 30,
		--something for irradiance = 31,
		"$reflection" = 32,
	},
	debug = {},
}

local function ParseTextures(pbrMap)
	local TEXTURE_DIR = "maps/"

	local boundTexUnits = {}

	for texName, tu in pairs(pbrMap.textures) do
		local tun = tonumber(tu)
		if texName:len() >= 1 then
			if texName[1] == "$" then
				if gl.TextureInfo(texName) then
					boundTexUnits[tun] = texName
				else
					Spring.Echo(string.format( "[%s]: Failed to find PBR Lua texture (%s) to be bound to texture unit %d", gadget:GetInfo().name, texName, tun ))
				end
			elseif texName = "%%%BRDF%%%" then
				boundTexUnits[tun] = texName -- just mark
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

local function ParseEverything(pbrMap)
	local boundTexUnits = ParseTextures(pbrMap)
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
