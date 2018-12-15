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

local function GetNextPowerOf2(tbl)
	local npot = {}
	for i = 1, #tbl do
		npot[i] = math.pow(2, math.ceil(math.log(tbl[i])/math.log(2)))
	end

	return unpack(npot)
end

local function GetFlagsTexturesUniforms()
	local mapInfo = VFS.Include("mapinfo.lua", nil, VFS.MAP)
	--GG.TableEcho(mapinfo, "mapinfo")
	local mapResources = mapInfo.resources
	local mapLighting = mapInfo.lighting
	local mapSplats = mapInfo.splats
	local mapWater = mapInfo.water
	local mapPBR = (mapInfo.custom or {}).pbr

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
-- /Gadget Global Vars/ --

local function InitGlobalVars()
	fwdShaderObjValid = false
	fwdShaderObj = nil

	firstTime = true
	updateHeights = true

	oldSunPos = {-1, -1, -1}
	updateSunPos = false
end

function gadget:Initialize()

	InitGlobalVars()

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
		--blabla
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

-- Shadow matrix and shadow params are wrong here!!!
-- Use gadget:DrawWorldShadow() for shadows instead
function gadget:DrawGenesis()
	if fwdShaderObjValid then
		CallAsTeam(Spring.GetMyTeamID(),  function()
			UpdateSomeUniforms()
			BindTextures()
		end)
	end
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
		end
	end
end


function gadget:DrawWorldShadow()
	fwdShaderObj:ActivateWith( function()
		fwdShaderObj:SetUniformMatrixAlways("shadowMat", gl.GetMatrixData("shadow"))
		fwdShaderObj:SetUniformFloat("shadowParams", gl.GetShadowMapParams())
	end)

end



function gadget:Shutdown()
	if fwdShaderObjValid then
		fwdShaderObj:Finalize()

		fwdShaderObjValid = false
		fwdShaderObj = nil

		Spring.SetMapShader(0, 0)
	end
end