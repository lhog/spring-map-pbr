function gadget:GetInfo()
	return {
		name      = "Map PBR Shader",
		desc      = "Applies PBR to map surface",
		author    = "ivand",
		date      = "2018-2019",
		license   = " ",
		layer     = 1,
		enabled   = true
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

	local hasSplatDetailNormalTex = false

	--GG.TableEcho(mapResources, "mapResources")

	for k, v in pairs(mapResources) do
		if 		k == string.lower("detailTex") then

		--elseif 	k ==  "splatDetailTex" then

		elseif 	k ==  string.lower("splatDistrTex") then

		elseif 	k ==  string.lower("skyReflectModTex") then
			table.insert(definitions, "SMF_SKY_REFLECTIONS")
		elseif 	k ==  string.lower("splatDetailNormalTex1") then
			hasSplatDetailNormalTex = true
		elseif 	k ==  string.lower("splatDetailNormalTex2") then
			hasSplatDetailNormalTex = true
		elseif 	k ==  string.lower("splatDetailNormalTex3") then
			hasSplatDetailNormalTex = true
		elseif 	k ==  string.lower("splatDetailNormalTex4") then
			hasSplatDetailNormalTex = true
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
		uniformsFloat["infoTexGen"] = { 1.0 / (pwr2mapx * Game.squareSize), 1.0 / (pwr2mapy * Game.squareSize) }
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

	-- SMF_SPECULAR_LIGHTING
	-- ^^ not supported, replaced by PBR, dummy texture should be provided

	-- SMF_DETAIL_TEXTURE_SPLATTING
	-- ^^ not supported

	-- SMF_DETAIL_NORMAL_TEXTURE_SPLATTING
	if hasSplatDetailNormalTex then
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
			uniformsFloat["groundAmbientColor"] = v
		elseif 	k == string.lower("groundDiffuseColor") then
			uniformsFloat["groundDiffuseColor"] = v
		elseif	k == string.lower("groundSpecularColor") then
			uniformsFloat["groundSpecularColor"] = v
		elseif	k == string.lower("groundSpecularExponent") then
			uniformsFloat["groundSpecularExponent"] = v
		elseif	k == string.lower("groundShadowDensity") then
			uniformsFloat["groundShadowDensity"] = v
		end
	end

	if gl.HasExtension("GL_ARB_texture_non_power_of_two") then
		uniformsFloat["normalTexGen"] = { 1.0/Game.mapSizeX, 1.0/Game.mapSizeZ }
	else
		uniformsFloat["normalTexGen"] = { 1.0/pwr2mapx, 1.0/pwr2mapy }
	end

	uniformsFloat["specularTexGen"] = { 1.0/Game.mapSizeX, 1.0/Game.mapSizeZ }

	uniformsFloat["infoTexIntensityMul"] = ((Spring.GetMapDrawMode() == "metal") and 1.0 or 0.0) + 1.0

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

local shaderObj
function gadget:Initialize()

	local definitions, uniformsFloat, uniformsInt, textures = GetFlagsTexturesUniforms()

	--GG.TableEcho({ definitions, uniformsFloat, uniformsInt, textures }, "GetFlagsTexturesUniforms")


	local vertCode = VFS.LoadFile("PBR/pbrMap.vert", VFS.MAP)
	local fragCode = VFS.LoadFile("PBR/pbrMap.frag", VFS.MAP)

	--Spring.Echo(vertCode, fragCode)

	shaderObj = LuaShader({
		definitions = definitions,
		vertex = vertCode,
		fragment = fragCode,
		uniformFloat = uniformsFloat,
		uniformInt = uniformsInt,

	}, "PBR Map Shader (Forward)")
	shaderObj:Initialize()

	Spring.SetMapShader(shaderObj:GetHandle(), 0)
end

local oldSunPos = {-1, -1, -1}
local updateSunPos
function gadget:Update(dt)
	local newSunX, newSunY, newSunZ = gl.GetSun("pos")
	if (newSunX ~= oldSunPos[1] or newSunY ~= oldSunPos[2] or newSunZ ~= oldSunPos[3]) then
		oldSunPos = { newSunX, newSunY, newSunZ }
		updateSunPos = true
	end
end

local updateHeights = true
function gadget:UnsyncedHeightMapUpdate()
	updateHeights = true
end

function gadget:DrawWorldPreUnit()

end

local firstTime = true
function gadget:DrawGenesis()
	if firstTime then
		--blabla
	end

	if updateHeights then
		shaderObj:ActivateWith( function()
			CallAsTeam(Spring.GetMyTeamID(),  function()
				local minH, maxH = Spring.GetGroundExtremes()
				shaderObj:SetUniformFloat("mapHeights", minH, maxH)
			end)
		end)
		updateHeights = false
	end

	if updateSunPos then
		Spring.Echo("updateSunPos!!!")
		shaderObj:ActivateWith( function()
			shaderObj:SetUniformMatrixAlways("shadowMat", gl.GetMatrixData("shadow"))
			shaderObj:SetUniformFloat("shadowParams", gl.GetShadowMapParams())
			local sunPosX, sunPosY, sunPosZ = gl.GetSun("pos")
			shaderObj:SetUniformFloat("lightDir", sunPosX, sunPosY, sunPosZ, 0.0)
		end)
		updateSunPos = false
	end

	shaderObj:ActivateWith( function()
		local cameraX, cameraY, cameraZ = Spring.GetCameraPosition()
		shaderObj:SetUniformFloat("cameraPos", cameraX, cameraY, cameraZ)
	end)

end

function gadget:Shutdown()
	shaderObj:Finalize()
	Spring.SetMapShader(0, 0)
end