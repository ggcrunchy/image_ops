--- A JPEG loader and associated functionality.

--
-- Permission is hereby granted, free of charge, to any person obtaining
-- a copy of this software and associated documentation files (the
-- "Software"), to deal in the Software without restriction, including
-- without limitation the rights to use, copy, modify, merge, publish,
-- distribute, sublicense, and/or sell copies of the Software, and to
-- permit persons to whom the Software is furnished to do so, subject to
-- the following conditions:
--
-- The above copyright notice and this permission notice shall be
-- included in all copies or substantial portions of the Software.
--
-- THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
-- EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
-- MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
-- IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
-- CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
-- TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
-- SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
--
-- [ MIT license: http://www.opensource.org/licenses/mit-license.php ]
--

-- Standard library imports --
local byte = string.byte
local char = string.char
local sub = string.sub

-- Modules --
local image_utils = require("image_ops.utils")

-- Imports --
local ReadU16 = image_utils.ReadU16

-- Exports --
local M = {}

--
local function ReadHeader (str, pos)
	while true do
		if byte(str, pos) ~= 0xFF then
			return false
		else
			local code = byte(str, pos + 1)

			if code == 0xC0 or code == 0xC1 then
				local h = ReadU16(str, pos + 5)
				local w = ReadU16(str, pos + 7)
				local ncomps = byte(str, pos + 9)
				local nr, ng, nb = byte(str, pos + 10)

				if ncomps == 3 then
					ng = byte(str, pos + 11)
					nb = byte(str, pos + 12)
				else
					ng, nb = 0, 0
				end

				return true, w, h, ncomps, nr, ng, nb
			elseif code == 0xD9 then
				return false
			end
		end

		pos = pos + ReadU16(str, pos + 2) + 2
	end
end

--
local function AuxInfo (str)
	if sub(str, 1, 2) == char(0xFF, 0xD8) then
		return ReadHeader(str, 3)
	end

	return false
end

--- DOCME
function M.GetInfo (name)
	return image_utils.ReadHeader(name, "*a", AuxInfo)
end

--- DOCME
M.GetInfoString = AuxInfo

-- Export the module.
return M