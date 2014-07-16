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
local assert = assert
local byte = string.byte
local char = string.char
local sub = string.sub

-- Modules --
local image_utils = require("image_ops.utils")

-- Imports --
local ReadU16 = image_utils.ReadU16

-- Exports --
local M = {}

-- --
local Component = { "Y", "Cb", "Cr", "I", "Q" }

--
local function GetStartOfFrame (str, pos, get_data)
	local nbits, data = byte(str, pos)
	local h = ReadU16(str, pos + 1)
	local w = ReadU16(str, pos + 3)

	if get_data then
		data, pos = { nbits = nbits }, pos + 5

		for i = 1, byte(str, pos) * 3, 3 do
			local factors = byte(str, pos + i + 1)
			local vert = factors % 16

			data[#data + 1] = {
				id = Component[byte(str, pos + i)],
				horz_samp_factor = .125 * (factors - vert),
				vert_samp_factor = vert,
				quantization_table = byte(str, pos + i + 2)
			}
		end
	end

	return w, h, data
end

--
local function ReadHeader (str, pos, get_data)
	while true do
		if byte(str, pos) ~= 0xFF then
			return false
		else
			local code = byte(str, pos + 1)

			-- Start Of Frame --
			if code == 0xC0 or code == 0xC1 then
				return true, GetStartOfFrame(str, pos + 4, get_data)

			-- End Of Image --
			elseif code == 0xD9 then
				return false
			end
		end

		pos = pos + ReadU16(str, pos + 2) + 2
	end
end

--
local function AuxInfo (str, get_data)
	if sub(str, 1, 2) == char(0xFF, 0xD8) then
		return ReadHeader(str, 3, get_data)
	end

	return false
end

--- DOCME
function M.GetInfo (name, get_data)
	return image_utils.ReadHeader(name, "*a", AuxInfo, get_data)
end

--- DOCME
M.GetInfoString = AuxInfo

--[[
	Decoding:

	http://en.wikipedia.org/wiki/JPEG
	http://kd5col.info/swag/GRAPHICS/0143.PAS.html
	http://vip.sugovica.hu/Sardi/kepnezo/JPEG%20File%20Layout%20and%20Format.htm
	http://www.xbdev.net/image_formats/jpeg/tut_jpg/jpeg_file_layout.php
]]

-- Default yield function: no-op
local function DefYieldFunc () end

-- --
local Stream, State = {}, {}

-- --
local function ReadStream (ht)
--	image_utils.Save
end

--
local function AuxLoad (jpeg, yfunc)
	local state, w, h

	assert(sub(jpeg, 1, 2) == char(0xFF, 0xD8), "Image is not a JPEG")

	yfunc = yfunc or DefYieldFunc

	local pos, total, pixels, ahtables, dhtables, qtables = 3, #jpeg

	while true do
		assert(byte(jpeg, pos) == 0xFF, "Not a segment")

		local code, from = byte(jpeg, pos + 1), pos + 4

		-- Start Of Frame --
		if code == 0xC0 or code == 0xC1 then
			w, h, state = GetStartOfFrame(jpeg, from, true)

		-- Huffman Table --
		elseif code == 0xC4 then
			ahtables, dhtables = ahtables or {}, dhtables or {}

			--
			local hbyte, ht = byte(jpeg, from), {}

			for i = 1, 16 do
				local count = byte(jpeg, from)

				ht[i], from = count, from + 1
			end

			--
			for i = 1, 16 do
				local symbols = {}

				for j = 1, ht[i] do
					symbols[j], from = byte(jpeg, from), from + 1
				end

				ht[i] = symbols
			end

			--
			if hbyte >= 16 then
				ahtables[hbyte - 15] = ht
			else
				dhtables[hbyte + 1] = ht
			end

		-- Start Of Scan --
		elseif code == 0xDA then
			--
			local n = byte(jpeg, from)

			Stream.m_bytes = jpeg
			Stream.m_bytes_pos = from + 2 * n + 5
			Stream.m_code_buf = 0
			Stream.m_code_size = 0

			for _ = 1, n do
				local comp, ti = Component[byte(jpeg, from + 1)], byte(jpeg, from + 2)
				local ac = ti % 16

				--
				ReadStream(dhtables[(ti - ac) * 2^-4 + 1])
				-- number of bits, value

				--
				local aht = ahtables[ac + 1]

				for _ = 1, 64 do
					ReadStream(aht)

					-- number of bits, code = number of zeros, number of bytes (bits?)
				end

				from = from + 2
			end
--[[
SOS                             16                    0xffd8              Start Of Scan
Ls                                16                    2Ns + 6           Scan header length
Ns                                8                      1-4                   Number of image components
Csj                               8                      0-255               Scan Component Selector
Tdj                               4                      0-1                   DC Coding Table Selector
Taj                               4                      0-1                   AC Coding Table Selector
Ss                                8                      0                      Start of spectral selection
Se                                8                      63                    End of spectral selection
Ah                                4                      0                      Successive Approximation Bit High
Ai                                 4                      0                      Successive Approximation Bit Low
-------------------------------------------------------
~~~~~ After the SOS Header Information follows the encoded data which is in the following format.~~~~~~~
 
[dc huf][value]  [ac huf][value]  [ac huf] [value]  [ac huf] [value]…..
repeats for each component.
]]

		-- Quantization table --
		elseif code == 0xDB then
			qtables = qtables or {}

			local qbyte = byte(jpeg, from)
			local is_16bit = qbyte > 16
			local qt = { is_16bit = is_16bit }

			if is_16bit then
				for pos = from + 1, from + 128, 2 do
					local a, b = byte(jpeg, pos, pos + 1)

					qt[#qt + 1] = a * 2^8 + b
				end
			else
				for i = 1, 64 do
					qt[i] = byte(jpeg, from + i)
				end
			end

			qtables[qbyte % 16 + 1] = qt

		-- End Of Image --
		elseif code == 0xD9 then
			break
		end

		pos = pos + ReadU16(jpeg, pos + 2) + 2

		assert(pos <= total, "Incomplete or corrupt JPEG file")
	end

	--
	local function Decode ()
--[[
		local decoded = DecodePixels(data, state, w, yfunc)

		decoded, data = CopyToImageData(decoded, state, w * h * 4, yfunc)

		return decoded
]]
	end

	--
	local JPEG = {}

	--- DOCME
	function JPEG:ForEach (func, arg)
		pixels = pixels or Decode()

		image_utils.ForEach(pixels, w, h, func, nil, arg)
	end

	--- DOCME
	function JPEG:ForEach_OnRow (func, on_row, arg)
		pixels = pixels or Decode()

		image_utils.ForEach(pixels, w, h, func, on_row, arg)
	end

	--- DOCME
	function JPEG:ForEachInColumn (func, col, arg)
		pixels = pixels or Decode()

		image_utils.ForEachInColumn(pixels, w, h, func, col, arg)
	end

	--- DOCME
	function JPEG:ForEachInRow (func, row, arg)
		pixels = pixels or Decode()

		image_utils.ForEachInRow(pixels, w, func, row, arg)
	end

	--- DOCME
	function JPEG:GetDims ()
		return w, h
	end

	--- DOCME
	function JPEG:GetPixels ()
		pixels = pixels or Decode()

		return pixels
	end

	--- DOCME
	function JPEG:SetYieldFunc (func)
		yfunc = func or DefYieldFunc
	end

	return JPEG
end

--- DOCME
function M.Load (name, yfunc)
	return image_utils.Load(name, AuxLoad, yfunc)
end

--- DOCME
M.LoadString = AuxLoad

-- Export the module.
return M