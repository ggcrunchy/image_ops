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
local floor = math.floor
local ipairs = ipairs
local max = math.max
local min = math.min
local sub = string.sub

-- Modules --
local image_utils = require("image_ops.utils")
local operators = require("bitwise_ops.operators")

-- Imports --
local bnot = operators.bnot
local ReadU16 = image_utils.ReadU16

-- Exports --
local M = {}

-- --
local Component = { "Y", "Cb", "Cr", "I", "Q" }

--
local function Nybbles (n)
	local low = n % 16

	return (n - low) * 2^-4, low
end

--
local function GetStartOfFrame (str, pos, get_data)
	local nbits, data = byte(str, pos)
	local h = ReadU16(str, pos + 1)
	local w = ReadU16(str, pos + 3)

	if get_data then
		data, pos = { nbits = nbits }, pos + 5

		for i = 1, byte(str, pos) * 3, 3 do
			local horz, vert = Nybbles(byte(str, pos + i + 1))

			data[#data + 1] = {
				id = Component[byte(str, pos + i)],
				horz_samp_factor = horz, vert_samp_factor = vert,
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

--
local function OnByte (cur_byte, jpeg, pos)
	if cur_byte == 0xFF then
		local next_byte = byte(jpeg, pos + 1)

		if next_byte == 0x00 then
			return pos + 1
		elseif next_byte == 0xFF then
			-- Fills, skip...
		else
			-- Segment :(
		end
	end
end

--
local function Decode (get_bit, ht)
	local code = 0

	for nbits = 1, 16 do
		local symbols = ht[nbits]

		code = get_bit(code)

		if symbols then
			local min_code = symbols.min_code

			if code < min_code + #symbols then
				return symbols[code - min_code + 1]
			end
		end
	end
end

--
local function Receive (get_bit, n)
	local bits = 0

	for _ = 1, n do
		bits = get_bit(bits)
	end

	return bits
end

--
local function Extend (v, t)
	if v < 2^(t - 1) then
		v = v - bnot(0xFFFFFFFF * 2^t)
	end

	return v
end

--
local function DHT (jpeg, from)
	local ht, spos, code = {}, from + 17, 0

	for i = 1, 16 do
		local n = byte(jpeg, from + i)

		if n > 0 then
			local symbols = { min_code = code }

			for j = 1, n do
				symbols[j], spos = byte(jpeg, spos), spos + 1
			end

			ht[i], code = symbols, code + n
		end

		code = 2 * code
	end

	return ht, spos
end

--
local function DQT (jpeg, from, qbyte)
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
-- ^^ TODO: Zagzig?
	return qt, from + (is_16bit and 128 or 64) + 1
end

-- --
local Cos = {}

do
	local Sqrt2_2 = .25 * math.sqrt(2)

	for x = 0, 7 do
		local k = 2 * x + 1

		Cos[#Cos + 1] = Sqrt2_2

		for u = 1, 7 do
			Cos[#Cos + 1] = .5 * math.cos(k * u * math.pi / 16)
		end
	end
end

-- --
local Zagzig = {
    0,  1,  8, 16,  9,  2,  3, 10,
   17, 24, 32, 25, 18, 11,  4,  5,
   12, 19, 26, 33, 40, 48, 41, 34,
   27, 20, 13,  6,  7, 14, 21, 28,
   35, 42, 49, 56, 57, 50, 43, 36,
   29, 22, 15, 23, 30, 37, 44, 51,
   58, 59, 52, 45, 38, 31, 39, 46,
   53, 60, 61, 54, 47, 55, 62, 63
}

-- --
local ZZ, Q = {}, {}

--
local function FillZeroes (k, n)
	for i = k, k + n - 1 do
		ZZ[i] = 0
	end

	return k + n
end

--
local function ReadMCU (get_bit, scans, preds, n, yfunc)
	local up_to = 64

	for i = 1, n do
		local scan = scans[i]
		local dht, aht, qt, hsf = scan.dht, scan.aht, scan.qt, scan.hsf

		for y = 1, scan.vsf do
			for x = 1, hsf do
				--
				local s = Decode(get_bit, dht)

				local extra, r = s % 16, 0

				if extra > 0 then
					r = Receive(get_bit, extra)
				end

				preds[i] = preds[i] + Extend(r, s)

				--
				local k, maxk, size = 1, 0, 64

				while k < 64 do
					local nzeroes, nbits = Nybbles(Decode(get_bit, aht))

					if nbits > 0 then
						k = FillZeroes(k, nzeroes)
						ZZ[k], k, maxk = Extend(Receive(get_bit, nbits), nbits), k + 1, k
					elseif nzeroes == 0xF then
						k = FillZeroes(k, 16)
					else
						local kp7 = k + 7

						size = kp7 - kp7 % 8

						FillZeroes(k, size - k)

						break
					end
				end

				--
				Q[1] = preds[i] * qt[1]

				for j = 1, size - 1 do
					Q[Zagzig[j + 1] + 1] = qt[j + 1] * ZZ[j]
				end

				for j = size, 63 do
					Q[Zagzig[j + 1] + 1] = 0
				end

				--
				local index = 1

				for y = 1, size, 8 do
					for x = 1, 64, 8 do
						local sum, j = 0, 1

						for v = y, y + 7 do
							local cosvy = Cos[v]

							for u = x, x + 7 do
								sum, j = sum + Cos[u] * cosvy * Q[j], j + 1
							end
						end

						ZZ[index], index = min(max(sum + 128, 0), 255), index + 1
					end
				end

				--
				for i = index, up_to do
					ZZ[i] = 0
				end

				up_to = index - 1

				--
				yfunc()
			end
		end
	end
end

--
local function SetupScan (jpeg, from, state, dhtables, ahtables, qtables, n)
	local scans, preds = {}, {}

	for i = 1, n do
		local comp = Component[byte(jpeg, from + 1)]

		for _, scomp in ipairs(state) do
			if comp == scomp.id then
				local di, ai = Nybbles(byte(jpeg, from + 2))

				scans[i], preds[i] = {
					hsf = scomp.horz_samp_factor, vsf = scomp.vert_samp_factor,
					dht = dhtables[di + 1], aht = ahtables[ai + 1],
					qt = qtables[scomp.quantization_table + 1]
				}, 0

				break
			end
		end

		from = from + 2
	end

	return scans, preds
end

-- Default yield function: no-op
local function DefYieldFunc () end

--
local function AuxLoad (jpeg, yfunc)
	local state, w, h

	assert(sub(jpeg, 1, 2) == char(0xFF, 0xD8), "Image is not a JPEG")

	yfunc = yfunc or DefYieldFunc

	local pos, total, ahtables, dhtables, qtables, pixels, restart = 3, #jpeg, {}, {}, {}

	while true do
		assert(byte(jpeg, pos) == 0xFF, "Not a segment")

		local code = byte(jpeg, pos + 1)

		if code == 0xD9 then
			break
		end

		local from, len = pos + 4, ReadU16(jpeg, pos + 2)
		local next_pos = pos + len + 2

		-- Start Of Frame --
		if code == 0xC0 or code == 0xC1 then
			w, h, state = GetStartOfFrame(jpeg, from, true)

			for _, comp in ipairs(state) do
				state.hmax = max(state.hmax or 0, comp.horz_samp_factor)
				state.vmax = max(state.vmax or 0, comp.vert_samp_factor)
			end

		-- Define Huffman Table --
		elseif code == 0xC4 then
			repeat
				local hbyte, ht, spos = byte(jpeg, from), DHT(jpeg, from)

				if hbyte >= 16 then
					ahtables[hbyte - 15] = ht
				else
					dhtables[hbyte + 1] = ht
				end

				from = spos
			until from == next_pos

		-- Start Of Scan --
		elseif code == 0xDA then
			--
			local n = byte(jpeg, from)
			local scans, preds = SetupScan(jpeg, from, state, dhtables, ahtables, qtables, n)

			--
			local get_bit, get_pos = image_utils.BitReader(jpeg, from + 2 * n + 4, OnByte)
			local hsize, vsize = state.hmax * 8, state.vmax * 8
			local nx, ny = floor((w + hsize - 1) / hsize), floor((h + vsize - 1) / vsize)

			for _ = 1, nx * ny do
				ReadMCU(get_bit, scans, preds, n, yfunc)
			end

			next_pos = get_pos() + 1

		-- Define Quantization table --
		elseif code == 0xDB then
			repeat
				local qbyte = byte(jpeg, from)

				qtables[qbyte % 16 + 1], from = DQT(jpeg, from, qbyte)
			until from == next_pos

		-- Define Restart Interval --
		elseif code == 0xDD then
			restart = ReadU16(jpeg, from)
		end

		pos = next_pos

		assert(pos <= total, "Incomplete or corrupt JPEG file")
	end

	--
	local JPEG = {}

	--- DOCME
	function JPEG:ForEach (func, arg)
	--	pixels = pixels or Decode()

		image_utils.ForEach(pixels, w, h, func, nil, arg)
	end

	--- DOCME
	function JPEG:ForEach_OnRow (func, on_row, arg)
	--	pixels = pixels or Decode()

		image_utils.ForEach(pixels, w, h, func, on_row, arg)
	end

	--- DOCME
	function JPEG:ForEachInColumn (func, col, arg)
	--	pixels = pixels or Decode()

		image_utils.ForEachInColumn(pixels, w, h, func, col, arg)
	end

	--- DOCME
	function JPEG:ForEachInRow (func, row, arg)
	--	pixels = pixels or Decode()

		image_utils.ForEachInRow(pixels, w, func, row, arg)
	end

	--- DOCME
	function JPEG:GetDims ()
		return w, h
	end

	--- DOCME
	function JPEG:GetPixels ()
	--	pixels = pixels or Decode()

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